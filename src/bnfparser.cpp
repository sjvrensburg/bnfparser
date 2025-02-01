#include "bnfparser.h"
#include <Rcpp.h>
#include <fstream>
#include <sstream>
#include <any>
#include <optional>
#include <regex>

BNFParser::BNFParser(const ParserConfig& cfg) : config(cfg) {
  setupErrorHandling();

  std::string grammar = R"(
      Grammar       <-  Spacing Definition* EndOfFile
      Definition    <-  Comment / Production
      Production    <-  NonTerminal Spacing ASSIGN Spacing Expression
      Expression    <-  Sequence (PIPE Sequence)*
      Sequence      <-  Term+
      Term          <-  (NonTerminal / Terminal / Operator) Spacing

      NonTerminal   <-  '<' [a-zA-Z_][a-zA-Z0-9_]* '>' Spacing
      Terminal      <-  (QuotedString / UnquotedTerminal) Spacing
      QuotedString  <-  < ['] (!['] .)* ['] > /
                        < ["] (!["] .)* ["] >
      UnquotedTerminal <- < !('<' / [0-9]) [a-zA-Z][a-zA-Z0-9_]* >

      Operator      <-  < [-+*/] > !Spacing

      ASSIGN       <-  '::='
      PIPE        <-  '|'
      Comment     <-  '#' (![\n] .)* [\n]?
      Spacing     <-  [ \t\r\n]*
      EndOfFile   <-  !.
  )";

  if (!parser.load_grammar(grammar)) {
    throw std::runtime_error("Failed to load grammar definition");
  }

  setupSemanticActions();
}

void BNFParser::setupSemanticActions() {
  parser["Production"] = [this](const peg::SemanticValues& sv) {
    try {
      RAIICurrentRule raii(*this);

      // Get the nonterminal name (sv[0])
      std::string non_terminal = std::any_cast<std::string>(sv[0]);
      current_rule->raw_name = non_terminal;
      current_rule->name = current_rule->raw_name.substr(1, current_rule->raw_name.length() - 2);
      current_rule->line_number = sv.line_info().first;

      // Get the expression (sv[1])
      if (const auto& alts = std::any_cast<std::vector<std::string>>(sv[1]); !alts.empty()) {
        current_rule->alternatives = alts;
      }

      if (validateRule(*current_rule)) {
        rules.push_back(*current_rule);
        return true;
      }
      return false;
    } catch (const std::exception& e) {
      error_log << "Line " << sv.line_info().first
                << ": Error processing rule: " << e.what() << "\n";
      return false;
    }
  };

  parser["Expression"] = [](const peg::SemanticValues& sv) -> std::any {
    std::vector<std::string> alts;
    // First sequence
    alts.push_back(std::any_cast<std::string>(sv[0]));
    // Rest of sequences (if any)
    for (size_t i = 1; i < sv.size(); i++) {
      alts.push_back(std::any_cast<std::string>(sv[i]));
    }
    return alts;
  };

  parser["Sequence"] = [](const peg::SemanticValues& sv) -> std::any {
    std::string seq;
    for (size_t i = 0; i < sv.size(); i++) {
      if (i > 0) seq += " ";
      seq += std::any_cast<std::string>(sv[i]);
    }
    return seq;
  };

  parser["Term"] = [](const peg::SemanticValues& sv) -> std::any {
    return std::string(sv.token());
  };

  parser["NonTerminal"] = [this](const peg::SemanticValues& sv) -> std::any {
    // Get full token including < and >
    std::string token = std::string(sv.token());

    // Extract inner name for references
    std::string name = token.substr(1, token.length() - 2);
    if (current_rule) {
      current_rule->references.insert(name);
    }

    return token; // Return the full <...> string
  };

  parser["Terminal"] = [this](const peg::SemanticValues& sv) -> std::any {
    if (config.strict_mode && sv.choice() == 1) {
      error_log << "Line " << sv.line_info().first
                << ": Unquoted terminal '" << std::string(sv.token()) << "'\n";
      return std::string();
    }
    return std::string(sv.token());
  };

  // Basic token actions
  parser["IdContent"] = [](const peg::SemanticValues& sv) -> std::any {
    return std::string(sv.token());
  };

  parser["ASSIGN"] = [](const peg::SemanticValues& /*sv*/) -> std::any {
    return std::string("::=");
  };

  parser["PIPE"] = [](const peg::SemanticValues& /*sv*/) -> std::any {
    return std::string("|");
  };

  parser["Operator"] = [](const peg::SemanticValues& sv) -> std::any {
    return std::string(sv.token());
  };

  parser["Number"] = [](const peg::SemanticValues& sv) -> std::any {
    return std::string(sv.token());
  };
}

std::string BNFParser::trim(const std::string& str) {
  if (str.empty()) return str;
  const auto start = str.find_first_not_of(" \t\r\n");
  if (start == std::string::npos) return "";
  const auto end = str.find_last_not_of(" \t\r\n");
  return str.substr(start, end - start + 1);
}

bool BNFParser::detectLeftRecursion(const std::vector<Rule>& rules) {
  std::map<std::string, std::set<std::string>> dependencies;

  // Build dependency graph
  for (const auto& rule : rules) {
    dependencies[rule.name] = rule.references;
  }

  // Check each rule for left recursion
  for (const auto& rule : rules) {
    std::set<std::string> visited;
    std::vector<std::string> path;
    path.push_back(rule.name);

    std::function<bool(const std::string&)> dfs = [&](const std::string& current) {
      if (std::find(path.begin(), path.end(), current) != path.begin()) {
        if (current == rule.name) {
          error_log << "Left recursion detected at line " << rule.line_number
                    << ":\n  " << buildRecursionPath(path) << "\n";
          return true;
        }
      }

      if (visited.count(current)) {
        return false;
      }

      visited.insert(current);
      path.push_back(current);

      for (const auto& dep : dependencies[current]) {
        if (dfs(dep)) {
          return true;
        }
      }

      path.pop_back();
      return false;
    };

    if (dfs(rule.name)) {
      return true;
    }
  }
  return false;
}

std::string BNFParser::buildRecursionPath(const std::vector<std::string>& path) {
  std::string result;
  for (size_t i = 0; i < path.size(); ++i) {
    if (i > 0) result += " → ";
    result += "<" + path[i] + ">";
  }
  return result;
}

bool BNFParser::validateReferences() {
  std::set<std::string> defined_rules;
  for (const auto& rule : rules) {
    defined_rules.insert(rule.name);
  }

  for (const auto& rule : rules) {
    for (const auto& ref : rule.references) {
      if (defined_rules.find(ref) == defined_rules.end()) {
        error_log << "Line " << rule.line_number
                  << ": Undefined rule referenced: '" << ref
                  << "' in rule '" << rule.name << "'\n";
        return false;
      }
    }
  }
  return true;
}

bool BNFParser::validateRule(const Rule& rule) {
  if (rule.name.empty()) {
    error_log << "Line " << rule.line_number << ": Empty non-terminal name found\n";
    return false;
  }

  // Validate rule name format
  static const std::regex name_pattern("[a-zA-Z_][a-zA-Z0-9_]*");
  if (!std::regex_match(rule.name, name_pattern)) {
    error_log << "Line " << rule.line_number
              << ": Invalid rule name format: '" << rule.name << "'\n";
    return false;
  }

  // Check for empty alternatives
  if (!config.allow_empty_rules) {
    for (const auto& alt : rule.alternatives) {
      if (trim(alt).empty()) {
        error_log << "Line " << rule.line_number
                  << ": Empty alternative found in rule '" << rule.name << "'\n";
        return false;
      }
    }
  }

  // Validate EBNF constructs
  if (!config.allow_ebnf) {
    static const std::string ebnf_chars = "[]{}*+?";
    for (const auto& alt : rule.alternatives) {
      for (char c : ebnf_chars) {
        if (alt.find(c) != std::string::npos) {
          error_log << "Line " << rule.line_number
                    << ": EBNF construct '" << c
                    << "' found in rule '" << rule.name
                    << "' but EBNF is disabled\n";
          return false;
        }
      }
    }
  }

  // Check for duplicate alternatives
  std::set<std::string> unique_alts;
  for (const auto& alt : rule.alternatives) {
    const std::string trimmed = trim(alt);
    if (!unique_alts.insert(trimmed).second) {
      error_log << "Line " << rule.line_number
                << ": Duplicate alternative '" << trimmed
                << "' found in rule '" << rule.name << "'\n";
      return false;
    }
  }

  return true;
}

bool BNFParser::parse(const std::string& input) {
  rules.clear();
  error_log.str("");
  error_log.clear();

  try {
    bool success = parser.parse(input);
    if (!success) {
      error_log << "Failed to parse input\n";
      return false;
    }

    if (config.detect_left_recursion && detectLeftRecursion(rules)) {
      return false;
    }

    if (config.validate_references && !validateReferences()) {
      return false;
    }

    return true;
  } catch (const std::exception& e) {
    error_log << "Exception during parsing: " << e.what() << "\n";
    return false;
  }
}

void BNFParser::setupErrorHandling() {
  // Enable packrat parsing for better performance
  parser.enable_packrat_parsing();

  // Set up logging for errors and diagnostics - note the corrected signature
  parser.set_logger([this](size_t line, size_t col, const std::string& msg) {
    error_log << "Line " << line << ", Column " << col;
    error_log << ": " << msg << "\n";
  });

  // Enable verbose trace output with correct verbose trace signature
  if (config.enableTracing) {
    parser.enable_trace(
      [this](const peg::Ope &ope, const char *s, size_t /*n*/,
             const peg::SemanticValues &vs, const peg::Context &c,
             const std::any &dt, std::any &trace_data) {
        auto pos = static_cast<size_t>(s - c.s);
        error_log << "ENTER [" << pos << "] Rule\n";
      },
      [this](const peg::Ope &ope, const char *s, size_t /*n*/,
             const peg::SemanticValues &vs, const peg::Context &c,
             const std::any &dt, size_t len, std::any &trace_data) {
        auto pos = static_cast<size_t>(s - c.s);
        error_log << "LEAVE [" << pos << "] Rule (len=" << len << ")\n";
      }
    );
  }
}

std::string BNFParser::getErrorLog() const {
  return error_log.str();
}

const std::vector<Rule>& BNFParser::getRules() const {
  return rules;
}

// [[Rcpp::export]]
Rcpp::List parseBNF(const std::string& filepath,
                    bool strict_mode = false,
                    bool preserve_whitespace = true,
                    bool allow_empty_rules = false,
                    bool detect_left_recursion = true,
                    bool allow_ebnf = true,
                    bool validate_references = true) {
  try {
    std::ifstream file(filepath);
    if (!file.is_open()) {
      Rcpp::stop("Could not open file: " + filepath);
    }
    std::stringstream buffer;
    buffer << file.rdbuf();

    ParserConfig config;
    config.strict_mode = strict_mode;
    config.preserve_whitespace = preserve_whitespace;
    config.allow_empty_rules = allow_empty_rules;
    config.detect_left_recursion = detect_left_recursion;
    config.allow_ebnf = allow_ebnf;
    config.validate_references = validate_references;

    BNFParser parser(config);

    if (!parser.parse(buffer.str())) {
      Rcpp::stop("Failed to parse BNF grammar:\n" + parser.getErrorLog());
    }

    // Convert to R list
    Rcpp::List result;
    for (const auto& rule : parser.getRules()) {
      Rcpp::CharacterVector alts(rule.alternatives.begin(), rule.alternatives.end());
      result[rule.name] = alts;
    }

    // Add metadata
    result.attr("class") = "bnf_grammar";
    result.attr("strict_mode") = config.strict_mode;
    result.attr("preserve_whitespace") = config.preserve_whitespace;
    result.attr("allow_ebnf") = config.allow_ebnf;

    return result;

  } catch (const std::exception& e) {
    Rcpp::stop(std::string("Error parsing BNF grammar: ") + e.what());
  }
}

// [[Rcpp::export]]
Rcpp::List parseBNFString(const std::string& grammar_string,
                          bool strict_mode = false,
                          bool preserve_whitespace = true,
                          bool allow_empty_rules = false,
                          bool detect_left_recursion = true,
                          bool allow_ebnf = true,
                          bool validate_references = true) {
  try {
    ParserConfig config;
    config.strict_mode = strict_mode;
    config.preserve_whitespace = preserve_whitespace;
    config.allow_empty_rules = allow_empty_rules;
    config.detect_left_recursion = detect_left_recursion;
    config.allow_ebnf = allow_ebnf;
    config.validate_references = validate_references;
    config.enableTracing = true; // Enable for debugging

    BNFParser parser(config);

    bool success = parser.parse(grammar_string);
    if (!success) {
      std::string error_log = parser.getErrorLog();
      if (error_log.empty()) {
        error_log = "Unknown parsing error occurred";
      }
      Rcpp::stop("Failed to parse BNF grammar:\n%s", error_log);
    }

    // Convert to R list with proper error handling
    Rcpp::List result;
    for (const auto& rule : parser.getRules()) {
      std::vector<std::string> alts(rule.alternatives.begin(),
                                    rule.alternatives.end());
      result[rule.name] = Rcpp::wrap(alts);
    }

    // Add metadata safely
    result.attr("class") = "bnf_grammar";
    result.attr("strict_mode") = config.strict_mode;
    result.attr("preserve_whitespace") = config.preserve_whitespace;
    result.attr("allow_ebnf") = config.allow_ebnf;

    return result;
  } catch (const std::exception& e) {
    Rcpp::stop("Error parsing BNF grammar: %s", e.what());
  } catch (...) {
    Rcpp::stop("Unknown error occurred while parsing BNF grammar");
  }
}
