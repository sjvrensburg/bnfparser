#include <Rcpp.h>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/support_line_pos_iterator.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/depth_first_search.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/phoenix/bind.hpp>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <stdexcept>

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;
namespace phoenix = boost::phoenix;

// Data structures
struct Rule {
  std::string name;
  std::vector<std::vector<std::string>> alternatives;
  size_t line_number;
  std::set<std::string> references;

  Rule() : line_number(0) {}
};

// Adapt the struct for Fusion
BOOST_FUSION_ADAPT_STRUCT(
  Rule,
  (std::string, name)
  (std::vector<std::vector<std::string>>, alternatives)
  (size_t, line_number)
  (std::set<std::string>, references)
)

struct ValidationResult {
  bool valid;
  std::string message;

  ValidationResult(bool v = true, const std::string& msg = "")
    : valid(v), message(msg) {}
};

// Grammar validator class
class GrammarValidator {
private:
  using Graph = boost::adjacency_list<
    boost::vecS,
    boost::vecS,
    boost::directedS>;
  using Vertex = boost::graph_traits<Graph>::vertex_descriptor;

  struct CycleDetector : public boost::dfs_visitor<> {
    bool& has_cycle;

    CycleDetector(bool& cycle) : has_cycle(cycle) {}

    template <typename Edge, typename Graph>
    void back_edge(Edge, const Graph&) {
      has_cycle = true;
    }
  };

public:
  // Validates the entire grammar
  static ValidationResult validate(const std::vector<Rule>& rules, const bool strict_mode = false) {
    // 1. Check for empty grammar
    if (rules.empty()) {
      return ValidationResult(false, "Grammar contains no rules");
    }

    // 2. Validate start symbol
    auto start_result = validateStartSymbol(rules);
    if (!start_result.valid) {
      return start_result;
    }

    // 3. Validate references
    auto ref_result = validateReferences(rules);
    if (!ref_result.valid) {
      return ref_result;
    }

    // 4. Check for left recursion
    auto rec_result = validateLeftRecursion(rules);
    if (!rec_result.valid) {
      return rec_result;
    }

    // 5. Validate operators
    auto op_result = validateOperators(rules);
    if (!op_result.valid) {
      return op_result;
    }

    // 6. Validate symbols
    auto symb_result = validateSymbols(rules, strict_mode);
    if (!symb_result.valid) {
      return symb_result;
    }

    return ValidationResult(true);
  }

private:
  static ValidationResult validateStartSymbol(const std::vector<Rule>& rules) {
    bool found = false;
    for (const auto& rule : rules) {
      if (rule.name == "<start>") {
        found = true;
        break;
      }
    }

    if (!found) {
      return ValidationResult(false, "Start symbol '<start>' not found in grammar");
    }

    return ValidationResult(true);
  }

  static ValidationResult validateReferences(const std::vector<Rule>& rules) {
    std::set<std::string> defined_symbols;
    std::map<std::string, size_t> symbol_lines;

    // First pass: collect all defined symbols
    for (const auto& rule : rules) {
      defined_symbols.insert(rule.name);
      symbol_lines[rule.name] = rule.line_number;
    }

    // Second pass: check references
    for (const auto& rule : rules) {
      for (const auto& ref : rule.references) {
        if (defined_symbols.find(ref) == defined_symbols.end()) {
          return ValidationResult(false,
                                  "Undefined symbol '" + ref + "' referenced in rule '" +
                                    rule.name + "' at line " + std::to_string(rule.line_number));
        }
      }
    }

    return ValidationResult(true);
  }

  static ValidationResult validateLeftRecursion(const std::vector<Rule>& rules) {
    std::map<std::string, size_t> rule_indices;
    for (size_t i = 0; i < rules.size(); ++i) {
      rule_indices[rules[i].name] = i;
    }

    Graph g(rules.size());

    // Build dependency graph
    for (const auto& rule : rules) {
      size_t from = rule_indices[rule.name];

      // Check immediate left recursion
      for (const auto& alt : rule.alternatives) {
        if (!alt.empty() && alt[0] == rule.name) {
          return ValidationResult(false,
                                  "Direct left recursion detected in rule '" + rule.name +
                                    "' at line " + std::to_string(rule.line_number));
        }
      }

      // Add edges for all references
      for (const auto& ref : rule.references) {
        size_t to = rule_indices[ref];
        boost::add_edge(from, to, g);
      }
    }

    // Check for indirect left recursion using cycle detection
    bool has_cycle = false;
    CycleDetector vis(has_cycle);
    boost::depth_first_search(g, boost::visitor(vis));

    if (has_cycle) {
      return ValidationResult(false, "Indirect left recursion detected in grammar");
    }

    return ValidationResult(true);
  }

  static ValidationResult validateSymbols(const std::vector<Rule>& rules, bool strict_mode) {
    if (!strict_mode) return ValidationResult(true);

    for (const auto& rule : rules) {
      for (const auto& alt : rule.alternatives) {
        for (const auto& symbol : alt) {
          // In strict mode, every symbol must be either:
          // 1. A non-terminal (<...>)
          // 2. A terminal ("...")
          // 3. An operator (\\eb...\\eb or _..._)
          bool is_valid =
            (symbol[0] == '<' && symbol.back() == '>') ||
            (symbol[0] == '"' && symbol.back() == '"') ||
            (symbol.find("\\eb") != std::string::npos) ||
            (symbol[0] == '_' && symbol.back() == '_');

          if (!is_valid) {
            return ValidationResult(
              false,
              "Bare identifier '" + symbol + "' found in rule '" +
                rule.name + "' at line " + std::to_string(rule.line_number) +
                ". In strict mode, all symbols must be non-terminals (<...>), " +
                "terminals (\"...\"), or operators."
            );
          }
        }
      }
    }
    return ValidationResult(true);
  }

  static ValidationResult validateOperators(const std::vector<Rule>& rules) {
    // Update valid_operators to include full patterns
    std::set<std::string> valid_operators = {
      "\\eb+\\eb", "\\eb-\\eb", "\\eb*\\eb", "\\eb_div_\\eb",
      "_exp_", "_log_", "_inv_", "_sin_", "_cos_"
    };

    auto strip_delimiters = [](const std::string& op) -> std::string {
      if (op.substr(0, 3) == "\\eb" && op.substr(op.length() - 3) == "\\eb") {
        return op.substr(3, op.length() - 6);
      }
      if (op[0] == '_' && op[op.length() - 1] == '_') {
        return op.substr(1, op.length() - 2);
      }
      return op;
    };

    for (const auto& rule : rules) {
      for (const auto& alt : rule.alternatives) {
        for (const auto& symbol : alt) {
          // Check for operator pattern
          if ((symbol.find("\\eb") != std::string::npos) ||
              (symbol[0] == '_' && symbol[symbol.length() - 1] == '_')) {

            if (valid_operators.find(symbol) == valid_operators.end()) {
              std::string stripped = strip_delimiters(symbol);
              return ValidationResult(
                false,
                "Invalid operator '" + stripped + "' in rule '" +
                  rule.name + "' at line " + std::to_string(rule.line_number)
              );
            }
          }
        }
      }
    }
    return ValidationResult(true);
  }
};

// Parser class
class EBNFParser {
private:
  template <typename Iterator>
  struct Grammar : qi::grammar<Iterator, std::vector<Rule>(), qi::space_type> {
    Grammar() : Grammar::base_type(start) {
      using qi::char_;
      using qi::lexeme;
      using phoenix::at_c;
      using phoenix::push_back;
      namespace fusion = boost::fusion;

      // Basic identifier: letter followed by letters, numbers, underscore
      identifier = lexeme[char_("a-zA-Z_") >> *char_("a-zA-Z0-9_")];

      // Non-terminals with angle brackets
      nonterminal = '<' >> identifier >> '>';

      // Terminals with quotes
      terminal = '"' >> lexeme[+(char_ - '"')] >> '"';

      // Operators with special delimiters
      escaped_operator =
        ("\\eb" >> lexeme[+(char_ - "\\eb")] >> "\\eb") |
        ('_' >> lexeme[+(char_ - '_')] >> '_');

      // Any valid symbol in the grammar
      symbol = nonterminal [qi::_val = qi::_1] |
        terminal [qi::_val = qi::_1] |
        escaped_operator [qi::_val = qi::_1] |
        identifier [qi::_val = qi::_1];

      // A sequence of symbols forms an alternative
      alternative = +symbol;

      // Multiple alternatives separated by |
      alternatives = alternative % '|';

      // Helper function to collect references
      auto collect_refs = [](Rule& rule, const std::vector<std::vector<std::string>>& alts) {
        for (const auto& alt : alts) {
          for (const auto& sym : alt) {
            if (sym.size() > 2 && sym[0] == '<' && sym.back() == '>') {
              rule.references.insert(sym);
            }
          }
        }
      };

      // Rule structure with semantic actions to populate Rule struct
      rule %=
        nonterminal [at_c<0>(qi::_val) = qi::_1]
      >> "::="
      >> alternatives [
      at_c<1>(qi::_val) = qi::_1,
        phoenix::bind(collect_refs, qi::_val, qi::_1)
      ]
      >> ';';

      // We'll handle line numbers in a separate pass during the parse function

      // Comments start with # and continue to end of line
      comment = '#' >> *(char_ - qi::eol) >> qi::eol;

      // Grammar is a sequence of rules and comments
      start = *(comment | rule);

      // Name the rules for better error messages
      identifier.name("identifier");
      nonterminal.name("nonterminal");
      terminal.name("terminal");
      escaped_operator.name("operator");
      symbol.name("symbol");
      alternative.name("alternative");
      alternatives.name("alternatives");
      rule.name("rule");
      comment.name("comment");
      start.name("grammar");

      // Error handling
      using qi::on_error;
      using qi::fail;

      on_error<fail>(start,
                     phoenix::bind([](Iterator first, Iterator last, Iterator err_pos, const qi::info& what) {
                       std::string msg = "Syntax error. Expected " +
                         std::string(what.tag) +
                         " here: \"" +
                         std::string(err_pos, last) +
                         "\"";
                       throw std::runtime_error(msg);
                     }, qi::_1, qi::_2, qi::_3, qi::_4)
      );
    }

    // Rule declarations
    qi::rule<Iterator, std::string(), qi::space_type> identifier;
    qi::rule<Iterator, std::string(), qi::space_type> nonterminal;
    qi::rule<Iterator, std::string(), qi::space_type> terminal;
    qi::rule<Iterator, std::string(), qi::space_type> escaped_operator;
    qi::rule<Iterator, std::string(), qi::space_type> symbol;
    qi::rule<Iterator, std::vector<std::string>(), qi::space_type> alternative;
    qi::rule<Iterator, std::vector<std::vector<std::string>>(), qi::space_type> alternatives;
    qi::rule<Iterator, Rule(), qi::space_type> rule;
    qi::rule<Iterator, void(), qi::space_type> comment;
    qi::rule<Iterator, std::vector<Rule>(), qi::space_type> start;
  };

public:
  static std::vector<Rule> parse(const std::string& input) {
    using BaseIterator = std::string::const_iterator;
    using Iterator = boost::spirit::line_pos_iterator<BaseIterator>;

    Grammar<Iterator> grammar;
    std::vector<Rule> rules;
    size_t current_line = 1;

    Iterator iter(input.begin());
    Iterator end(input.end());

    bool success = qi::phrase_parse(iter, end, grammar, qi::space, rules);

    if (!success || iter != end) {
      throw std::runtime_error("Failed to parse complete input");
    }

    // Set line numbers in a separate pass
    for (auto& rule : rules) {
      // For now, we'll just set sequential line numbers
      // In a more sophisticated implementation, we could track actual lines
      rule.line_number = current_line++;
    }

    return rules;
  }
};

// Converter class for R output
class RConverter {
public:
  static Rcpp::List toRList(const std::vector<Rule>& rules) {
    Rcpp::List result;

    for (const auto& rule : rules) {
      Rcpp::List rule_info;

      // Convert alternatives
      Rcpp::List alternatives;
      for (const auto& alt : rule.alternatives) {
        alternatives.push_back(Rcpp::wrap(alt));
      }
      rule_info["alternatives"] = alternatives;

      // Add line number
      rule_info["line_number"] = rule.line_number;

      // Add references
      rule_info["references"] = Rcpp::wrap(
        std::vector<std::string>(
          rule.references.begin(),
          rule.references.end()
        )
      );

      result[rule.name] = rule_info;
    }

    result.attr("class") = "bnf_grammar";
    return result;
  }
};

// Internal parsing function
Rcpp::List parseGrammar(const std::string& input, bool strict_validation = true, bool strict_mode = false) {
  // Parse the grammar
  std::vector<Rule> rules = EBNFParser::parse(input);

  // Validate if requested
  if (strict_validation) {
    auto validation_result = GrammarValidator::validate(rules, strict_mode);
    if (!validation_result.valid) {
      throw std::runtime_error(validation_result.message);
    }
  }

  // Convert to R list
  return RConverter::toRList(rules);
}

// R-facing function
// [[Rcpp::export]]
Rcpp::List parseBNF(const std::string& input, bool strict_validation = true, bool strict_mode = false) {
  try {
    return parseGrammar(input, strict_validation, strict_mode);
  } catch(const std::invalid_argument& e) {
    Rcpp::stop("Invalid input: %s", e.what());
  } catch(const std::runtime_error& e) {
    Rcpp::stop("Parsing error: %s", e.what());
  } catch(const std::exception& e) {
    Rcpp::stop("Unexpected error: %s", e.what());
  } catch(...) {
    Rcpp::stop("Unknown error occurred while parsing grammar");
  }
}
