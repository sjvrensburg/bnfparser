//' BNF Parser Header
//'
//' @description
//' Header file defining the core classes and structures for BNF grammar parsing.
//' This includes configuration options, rule representation, and the main parser class.
//'
//' @details
//' The parser supports both standard BNF and extended BNF notation, with
//' configurable options for validation and parsing behavior. The main components are:
//' \itemize{
//'   \item ParserConfig: Structure for parser configuration
//'   \item Rule: Structure representing a grammar rule
//'   \item BNFParser: Main parser class
//' }
//'
//' @author Your Name
//' @references
//' \itemize{
//'   \item Backus-Naur Form: \url{https://en.wikipedia.org/wiki/Backus-Naur_form}
//'   \item PEG Parser: \url{https://github.com/yhirose/cpp-peglib}
//' }

#ifndef BNFPARSER_H
#define BNFPARSER_H

#include <string>
#include <vector>
#include <set>
#include <map>
#include <stack>
#include <memory>
#include <optional>
#include <peglib.h>

//' Parser Configuration
//'
//' @description
//' Structure containing configuration options for the BNF parser.
//'
//' @field strict_mode Requires all terminals to be quoted
//' @field preserve_whitespace Preserves whitespace in rules
//' @field allow_empty_rules Allows empty rule alternatives
//' @field detect_left_recursion Enables detection of left recursion
//' @field allow_ebnf Allows extended BNF constructs
//' @field validate_references Validates all rule references
struct ParserConfig {
  bool strict_mode = false;
  bool preserve_whitespace = true;
  bool allow_empty_rules = false;
  bool detect_left_recursion = true;
  bool allow_ebnf = true;
  bool validate_references = true;
  bool enableTracing = false;      // Enable detailed tracing for debugging
  bool enableOptimization = true;  // Enable performance optimizations
};

//' Grammar Rule
//'
//' @description
//' Structure representing a single rule in the grammar.
//'
//' @field name Rule name without angle brackets
//' @field raw_name Original rule name with angle brackets
//' @field alternatives Vector of alternative productions
//' @field references Set of referenced rule names
struct Rule {
  std::string name;
  std::string raw_name;
  std::vector<std::string> alternatives;
  std::set<std::string> references;
  size_t line_number;  // Track line number for better error reporting

  //' @description Default constructor
  Rule() = default;

  //' @description Constructor with name initialization
  //' @param n Rule name
  //' @param rn Raw rule name
  //' @param ln Line number
  Rule(const std::string& n, const std::string& rn, size_t ln = 0)
    : name(n), raw_name(rn), line_number(ln) {}
};

//' BNF Parser Class
//' BNF Parser Class
//'
//' @description
//' Main parser class for processing BNF grammars.
class BNFParser {
 private:
  // RAII wrapper for current_rule management
  class RAIICurrentRule {
    BNFParser& parser_;

   public:
    explicit RAIICurrentRule(BNFParser& parser) : parser_(parser) {
      parser_.current_rule = std::make_unique<Rule>();
    }

    ~RAIICurrentRule() { parser_.current_rule.reset(); }

    RAIICurrentRule(const RAIICurrentRule&) = delete;
    RAIICurrentRule& operator=(const RAIICurrentRule&) = delete;
  };

  peg::parser parser;
  std::vector<Rule> rules;
  ParserConfig config;
  std::stringstream error_log;

  // Current rule being processed (for semantic actions)
  std::unique_ptr<Rule> current_rule;

  //' @description Trim whitespace from a string
  //' @param str Input string
  //' @return Trimmed string
  static std::string trim(const std::string& str);

  //' @description Detect left recursion in rules
  //' @param rules Vector of grammar rules
  //' @return true if left recursion is detected
  bool detectLeftRecursion(const std::vector<Rule>& rules);

  //' @description Validate a single rule
  //' @param rule Rule to validate
  //' @return true if rule is valid
  bool validateRule(const Rule& rule);

  //' @description Validate rule references
  //' @return true if all references are valid
  bool validateReferences();

  //' @description Set up parser semantic actions
  void setupSemanticActions();

  //' @description Build recursion path
  std::string buildRecursionPath(const std::vector<std::string>& path);

 public:
  //' @description Constructor
  //' @param cfg Parser configuration
  explicit BNFParser(const ParserConfig& cfg = ParserConfig());

  //' @description Parse input grammar
  //' @param input Input grammar string
  //' @return true if parsing succeeds
  bool parse(const std::string& input);

  //' @description Setup error handling
  void setupErrorHandling();

  //' @description Get error messages
  //' @return String containing error messages
  std::string getErrorLog() const;

  //' @description Get parsed rules
  //' @return Vector of parsed rules
  const std::vector<Rule>& getRules() const;
};

#endif // BNFPARSER_H
