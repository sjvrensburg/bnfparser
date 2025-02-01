#' Parse a BNF Grammar File
#'
#' Parses a BNF (Backus-Naur Form) grammar file and returns a structured representation.
#'
#' @param filepath Character string specifying the path to the BNF grammar file
#' @param strict_mode Logical; if TRUE, requires all terminals to be quoted
#' @param preserve_whitespace Logical; if TRUE, preserves whitespace in rules
#' @param allow_empty_rules Logical; if TRUE, allows empty rule alternatives
#' @param detect_left_recursion Logical; if TRUE, detects and reports left recursion
#' @param allow_ebnf Logical; if TRUE, allows EBNF constructs
#' @param validate_references Logical; if TRUE, validates all rule references
#'
#' @return A list of class "bnf_grammar" containing the parsed grammar rules
#' @export
#'
#' @examples
#' \dontrun{
#' grammar <- parseBNF("path/to/grammar.bnf")
#' # With specific options
#' grammar <- parseBNF("path/to/grammar.bnf",
#'                     strict_mode = TRUE,
#'                     detect_left_recursion = TRUE)
#' }
parseBNF <- function(filepath,
                     strict_mode = FALSE,
                     preserve_whitespace = TRUE,
                     allow_empty_rules = FALSE,
                     detect_left_recursion = TRUE,
                     allow_ebnf = TRUE,
                     validate_references = TRUE) {
  if (!file.exists(filepath)) {
    stop("File does not exist: ", filepath)
  }
  .Call('_bnfparser_parseBNF', PACKAGE = 'bnfparser',
        filepath, strict_mode, preserve_whitespace, allow_empty_rules,
        detect_left_recursion, allow_ebnf, validate_references)
}

#' Parse a BNF Grammar String
#'
#' Parses a BNF (Backus-Naur Form) grammar provided as a string and returns
#' a structured representation.
#'
#' @param grammar_string Character string containing the BNF grammar
#' @inheritParams parseBNF
#'
#' @return A list of class "bnf_grammar" containing the parsed grammar rules
#' @export
parseBNFString <- function(grammar_string,
                           strict_mode = FALSE,
                           preserve_whitespace = TRUE,
                           allow_empty_rules = FALSE,
                           detect_left_recursion = TRUE,
                           allow_ebnf = TRUE,
                           validate_references = TRUE,
                           enable_tracing = FALSE) {
  if (!is.character(grammar_string) || length(grammar_string) != 1) {
    stop("grammar_string must be a single character string")
  }

  if (enable_tracing) {
    message("Tracing enabled for debugging")
  }

  .Call('_bnfparser_parseBNFString', PACKAGE = 'bnfparser',
        grammar_string, strict_mode, preserve_whitespace, allow_empty_rules,
        detect_left_recursion, allow_ebnf, validate_references)
}

#' Print Method for BNF Grammar Objects
#'
#' @param x An object of class "bnf_grammar"
#' @param ... Additional arguments passed to print
#'
#' @return Invisibly returns the input object
#' @export
print.bnf_grammar <- function(x, ...) {
  cat("BNF Grammar with", length(x), "rules:\n\n")

  # Print each rule
  for (rule_name in names(x)) {
    cat("<", rule_name, "> ::= ", sep="")
    alternatives <- x[[rule_name]]
    cat(paste(alternatives, collapse=" | "), "\n")
  }

  # Print metadata
  cat("\nMetadata:\n")
  cat("  Strict mode:", attr(x, "strict_mode"), "\n")
  cat("  Preserve whitespace:", attr(x, "preserve_whitespace"), "\n")
  cat("  EBNF allowed:", attr(x, "allow_ebnf"), "\n")

  invisible(x)
}

#' Summary Method for BNF Grammar Objects
#'
#' @param object An object of class "bnf_grammar"
#' @param ... Additional arguments passed to summary
#'
#' @return Invisibly returns the input object
#' @export
summary.bnf_grammar <- function(object, ...) {
  n_rules <- length(object)
  n_alternatives <- sum(sapply(object, length))

  cat("BNF Grammar Summary:\n")
  cat("  Number of rules:", n_rules, "\n")
  cat("  Total alternatives:", n_alternatives, "\n")
  cat("  Average alternatives per rule:",
      round(n_alternatives/n_rules, 2), "\n")

  # List rules with their alternative counts
  cat("\nRules and their alternative counts:\n")
  alt_counts <- sapply(object, length)
  for (rule_name in names(alt_counts)) {
    cat("  ", rule_name, ": ", alt_counts[rule_name], "\n", sep="")
  }

  invisible(object)
}

#' Validate BNF Grammar
#'
#' Checks if an object is a valid BNF grammar
#'
#' @param x Object to check
#' @return TRUE if x is a valid BNF grammar object, FALSE otherwise
#' @export
is.bnf_grammar <- function(x) {
  inherits(x, "bnf_grammar") &&
    is.list(x) &&
    all(sapply(x, is.character)) &&
    !is.null(attr(x, "strict_mode")) &&
    !is.null(attr(x, "preserve_whitespace")) &&
    !is.null(attr(x, "allow_ebnf"))
}
