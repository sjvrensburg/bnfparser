#' Parse EBNF grammar from a file
#'
#' @description Reads and parses an EBNF grammar from a text file, handling UTF-8 encoding
#'   and potential BOM marks. The grammar is validated according to the specification,
#'   checking for undefined symbols, left recursion (if enabled), and proper operator usage.
#'
#' @param file_path Character string specifying the path to the grammar file.
#' @param strict_validation Logical; whether to perform strict validation of the grammar
#'        including checking for undefined symbols, left recursion, and proper operator usage.
#'        Defaults to `TRUE`.
#' @param strict_mode Logical; whether to enforce strict symbol definitions. In strict mode, all
#'        symbols must be non-terminals (`<...>`), terminals (`"..."`), or operators.
#'        Defaults to `FALSE`.
#' @param encoding Character string specifying the encoding to be assumed for the file.
#'        Defaults to "UTF-8".
#'
#' @return A named list representing the parsed grammar. The names of the list are the non-terminal
#'   symbols. Each element of the list is itself a list with the following components:
#'     \itemize{
#'       \item{\code{alternatives}:}{ A list of character vectors. Each character vector represents an alternative
#'           for the non-terminal symbol, consisting of a sequence of symbols.}
#'       \item{\code{line_number}:}{ The line number in the input where the rule for this non-terminal was defined.}
#'       \item{\code{references}:}{ A character vector of non-terminal symbols referenced by this rule.}
#'     }
#'
#' @details The function reads a text file containing an EBNF grammar specification and parses it into
#'   a structured representation. It handles potential encoding issues by:
#'   \itemize{
#'     \item Automatically detecting and removing UTF-8 BOM if present
#'     \item Converting non-ASCII characters to their ASCII equivalents where possible
#'     \item Providing explicit encoding control through the encoding parameter
#'   }
#'
#'   The grammar validation can be controlled through two parameters:
#'   \itemize{
#'     \item \code{strict_validation}: When TRUE, performs comprehensive validation including:
#'       \itemize{
#'         \item Checking for undefined symbols
#'         \item Detecting left recursion (with special handling for expression-like grammars)
#'         \item Validating operator usage
#'       }
#'     \item \code{strict_mode}: When TRUE, enforces strict symbol definition rules:
#'       \itemize{
#'         \item All non-terminals must be enclosed in angle brackets (e.g., `<expression>`)
#'         \item All terminals must be enclosed in quotes (e.g., `"+"`)
#'         \item All operators must use proper delimiters (`\\eb...\\eb` or `_..._`)
#'       }
#'   }
#'
#' @section Error Handling:
#'   The function will stop with an error message if:
#'   \itemize{
#'     \item The specified file does not exist
#'     \item The file cannot be read with the specified encoding
#'     \item The text contains invalid characters that cannot be converted to ASCII
#'     \item The grammar fails validation (when strict_validation is TRUE)
#'     \item The grammar contains improper symbol definitions (when strict_mode is TRUE)
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage with default settings
#' grammar <- parseBNFFile("path/to/grammar.txt")
#'
#' # Parse without strict validation
#' grammar <- parseBNFFile("path/to/grammar.txt", strict_validation = FALSE)
#'
#' # Parse with strict mode enabled
#' grammar <- parseBNFFile("path/to/grammar.txt", strict_mode = TRUE)
#'
#' # Parse a file with specific encoding
#' grammar <- parseBNFFile("path/to/grammar.txt", encoding = "latin1")
#'
#' # Example grammar file content:
#' #   <start> ::= <expression> ;
#' #   <expression> ::= <term> | <expression> "+" <term> ;
#' #   <term> ::= <factor> | <term> "*" <factor> ;
#' #   <factor> ::= "(" <expression> ")" | "1" | "2" ;
#' }
#'
#' @seealso \code{\link{parseBNF}} for parsing grammar strings directly
#'
#' @export
parseBNFFile <- function(file_path,
                         strict_validation = TRUE,
                         strict_mode = FALSE,
                         encoding = "UTF-8") {

  # Input validation
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("file_path must be a single character string")
  }
  if (!is.logical(strict_validation) || length(strict_validation) != 1) {
    stop("strict_validation must be a single logical value")
  }
  if (!is.logical(strict_mode) || length(strict_mode) != 1) {
    stop("strict_mode must be a single logical value")
  }
  if (!is.character(encoding) || length(encoding) != 1) {
    stop("encoding must be a single character string")
  }

  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  # Read the file with specified encoding
  text <- tryCatch({
    readLines(file_path, encoding = encoding, warn = FALSE)
  }, error = function(e) {
    stop("Error reading file: ", e$message)
  })

  # Combine lines into a single string
  grammar_string <- paste(text, collapse = "\n")

  # Remove UTF-8 BOM if present
  if (grepl("^\uFEFF", grammar_string)) {
    grammar_string <- sub("^\uFEFF", "", grammar_string)
  }

  # Convert to ASCII where possible, replacing non-ASCII with closest equivalents
  grammar_string <- tryCatch({
    converted <- iconv(grammar_string, from = encoding, to = "ASCII//TRANSLIT")
    if (is.na(converted)) {
      stop("Failed to convert text encoding")
    }
    converted
  }, error = function(e) {
    stop("Error converting text encoding: ", e$message)
  })

  print(grammar_string)

  # Parse the grammar using the existing parseBNF function
  tryCatch({
    parseBNF(grammar_string, strict_validation, strict_mode)
  }, error = function(e) {
    stop("Error parsing grammar: ", e$message)
  })
}
