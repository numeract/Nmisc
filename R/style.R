nmisc_style <- styler::tidyverse_style(
    scope = "tokens",
    strict = FALSE, indent_by = 4,
    start_comments_with_one_space = TRUE,
    reindention = styler::tidyverse_reindention(),
    math_token_spacing = styler::tidyverse_math_token_spacing()
)


linters <- list(
    line_length_linter = lintr::line_length_linter(80),
    absolute_path_linter = lintr::absolute_path_linter(),
    assignment_linter = lintr::assignment_linter,
    closed_curly_linter = lintr::closed_curly_linter,
    commas_linter = lintr::commas_linter,
    infix_spaces_linter = lintr::infix_spaces_linter,
    no_tab_linter = lintr::no_tab_linter,
    object_name_linter = lintr::object_name_linter(),
    open_curly_linter = lintr::open_curly_linter,
    spaces_inside_linter = lintr::spaces_inside_linter,
    spaces_left_parentheses_linter = lintr::spaces_left_parentheses_linter,
    trailing_blank_lines_linter = lintr::trailing_blank_lines_linter,
    trailing_whitespace_linter = lintr::trailing_whitespace_linter
)


#' Fix R coding style
#'  
#' Fix R coding style issues in a specified file or directory, 
#'  based on Nmisc style
#'  
#' @param path The path to the file or directory you want to check.
#'  
#' @examples
#' \donttest{fix_style("file_name.R")}
#' 
#' @export
fix_style <- function(path = getwd()) {
    
    is_string <- is.character(path) && length(path) == 1
    if (!is_string) {
        stop("path is not a string.")
    }
    
    if (!file.exists(path)) {
        stop(paste0("Cannot find path: ", path, "."))
    }
    
    is_file <- file.exists(path) && !dir.exists(path)
    if (is_file) {
        styler::style_file(path = path,
                           transformers = nmisc_style)
    } else {
        files <- list.files(path = ".",
                            pattern = "\\.R$",
                            recursive = TRUE)
        
        purrr::map(files,
                   styler::style_file,
                   transformers = nmisc_style)
    }
}


#' Check R code styling for a specified R file or directory
#'
#' Check adherence to the Nmisc style, syntax errors and possible
#'  semantic issues.
#'
#' @param path The path to the file or directory you want to check.
#' @param recursive Should it also check subdirectories? (for directories only)
#' @param verbose Should linted files be displayed?
#' @param ... Other lintr::lint parameters.
#'
#' @examples
#' \donttest{check_style("file_name.R")}
#' \donttest{check_style("file_name.R", recursive = TRUE, verbose = TRUE)}
#' \donttest{check_style("file_name.R", parse_settings = TRUE, cache = TRUE)}
#'
#' @export
check_style <- function(path = getwd(),
                        recursive = TRUE,
                        verbose = FALSE,
                        ...) {
    
    is_string <- is.character(path) && length(path) == 1
    if (!is_string) {
        stop("path is not a string.")
    }
    
    if (!file.exists(path)) {
        stop(paste0("Cannot find path: ", path, "."))
    }
    
    is_file <- file.exists(path) && !dir.exists(path)
    if (is_file) {
        lintr::lint(filename = path,
                    linters = linters,
                    ... = ...)
    } else {
        files <- list.files(path = ".",
                            pattern = "\\.R$",
                            recursive = recursive)
        purrr::map(files,
                   function(f) {
                       if (verbose) {
                           print(paste0("Linting: ", f))
                       }
                       lintr::lint(f, linters = linters, ... = ...)
                   })
    }
}

# New linters only available in nightly build

# linters <- list(
#     line_length_linter = lintr::line_length_linter(80),
#     nonportable_path_linter = lintr::nonportable_path_linter,
#     absolute_path_linter = lintr::absolute_path_linter,
#     assignment_linter = lintr::assignment_linter,
#     open_curly_linter = lintr::open_curly_linter,
#     implicit_integer_linter = lintr::implicit_integer_linter,
#     extraction_operator_linter = lintr::extraction_operator_linter,
#     closed_curly_linter = lintr::closed_curly_linter(),
#     commas_linter = lintr::commas_linter,
#     infix_spaces_linter = lintr::infix_spaces_linter,
#     no_tab_linter = lintr::no_tab_linter,
#     object_name_linter = lintr::object_name_linter,
#     open_curly_linter = lintr::open_curly_linter,
#     spaces_inside_linter = lintr::spaces_inside_linter,
#     spaces_left_parentheses_linter = lintr::spaces_left_parentheses_linter,
#     trailing_blank_lines_linter = lintr::trailing_blank_lines_linter,
#     undesirable_function_linter = lintr::undesirable_function_linter,
#     undesirable_operator_linter = lintr::undesirable_operator_linter,
#     unneeded_concatenation_linter = lintr::unneeded_concatenation_linter,
#     single_quotes_linter = lintr::single_quotes_linter,
#     seq_linter = lintr::seq_linter,
#     semicolon_terminator_linter = lintr::semicolon_terminator_linter,
#     pipe_continuation_linter = lintr::pipe_continuation_linter,
#     camel_case_linter = lintr::camel_case_linter,
#     T_and_F_symbol_linter = lintr::T_and_F_symbol_linter
# )
