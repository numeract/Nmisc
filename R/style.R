nmisc_style <- styler::tidyverse_style(
    scope = "tokens",
    strict = FALSE, indent_by = 4,
    start_comments_with_one_space = TRUE,
    reindention = styler::tidyverse_reindention(),
    math_token_spacing = styler::tidyverse_math_token_spacing()
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


trailing_whitespace_linter2 <- function(source_file) {
    
    res <- rex::re_matches(
        source_file$lines,
        rex::rex(
            capture(name = "space", some_of(" ", rex::regex("\\t"))),
            or(newline, end)),
        global = TRUE,
        locations = TRUE)
    
    # adjust res for lines we want to skip
    if (length(source_file$lines) >= 1L) {
        skip_lines <- c(
            "#' ",
            "    "
        )
        for (i in seq_along(source_file$lines)) {
            ln <- source_file$lines[i]
            if (ln %in% skip_lines) {
                res[[i]] <- data.frame(
                    space = NA_character_, 
                    space.start = NA_integer_, 
                    space.end = NA_integer_, 
                    stringsAsFactors = FALSE
                )
            }
        }
    }
    
    lapply(seq_along(source_file$lines), function(itr) {
        
        mapply(
            FUN = function(start, end) {
                if (is.na(start)) {
                    return()
                }
                line_number <- names(source_file$lines)[itr]
                lintr::Lint(
                    filename = source_file$filename,
                    line_number = line_number,
                    column_number = start,
                    type = "style",
                    message = "Trailing whitespace is superfluous.",
                    line = source_file$lines[as.character(line_number)],
                    ranges = list(c(start, end)),
                    linter = "trailing_whitespace_linter"
                )
            },
            start = res[[itr]]$space.start,
            end = res[[itr]]$space.end,
            SIMPLIFY = FALSE
        )
    })
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
    
    is_string <- is.character(path) && length(path) == 1L
    if (!is_string) {
        stop("path is not a string.")
    }
    
    if (!file.exists(path)) {
        stop(paste0("Cannot find path: ", path, "."))
    }
    
    linters <- lintr::default_linters
    linters[['trailing_whitespace_linter']] <- trailing_whitespace_linter2
    
    is_file <- file.exists(path) && !dir.exists(path)
    if (is_file) {
        lintr::lint(
            filename = path, linters = linters, ...)
    } else {
        files <- list.files(
            path = ".", pattern = "\\.R$", recursive = recursive)
        purrr::map(files, function(f) {
            if (verbose) {
                print(paste0("Linting: ", f))
            }
            lintr::lint(f, linters = linters, ...)
        })
    }
}
