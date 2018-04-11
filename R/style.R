# surpress R CMD check notes
reorder_lints <- lintr:::reorder_lints
flatten_lints <- lintr:::flatten_lints
capture <- rex:::capture
end <- rex:::shortcuts$end
newline <- rex:::shortcuts$newline
or <- rex:::or
some_of <- rex:::some_of

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


# avoid recomputing/redefining over and over
# skip multiple of 4 indents
.WHITESPACE_OK <- seq.int(4, 80, 4) %>%
    purrr::map_chr(~ paste0(rep(" ", .), collapse = ""))
.WHITESPACE_OK <- c(.WHITESPACE_OK, "#' ")


trailing_whitespace_linter2 <- function(source_file) {
    
    res <- rex::re_matches(
        source_file$lines,
        rex::rex(
            # match 2 or more spaces; it's OK to have one space at the end
            capture(name = "space", rex::regex("[ \\t]{2,}")),
            or(newline, end)),
        global = TRUE,
        locations = TRUE)
    
    # adjust res for lines we want to skip
    if (length(source_file$lines) >= 1L) {
        
        for (i in seq_along(source_file$lines)) {
            ln <- source_file$lines[i]
            if (ln %in% .WHITESPACE_OK) {
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
                    linter = "trailing_whitespace_linter2"
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
#'   semantic issues.
#' 
#' @param path The path to the file or directory you want to check.
#' @param top An integer scalar (or a vector) to list the most frequent issues.
#' @param exclude File names to be excluded from linting.
#' @param ... Other lintr::lint parameters.
#' 
#' @examples
#' \donttest{check_style("file_name.R")}
#' \donttest{check_style("file_name.R", top = 2:3)}
#' \donttest{check_style(".", exclude = c("R/foo.R", "R/bar.R"))}
#' 
#' @export
check_style <- function(path = ".",
                        top = NULL,
                        exclude = NULL,
                        ...) {
    
    if (!rlang::is_string(path)) {
        stop("`path` must be a string.")
    }
    if (!file.exists(path)) {
        stop(paste0("Cannot find `path`: ", path))
    }
    
    if (length(top) > 0L && !rlang::is_integerish(top)) {
        stop("`top` must be an integer.")
    }
    
    if (length(exclude) > 0L && !rlang::is_character(exclude)) {
        stop("`exclude` must be a character vector of file paths.")
    }
    
    # start with default linter, modify to suit our purposes
    linters <- lintr::default_linters
    # keep indented spaces (multiple of 4) and one space after #'
    linters[["trailing_whitespace_linter"]] <- NULL
    linters[["trailing_whitespace_linter2"]] <- trailing_whitespace_linter2
    # allow for both underscore_name and camelCase
    linters[["object_name_linter"]] <- NULL
    # closed and open curlies on the same line are useful for
    # RStudio IDE document outline; used in R6 and tests.
    linters[["closed_curly_linter"]] <- NULL
    linters[["open_curly_linter"]] <- NULL
    
    # create a vector of files
    if (dir.exists(path)) {
        files <- list.files(
            path = path, pattern = "\\.(R|r)$", recursive = TRUE)
        files <- files %if_not_in% exclude
    } else {
        files <- path
    }
    
    # follow lint_package process
    lints <- flatten_lints(lapply(files, function(file) {
        lintr::lint(file, linters = linters, ...)
    }))
    lints <- reorder_lints(lints)
    
    # select top N most freq linters
    if (!is.null(top)) {
        frequent_linters <- lints %>%
            purrr::map_chr("linter") %>%
            table() %>%
            sort(decreasing = TRUE) %>%
            names()
        top_frequent_linters <- frequent_linters[top]
        lints <- lints %>%
            purrr::keep(~ .$linter %in% top_frequent_linters)
    }
    class(lints) <- "lints"
    
    lints
}
