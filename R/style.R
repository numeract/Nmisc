tidy_style <- styler::tidyverse_style(
    scope = "tokens",
    strict = FALSE, indent_by = 4,
    start_comments_with_one_space = TRUE,
    reindention = styler::tidyverse_reindention(),
    math_token_spacing = styler::tidyverse_math_token_spacing()
)


#' Perform R code styling on an R file.
#'
#' Pretty-print R code without changing the user's formatting intent.
#'
#' @param file_path The path or the name of the file
#' you want to style.
#'
#' @return Invisibly returns a data frame that indicates for
#' each file considered for styling whether or not it was
#' actually changed.
#'
#' @section Warning:
#' This function overwrites files
#' (if styling results in a change of the code to be formatted).
#' It is strongly suggested to only style files that are under
#' version control or to create a backup copy.
#'
#' @family code styling functions
#'
#' @examples
#' \donttest{style_script("file_name.R")}
#'
#' @export
style_script <- function(file_path) {
    if (!file.exists(file_path)) {
        stop(paste0(
            "Cannot find file: ", file_path,
            ". Did you specify the right path?"
        ))
    }
    
    styler::style_file(
        path = file_path,
        transformers = tidy_style
    )
}


#' Perform R code styling on an R package.
#'
#' Pretty-print all R files without changing the user's formatting intent.
#'
#' @param package_path The path of the root directory of the package
#' you want to style.
#'
#' @return Invisibly returns a data frame that indicates for
#' each file considered for styling whether or not it was
#' actually changed.
#'
#' @section Warning:
#' This function overwrites files
#' (if styling results in a change of the code to be formatted).
#' It is strongly suggested to only style files that are under
#' version control or to create a backup copy.
#'
#' @family code styling functions
#'
#' @examples
#' \donttest{style_package(package_path = ".")}
#' \donttest{style_package()}
#'
#' @export
style_package <- function(package_path = ".") {
    if (!dir.exists(package_path)) {
        stop(paste0(
            "Cannot find directory: ", package_path,
            ". Did you specify the right path?"
        ))
    }
    styler::style_pkg(
        pkg = package_path,
        transformers = tidy_style
    )
}


linters <- list(
    line_length_linter = lintr::line_length_linter(80),
    absolute_paths_linter = lintr::absolute_paths_linter,
    assignment_linter = lintr::assignment_linter,
    closed_curly_linter = lintr::closed_curly_linter,
    commas_linter = lintr::commas_linter,
    infix_spaces_linter = lintr::infix_spaces_linter,
    no_tab_linter = lintr::no_tab_linter,
    multiple_dots_linter = lintr::multiple_dots_linter,
    open_curly_linter = lintr::open_curly_linter,
    spaces_inside_linter = lintr::spaces_inside_linter,
    spaces_left_parentheses_linter = lintr::spaces_left_parentheses_linter,
    trailing_blank_lines_linter = lintr::trailing_blank_lines_linter
)


#' Check R code styling for a specified R file.
#'
#' Check adherence to a given style, syntax errors and possible
#' semantic issues.
#'
#' @param file_path The path to the file you want to check.
#'
#' @family code styling functions
#'
#' @examples
#' \donttest{check_style("file_name.R")}
#'
#' @export
check_style <- function(file_path) {
    
    # lintr::lint_package(path = ".", linters = linters)
    
    if (!file.exists(file_path)) {
        stop(paste0(
            "Cannot find file: ", file_path,
            ". Did you specify the right path?"
        ))
    }
    
    lintr::lint(file_path, linters = linters)
}


#' Check R code styling for an entire package.
#'
#' Check adherence to a given style, syntax errors and possible
#' semantic issues for all R files.
#'
#' @param package_path The path to the root directory of your package.
#'
#' @family code styling functions
#'
#' @examples
#' \donttest{check_package_style(package_path = ".")}
#' \donttest{check_package_style()}
#'
#' @export
check_package_style <- function(package_path = ".") {
    if (!dir.exists(package_path)) {
        stop(paste0(
            "Cannot find directory: ", package_path,
            ". Did you specify the right path?"
        ))
    }
    
    lintr::lint_package(path = package_path, linters = linters)
}



trailing_whitespace_linter2 <- function(source_file) {
    res <- re_matches(
        source_file$lines,
        rex(or(none_of(" ")),
            capture(name = "space", some_of(" ", regex("\\t"))),
            or(newline, end)),
        global = TRUE,
        locations = TRUE)
    
    lapply(seq_along(source_file$lines), function(itr) {
        
        mapply(
            FUN = function(start, end) {
                if (is.na(start)) {
                    return()
                }
                line_number <- names(source_file$lines)[itr]
                Lint(
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

