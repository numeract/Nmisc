add_package_info <- function(package_df) {
    
    if (nrow(package_df) == 0L) {
        package_df <- 
            package_df %>%
            tibble::add_column(
                is_base = logical(),
                source = character(),
                source_path = character(),
                version = character(),
                is_installed = logical()
            )
    } else {
        package_df$is_base <- NA
        package_df$source <- NA_character_
        package_df$source_path <- NA_character_
        package_df$version <- NA_character_
        installed_packages <- rownames(utils::installed.packages())
        package_df$is_installed <- purrr::map_lgl(
            package_df$package_name, ~ . %in% installed_packages)
        for (i in seq_nrow(package_df)) {
            # if the package is already installed, take information from
            # package description, otherwise look for package
            # only in CRAN repository
            if (package_df$is_installed[i]) {
                desc <- utils::packageDescription(
                    pkg = package_df$package_name[i], 
                    fields = c(
                        "Priority", "Version", "Repository",
                        "GithubRepo", "GithubUsername"))
                desc <- unlist(desc)
                package_df$is_base[i] <- identical(desc[['Priority']], "base")
                if (is.na(desc[['Repository']])) {
                    if (!is.na(desc[["GithubRepo"]])) {
                        package_df$source[i] <- "GitHub"
                        package_df$source_path[i] <- paste0(
                            desc[["GithubUsername"]], "/", desc[["GithubRepo"]])
                    }    
                } else if (!is.na(desc[['Repository']])) {
                    package_df$source[i] <- desc[['Repository']]
                }
                package_df$version[i] <- desc[['Version']]
            } else {
                package_df$is_base[i] <- FALSE
            }
        }
    }
    
    package_df
}


get_file_text <- function(project_path, 
                          include_pattern, 
                          exclude_pattern) {
    
    # look in the files of interest only
    project_files <- 
        list.files(
            path = project_path, 
            pattern = include_pattern, 
            full.names = TRUE, 
            recursive = TRUE
        ) %>%
        purrr::discard(~ grepl(exclude_pattern, .))
    
    # read every line of text from each file, discard commented lines,
    # eliminate new line for Windows, eliminate spaces,
    # eliminate new line for Unix/MacOS, eliminate quotes, eliminate tabs
    exclude_comments_pattern <- '^#.*'
    lines <- 
        project_files %>% 
        purrr::map(readLines) %>%
        unlist(use.names = FALSE) %>%
        purrr::discard(~ grepl(exclude_comments_pattern, .)) %>%
        stringr::str_replace_all("[\r\n]" , "") %>%
        stringr::str_replace_all('\\n', '') %>%
        stringr::str_replace_all('"', '') %>%
        stringr::str_replace_all('\'', '') %>%
        stringr::str_replace_all('\\t', '')
    
    # create a string with all the pasted lines
    lines <- paste(lines, collapse = '')
}


get_loaded_packages <- function() {
    
    # get the names of the package already loaded in the current session
    nms <- names(utils::sessionInfo()$loadedOnly)
    if (length(nms) == 0L) {
        package <- tibble::tibble(
            package_name = character(),
            requested_by = character()
        )
    } else {
        package <- tibble::tibble(
            package_name = nms,
            requested_by = "loaded"
        )
    }
    
    add_package_info(package)
}


get_referenced_packages <- function(project_path, 
                                    include_pattern, 
                                    exclude_pattern) {
    
    code_lines <- get_file_text(project_path, include_pattern, exclude_pattern)
    
    regex_pattern <- '[_.a-zA-Z]+::'
    # extract only the names of the package used with "::" operator,
    # add the column requested by, select rows with distinct package names
    referenced_package_lst <- stringr::str_extract_all(
        code_lines, regex_pattern) %>% 
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE)
    
    if (length(referenced_package_lst) > 0) {
        referenced_package <- 
            referenced_package_lst %>%
            tibble::as.tibble() %>%
            dplyr::mutate(
                value = gsub('::$', '', .data$value), 
                requested_by  = "reference"
            ) %>%
            dplyr::rename(package_name = .data$value) %>%
            dplyr::distinct(.data$package_name, .keep_all = TRUE)
    } else {
        referenced_package <- tibble::tibble(
            package_name = character(),
            requested_by = character()
        )
    }
    
    add_package_info(referenced_package)
}


get_library_packages <- function(project_path, 
                                 include_pattern, 
                                 exclude_pattern) {
    
    code_lines <- get_file_text(project_path, include_pattern, exclude_pattern)
    
    # create a data frame with the package loaded individually with "library"
    regex_pattern_single <- '(?<=library\\()([a-zA-Z1-9-_]+?)(?=\\))'
    library_package_lst <- 
        stringr::str_extract_all(code_lines, regex_pattern_single) %>% 
        purrr::discard(~ length(.) == 0) %>%
        unlist(use.names = FALSE) 
    
    # create a data frame with the package loaded individually with "require"
    regex_pattern_single <- '(?<=require\\()([a-zA-Z1-9-_]+?)(?=\\))'
    required_package_lst <- stringr::str_extract_all(
        code_lines, regex_pattern_single) %>% 
        purrr::discard(~ length(.) == 0) %>%
        unlist(use.names = FALSE) 
    
    library_package_lst <- c(library_package_lst, required_package_lst)
    if (length(library_package_lst) > 0) {
        # rename columns, add requested_by column, select distinct rows taking 
        # into consideration "package_name" column
        library_package_df <- 
            library_package_lst %>%
            tibble::as_tibble() %>%
            dplyr::mutate(requested_by = "library") %>%
            dplyr::rename(package_name = .data$value) %>%
            dplyr::distinct(.data$package_name, .keep_all = TRUE)
    } else {
        library_package_df <- tibble::tibble(
            package_name = character(),
            requested_by = character()
        )
    }
    
    add_package_info(library_package_df)
}


get_description_packages <- function(description_path = "DESCRIPTION", 
                                     options = c("Depends", "Imports")) {
    
    desc_package_df <- tibble::tibble(
        package_name = character(),
        requested_by = character()
    )
    
    if (!file.exists(description_path)) {
        warning(paste0("File '", description_path, "' does not exist!"))
        return(add_package_info(desc_package_df))
    }
    
    description_data <- NULL
    tryCatch({
        description_data <- read.dcf(description_path)
    }, error = function(cond) {})
    if (is.null(description_data)) {
        warning(paste0("File '", description_path, "' is not a dcf file!"))
        return(add_package_info(desc_package_df))
    }
    
    col_names <- intersect(colnames(description_data), options)
    desc_package <- 
        description_data[, col_names, drop = FALSE] %>%
        gsub(pattern = "\n", replacement = "") %>%
        strsplit(",") %>%
        unlist() %>%
        stringr::str_trim(.) %>%
        rlang::set_names(nm = NULL)
    
    # ignore line specifying R version dependency
    # keep only package dependencies in description
    desc_package <- desc_package[!grepl("^R [(]", desc_package)]
    if (length(desc_package) > 0L) {
        desc_package_df <- 
            desc_package %>%
            tibble::as_tibble() %>%
            dplyr::mutate(requested_by = "description") %>%
            dplyr::rename(package_name = .data$value) %>%
            dplyr::distinct(.data$package_name, .keep_all = TRUE)
    }
    
    add_package_info(desc_package_df)
}


#' Get information about the package used in the project
#' 
#' @description 
#' The function returns a data frame containing information about
#'   packages that are loaded with \code{library()}, \code{require()}, used
#'   with \code{::} operator, listed in the DESCRIPTION file,
#'   and/or already loaded.
#' 
#' @param project_path A string representing the path of the project root 
#'   in which the function will look recursively in order to find files that
#'   fit \code{include_pattern}
#' @param include_pattern A string representing a regex that matches 
#'   project files in which to look for packages. By default, 
#'   \code{get_packages} looks for all .R and .Rmd files in the current project.
#' @param exclude_pattern A string representing a regex that matches 
#'   project files to exclude. By default, 
#'   \code{get_packages} excludes all files found in "tests" folder.
#' @param package_options A character vector that represents the method through
#'   which packages are loaded or referenced. The options are: 
#'   \code{referenced} for packages referenced by the \code{::} operator, 
#'   \code{library} for packages loaded using \code{library()} or \code{require()},
#' \code{description} for packages mentioned in \code{DESCRIPTION} file, and
#' \code{loaded} for packages already loaded in the current session.
#' 
#' @return A data frame containing package information:
#' \item{package_name}{The name of the package}
#' \item{requested_by}{The context in which the package was used}
#' \item{is_base}{Whether package is part of the core R packages}
#' \item{source}{The source from which the package was installed}
#' \item{version}{The version of the package, if installed locally}
#' \item{is_installed}{Whether the package is installed locally}
#' 
#' @seealso \code{\link{generate_install_file}}
#' 
#' @examples
#' \dontrun{
#' package_df <- get_packages(
#'     project_path = '.',
#'     include_pattern = '\\.R$', 
#'     exclude_pattern = '', 
#'     package_options = c('referenced'))
#' }
#' 
#' @export
get_packages <- function(
        project_path = ".",
        include_pattern = "\\.R(md)?$",
        exclude_pattern = "tests/",
        package_options = c("referenced", "library", "description")
) {
    
    package <- tibble::tibble(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        source_path = character(),
        version = character(),
        is_installed = logical()
    )
    
    if ("referenced" %in% package_options) {
        package <- 
            package %>% 
            dplyr::bind_rows(get_referenced_packages(
                project_path, include_pattern, exclude_pattern))
    }
    
    if ("library" %in% package_options) {
        package <- 
            package %>% 
            dplyr::bind_rows(get_library_packages(
                project_path, include_pattern, exclude_pattern))
    }
    
    if ("description" %in% package_options) {
        package <- 
            package %>% 
            dplyr::bind_rows(get_description_packages(
                file.path(project_path, "DESCRIPTION")))
    }
    
    if ("loaded" %in% package_options) {
        package <- 
            package %>% 
            dplyr::bind_rows(get_loaded_packages())
    }
    
    if (nrow(package) > 0) {
        # keep only the first entry for each package, given the order above
        package <- 
            package %>%
            dplyr::distinct(.data$package_name, .keep_all = TRUE)
    }
    
    package
}


#' Generates an R file to install packages used by the project.
#' 
#' @description
#' The function takes the output of \code{get_packages} and
#'   writes in a file the commands needed to install and update
#'   package used throughout the project.
#' 
#' @param file The name of the file to be created.
#' @param package_df A data frame obtained with \code{get_packages} that 
#'   contains information regarding the name, version and source of the package.
#' @param include_core_package Logical, whether to include in the 
#'   generated install file package which come with R by default
#' 
#' @return Nothing
#' 
#' @examples
#' \dontrun{
#' package_df <- get_packages(package_options = c("library"))
#' generate_install_file("install_packages.R", package_df)
#' }
#' 
#' @seealso \code{\link{get_packages}}
#' 
#' @export
generate_install_file <- function(file,
                                  package_df = get_packages(), 
                                  include_core_package = FALSE) {
    
    # always exclude base package
    package_df <- 
        package_df %>%
        tibble::as_tibble() %>%
        dplyr::filter(.data$package_name != "base")
    
    if (!include_core_package) {
        package_df <- dplyr::filter(package_df, !.data$is_base)
    }
    
    if (nrow(package_df) == 0L) {
        msg <- paste0(
            "No packages to install, file '", file, "' not created.")
        cat(msg, "\n")
        return(invisible(NULL))
    }
    
    package_df_cran <- dplyr::filter(package_df, .data$source == "CRAN")
    if (nrow(package_df_cran) == 0L) { 
        cran_packages_line <- "cran_packages <- character()\n"
    } else {
        cran_packages_line <- paste(
            package_df_cran$package_name, collapse = "','")
        cran_packages_line <- paste0(
            "cran_packages <- c('", cran_packages_line, "')\n")
    }
    
    package_df_github <- dplyr::filter(package_df, .data$source == "GitHub")
    if (nrow(package_df_github) == 0L) {
        github_packages_line <- "github_packages <- character()\n"
    } else {
        github_packages_line <- paste(
            package_df_github$package_name, collapse = "','")
        github_packages_line <- paste0(
            "github_packages <- c('", github_packages_line, "')\n")
    }
    
    other_df <- 
        package_df %>%
        dplyr::filter(!(.data$source %in% c("CRAN", "GitHub")))
    if (nrow(other_df) > 0L) {
        message("Cannot identify the source for the following packages:")
        print(other_df, n = nrow(other_df))
    }
    
    # if there are github packages that are not already installed
    # first install 'devtools' package and then use it to 
    # install other packages
    code_text <- paste0(
        cran_packages_line,
        github_packages_line,
"installed_packages <- rownames(utils::installed.packages())
missing_packages <- base::setdiff(cran_packages, installed_packages)
if (length(missing_packages) > 0) {
    utils::install.packages(missing_packages)
}
utils::update.packages(oldPkgs = cran_packages, ask = FALSE)

if (length(github_packages) > 0) {
    utils::install.packages('devtools', quiet = TRUE)
    devtools::install_github(github_packages, quiet = TRUE)
}")
    
    write(code_text, file)
    
    msg <- paste0(
        "Successfully created file '", file, "'.")
    cat(msg, "\n")
    
    invisible(NULL)
}
