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
                        package_df$source[i] <- "github"
                        package_df$source_path[i] <- paste0(
                            desc[["GithubUsername"]], "/", desc[["GithubRepo"]])
                    }  
                } else {
                    package_df$source[i] <- desc[['Repository']]
                    package_df$source_path[i] <- NA
                }
                package_df$version[i] <- desc[['Version']]
            } else {
                package_df$is_base[i] <- FALSE
            }
        }
    }
    
    package_df
}


get_file_text <- function(project_path, include_pattern, exclude_pattern) {
    
    # look in the files of interest only
    project_files <- 
        list.files(
            path = project_path, pattern = include_pattern, 
            full.names = TRUE, recursive = TRUE
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
        purrr::discard(~grepl(exclude_comments_pattern, .)) %>%
        stringr::str_replace_all("[\r\n]" , "") %>%
        stringr::str_replace_all('\\n', '') %>%
        stringr::str_replace_all('"', '') %>%
        stringr::str_replace_all('\'', '') %>%
        stringr::str_replace_all('\\t', '')
    
    # create a string with all the pasted lines
    lines <- paste(lines, collapse = '')
}


get_loaded_package <- function() {
    
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


get_referenced_package <- function(
    project_path, include_pattern, exclude_pattern) {
    
    code_lines <- get_file_text(project_path, include_pattern, exclude_pattern)
    
    regex_pattern <- '[_.a-zA-Z]+::'
    # extract only the names of the package used with "::" operator,
    # add the column requested by, select rows with distinct package names
    referenced_package_lst <- stringr::str_extract_all(
        code_lines, regex_pattern) %>% 
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE)
    
    if (length(referenced_package_lst) != 0) {
        referenced_package <- 
            referenced_package_lst %>%
            tibble::as.tibble() %>%
            dplyr::mutate(value = gsub('::$', '', .data$value), 
                          requested_by  = "reference") %>%
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


get_library_package <- function(project_path, 
                                include_pattern, 
                                exclude_pattern) {
    
    code_lines <- get_file_text(project_path, include_pattern, exclude_pattern)
    
    # create a data frame with the package loaded individually with "library"
    regex_pattern_single <- '(?<=library\\()([a-zA-Z1-9-_]+?)(?=\\))'
    library_package_lst <- 
        stringr::str_extract_all(code_lines, regex_pattern_single) %>% 
        purrr::discard(~ length(.) == 0) %>%
        unlist(use.names = FALSE) 
    
    if (length(library_package_lst) != 0) {
        # rename columns, add requested_by column, select distinct rows taking 
        # into consideration "package_name" column
        library_package_df <- 
            library_package_lst %>%
            tibble::as_tibble() %>%
            dplyr::mutate(
                package_name = .data$value,
                requested_by = "library"
            ) %>%
            dplyr::distinct(.data$package_name, .keep_all = TRUE)
        
    } else {
        library_package_df <- tibble::tibble(
            package_name = character(),
            requested_by = character()
        )
    }
    
    add_package_info(library_package_df)
}


get_required_package <- function(project_path, 
                                 include_pattern, 
                                 exclude_pattern) {
    
    code_lines <- get_file_text(project_path, include_pattern, exclude_pattern)
    
    # create a data frame with the package loaded individually with "require"
    regex_pattern_single <- '(?<=require\\()([a-zA-Z1-9-_]+?)(?=\\))'
    required_package_lst <- stringr::str_extract_all(
        code_lines, regex_pattern_single) %>% 
        purrr::discard(~ length(.) == 0) %>%
        unlist(use.names = FALSE) 
    
    if (length(required_package_lst) != 0) {
        # rename columns, add requested_by column, select distinct rows taking 
        # into consideration "package_name" column
        required_package_df <- 
            required_package_lst %>%
            tibble::as_tibble() %>%
            dplyr::mutate(
                package_name = .data$value,
                requested_by = "require"
            ) %>%
            dplyr::distinct(.data$package_name, .keep_all = TRUE)
        
    } else {
        required_package_df <- tibble::tibble(
            package_name = character(),
            requested_by = character()
        )
    }
    
    add_package_info(required_package_df)
}


get_description_package <- function(description_path = "DESCRIPTION", 
                                     options = c("Depends", "Imports")) {
    
    if (!file.exists(description_path)) {
        stop(paste("File", description_path, "does not exist!"))
    }
    
    description_data <- read.dcf(description_path)
    col_names <- intersect(colnames(description_data), options)
    desc_package <- 
        description_data[, col_names, drop = FALSE] %>%
        gsub(pattern = "\n", replacement = "") %>%
        strsplit(",") %>%
        unlist() %>%
        stats::setNames(NULL) 
    
    # ignore line specifying R version dependency
    # keep only package dependencies in description
    desc_package <- desc_package[!grepl("^R [(]", desc_package)]
    if (length(desc_package) != 0) {
        desc_package_df <- 
            desc_package %>%
            tibble::as_tibble() %>%
            dplyr::mutate(requested_by = "description") %>%
            dplyr::rename(package_name = .data$value) %>%
            dplyr::distinct(.data$package_name, .keep_all = TRUE)
    } else {
        desc_package_df <- tibble::tibble(
            package_name = character(),
            requested_by = character()
        )
    }
    
    add_package_info(desc_package_df)
}


# check if a package is already installed as a dependency by comparing the
# package that depend on it with the list of package to be installed
installed_as_dependency <- function(package_name, package_vector) {
    
    depend_on_package <- tools::dependsOnPkgs(package_name)
    common_deps_length <- length(
        intersect(depend_on_package, package_vector))
    
    common_deps_length > 0
}


#' Get information about the package used in the project
#' 
#' @description
#' 
#'  the function returns a data frame containing information regarding 
#'  package that are loaded with \code{library()}, \code{require()}, used
#'  with \code{::} operator and/or already loaded package
#'  
#' @param project_path A string representing the path of the project root 
#' in which the function will look recursively in order to find files that
#' fit \code{include_pattern}
#' @param include_pattern A string representing a regex that matches 
#' project files in which to look for package. By default, \code{get_packages}
#' includes all .R files in the current project
#' @param exclude_pattern A string representing a regex that matches 
#' project files in which not to look for package. By default, 
#' \code{get_packages} excludes all files found in "tests" folder.
#' @param package_options A character vector that represents the method through
#' which package are loaded or referenced. The options are: \code{libray} for 
#' package loaded using \code{libray()}, \code{required} for 
#' package loaded using \code{require()}, \code{referenced} for 
#' package referenced using \code{::} operator, \code{loaded}, used to 
#' include package already loaded in the current session and \code{description}
#' used to include package mentioned in \code{DESCRIPTION} file
#' 
#' @return A data frame containing package information. 
#' Column \code{package_name} contains the name of the package, column 
#' \code{requested_by} contains the name of the context in which the package
#' was loaded (with \code{library} or \code{require}) or called (by reference).
#' Column \code{is_base} is logical, TRUE meaning that the package is part of
#' the core R packages installed by default with R. 
#' Column \code{source} specifies the source from which the package was 
#' installed. Colunmn \code{version} specifies the version of the package, and
#' the column \code{is_installed} contains a logical determining whether or not
#' the package is installed on the machine from which the function is called.
#' 
#' @examples
#' \dontrun{
#' package_df <- get_packages(
#'     project_path = '.',
#'     include_pattern = '\\.R(md)?$', 
#'     exclude_pattern = '', 
#'     package_options = c('referenced'))
#' }
#' 
#' @export
get_packages <- function(
        project_path = ".",
        include_pattern = "\\.R(md)?$", 
        exclude_pattern = "tests|^_", 
        package_options = c("library", "required", "referenced", "description")
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
    
    if ("library" %in% package_options) {
        package <- 
            package %>% 
            dplyr::bind_rows(get_library_package(
                project_path, include_pattern, exclude_pattern))
    }
    if ("required" %in% package_options) {
        package <- 
            package %>% 
            dplyr::bind_rows(get_required_package(
                project_path, include_pattern, exclude_pattern))
    }
    if ("referenced" %in% package_options) {
        package <- 
            package %>% 
            dplyr::bind_rows(get_referenced_package(
                project_path, include_pattern, exclude_pattern))
    }
    
    if ("loaded" %in% package_options) {
        package <- 
            package %>% 
            dplyr::bind_rows(get_loaded_package())
    }
    
    if ("description" %in% package_options) {
        if (project_path !=  ".") {
            description_path <- paste0(project_path, "DESCRIPTION")
        } else {
            description_path <- "DESCRIPTION"
        }
        package <- get_description_package(description_path) %>%
            dplyr::union(package) %>%
            dplyr::distinct(.data$package_name, .keep_all = TRUE)
    }
    
    # keep distinct rows taking into account "package_name" and "version" cols
    if (nrow(package) != 0) {
        package <- 
            package %>%
            dplyr::distinct(.data$package_name, version, .keep_all = TRUE)
    }
    
    package
}


#' Takes a data frame containing package information and generates an install
#' file based on it
#' 
#' @description
#' 
#'  the function takes the output of \code{get_packages} and
#'  writes in a file the commands needed to install
#'  package used throughout the project that are not already installed.
#'   
#' @param package_df A data frame obtained with \code{get_packages} that 
#' contains information regarding the name, version and source of the package.
#' @param include_core_package Logical, whether or not to include in the 
#' generated install file package which come with R by default
#' @param file Tle name of the file to be created.
#' 
#' @return Nothing
#' 
#' @examples
#' \dontrun{
#' package_df <- get_packages(package_options = c("library"))
#' generate_install_file(package_df)
#' }
#' 
#' @seealso \code{\link{get_packages}}
#' 
#' @export
generate_install_file <- function(package_df, 
                                  include_core_package = FALSE, 
                                  file = "install_package.R") {
    
    package_df <- package_df[!(package_df$package_name == "base"), ]
    is_dependency <- purrr::map_lgl(
        package_df$package_name,
        ~ installed_as_dependency(., package_df$package_name))
    
    package_df <- package_df[!is_dependency, ]
    
    if (!include_core_package) {
        package_df <- dplyr::filter(package_df, !.data$is_base)
    }
    
    if (nrow(package_df) == 0) {
        print("No package to install")
    } else {
        package_df_cran <- 
            package_df %>%
            dplyr::filter(source == "CRAN")
        
        package_df_github <-  package_df %>%
            dplyr::filter(source == "github")
            
        cran_package <- paste(
            package_df_cran$package_name, collapse = "','")
        github_package <- paste(
            package_df_github$source_path, collapse = "','")
        vector_cran_package <- paste0(
            "cran_package <- ","c('", cran_package, "') \n")
        vector_github_package <-  paste0(
            "github_package <- ", "c('", github_package, "') \n")
        
        # if there are github package that are not already installed
        # first install 'devtools' package and then use it to 
        # install other package
        install_all_statement <- paste(
            vector_cran_package,
            vector_github_package,
            "
tryCatch({
    install.package(cran_package, quiet = TRUE)
    if (!identical(github_package, c(''))) {
        install.package('devtools', quiet = TRUE)
        devtools::install_github(github_package, quiet = TRUE)
    }
}, error = function(cond) {message(cond)})", sep = "")  
        
        write(install_all_statement, file) 
        cat("Succesfully created install_packages.R file")
    }
  
}
