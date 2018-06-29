INSTALLED_PACKAGES <- rownames(utils::installed.packages())
# quiets concerns of R CMD check
if (getRversion() >= "2.15.1")  
    utils::globalVariables(c("value", "package_name", "is_base"))

add_packages_info <- function(packages_df) {
    if (nrow(packages_df) == 0) {
        packages_df <- packages_df %>%
            tibble::add_column(is_base = logical(),
                               source = character(),
                               version = character(),
                               is_installed = character())
    } else {
        packages_df$is_base <- NA
        packages_df$source <- NA_character_
        packages_df$version <- NA_character_
        packages_df$is_installed <- purrr::map_lgl(
            packages_df$package_name,
            function(x) x %in% INSTALLED_PACKAGES)
        for (i in seq_nrow(packages_df)) {
            # if the package is already installed, take information from package 
            # description, otherwise look for package in CRAN repository
            if (packages_df$is_installed[i]) {
                desc <-  utils::packageDescription(
                    packages_df$package_name[i], 
                    fields = c(
                        "Priority", "Version", "Repository",
                        "GithubRepo", "GithubUsername"))
                desc <- unlist(desc)
                packages_df$is_base[i] <- identical(desc[['Priority']], "base")
                if (is.na(desc[['Repository']])) {
                    if (!is.na(desc[["GithubRepo"]])) {
                        packages_df$source[i] <- paste0(
                            desc[["GithubUsername"]], "/", desc[["GithubRepo"]])
                    }   
                } else {
                    packages_df$source[i] <- desc[['Repository']]
                }
                packages_df$version[i] <- desc[['Version']]
            } else {
                packages_df$source[i] <- NA_character_
                packages_df$is_base[i] <- FALSE
                packages_df$version[i] <- NA
            }
        }
    }
    packages_df
}


get_file_text <- function(include_pattern, exclude_pattern) {
    
    # look in the files of interest only
    project_files <- list.files(
        path = ".", pattern = include_pattern, recursive = TRUE) %>%
        purrr::discard(~grepl(exclude_pattern, .))
    
    # read every line of text from each file, discard commented lines,
    # eliminate new line for Windows machines, eliminate spaces,
    # eliminate new line for Unix/MaxOS, eliminate quotes, eliminate tabs
    exclude_comments_pattern <- '^#.*'
    lines <- project_files %>% 
        purrr::map(readLines) %>%
        unlist(use.names = FALSE) %>%
        purrr::discard(~grepl( exclude_comments_pattern, .)) %>%
        stringr::str_replace_all("[\r\n]" , "") %>%
        stringr::str_replace_all('\\n', '') %>%
        stringr::str_replace_all('"', '') %>%
        stringr::str_replace_all('\'', '') %>%
        stringr::str_replace_all('\\t', '')
    
    # create a string with all the pasted lines
    lines <- paste(lines, collapse = '')
}


get_loaded_packages <- function() {
    
    # get the names of the packages already loaded in the current session
    packages <- dplyr::data_frame(
        package_name = names(utils::sessionInfo()$loadedOnly), 
        stringsAsFactors = FALSE) %>%
        dplyr::mutate(requested_by = "loaded")

    add_packages_info(packages)
}


get_referenced_packages <- function(include_pattern, exclude_pattern) {
    
    code_lines <- get_file_text(include_pattern, exclude_pattern)
    
    regex_pattern <- '[_.a-zA-Z]+::'
    # extract only the names of the packages used with "::" operator,
    # add the column requested by, select rows with distinct package names
    packages <- stringr::str_extract_all(code_lines, regex_pattern) %>% 
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>%
        dplyr::as_data_frame() %>%
        dplyr::mutate(value = gsub('::$', '', value), 
                      requested_by  = "reference") %>%
        dplyr::rename(package_name = value) %>%
        dplyr::distinct(package_name, .keep_all = TRUE)
    
    add_packages_info(packages)
}


get_library_packages <- function(include_pattern, exclude_pattern) {
    
    code_lines <- get_file_text(include_pattern, exclude_pattern)
    
    # create a data frame with the packages loaded individually with "library"
    regex_pattern_single <- '(?<=library\\()([a-zA-Z1-9-_]+?)(?=\\))'
    single_packages <- stringr::str_extract_all(
        code_lines, regex_pattern_single) %>% 
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>% 
        dplyr::as_data_frame()
    
    # create a data frame with multiple packages
    # loaded with a single "library" call
    regex_pattern_multiple <- '(?<=library\\(c\\()(\\X+?)(?=\\)\\))'
    multiple_packages <- stringr::str_extract_all(
        code_lines, regex_pattern_multiple) %>%
        unlist(use.names = FALSE) %>%
        strsplit(",") %>%
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>%
        dplyr::as_data_frame()
    
    # bind the two data frames 
    all_library_packages <- single_packages %>%
        dplyr::bind_rows(multiple_packages)
    
    if (nrow(all_library_packages) == 0) {
        dplyr::as_data_frame( all_library_packages)
    } else {
        # rename columns, add requested_by column, select distinct rows taking 
        # into consideration "package_name" column
        all_library_packages <-  all_library_packages %>%
            dplyr::mutate(package_name = value,
                          requested_by  = "library") %>%
            dplyr::distinct(package_name, .keep_all = TRUE) 
        add_packages_info(all_library_packages)
    }
}


get_required_packages <- function(include_pattern, exclude_pattern) {
    
    code_lines <- get_file_text(include_pattern, exclude_pattern)
    
    # create a data frame with the packages loaded individually with "require"
    regex_pattern_single <- '(?<=require\\()([a-zA-Z1-9-_]+?)(?=\\))'
    single_packages <- stringr::str_extract_all(
        code_lines, regex_pattern_single) %>% 
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>% 
        dplyr::as_data_frame()
    
    # create a data frame with multiple packages
    # loaded with a single "require" call
    regex_pattern_multiple <- '(?<=require\\(c\\()(\\X+?)(?=\\)\\))'
    multiple_packages <- stringr::str_extract_all(
        code_lines, regex_pattern_multiple) %>%
        unlist(use.names = FALSE) %>%
        strsplit(",") %>%
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>%
        dplyr::as_data_frame()
    
    # bind the two data frames 
    all_required_packages <- single_packages %>%
        dplyr::bind_rows(multiple_packages)
    
    if (nrow(all_required_packages) == 0) {
        dplyr::as_data_frame(all_required_packages) 
    } else {
        # rename columns, add requested_by column, select distinct rows taking 
        # into consideration "package_name" column
        all_required_packages <- all_required_packages %>%
            dplyr::mutate(package_name = value,
                          requested_by = "require") %>%
            dplyr::distinct(package_name, .keep_all = TRUE)
        add_packages_info(all_required_packages)
    }
}


get_description_packages <- function(
    description_path = "DESCRIPTION", 
    options = c("Depends", "Imports")) {
    description_data <- read.dcf(description_path)
    desc_packages <- 
        description_data[, intersect(colnames(description_data), options)] %>%
        gsub(pattern = "\n", replacement = "") %>%
        strsplit(",") %>%
        unlist() %>%
        stats::setNames(NULL) 
    
    desc_packages <- desc_packages[!grepl("^R [(]", desc_packages)]
    desc_packages <- desc_packages %>%
        dplyr::as_data_frame() %>%
        dplyr::mutate(requested_by  = "description") %>%
        dplyr::rename(package_name = value) %>%
        dplyr::distinct(package_name, .keep_all = TRUE)
    
    add_packages_info(desc_packages)
}


# check if a package is already installed as a dependency by comparing the
# packages that depend on it with the list of packages to be installed
installed_as_dependency <- function(package_name, package_list) {
    depend_on_package <- tools::dependsOnPkgs(package_name)
    common_deps_length <- length(intersect(depend_on_package, package_list))
    if (common_deps_length  == 0) {is_dependency <- FALSE}
    else{is_dependency <- TRUE}
    is_dependency
}


#' Get information about the packages used in the project
#' 
#' @description
#' 
#'  the function returns a data frame containing information regarding 
#'  packages that are loaded with \code{library()}, \code{require()}, used
#'  with \code{::} operator and/or already loaded packages
#'  
#' 
#' @param include_pattern A string representing a regex that matches 
#' project files in which to look for packages. By default, \code{get_packages}
#' includes all .R files in the current project
#' @param exclude_pattern A string representing a regex that matches 
#' project files in which not to look for packages. By default, 
#' \code{get_packages} excludes all files found in "tests" folder.
#' @param package_options A character vector that represents the method through
#' which packages are loaded or referenced. The options are: \code{libray} for 
#' packages loaded using  \code{libray()}, \code{required} for 
#' packages loaded using  \code{require()}, \code{referenced} for 
#' packages referenced using  \code{::} operator, \code{loaded}, used to 
#' include packages already loaded in the current session and \code{description}
#' used to include packages mentioned in \code{DESCRIPTION} file
#' 
#' @return A data frame containing package information.
#' 
#' @examples
#' get_packages(
#' '\\.R(md)?$', 
#' 'test\\.R(md)?$', 
#' c('required', 'library'))
#' 
#' @export
get_packages <- function(
    include_pattern = '\\.R(md)?$', 
    exclude_pattern = 'tests|^_', 
    package_options = c('library', 'required', 'referenced', 'description')) {
    
    packages <- dplyr::data_frame()
    
    if ("library" %in% package_options) {
        packages <- packages %>% 
            dplyr::bind_rows(
                get_library_packages(include_pattern, exclude_pattern))
    }
    if ("required" %in% package_options) {
        packages <- packages %>% 
            dplyr::bind_rows(get_required_packages(
                include_pattern, exclude_pattern))
    }
    if ("referenced" %in% package_options) {
        packages <- packages %>% 
            dplyr::bind_rows(get_referenced_packages(
                include_pattern, exclude_pattern))
    }
    
    if ("loaded" %in% package_options) {
        packages <- packages %>% 
            dplyr::bind_rows(get_loaded_packages())
    }
    
    if ("description" %in% package_options) {
        packages <- get_description_packages() %>%
            dplyr::union(packages) %>%
            dplyr::distinct(package_name, .keep_all = TRUE)
    }
    
    # keep distinct rows taking into account "package_name" and "version" cols
    if (nrow(packages) != 0) {
       packages <- packages %>%
            dplyr::distinct(package_name, version, .keep_all = TRUE)
    }
    packages

}



#' Takes a data frame containing package information and generates an install
#' file based on it
#' 
#' @description
#' 
#'  the function takes the output of \code{get_packages} and
#'  writes in a file the commands needed in order to install
#'  packages used through the project that are not already installed.
#'   
#' @param packages_df A data frame obtained with \code{get_packages} that 
#' contains information regarding the name, version and source of the package.
#' @param include_core_packages Logical, whether or not to include in the 
#' generated install file packages which come with R by default
#' 
#' @return Nothing
#' 
#' @examples
#' packages_df <- get_packages(
#'                'included_file\\.R(md)?$', 
#'                'excluded_file\\.R(md)?$', 
#'                 c('required', 'library'))
#' generate_install_file(packages_df)
#' 
#' @seealso \code{\link{get_packages}}
#' 
#' @export
generate_install_file <- function(packages_df, include_core_packages = FALSE) {
    packages_df <- packages_df[!(packages_df$package_name == "base"), ]
    is_dependency <- purrr::map_lgl(
        packages_df$package_name,
        ~installed_as_dependency(., packages_df$package_name))
    
    packages_df <- packages_df[!is_dependency, ]
    
    if (!include_core_packages) {
        packages_df <- packages_df %>% 
            dplyr::filter(is_base == FALSE)
    }
    
    packages_df_cran <- packages_df %>%
        dplyr::filter(source == "CRAN")
    packages_df_github <- packages_df %>%
        dplyr::filter(is.na(source))
    if (nrow(packages_df) == 0) {
        print("All necessary packages are already installed!")
    } else {
        
        cran_packages <- paste(
            packages_df_cran$package_name, 
            collapse = "','")
        
        github_packages <- paste(
            packages_df_github$package_name, 
            collapse = "','")
        
        vector_cran_packages <- paste0(
            "cran_packages <- ",
            "c('",
            cran_packages, 
            "') \n")
        
        vector_github_packages <-  paste0(
            "github_packages <- ", "c('", 
            github_packages,
            "') \n")
        
        # if there are github packages that are not already installed
        # first install 'devtools' package and then use it to 
        # install other packages
        install_devtools_stmt <- 'install.packages("devtools") \n'
        install_github_stmt <- paste0('devtools::install_github(',
                                      'github_packages, ',
                                      "quiet = TRUE) \n") 
        install_cran_stmt <- paste0('install.packages(',
                                    'cran_packages, ',
                                    'quiet = TRUE) \n')
        
        install_all_statement <- paste0(
            vector_cran_packages,
            vector_github_packages,
            'tryCatch({ \n',
            'if (length(github_packages) != 0) { \n \t',
            install_devtools_stmt,
            '\t',
            install_github_stmt,
            '} \n' ,
            install_cran_stmt,
            '}, ', 
            'error = function(cond) { message(cond)})')  
        
        write(install_all_statement, file = "install_packages.R") 
    }
}

