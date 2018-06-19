INSTALLED_PACKAGES <- rownames(installed.packages())

add_packages_info <- function(packages_df) {
    
    packages_df$is_installed <- purrr::map_lgl(
        packages_df$package_name,
        function(x) x %in% INSTALLED_PACKAGES)
    
    cran_packages <- available.packages()[, "Package"]
    
    for (i in 1:nrow(packages_df)) {
        # if the package is already installed, we take information from package 
        # descriptions, otherwise we look for it in CRAN repository
        if (packages_df$is_installed[i]) {
            desc <- lapply(packages_df$package_name[i],
                           utils::packageDescription)
            packages_df$is_base[i] <- purrr::map_lgl(
                desc, function(x) identical(x$Priority, "base"))
            packages_df$source[i] <- sapply(desc, "[", "Repository")
            packages_df$version[i] <- sapply(desc, "[", "Version")
            
        } else {
            if (packages_df$package_name[i] %in% cran_packages) {
                packages_df$source[i] <- 'CRAN'
            } else {
                packages_df$source[i] <- NA_character_
            }
            
            packages_df$is_base[i] <- FALSE
            packages_df$version[i] <- NA
        }
    }
    packages_df <- packages_df[!packages_df$is_base, ]
}



prepare_file_text <- function(include_pattern, exclude_pattern) {
    
    fls <- list.files(
        path = ".", pattern = include_pattern, recursive = TRUE) %>%
        purrr::discard(~grepl(exclude_pattern, .))
    
    exclude_comments_pattern <- '^#.*'
    lns <- fls %>%
        purrr::map(readLines) %>%
        unlist(use.names = FALSE) %>%
        purrr::discard(~grepl(exclude_comments_pattern, .)) %>%
        stringr::str_replace_all("[\r\n]" , "") %>%
        stringr::str_replace_all('\\s', '') %>%
        stringr::str_replace_all('\\n', '') %>%
        stringr::str_replace_all('"', '') %>%
        stringr::str_replace_all('\'', '') %>%
        stringr::str_replace_all('\\t', '')
    
    lns <- paste(lns, collapse = '')
    
    lns
}


loaded_packages <- function() {
    
    pkgs <- dplyr::data_frame(
        package_name = names(sessionInfo()$loadedOnly), 
        stringsAsFactors = FALSE) %>%
        dplyr::mutate(requested_by = "loaded")
    
    add_packages_info(pkgs)
}


referenced_packages <- function(include_pattern, exclude_pattern) {
    
    lns <- prepare_file_text(include_pattern, exclude_pattern)
    
    regex_pattern <- '[[:alnum:]_.]+::'
    pkg <- stringr::str_extract_all(lns, regex_pattern) %>% 
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>%
        dplyr::as_data_frame() %>%
        dplyr::mutate(value = gsub('::$', '', value), 
                      requested_by  = "reference") %>%
        dplyr::rename(package_name = value) %>%
        dplyr::distinct()
    
    add_packages_info(pkg)
}


library_packages <- function(include_pattern, exclude_pattern) {
    
    lns <- prepare_file_text(include_pattern, exclude_pattern)
    
    regex_pattern_single <- '(?<=library\\()([a-zA-Z1-9-_]+?)(?=\\))'
    pkg_single <- stringr::str_extract_all(lns, regex_pattern_single) %>% 
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>% 
        tibble::as_tibble()
    
    regex_pattern_multiple <- '(?<=library\\(c\\()(\\X+?)(?=\\)\\))'
    pkg_multiple <- stringr::str_extract_all(lns, regex_pattern_multiple) %>%
        unlist(use.names = FALSE) %>%
        strsplit(",") %>%
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>%
        tibble::as_tibble()
    
    pkg <- pkg_single %>%
        dplyr::bind_rows(pkg_multiple)
    
    if (nrow(pkg) == 0) {
        as.data.frame(pkg) 
    } else {
        pkg <- pkg %>%
            dplyr::mutate(package_name = value,
                          requested_by  = "library") %>%
            dplyr::distinct() %>%
            as.data.frame()
        add_packages_info(pkg)
    }
}


required_packages <- function(include_pattern, exclude_pattern) {
    
    lns <- prepare_file_text(include_pattern, exclude_pattern)
    
    regex_pattern_single <- '(?<=require\\()([a-zA-Z1-9-_]+?)(?=\\))'
    pkg_single <- stringr::str_extract_all(lns, regex_pattern_single) %>% 
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>% 
        tibble::as_tibble() 
    
    regex_pattern_multiple <- '(?<=require\\(c\\()(\\X+?)(?=\\)\\))'
    pkg_multiple <- stringr::str_extract_all(lns, regex_pattern_multiple) %>%
        unlist(use.names = FALSE) %>%
        strsplit(",") %>%
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>%
        tibble::as_tibble()
    
    pkg <- pkg_single %>%
        dplyr::bind_rows(pkg_multiple)
    
    if (nrow(pkg) == 0) {
        as.data.frame(pkg) 
    } else {
        pkg <- pkg %>%
            dplyr::mutate(package_name = value,
                          requested_by = "require") %>%
            dplyr::distinct() %>%
            as.data.frame()
        add_packages_info(pkg)
    }
    
}


get_packages <- function(
    include_pattern = '.*\\.R(md)?$', 
    exclude_pattern = 'EDA/|test|^_', 
    package_options = NULL) {
    
    packages <- dplyr::data_frame()
    if (!is.null(package_options)) {
        if ("library" %in% package_options) {
            packages <- packages %>% 
                dplyr::bind_rows(
                    library_packages(include_pattern, exclude_pattern))
        }
        if ("required" %in% package_options) {
            packages <- packages %>% 
                dplyr::bind_rows(required_packages(
                    include_pattern, exclude_pattern))
        }
        if ("referenced" %in% package_options) {
            packages <- packages %>% 
                dplyr::bind_rows(referenced_packages(
                    include_pattern, exclude_pattern))
        }
        packages %>%
            dplyr::distinct()
    } else {
        packages <- packages %>% 
            dplyr::bind_rows(library_packages(
                include_pattern, exclude_pattern)) %>%
            dplyr::bind_rows(required_packages(
                include_pattern, exclude_pattern)) %>%
            dplyr::bind_rows(referenced_packages(
                include_pattern, exclude_pattern))  %>%
            dplyr::distinct()
    }
}


generate_install_file <- function(packages_df) {
    
    packages_df <- packages_df[!packages_df$is_installed, ]
    packages_df_cran <- packages_df[packages_df$source == 'CRAN', ]
    packages_df_github <- packages_df[packages_df$source != 'CRAN', ]
    
    if (nrow(packages_df) == 0) {
        print("All necessary packages are already installed!")
    } else {
        if (nrow(packages_df_github) == 0) {
            
            packages_df_cran <- paste(
                packages_df_cran$package_name, 
                collapse = "','")
            install_stmt_cran <- paste0(
                "install.packages(c('",
                packages_df_cran, 
                "'), quiet = TRUE",
                ")")
            
            write(install_stmt_cran, file = "instpackages_df.R")
            
        } else if (nrow(packages_df_cran) == 0) {
            
            packages_df_github <- paste(
                packages_df_github$source, 
                collapse = "','")
            install_stmt_github <-  paste0(
                "devtools::install_github(c('", 
                packages_df_github,
                "'), quiet = TRUE",
                ")")
            
            write(install_stmt_github, file = "instpackages_df.R")
        } else {
            packages_df_cran <- paste(
                packages_df_cran$package_name,
                collapse = ",")
            packages_df_github <- paste(
                packages_df_github$source, 
                collapse = "','")
            
            install_stmt_cran <- paste0(
                "install.packages(c(",
                packages_df_cran, 
                ', quiet = TRUE',
                "))")
            
            install_stmt_github <-  paste0(
                "devtools::install_github(c('", 
                packages_df_github,
                "'), quiet = TRUE",
                ")")
            install_stmt <- paste0(install_stmt_cran, '; ', install_stmt_github)
            
            write(install_stmt, file = "install_packages.R") 
        }
    }
}
