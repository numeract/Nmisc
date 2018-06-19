INSTALLED_PACKAGES <- rownames(installed.packages())
add_packages_info <- function(packages_df) {
    packages_df$is_installed <- purrr::map_lgl(
        packages_df$package_name,
        function(x) x %in% INSTALLED_PACKAGES)
    
    cran_packages <- available.packages()[, "Package"]
    
    for (i in 1:nrow(packages_df)) {
        if (packages_df$is_installed[i]) {
            desc <- lapply(packages_df$package_name[i], utils::packageDescription)
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



prepare_file_text <- function() {
    
    file_pattern <- '.*\\.R(md)?$'
    exclude_pattern <- 'EDA/|test|^_'
    
    fls <- list.files(path = ".", pattern = file_pattern, recursive = TRUE) %>%
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


referenced_packages <- function() {
    
    lns <- prepare_file_text()
    
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


library_packages <- function() {
    
    lns <- prepare_file_text()
    
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


required_packages <- function() {
    
    lns <- prepare_file_text()
    
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


generate_install_file <- function() {
    
    referenced_pkgs <- referenced_packages()
    loaded_pkgs <- loaded_packages()
    library_pkgs <- library_packages()
    required_pkgs <- required_packages()
    
    all_packages <- referenced_pkgs %>%
        dplyr::bind_rows(required_pkgs) %>%
        dplyr::bind_rows(library_pkgs) %>%
        dplyr::distinct()
    
    all_packages <- all_packages[!all_packages$is_installed, ]
    all_packages_cran <- all_packages[all_packages$source == 'CRAN', ]
    all_packages_github <- all_packages[all_packages$source != 'CRAN', ]
    
    if (nrow(all_packages) == 0) {
        print("All necessary packages are already installed!")
    } else {
        if (nrow(all_packages_github) == 0) {
            
            all_packages_cran <- paste(
                all_packages_cran$package, 
                collapse = "','")
            install_stmt_cran <- paste0(
                "install.packages(c('",
                all_packages_cran, 
                "'), quiet = TRUE",
                ")")
            
            write(install_stmt_cran, file = "install_packages.R")
            
        } else if (nrow(all_packages_cran) == 0) {
            
            all_packages_github <- paste(
                all_packages_github$source, 
                collapse = "','")
            install_stmt_github <-  paste0(
                "devtools::install_github(c('", 
                all_packages_github,
                "'), quiet = TRUE",
                ")")
            
            write(install_stmt_github, file = "install_packages.R")
        } else {
            all_packages_cran <- paste(
                all_packages_cran$package,
                collapse = ",")
            all_packages_github <- paste(
                all_packages_github$source, 
                collapse = "','")
            
            install_stmt_cran <- paste0(
                "install.packages(c(",
                all_packages_cran, 
                ', quiet = TRUE',
                "))")
           
            install_stmt_github <-  paste0(
                "devtools::install_github(c('", 
                all_packages_github,
                "'), quiet = TRUE",
                ")")
            install_stmt <- paste0(install_stmt_cran, ';', install_stmt_github)
            
            write(install_stmt, file = "install_packages.R") 
        }
    }
}

