add_packages_info <- function(pkgs) {
 
    pkgs$is_installed <- purrr::map_lgl(
        pkgs$package,
        function(x) x %in% rownames(installed.packages()))
    
    crans <- available.packages()[, "Package"]
    
    for (i in 1:nrow(pkgs)) {
        if (pkgs$is_installed[i]) {
            
            desc <- purrr::map(pkgs$package[i], utils::packageDescription)
            pkgs$is_base[i] <- purrr::map_lgl(
                desc, function(x) identical(x$Priority, "base"))
            pkgs$source[i] <- sapply(desc, "[", "Repository")
            pkgs$version[i] <- sapply(desc, "[", "Version")
        } else {
            if (pkgs$package[i] %in% crans) {
                pkgs$source[i] <- 'CRAN'
            } else {
                pkgs$source[i] <- NA_character_
            }
            
            pkgs$is_base[i] <- FALSE
            pkgs$version[i] <- NA
        }
    }
    pkgs <- pkgs[!pkgs$is_base, ]
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
    
    pkgs <- data.frame(
        package = names(sessionInfo()$loadedOnly), 
        stringsAsFactors = FALSE)
    
    add_packages_info(pkgs)
    pkgs$requested_by <- "loaded"
}


referenced_packages <- function() {
    
    lns <- prepare_file_text()
    
    regex_pattern <- '[[:alnum:]_.]+::'
    pkg <- stringr::str_extract_all(lns, regex_pattern) %>% 
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>%
        dplyr::as_data_frame() %>%
        dplyr::mutate(value = gsub('::$', '', value)) %>%
        dplyr::rename(package = value) %>%
        dplyr::distinct()
    
    add_packages_info(pkg)
    pkgs$requested_by <- "reference"
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
            dplyr::rename(package = value) %>%
            dplyr::distinct() %>%
            as.data.frame()
        add_packages_info(pkg)
    }
    pkgs$requested_by <- "library"
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
            dplyr::rename(package = value) %>%
            dplyr::distinct() %>%
            as.data.frame()
        add_packages_info(pkg)
    }
    pkgs$requested_by <- "require"
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

