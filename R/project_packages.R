add_packages_info <- function(pkgs) {
    
    desc <- lapply(pkgs$package, utils::packageDescription)
    pkgs$is_base <- vapply(
        desc, function(x) identical(x$Priority, "base"), logical(1))
    
    pkgs$source <- sapply(desc, "[", "Repository")
    pkgs$version <- sapply(desc, "[", "Version")

    pkgs <- pkgs[!pkgs$is_base, ]
}


loaded_packages <- function() {
    
    pkgs <- data.frame(
        package = names(sessionInfo()$loadedOnly), 
        stringsAsFactors = FALSE)
    
    add_packages_info(pkgs)
}


referenced_packages <- function() {
    
    file_pattern <- '.*\\.R(md)?$'
    exclude_pattern <- 'EDA/|test|^_'
    fls <- list.files(path = ".", pattern = file_pattern, recursive = TRUE) %>%
        purrr::discard(~grepl(exclude_pattern, .))
    
    lns <- fls %>%
        purrr::map(readLines) %>%
        unlist(use.names = FALSE)
    
    regex_pattern <- '[[:alnum:]_.]+::'
    pkg <- stringr::str_extract_all(lns, regex_pattern) %>% 
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>% 
        tibble::as_tibble() %>%
        dplyr::mutate(value = gsub('::$', '', value)) %>%
        dplyr::rename(package = value) %>%
        dplyr::distinct() %>%
        as.data.frame()
    
    add_packages_info(pkg)
}



library_packages <- function() {
    
    file_pattern <- '.*\\.R(md)?$'
    exclude_pattern <- 'EDA/|test|^_'
    fls <- list.files(path = ".", pattern = file_pattern, recursive = TRUE) %>%
        purrr::discard(~grepl(exclude_pattern, .))
    
    lns <- fls %>%
        purrr::map(readLines) %>%
        unlist(use.names = FALSE)
    
    regex_pattern <- '(?<=library\\(\\")([^\r\n]+)(?=\\"\\))'
    pkg <- stringr::str_extract_all(lns, regex_pattern) %>% 
        purrr::discard(~length(.) == 0) %>%
        unlist(use.names = FALSE) %>% 
        tibble::as_tibble() %>%
        dplyr::rename(package = value) %>%
        dplyr::distinct() %>%
        as.data.frame()
    
    add_packages_info(pkg)
}



install_project_packages <- function() {
    
    referenced_pkgs <- referenced_packages()
    loaded_pkgs <- loaded_packages()
    library_pkgs <- library_packages()
    
    all_packages <- referenced_pkgs %>%
        # dplyr::bind_rows(loaded_pkgs) %>%
        dplyr::bind_rows(library_pkgs) %>%
        dplyr::distinct(package)
    
    all_packages$is_installed <- vapply(
        all_packages$package,
        function(x) x %in% rownames(installed.packages()), 
        logical(1))
    
    all_packages <- all_packages[!all_packages$is_installed, ]
    #install.packages(all_packages$package)
}

