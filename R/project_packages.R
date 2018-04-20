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
