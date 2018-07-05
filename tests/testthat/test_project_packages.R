context("Testing project_packages.R")


test_that("add_packages_info works", {
    packages_df <- dplyr::data_frame(
        package_name = "dplyr",
        requested_by = "reference")
    
    packages_info <- add_packages_info(packages_df)
    expected_df <- dplyr::data_frame(
        package_name = "dplyr", 
        requested_by = "reference", 
        is_base = FALSE, 
        source = "CRAN", 
        version = "0.7.6", 
        is_installed = TRUE
    )
    expect_equal(packages_info[-5], expected_df[-5])
})


test_that("add_packages_info works with empty df as input", {
    packages_info <- add_packages_info(dplyr::data_frame())
    expected_df <- dplyr::data_frame(
        is_base = logical(),
        source = character(),
        version = character(),
        is_installed = logical()
    )
    expect_equal(packages_info, expected_df)
})


test_that("add_packages_info works with not installed package", {
    packages_info <- add_packages_info(dplyr::data_frame(
        package_name = "random_package",
        requested_by = "reference"))
    expected_df <- dplyr::data_frame(
        package_name = "random_package",
        requested_by = "reference",
        is_base = FALSE,
        source = NA_character_,
        version = NA_character_,
        is_installed = FALSE)
    
    expect_equal(packages_info, expected_df)
})


test_that("add_packages_info stops with invalid input", {
    expect_error(add_packages_info(NULL))
    expect_error(add_packages_info(NA))
    expect_error(add_packages_info(NA_character_))
    expect_error(add_packages_info(list()))
    expect_error(add_packages_info(c()))
})


test_that("get_referenced_packages has consistent output", {
    file_path <- "../../R/os.R"
    project_path <- "../../"
    if (!file.exists(file_path)) {
        project_path <- "../../Nmisc/"
        stopifnot(dir.exists(project_path))
    }
    referenced_packages <- get_library_packages(project_path, "os.R", "")
    expected_output <-  dplyr::data_frame(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        version = character(),
        is_installed = logical())
    
    expect_equal(referenced_packages, expected_output)
})


test_that("get_library_packages has consistent output", {
    file_path <- "../../R/os.R"
    project_path <- "../../"
    if (!file.exists(file_path)) {
        project_path <- "../../Nmisc/"
        stopifnot(dir.exists(project_path))
    }
    library_packages <- get_library_packages(project_path, "os.R", "")
    expected_output <-  dplyr::data_frame(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        version = character(),
        is_installed = logical())

    expect_equal(library_packages, expected_output)
})


test_that("get_required_packages has consistent output", {
    file_path <- "../../R/os.R"
    project_path <- "../../"
    if (!file.exists(file_path)) {
        project_path <- "../../Nmisc/"
        stopifnot(dir.exists(project_path))
    }
    required_packages <- get_required_packages(project_path, "os.R", "")
    expected_output <-  dplyr::data_frame(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        version = character(),
        is_installed = logical())

    expect_equal(required_packages, expected_output)
})


test_that("get_description_packages has consistent output", {
    file_path <- "../../DESCRIPTION"
    if (!file.exists(file_path)) {
        file_path <- "../../Nmisc/DESCRIPTION"
        stopifnot(file.exists(file_path))
    }

    description_packages <- get_description_packages(
        description_path = file_path,
        options = c("Depends"))
    expected_output <-  dplyr::data_frame(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        version = character(),
        is_installed = logical())

    expect_equal(description_packages, expected_output)
})


test_that("get_description_packages stops with wrong description path", {
    expect_error(description_packages <- get_description_packages(
        description_path = "../../R/os.R",
        options = c("Depends")))
})


test_that("get_packages works", {
    dir_path <- "../../R/"
    project_path <- "../../"
    if (!dir.exists(dir_path)) {
        project_path <- "../../Nmisc/"
        stopifnot(dir.exists(project_path))
    }
    packages <- get_packages(project_path = project_path, 
                             package_options = c('description'))
    standard_package <- c("dplyr")
    standard_packages_found <- standard_package %in% packages$package_name
    all_found <- all(standard_packages_found)
    expect_true(all_found)
})


test_that("generate_install_file works", {
    dir_path <- "../../R/"
    project_path <- "../../"
    if (!dir.exists(dir_path)) {
        # in order to pass check packaqe
        project_path <- "../../Nmisc/"
        nchar_expected <- 28
        stopifnot(dir.exists(project_path))
    } else {
        nchar_expected <-
            nchar("cran_packages <- c('rex','dplyr','stringr','rappdirs')\n")
    }
    needed_packages <- get_packages(
        project_path = project_path,
        ".R",
        package_options = c("description"))
    generate_install_file(needed_packages)

   

    install_packages_content <- readLines("install_packages.R", n = 1)
    nchar_install_packages <- nchar(install_packages_content)
    expect_equal(nchar_expected, nchar_install_packages)
    unlink("install_packages.R")
})
