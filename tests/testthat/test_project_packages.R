context("Testing project_package.R")


test_that("add_package_info works", {
    package_df <- dplyr::data_frame(
        package_name = "dplyr",
        requested_by = "reference")
    
    package_info <- add_package_info(package_df)
    expected_df <- dplyr::data_frame(
        package_name = "dplyr", 
        requested_by = "reference", 
        is_base = FALSE, 
        source = "CRAN", 
        source_path = NA_character_,
        version = "0.7.6", 
        is_installed = TRUE
    )
    expect_equal(package_info[[4]], expected_df[[4]])
})


test_that("add_package_info works with empty df as input", {
    package_info <- add_package_info(dplyr::data_frame())
    expected_df <- dplyr::data_frame(
        is_base = logical(),
        source = character(),
        source_path = character(),
        version = character(),
        is_installed = logical()
    )
    expect_equal(package_info, expected_df)
})


test_that("add_package_info works with not installed package", {
    package_info <- add_package_info(dplyr::data_frame(
        package_name = "random_package",
        requested_by = "reference"))
    expected_df <- dplyr::data_frame(
        package_name = "random_package",
        requested_by = "reference",
        is_base = FALSE,
        source = NA_character_,
        source_path = NA_character_,
        version = NA_character_,
        is_installed = FALSE)
    
    expect_equal(package_info, expected_df)
})


test_that("add_package_info stops with invalid input", {
    expect_error(add_package_info(NULL))
    expect_error(add_package_info(NA))
    expect_error(add_package_info(NA_character_))
    expect_error(add_package_info(list()))
    expect_error(add_package_info(c()))
})


test_that("get_referenced_package has consistent output", {
    file_path <- "../../R/os.R"
    project_path <- "../../"
    if (!file.exists(file_path)) {
        project_path <- "../../Nmisc/"
        stopifnot(dir.exists(project_path))
    }
    referenced_package <- get_library_package(project_path, "os.R", "")
    expected_output <-  dplyr::data_frame(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        source_path = character(),
        version = character(),
        is_installed = logical())
    
    expect_equal(referenced_package, expected_output)
})


test_that("get_library_package has consistent output", {
    file_path <- "../../R/os.R"
    project_path <- "../../"
    if (!file.exists(file_path)) {
        project_path <- "../../Nmisc/"
        stopifnot(dir.exists(project_path))
    }
    library_package <- get_library_package(project_path, "os.R", "")
    expected_output <-  dplyr::data_frame(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        source_path = character(),
        version = character(),
        is_installed = logical())

    expect_equal(library_package, expected_output)
})


test_that("get_required_package has consistent output", {
    file_path <- "../../R/os.R"
    project_path <- "../../"
    if (!file.exists(file_path)) {
        project_path <- "../../Nmisc/"
        stopifnot(dir.exists(project_path))
    }
    required_package <- get_required_package(project_path, "os.R", "")
    expected_output <-  dplyr::data_frame(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        source_path = character(),
        version = character(),
        is_installed = logical())

    expect_equal(required_package, expected_output)
})


test_that("get_description_package has consistent output", {
    file_path <- "../../DESCRIPTION"
    if (!file.exists(file_path)) {
        file_path <- "../../Nmisc/DESCRIPTION"
        stopifnot(file.exists(file_path))
    }

    description_package <- get_description_package(
        description_path = file_path,
        options = c("Depends"))
    expected_output <-  dplyr::data_frame(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        source_path = character(),
        version = character(),
        is_installed = logical())

    expect_equal(description_package, expected_output)
})


test_that("get_description_package stops with wrong description path", {
    expect_error(description_package <- get_description_package(
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
    package <- get_packages(project_path = project_path, 
                            package_options = c('description'))
    standard_package <- c("dplyr")
    standard_package_found <- standard_package %in% package$package_name
    all_found <- all(standard_package_found)
    expect_true(all_found)
})


test_that("generate_install_file works", {
    dir_path <- "../../R/"
    project_path <- "../../"
    if (!dir.exists(dir_path)) {
        # in order to pass check packaqe
        project_path <- "../../Nmisc/"
        nchar_expected <- 27
        stopifnot(dir.exists(project_path))
    } else {
        nchar_expected <-
            nchar("cran_package <- c('rex','dplyr','stringr','rappdirs')\n")
    }
    needed_package <- get_packages(
        project_path = project_path,
        ".R",
        package_options = c("description"))
    generate_install_file(needed_package)

   

    install_package_content <- readLines("install_package.R", n = 1)
    nchar_install_package <- nchar(install_package_content)
    expect_equal(nchar_expected, nchar_install_package)
    unlink("install_package.R")
})
