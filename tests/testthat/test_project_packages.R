context("Testing project_packages.R")

test_that("add_packages_info works", {
    packages_df <- dplyr::data_frame(package_name = "dplyr",
                                     requested_by = "reference")
    
    packages_info <- add_packages_info(packages_df)
    expected_df <- dplyr::data_frame(
        package_name = "dplyr", requested_by = "reference", 
        is_base = FALSE, source = "CRAN", 
        version = "0.7.5", is_installed = TRUE)
    
    expect_equal(packages_info, expected_df)
})


test_that("add_packages_info works with empty df as input", {
    packages_info <- add_packages_info(dplyr::data_frame())
    expected_df <- dplyr::data_frame(is_base = logical(),
                                     source = character(),
                                     version = character(),
                                     is_installed = logical())
    
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


test_that("get_library_packages has the same output", {
    library_packages <- get_library_packages(".R", "")
    expected_output <-  dplyr::data_frame(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        version = character(),
        is_installed = logical())
    
    expect_equal(library_packages, expected_output)
})


test_that("get_library_packages returns the same output", {
    library_packages <- get_library_packages(".R", "")
    expected_output <-  dplyr::data_frame(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        version = character(),
        is_installed = logical())
    
    expect_equal(library_packages, expected_output)
})


test_that("get_referenced_packages returns the same output", {
    referenced_packages <- get_library_packages("os.R", "")
    expected_output <-  dplyr::data_frame(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        version = character(),
        is_installed = logical())
    
    expect_equal(referenced_packages, expected_output)
})


test_that("get_required_packages returns the same output", {
    required_packages <- get_library_packages(".R", "")
    expected_output <-  dplyr::data_frame(
        package_name = character(),
        requested_by = character(),
        is_base = logical(),
        source = character(),
        version = character(),
        is_installed = logical())
    
    expect_equal(required_packages, expected_output)
})

