local({
  # R sets this when running tests, which messes up install.packages
  # https://github.com/hadley/testthat/issues/144
  old <- Sys.getenv("R_TESTS")
  Sys.setenv(R_TESTS = "")
  on.exit(Sys.setenv(R_TESTS = old))

  # Install test packages
  lib_dir <- tempfile()
  dir.create(lib_dir)
  on.exit(unlink(lib_dir, recursive = TRUE), add = TRUE)
  install.packages("testPkg", repos = NULL, lib = lib_dir)
  library("testPkg", lib.loc = lib_dir)

  ## Tests
  test_that("annotation equals the one in generics", {
      expect_equal(generics::annotation, annotation)
  })

  test_that("annotation works for S3 objects", {

      t1 <- testS3()
      expect_identical(t1$annotation, "S3")
      expect_identical(annotation(t1), "S3")
      expect_identical(annotation(t1), t1$annotation)

      t2 <- structure(list(), class = "unknown")
      expect_error(annotation(t2), "no applicable method for")
  })

  test_that("annotation works for S4 objects", {

      t1 <- testS4()
      expect_identical(t1@annotation, "S4")
      expect_identical(annotation(t1), "S4")
      expect_identical(annotation(t1), t1@annotation)

      t2 <- structure(list(), class = "unknown")
      expect_error(annotation(t2), "no applicable method for")
  })

  test_that("annotation equals the one in generics", {
      expect_equal(generics::`annotation<-`, `annotation<-`)
  })

  test_that("annotation<- works for S3 objects", {

      t1 <- testS3()
      expect_identical(t1$annotation, "S3")

      annotation(t1) <- "testing"
      expect_identical(t1$annotation, "testing")
      expect_identical(annotation(t1), "testing")
      expect_identical(annotation(t1), t1$annotation)

      t2 <- structure(list(), class = "unknown")
      expect_error(annotation(t2) <- "testing", "no applicable method for")
  })
  test_that("annotation<- works for S4 objects", {

      t1 <- testS4()
      expect_identical(t1@annotation, "S4")

      annotation(t1) <- "testing"
      expect_identical(t1@annotation, "testing")
      expect_identical(annotation(t1), "testing")
      expect_identical(annotation(t1), t1@annotation)

      t2 <- structure(list(), class = "unknown")
      expect_error(annotation(t2) <- "testing", "no applicable method for")
  })
})
