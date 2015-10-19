context("errors")

test_that("check for errors",
          {
          expect_that(getStandardInfo("ferulic acid"),not(throws_error()))

          }
)
