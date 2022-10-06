library(testthat)

test_that("test extract estimates", {
  estimates <- extractEstimates("With a HR of 1.5 (1.0 - 2.0) clearly ...")
  expect_equal(estimates[1, ],
               tibble(
                 rr = 1.5,
                 p = NA,
                 ciLb = 1,
                 ciUb = 2,
                 sourceText = "hr of 1.5 (1.0 - 2.0)",
                 estimateDigits = 1,
                 pDigits = NA
               ))
  
  estimates <- extractEstimates("With a HR of 1.51 (1.01 - 2.05) clearly ...")
  expect_equal(estimates[1, ],
               tibble(
                 rr = 1.51,
                 p = NA,
                 ciLb = 1.01,
                 ciUb = 2.05,
                 sourceText = "hr of 1.51 (1.01 - 2.05)",
                 estimateDigits = 2,
                 pDigits = NA
               ))
     
  estimates <- extractEstimates("With a HR of 1.5 (1.0 - 2.0), compared to a HR of 2.3 (CI 95% 1.1 - 4.2) clearly ...")
  expect_equal(estimates[1, ],
               tibble(
                 rr = 1.5,
                 p = NA,
                 ciLb = 1,
                 ciUb = 2,
                 sourceText = "hr of 1.5 (1.0 - 2.0)",
                 estimateDigits = 1,
                 pDigits = NA
               ))     
  expect_equal(estimates[2, ],
               tibble(
                 rr = 2.3,
                 p = NA,
                 ciLb = 1.1,
                 ciUb = 4.2,
                 sourceText = "hr of 2.3 (ci 95% 1.1 - 4.2)",
                 estimateDigits = 1,
                 pDigits = NA
               ))  
  
  estimates <- extractEstimates("With a HR of 1.5 [1.0 - 2.0] clearly ...")
  expect_equal(estimates[1, ],
               tibble(
                 rr = 1.5,
                 p = NA,
                 ciLb = 1,
                 ciUb = 2,
                 sourceText = "hr of 1.5 [1.0 - 2.0]",
                 estimateDigits = 1,
                 pDigits = NA
               ))
  
  estimates <- extractEstimates("With a clear effect (HR = 1.5, 1.0 - 2.0) suggesting ...")
  expect_equal(estimates[1, ],
               tibble(
                 rr = 1.5,
                 p = NA,
                 ciLb = 1,
                 ciUb = 2,
                 sourceText = "(hr = 1.5, 1.0 - 2.0)",
                 estimateDigits = 1,
                 pDigits = NA
               ))
  
  estimates <- extractEstimates("With a clear effect (HR = 1.5, p = 0.01) suggesting ...")
  expect_equal(estimates[1, ],
               tibble(
                 rr = 1.5,
                 p = 0.01,
                 ciLb = NA,
                 ciUb = NA,
                 sourceText = "(hr = 1.5, p = 0.01)",
                 estimateDigits = 1,
                 pDigits = 2
               ))
  
  estimates <- extractEstimates("With OR = 1.5 (p = 0.01), suggesting ...")
  expect_equal(estimates[1, ],
               tibble(
                 rr = 1.5,
                 p = 0.01,
                 ciLb = NA,
                 ciUb = NA,
                 sourceText = "or = 1.5 (p = 0.01)",
                 estimateDigits = 1,
                 pDigits = 2
               ))
  
})

