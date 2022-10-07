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
  
  estimates <- extractEstimates("Based on propensity-score based Cox model, ARB new user group had a better overall (HR: .878, 95%CI, .854 to .902), and cardiovascular (HR: .841, 95%CI, .800 to .84) survival and had a lower risk for major cardiovascular events (HR: .886, 95%CI, .868 to .905).")
  expect_equal(estimates[1, ],
               tibble(
                 rr = 0.878,
                 p = NA,
                 ciLb = 0.854,
                 ciUb = 0.902,
                 sourceText = "(hr: .878, 95%ci, .854 to .902)",
                 estimateDigits = 3,
                 pDigits = NA
               ))
  expect_equal(estimates[2, ],
               tibble(
                 rr = 0.841,
                 p = NA,
                 ciLb = 0.800,
                 ciUb = 0.840,
                 sourceText = "(hr: .841, 95%ci, .800 to .84)",
                 estimateDigits = 3,
                 pDigits = NA
               ))
  expect_equal(estimates[3, ],
               tibble(
                 rr = 0.886,
                 p = NA,
                 ciLb = 0.868,
                 ciUb = 0.905,
                 sourceText = "(hr: .886, 95%ci, .868 to .905)",
                 estimateDigits = 3,
                 pDigits = NA
               ))
  
  estimates <- extractEstimates("baseline MEST-T2 score (hazard ratio [HR] 3.3, 95% CI 1.7-6.5, P < 0.001), ")
  expect_equal(estimates[1, ],
               tibble(
                 rr = 3.3,
                 p = 0.001,
                 ciLb = 1.7,
                 ciUb = 6.5,
                 sourceText = "(hazard ratio [hr] 3.3, 95% ci 1.7-6.5, p < 0.001)",
                 estimateDigits = 1,
                 pDigits = 3
               ))
})

