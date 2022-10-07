# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of MedlineAnalysis
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

numberPattern <- "[0-9]?[0-9]?[0-9]?\\.[0-9][0-9]?[0-9]?"
emPattern <- paste0("(odds ratio|o.r.|or|relative risk|r.r.|rr|hazard ratio|h.r.|hr|hazard|rate ratio)([^0-9a-z]*| is | of )", numberPattern)
pValuePattern <- "p ?[<=>] ?0?\\.[0-9][0-9]?[0-9]?"
ciPattern <- paste0(numberPattern, " ?(-|to) ?", numberPattern)
estimatePattern <- paste0("(", emPattern, " ?[\\(\\[]|[\\(\\[][^)\\]]*", emPattern, ")[^(\\[]*(", pValuePattern, "|", ciPattern, ")[^(\\[]*[\\)\\]]")

#' Extract effect-size estimates from text
#'
#' @param abstract   The text from which to extract the estimates.
#'
#' @return
#' A tibble with one row per estimate.
#'
#' @export
extractEstimates <- function(abstract) {
  estimates <- stringr::str_extract_all(tolower(abstract), estimatePattern, )[[1]]
  estimates
  estimates <- lapply(estimates, extractEstimate)
  estimates <- bind_rows(estimates)
  return(estimates)
}

# estimate = estimates[1]
extractEstimate <- function(estimate) {
  result <- tibble(
    rr = NA,
    p = NA,
    ciLb = NA,
    ciUb = NA,
    sourceText = estimate,
    estimateDigits = NA,
    pDigits = NA
  )

  substring <- stringr::str_extract(estimate, emPattern)
  if (!is.na(substring)) {
    rrString <- stringr::str_extract(substring, numberPattern)
    result$rr <- as.numeric(rrString)
    result$estimateDigits <- nchar(stringr::str_extract(rrString, "\\.[0-9]+")) - 1
  }
  substring <- stringr::str_extract(estimate, pValuePattern)
  if (!is.na(substring)) {
    pString <- stringr::str_extract(substring, numberPattern)
    result$p <- as.numeric(pString)
    result$pDigits <- nchar(stringr::str_extract(pString, "\\.[0-9]+")) - 1
  }

  substring <- stringr::str_extract(estimate, ciPattern)
  if (!is.na(substring)) {
    ci <- as.numeric(stringr::str_extract_all(substring, numberPattern)[[1]])
    result$ciLb <- ci[1]
    result$ciUb <- ci[2]
  }
  return(result)
}

#' Jitter estimates
#'
#' @description
#' Adds random noise to effect-size estimates such that the estimated statistics still
#' round to the numbers extracted from the text.
#'
#' @param estimates A tibble containing effect-size estimates as extracted using the
#'                  [extractEstimates()] function.
#'
#' @return
#' A tibble containing effect-size estimates, with noise added.
#'
#' @export
jitterEstimates <- function(estimates) {
  estimates$rr <- jitterNumber(estimates$rr, estimates$estimateDigits)
  estimates$p <- jitterNumber(estimates$p, estimates$pDigits)
  estimates$ciLb <- jitterNumber(estimates$ciLb, estimates$estimateDigits)
  estimates$ciUb <- jitterNumber(estimates$ciUb, estimates$estimateDigits)
  return(estimates)
}

jitterNumber <- function(number, digits) {
  noise <- runif(length(number), -0.5, 0.5)
  noise <- noise * 10^(-digits)
  # all(round(number, digits) == round(number + noise, digits), na.rm = T)
  number <- number + noise
  return(number)
}

#' Compute logRr and seLogRr for estimates
#'
#' @param estimates A tibble containing effect-size estimates as extracted using the
#'                  [extractEstimates()] function.
#'
#' @return
#' A tibble containing effect-size estimates, with the logRr and seLogRr columns added.
#'
#' @export
computeLogRrAndSeLogRr <- function(estimates) {
  estimates <- suppressWarnings(
    estimates %>%
      mutate(
        seFromCi = (log(.data$ciUb) - log(.data$ciLb)) / (2 * qnorm(0.975)),
        seFromP = abs(log(.data$rr) / qnorm(.data$p))
      ) %>%
      mutate(
        logRr = log(.data$rr),
        seLogRr = ifelse(is.na(.data$seFromCi), .data$seFromP, .data$seFromCi)
      ) %>%
      select(-.data$seFromCi, -.data$seFromP)
  )
  return(estimates)
}

#' Plot effect-size estimates
#'
#' @param estimates A tibble containing effect-size estimates as extracted using the
#'                  [extractEstimates()] function.
#' @param jitter    Should the estimates be jittered when plotting?
#' @param fileName  If provided, the plot will be saved to this file.
#'
#' @return
#' Returns a ggplot plot.
#' 
#' @export
plotEstimates <- function(estimates, jitter = TRUE, fileName = NULL) {
  counts <- estimates %>%
    mutate(significant = ifelse(is.na(.data$p), .data$ciLb > 1 | .data$ciUb < 1, .data$p < 0.05)) %>%
    summarise(
      significantCount = sum(.data$significant, na.rm = TRUE),
      totalCount = n()
    )

  labels <- tibble(
    logRr = c(log(0.15), log(0.15)),
    seLogRr = c(0.9, 0.8),
    label = c(
      paste0(formatC(counts$totalCount, big.mark = ","), " estimates"),
      label2 <- paste0(formatC(100 * (1 - counts$significantCount / counts$totalCount), digits = 1, format = "f"), "% of CIs include 1")
    )
  )

  if (jitter) {
    estimates <- jitterEstimates(estimates)
  }

  if (!"seLogRr" %in% colnames(estimates)) {
    estimates <- computeLogRrAndSeLogRr(estimates)
  }
  estimates <- estimates %>%
    filter(!is.na(.data$seLogRr))

  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  themeLA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 0)

  alpha <- 1 - min(0.95 * (nrow(estimates) / 50000)^0.1, 0.95)
  plot <- ggplot2::ggplot(estimates, ggplot2::aes(x = .data$logRr, y = .data$seLogRr)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_abline(slope = 1 / qnorm(0.025), colour = rgb(0.8, 0, 0), linetype = "dashed", size = 1, alpha = 0.5) +
    ggplot2::geom_abline(slope = 1 / qnorm(0.975), colour = rgb(0.8, 0, 0), linetype = "dashed", size = 1, alpha = 0.5) +
    ggplot2::geom_point(size = 1, color = rgb(0, 0, 0, alpha = 0.05), alpha = alpha, shape = 16) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_label(ggplot2::aes(label = .data$label), alpha = 1, hjust = "left", size = 5, data = labels) +
    ggplot2::scale_x_continuous("Effect size", limits = log(c(0.1, 10)), breaks = log(breaks), labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      axis.title = theme,
      legend.key = ggplot2::element_blank(),
      strip.text.x = theme,
      strip.text.y = theme,
      strip.background = ggplot2::element_blank(),
      legend.position = "none"
    )
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 8, height = 5, dpi = 300)
  }
}
