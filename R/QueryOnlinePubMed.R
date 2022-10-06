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

#' Get Observational research query.
#'
#' @return
#' A query for finding all observational studies in PubMed.
#' 
#' @references 
#' Schuemie MJ, Ryan PB, Hripcsak G, Madigan D, Suchard MA. Improving reproducibility by using high-throughput observational studies with empirical calibration. Philos Trans A Math Phys Eng Sci. 2018 Sep 13;376(2128):20170356. doi: 10.1098/rsta.2017.0356. PMID: 30082302; PMCID: PMC6107542.
#' 
#' 
#' @export
getObservationalResearchQuery <- function() {
  query <- "(\"population-based\" [Title/Abstract] OR observational [Title/Abstract] OR pharmacoepidemiology [Title/Abstract]) AND ((\"Cohort Studies\" [MeSH] OR \"cohort\" [Title/Abstract] OR \"propensity score\" [Title/Abstract]) OR (\"Case-Control Studies\" [MeSH] OR \"case control\" [Title/Abstract]) OR (\"self controlled case series\" [Title/Abstract] OR (\"sccs\" [Title/Abstract] AND \"self-controlled\" [Title/Abstract])) OR (\"case-crossover\" [Title/Abstract]) ) AND (\"1900/01/01\"[PDAT]:\"3000/12/31\"[PDAT])"
  return(query)
}

#' Query online PubMed
#'
#' @param query  A string representing a PubMed query.
#'
#' @return
#' A vector of PMIDs.
#' 
#' @export
queryOnlinePubmed <- function(query) {
  # Number of PMIDs per query. Max allowed is 100,000:
  retMax <- 10000
  
  pmids <- c()
  totalCount <- Inf
  pb <- txtProgressBar(style = 3)
  while(length(pmids) < totalCount) {
    retStart <- length(pmids)
    url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmax=%d&retstart=%d&term=%s"
    url <- sprintf(url, retMax, retStart, URLencode(query))
    batch <- readLines(url)
    batch <- xml2::read_xml(paste(batch, collapse = "\n"), batch)
    totalCount <- as.integer(xml2::xml_text(xml2::xml_find_first(batch, "Count")))
    batch <- xml2::xml_text(xml2::xml_find_all(xml2::xml_find_first(batch, "IdList"), "Id"))
    pmids <- c(pmids, batch)
    setTxtProgressBar(pb, length(pmids) / as.numeric(totalCount))
    # Don't flood NLM with queries. Sleep for 1 second until next one:
    Sys.sleep(1)
  }
  close(pb)
  return(pmids)
}