# Code used for the 2022 OHDSI Symposium, showing distribution of estimates in 
# hypertension observational research
library(MedlineAnalysis)
library(dplyr)

folder <- "s:/temp/MedlineHypertension"
# dir.create(folder)

connectionDetails <- createConnectionDetails(
  dbms = "postgresql",
  user = keyring::key_get("medlineUser"),
  password = keyring::key_get("medlinePassword"),
  connectionString = keyring::key_get("medlineConnectionString")
)
databaseSchema <- keyring::key_get("medlineDatabase")


# Find all PMIDs of observational research papers ------------------------------
query <- getObservationalResearchQuery()
pmids <- queryOnlinePubmed(query)
saveRDS(pmids, file.path(folder, "AllObsResearchPmids.rds"))


# Extract all effect-size estimates from given PMIDs ---------------------------
pmids <- readRDS(file.path(folder, "AllObsResearchPmids.rds"))
# pmids <- queryOnlinePubmed("schuemie")
# pmids <- 36139140

fetchSettings <- createFetchSettings(
  abstract = TRUE
)

processCitation <- function(citation) {
  estimates <- extractEstimates(citation$abstract) %>%
    mutate(pmid = citation$pmid) %>%
    return()
}

estimates <- applyCitations(
  pmids = pmids,
  fun = processCitation,
  fetchSettings = fetchSettings,
  connectionDetails = connectionDetails,
  databaseSchema = databaseSchema
)
estimates <- bind_rows(estimates)
saveRDS(estimates, file.path(folder, "estimates.rds"))

# Plot estimates ---------------------------------------------------------------
estimates <- readRDS(file.path(folder, "estimates.rds"))

plotEstimates(estimates, fileName = file.path(folder, "AllObsResearch.png"))


