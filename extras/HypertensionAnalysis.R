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

# Find all PMIDs of hypertension medication research ---------------------------
# drugs <- c("Acebutolol", "Aliskiren", "Amiloride", "Amlodipine", "Atenolol", "Azilsartan", "Benazepril", "Betaxolol", "Bisoprolol", "Bumetanide", "Candesartan", "Captopril", "Carvedilol", "Chlorthalidone", "Clonidine", "Diltiazem", "Doxazosin", "Enalapril", "Eplerenone", "Eprosartan", "Felodipine", "Fosinopril", "Furosemide", "Guanfacine", "Hydralazine", "Hydrochlorothiazide", "Indapamide", "irbesartan", "Isradipine", "Labetalol", "Lisinopril", "Losartan", "Methyldopa", "Metolazone", "Metoprolol", "Minoxidil", "Moexipril", "Nadolol", "Nebivolol", "Nicardipine", "Nifedipine", "Nisoldipine", "Olmesartan", "Penbutolol", "Perindopril", "Pindolol", "Prazosin", "Propranolol", "Quinapril", "Ramipril", "Spironolactone", "Telmisartan", "Terazosin", "Torsemide", "Trandolapril", "Triamterene", "Valsartan", "Verapamil", "Angiotensin-converting enzyme inhibitors", "ACE inhibitors", "Alpha-1 blockers", "Angiotensin receptor blockers", "Direct vasodilators", "Diuretics", "Alpha-1 blockers", "Beta blockers", "Calcium channel blockers")
# query <- sprintf("hypertension AND (\"%s\") AND (%s)", 
#                  paste(drugs, collapse = "\" OR \""),
#                  getObservationalResearchQuery())

query <- "(hypertension[MeSH Terms] OR hypertension[Title/Abstract]) 
AND (Acebutolol OR Aliskiren OR Amiloride OR Amlodipine OR Atenolol OR Azilsartan OR Benazepril OR Betaxolol OR Bisoprolol OR Bumetanide OR Candesartan OR Captopril OR Carvedilol OR Chlorthalidone OR Clonidine OR Diltiazem OR Doxazosin OR Enalapril OR Eplerenone OR Eprosartan OR Felodipine OR Fosinopril OR Furosemide OR Guanfacine OR Hydralazine OR Hydrochlorothiazide OR Indapamide OR irbesartan OR Isradipine OR Labetalol OR Lisinopril OR Losartan OR Methyldopa OR Metolazone OR Metoprolol OR Minoxidil OR Moexipril OR Nadolol OR Nebivolol OR Nicardipine OR Nifedipine OR Nisoldipine OR Olmesartan OR Penbutolol OR Perindopril OR Pindolol OR Prazosin OR Propranolol OR Quinapril OR Ramipril OR Spironolactone OR Telmisartan OR Terazosin OR Torsemide OR Trandolapril OR Triamterene OR Valsartan OR Verapamil OR \"Angiotensin-converting enzyme inhibitors\" OR \"ACE inhibitors\" OR \"Alpha-1 blockers\" OR \"Angiotensin receptor blockers\" OR \"Direct vasodilators\" OR \"Diuretics\" OR \"Alpha-1 blockers\" OR \"Beta blockers\" OR \"Calcium channel blockers\") 
AND (\"population-based\" [Title/Abstract] OR observational [Title/Abstract] OR pharmacoepidemiology [Title/Abstract] OR \"epidemiology\" OR Medicaid OR Medicare OR Truven OR MarketScan OR i3 OR Iqvia OR CPRD OR GPRD OR Optum OR Medstat OR \"Nationwide Inpatient Sample\" OR \"National Inpatient Sample\" OR PharMetrics OR PHARMO OR HealthCore OR OMOP OR Sentinel OR PCORNet OR \"ICD-9\"[Title/Abstract] OR \"ICD-10\"[Title/Abstract] OR IMS[Title/Abstract] OR \"electronic medical records\"[Text Word] OR \"Denmark/epidemiology\"[MeSH Terms] OR \"Veterans Affairs\"[Title/Abstract] OR \"Premier database\"[Title/Abstract] OR \"National Health Insurance Research Database\"[Title/Abstract] OR \"Outcome Assessment\"[Title/Abstract] OR \"insurance database\"[Title/Abstract] OR \"Database Management System\"[MeSH Terms] OR \"Medical Records Systems, Computerized\"[MeSH Terms])
AND (\"Cohort Studies\" [MeSH] OR cohort [Title/Abstract] OR \"propensity score\" [Title/Abstract] OR \"Case-Control Studies\" [MeSH] OR \"case control\" [Title/Abstract] OR \"self controlled case series\" [Title/Abstract] OR (sccs [Title/Abstract] AND \"self-controlled\" [Title/Abstract]) OR \"case-crossover\" [Title/Abstract] OR \"relative risk\" OR \"hazard\" OR \"hazards\" OR \"rate ratio\" OR \"RR\" OR \"safety\" OR \"effectiveness\" OR \"comparative\" OR \"causal\" OR \"effects\")
NOT (\"Clinical Trial\"[pt] OR \"Editorial\"[pt] OR \"Letter\"[pt] OR \"Randomized Controlled Trial\"[pt] OR \"Clinical Trial, Phase I\"[pt] OR \"Clinical Trial, Phase II\"[pt] OR \"Clinical Trial, Phase III\"[pt] OR \"Clinical Trial, Phase IV\"[pt] OR Comment[pt] OR \"Controlled Clinical Trial\"[pt] OR Letter[pt] OR \"Case Reports\"[pt] OR \"Clinical Trials as Topic\"[Mesh] OR \"double-blind\"[All] OR \"placebo-controlled\"[All] OR \"pilot study\"[All] OR \"pilot projects\"[Mesh] OR \"Prospective Studies\"[Mesh] OR Genetics[Mesh] OR Genotype[Mesh] OR biomarker[Title/Abstract])"

query <- gsub("\n", " ", query)
# writeLines(query)
pmids <- queryOnlinePubmed(gsub("\n", " ", query))
saveRDS(pmids, file.path(folder, "AntiHypertensivesObsResearchPmids.rds"))

# saveRDS(estimates, file.path(folder, "estimatesHypertension.rds"))
estimates <- readRDS(file.path(folder, "estimatesHypertension.rds"))
plotEstimates(estimates, fileName = file.path(folder, "Hypertension.png"))

# Extract all effect-size estimates from given PMIDs ---------------------------
pmids <- readRDS(file.path(folder, "AllObsResearchPmids.rds"))
length(pmids)

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
plotEstimates(estimates, showSignficant = FALSE, fileName = file.path(folder, "AllObsResearchBuild.png"))

# Plot LEGEND estimates --------------------------------------------------------
connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = paste(Sys.getenv("shinydbServer"),
                                                            Sys.getenv("shinydbDatabase"),
                                                            sep = "/"),
                                             port = Sys.getenv("shinydbPort"),
                                             user = Sys.getenv("shinydbUser"),
                                             password = Sys.getenv("shinydbPw"))

connection <- connect(connectionDetails)
databaseSchema <- Sys.getenv("shinydbSchema")
sql <- "
SELECT target_id,
  comparator_id,
  cohort_method_result.outcome_id,
  analysis_id,
  database_id,
  calibrated_log_rr AS log_rr,
  calibrated_se_log_rr AS se_log_rr,
  calibrated_p AS p
FROM @database_schema.cohort_method_result
INNER JOIN @database_schema.outcome_of_interest
  ON cohort_method_result.outcome_id = outcome_of_interest.outcome_id
WHERE analysis_id IN (1,2,3,4);"
estimates <- renderTranslateQuerySql(
  connection = connection,
  sql = sql,
  database_schema = databaseSchema,
  snakeCaseToCamelCase = TRUE
) %>%
  as_tibble()

estimates <- estimates %>%
  filter(analysisId < 5)



x <- renderTranslateQuerySql(
  connection = connection,
  sql = "SELECT * FROM @database_schema.outcome_of_interest",
  database_schema = databaseSchema,
  snakeCaseToCamelCase = TRUE
) %>%
  as_tibble()
disconnect(connection)
saveRDS(estimates, file.path(folder, "legendEstimates.rds"))


estimates <- readRDSsaveRDS(file.path(folder, "legendEstimates.rds"))
estimates$ciLb <- NA
estimates$ciUb <- NA
plotEstimates(estimates, jitter = FALSE, size = 0.2, fileName = file.path(folder, "Legend.png"))

# Show z-curve -----------------------------------------------------------------
library(ggplot2)

estimates <- readRDS(file.path(folder, "Estimates.rds"))

estimates <- estimates %>%
  computeLogRrAndSeLogRr() %>%
  filter(!is.na(seLogRr)) %>%
  mutate(z = logRr/seLogRr)

# Randomly sample 1 value per PMID:
estimates <- estimates %>%
  group_by(pmid) %>%
  slice_sample(n = 1)

plot <- ggplot(estimates, aes(x = z)) +
  geom_histogram() +
  geom_vline(xintercept = 1.96, color = "red") + 
  scale_x_continuous("z-scores", limits = c(-0.04,6))

ggsave(file.path(folder, "z.png"), plot)
