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

#' Create fetch settings
#'
#' @description 
#' Creates fetch settings to be used in the [applyCitations()] function.
#'
#' @param title     Should the citation title be extracted? If TRUE, the 'title' column will be available.
#' @param abstract  Should the abstract title be extracted? If TRUE, the 'abstact' column will be available.
#'
#' @return
#' Returns an object of type `fetchSettings`.
#' 
#' @export
createFetchSettings <- function(
    title = FALSE,
    abstract = FALSE
) {
  settings <- list()
  for (name in names(formals(createFetchSettings))) {
    settings[[name]] <- get(name)
  }
  class(settings) <- "fetchSettings"
  return(settings)
}

#' Apply to citations
#' 
#' @description 
#' Apply a function to each citation in a set.
#'
#' @param pmids               A vector of PMIDs.
#' @param fun                 The function to call per citation. This function should have exactly 1 argument,
#'                            a data frame containing one row with the citation. This data frame will at least 
#'                            have a `pmid` column, and more depending on the `fetchSettings` argument.
#' @param fetchSettings       An object of type `fetchSettings` as created using [createFetchSettings()].
#' @param connectionDetails   An object of type `connectionDetails` as created using `DatabaseConnector::createConnectionDetails()`.
#' @param databaseSchema      The database schema containing the MEDLINE database.
#' @param tempEmulationSchema Some database platforms like Oracle and Impala do not truly support temp tables. To emulate temp 
#'                            tables, provide a schema with write privileges where temp tables can be created.
#' @param ...                 Additional arguments to pass to fun.
#'
#' @return
#' Returns the list of results from the calls to `fun`.
#' 
#' @export
applyCitations <- function(pmids, fun, fetchSettings, connectionDetails, databaseSchema, tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"), ...){
  startTime <- Sys.time()
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  DatabaseConnector::insertTable(
    connection = connection,
    tableName = "#pmids",
    data = data.frame(pmid = as.integer(pmids)),
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    camelCaseToSnakeCase = TRUE
  )
  
  pathToSql <- system.file("sql", 
                           "sql_server",
                           "IteratorQuery.sql",
                           package = "MedlineAnalysis"
  )
  # pathToSql <- "inst/sql/sql_server/IteratorQuery.sql"
  
  sql <- SqlRender::readSql("inst/sql/sql_server/IteratorQuery.sql")

  pb <- txtProgressBar(style = 3)
  results <- DatabaseConnector::renderTranslateQueryApplyBatched(
    connection = connection,
    sql = sql,
    fun = batchFun,
    args = list(x = fun, xArgs = list(...), pb = pb, total = length(pmids)),
    snakeCaseToCamelCase = TRUE,
    tempEmulationSchema = tempEmulationSchema,
    title = fetchSettings$title,
    abstract = fetchSettings$abstract,
    database_schema = databaseSchema,
  )
  setTxtProgressBar(pb, 1)
  close(pb)
  results <- do.call(c, results)
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql <- "TRUNCATE TABLE #pmids; DROP TABLE #pmids;",
    progressBar = FALSE,
    reportOverallTime = FALSE,
    tempEmulationSchema = tempEmulationSchema
  )
  delta <- Sys.time() - startTime
  inform(paste("Procesing citations took", signif(delta, 3), attr(delta, "units")))
  
  invisible(results)
}

batchFun <- function(batch, position, x, xArgs, pb, total) {
  setTxtProgressBar(pb, (position - 1) / total)
  if ("abstract" %in% colnames(batch)) {
    batch$abstract <- gsub("\\\\n", "\n", batch$abstract)
  }
  doCall <- function(citation, x, xArgs) {
    return(do.call(x, append(list(citation = citation), xArgs)))
  }
  results <- lapply(split(batch, seq_len(nrow(batch))), doCall, x = x, xArgs = xArgs)
  return(results)
}
