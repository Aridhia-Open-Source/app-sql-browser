# ══════════════════════════════════════════════════════════════════════════════
#  Aridhia DRE - SQL Workbench  v2
#  Single-file app.R for deployment to a DRE Project Workspace
#
#  Launch:  shiny::runApp()
#
#  Requires xaputils (pre-installed in all DRE workspace R environments).
#  Falls back to a "no connection" state for local development.
# ══════════════════════════════════════════════════════════════════════════════


# ── 1. DEPENDENCIES ───────────────────────────────────────────────────────────

required_packages <- c("shiny", "shinydashboard", "DT", "DBI", "httr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Required package '", pkg, "' is not installed."), call. = FALSE)
  }
}

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(DBI)
})


# ── 2. HELPERS ────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

pg_str   <- function(s) paste0("'", gsub("'", "''", as.character(s)), "'")
fq_tbl   <- function(schema, table) {
  # Strip any pre-existing quotes and schema prefix that may be embedded in
  # the table name (e.g. from workspace metadata returning 'schema"."table')
  table <- gsub('"', '', table, fixed = TRUE)   # remove all double-quotes
  if (grepl(".", table, fixed = TRUE)) {
    # Already schema-qualified - use as-is, split on first dot
    parts  <- strsplit(table, ".", fixed = TRUE)[[1]]
    schema <- parts[1]
    table  <- paste(parts[-1], collapse = ".")
  }
  sprintf('"%s"."%s"', schema, table)
}
tbl_alias <- function(i) paste0("t", i)


# ── 3. DATABASE CONNECTION ────────────────────────────────────────────────────

DRE_CONN <- tryCatch(
  { library(xaputils); message("[ sql workbench ] connected via xap.conn"); xap.conn },
  error = function(e) {
    message("[ sql workbench ] xaputils not available - no DB connection")
    NULL
  }
)

conn_valid <- function() !is.null(DRE_CONN)

db_query <- function(sql) {
  if (is.null(DRE_CONN)) stop("No database connection. Run inside a DRE Workspace.")
  warn_msg <- NULL
  result <- withCallingHandlers(
    tryCatch(
      dbGetQuery(DRE_CONN, sql),
      error = function(e) stop(conditionMessage(e))
    ),
    warning = function(w) {
      warn_msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  # RPostgreSQL returns NULL and emits a warning on query failure
  if (is.null(result)) {
    msg <- warn_msg %||% "Query failed (no result returned)"
    # Extract the PostgreSQL error from the warning message if present
    pg_err <- regmatches(msg, regexpr("ERROR:.*", msg))
    stop(if (length(pg_err) > 0) pg_err else msg)
  }
  result
}


# ── 4. DATABASE HELPERS ───────────────────────────────────────────────────────

get_schemas <- function() {
  sql <- "
    SELECT schema_name FROM information_schema.schemata
    WHERE schema_name NOT IN (
      'information_schema','pg_catalog','pg_toast','pg_temp_1','pg_toast_temp_1')
    AND schema_name NOT LIKE 'pg_%'
    ORDER BY CASE WHEN schema_name='public' THEN 0 ELSE 1 END, schema_name"
  tryCatch(db_query(sql)$schema_name, error = function(e) character(0))
}

get_tables <- function(schema) {
  sql <- sprintf(
    "SELECT table_name FROM information_schema.tables
     WHERE table_schema=%s AND table_type='BASE TABLE' ORDER BY table_name",
    pg_str(schema))
  tryCatch(db_query(sql)$table_name, error = function(e) character(0))
}

get_columns <- function(schema, table) {
  sql <- sprintf(
    "SELECT column_name, data_type, is_nullable
     FROM information_schema.columns
     WHERE table_schema=%s AND table_name=%s ORDER BY ordinal_position",
    pg_str(schema), pg_str(table))
  tryCatch(db_query(sql),
    error = function(e)
      data.frame(column_name = character(), data_type = character(),
                 is_nullable = character(), stringsAsFactors = FALSE))
}



# ── 5. OMOP CDM DETECTION ─────────────────────────────────────────────────────
#
# Checks whether a schema looks like an OMOP CDM by matching its table names
# against the canonical CDM table list. 6+ matches = OMOP.
# Also reads cdm_source if present for version / source description.

OMOP_CDM_TABLES <- c(
  "person", "observation_period", "visit_occurrence", "visit_detail",
  "condition_occurrence", "drug_exposure", "procedure_occurrence",
  "device_exposure", "measurement", "observation", "death", "note",
  "specimen", "fact_relationship", "location", "care_site", "provider",
  "payer_plan_period", "cost", "drug_era", "dose_era", "condition_era",
  "concept", "vocabulary", "concept_relationship", "concept_ancestor",
  "concept_synonym", "domain", "concept_class", "relationship",
  "source_to_concept_map", "drug_strength", "cohort", "cohort_definition",
  "attribute_definition", "cdm_source"
)

detect_omop <- function(schema) {
  tbls   <- tolower(get_tables(schema))
  hits   <- intersect(tbls, OMOP_CDM_TABLES)
  is_cdm <- length(hits) >= 6

  cdm_info <- NULL
  if (is_cdm && "cdm_source" %in% tbls) {
    cdm_info <- tryCatch(
      db_query(sprintf('SELECT * FROM "%s"."cdm_source" LIMIT 1', schema)),
      error = function(e) NULL)
  }

  list(
    is_omop   = is_cdm,
    hit_count = length(hits),
    hits      = hits,
    cdm_info  = cdm_info
  )
}

# Helper: find the actual stored column names in the concept table.
# PostgreSQL is case-sensitive when columns are created with quoted identifiers.
# We query information_schema to get the real names, then quote them properly.

get_concept_col_names <- function(omop_schema) {
  sql <- sprintf(
    "SELECT column_name FROM information_schema.columns
     WHERE table_schema = %s
       AND table_name   = 'concept'
       AND lower(column_name) IN ('concept_id', 'concept_name', 'domain_id', 'vocabulary_id')
     ORDER BY ordinal_position",
    pg_str(omop_schema))
  cols <- tryCatch(db_query(sql)$column_name, error = function(e) character(0))

  # Map lower -> actual stored name
  m <- setNames(cols, tolower(cols))
  list(
    concept_id     = m[["concept_id"]]     %||% "concept_id",
    concept_name   = m[["concept_name"]]   %||% "concept_name",
    domain_id      = m[["domain_id"]]      %||% "domain_id",
    vocabulary_id  = m[["vocabulary_id"]]  %||% "vocabulary_id"
  )
}



# ── OMOP CDM v5.4 expected column type categories ────────────────────────────
# Values are categories (integer / numeric / text / date / timestamp) so we
# can compare against the actual PostgreSQL type without being brittle about
# the exact stored type (int4 vs bigint vs integer are all "integer").
OMOP_CDM_EXPECTED <- list(

  person = list(
    person_id = "integer", gender_concept_id = "integer",
    year_of_birth = "integer", month_of_birth = "integer",
    day_of_birth = "integer", birth_datetime = "timestamp",
    race_concept_id = "integer", ethnicity_concept_id = "integer",
    location_id = "integer", provider_id = "integer",
    care_site_id = "integer", person_source_value = "text",
    gender_source_value = "text", gender_source_concept_id = "integer",
    race_source_value = "text", race_source_concept_id = "integer",
    ethnicity_source_value = "text", ethnicity_source_concept_id = "integer"
  ),

  observation_period = list(
    observation_period_id = "integer", person_id = "integer",
    observation_period_start_date = "date", observation_period_end_date = "date",
    period_type_concept_id = "integer"
  ),

  visit_occurrence = list(
    visit_occurrence_id = "integer", person_id = "integer",
    visit_concept_id = "integer", visit_start_date = "date",
    visit_start_datetime = "timestamp", visit_end_date = "date",
    visit_end_datetime = "timestamp", visit_type_concept_id = "integer",
    provider_id = "integer", care_site_id = "integer",
    visit_source_value = "text", visit_source_concept_id = "integer",
    admitting_source_concept_id = "integer", admitting_source_value = "text",
    discharge_to_concept_id = "integer", discharge_to_source_value = "text",
    preceding_visit_occurrence_id = "integer"
  ),

  visit_detail = list(
    visit_detail_id = "integer", person_id = "integer",
    visit_detail_concept_id = "integer", visit_detail_start_date = "date",
    visit_detail_start_datetime = "timestamp", visit_detail_end_date = "date",
    visit_detail_end_datetime = "timestamp", visit_detail_type_concept_id = "integer",
    provider_id = "integer", care_site_id = "integer",
    admitting_source_concept_id = "integer", discharge_to_concept_id = "integer",
    preceding_visit_detail_id = "integer", visit_detail_source_value = "text",
    visit_detail_source_concept_id = "integer", admitting_source_value = "text",
    discharge_to_source_value = "text", visit_occurrence_id = "integer"
  ),

  condition_occurrence = list(
    condition_occurrence_id = "integer", person_id = "integer",
    condition_concept_id = "integer", condition_start_date = "date",
    condition_start_datetime = "timestamp", condition_end_date = "date",
    condition_end_datetime = "timestamp", condition_type_concept_id = "integer",
    condition_status_concept_id = "integer", stop_reason = "text",
    provider_id = "integer", visit_occurrence_id = "integer",
    visit_detail_id = "integer", condition_source_value = "text",
    condition_source_concept_id = "integer", condition_status_source_value = "text"
  ),

  drug_exposure = list(
    drug_exposure_id = "integer", person_id = "integer",
    drug_concept_id = "integer", drug_exposure_start_date = "date",
    drug_exposure_start_datetime = "timestamp", drug_exposure_end_date = "date",
    drug_exposure_end_datetime = "timestamp", verbatim_end_date = "date",
    drug_type_concept_id = "integer", stop_reason = "text",
    refills = "integer", quantity = "numeric", days_supply = "integer",
    sig = "text", route_concept_id = "integer", lot_number = "text",
    provider_id = "integer", visit_occurrence_id = "integer",
    visit_detail_id = "integer", drug_source_value = "text",
    drug_source_concept_id = "integer", route_source_value = "text",
    dose_unit_source_value = "text"
  ),

  procedure_occurrence = list(
    procedure_occurrence_id = "integer", person_id = "integer",
    procedure_concept_id = "integer", procedure_date = "date",
    procedure_datetime = "timestamp", procedure_type_concept_id = "integer",
    modifier_concept_id = "integer", quantity = "integer",
    provider_id = "integer", visit_occurrence_id = "integer",
    visit_detail_id = "integer", procedure_source_value = "text",
    procedure_source_concept_id = "integer", modifier_source_value = "text"
  ),

  device_exposure = list(
    device_exposure_id = "integer", person_id = "integer",
    device_concept_id = "integer", device_exposure_start_date = "date",
    device_exposure_start_datetime = "timestamp", device_exposure_end_date = "date",
    device_exposure_end_datetime = "timestamp", device_type_concept_id = "integer",
    unique_device_id = "text", quantity = "integer",
    provider_id = "integer", visit_occurrence_id = "integer",
    visit_detail_id = "integer", device_source_value = "text",
    device_source_concept_id = "integer"
  ),

  measurement = list(
    measurement_id = "integer", person_id = "integer",
    measurement_concept_id = "integer", measurement_date = "date",
    measurement_datetime = "timestamp", measurement_time = "text",
    measurement_type_concept_id = "integer", operator_concept_id = "integer",
    value_as_number = "numeric", value_as_concept_id = "integer",
    unit_concept_id = "integer", range_low = "numeric", range_high = "numeric",
    provider_id = "integer", visit_occurrence_id = "integer",
    visit_detail_id = "integer", measurement_source_value = "text",
    measurement_source_concept_id = "integer", unit_source_value = "text",
    value_source_value = "text"
  ),

  observation = list(
    observation_id = "integer", person_id = "integer",
    observation_concept_id = "integer", observation_date = "date",
    observation_datetime = "timestamp", observation_type_concept_id = "integer",
    value_as_number = "numeric", value_as_string = "text",
    value_as_concept_id = "integer", qualifier_concept_id = "integer",
    unit_concept_id = "integer", provider_id = "integer",
    visit_occurrence_id = "integer", visit_detail_id = "integer",
    observation_source_value = "text", observation_source_concept_id = "integer",
    unit_source_value = "text", qualifier_source_value = "text"
  ),

  death = list(
    person_id = "integer", death_date = "date", death_datetime = "timestamp",
    death_type_concept_id = "integer", cause_concept_id = "integer",
    cause_source_value = "text", cause_source_concept_id = "integer"
  ),

  note = list(
    note_id = "integer", person_id = "integer", note_date = "date",
    note_datetime = "timestamp", note_type_concept_id = "integer",
    note_class_concept_id = "integer", note_title = "text",
    note_text = "text", encoding_concept_id = "integer",
    language_concept_id = "integer", provider_id = "integer",
    visit_occurrence_id = "integer", visit_detail_id = "integer",
    note_source_value = "text"
  ),

  specimen = list(
    specimen_id = "integer", person_id = "integer",
    specimen_concept_id = "integer", specimen_type_concept_id = "integer",
    specimen_date = "date", specimen_datetime = "timestamp",
    quantity = "numeric", unit_concept_id = "integer",
    anatomic_site_concept_id = "integer", disease_status_concept_id = "integer",
    specimen_source_id = "text", specimen_source_value = "text",
    unit_source_value = "text", anatomic_site_source_value = "text",
    disease_status_source_value = "text"
  ),

  location = list(
    location_id = "integer", address_1 = "text", address_2 = "text",
    city = "text", state = "text", zip = "text", county = "text",
    location_source_value = "text", country_concept_id = "integer",
    country_source_value = "text", latitude = "numeric", longitude = "numeric"
  ),

  care_site = list(
    care_site_id = "integer", care_site_name = "text",
    place_of_service_concept_id = "integer", location_id = "integer",
    care_site_source_value = "text", place_of_service_source_value = "text"
  ),

  provider = list(
    provider_id = "integer", provider_name = "text", npi = "text",
    dea = "text", specialty_concept_id = "integer", care_site_id = "integer",
    year_of_birth = "integer", gender_concept_id = "integer",
    provider_source_value = "text", specialty_source_value = "text",
    specialty_source_concept_id = "integer", gender_source_value = "text",
    gender_source_concept_id = "integer"
  ),

  drug_era = list(
    drug_era_id = "integer", person_id = "integer",
    drug_concept_id = "integer", drug_era_start_date = "date",
    drug_era_end_date = "date", drug_exposure_count = "integer",
    gap_days = "integer"
  ),

  dose_era = list(
    dose_era_id = "integer", person_id = "integer",
    drug_concept_id = "integer", unit_concept_id = "integer",
    dose_value = "numeric", dose_era_start_date = "date",
    dose_era_end_date = "date"
  ),

  condition_era = list(
    condition_era_id = "integer", person_id = "integer",
    condition_concept_id = "integer", condition_era_start_date = "date",
    condition_era_end_date = "date", condition_occurrence_count = "integer"
  ),

  concept = list(
    concept_id = "integer", concept_name = "text", domain_id = "text",
    vocabulary_id = "text", concept_class_id = "text",
    standard_concept = "text", concept_code = "text",
    valid_start_date = "date", valid_end_date = "date",
    invalid_reason = "text"
  ),

  vocabulary = list(
    vocabulary_id = "text", vocabulary_name = "text",
    vocabulary_reference = "text", vocabulary_version = "text",
    vocabulary_concept_id = "integer"
  ),

  domain = list(
    domain_id = "text", domain_name = "text", domain_concept_id = "integer"
  ),

  concept_class = list(
    concept_class_id = "text", concept_class_name = "text",
    concept_class_concept_id = "integer"
  ),

  relationship = list(
    relationship_id = "text", relationship_name = "text",
    is_hierarchical = "text", defines_ancestry = "text",
    reverse_relationship_id = "text", relationship_concept_id = "integer"
  ),

  concept_relationship = list(
    concept_id_1 = "integer", concept_id_2 = "integer",
    relationship_id = "text", valid_start_date = "date",
    valid_end_date = "date", invalid_reason = "text"
  ),

  concept_ancestor = list(
    ancestor_concept_id = "integer", descendant_concept_id = "integer",
    min_levels_of_separation = "integer", max_levels_of_separation = "integer"
  ),

  concept_synonym = list(
    concept_id = "integer", concept_synonym_name = "text",
    language_concept_id = "integer"
  ),

  source_to_concept_map = list(
    source_code = "text", source_concept_id = "integer",
    source_vocabulary_id = "text", source_code_description = "text",
    target_concept_id = "integer", target_vocabulary_id = "text",
    valid_start_date = "date", valid_end_date = "date",
    invalid_reason = "text"
  ),

  drug_strength = list(
    drug_concept_id = "integer", ingredient_concept_id = "integer",
    amount_value = "numeric", amount_unit_concept_id = "integer",
    numerator_value = "numeric", numerator_unit_concept_id = "integer",
    denominator_value = "numeric", denominator_unit_concept_id = "integer",
    box_size = "integer", valid_start_date = "date",
    valid_end_date = "date", invalid_reason = "text"
  ),

  cdm_source = list(
    cdm_source_name = "text", cdm_source_abbreviation = "text",
    cdm_holder = "text", source_description = "text",
    source_documentation_reference = "text", cdm_etl_reference = "text",
    source_release_date = "date", cdm_release_date = "date",
    cdm_version = "text", vocabulary_version = "text"
  ),

  payer_plan_period = list(
    payer_plan_period_id = "integer", person_id = "integer",
    payer_plan_period_start_date = "date", payer_plan_period_end_date = "date",
    payer_concept_id = "integer", payer_source_value = "text",
    payer_source_concept_id = "integer", plan_concept_id = "integer",
    plan_source_value = "text", plan_source_concept_id = "integer",
    sponsor_concept_id = "integer", sponsor_source_value = "text",
    sponsor_source_concept_id = "integer", family_source_value = "text",
    stop_reason_concept_id = "integer", stop_reason_source_value = "text",
    stop_reason_source_concept_id = "integer"
  )
)

# Convert an actual PostgreSQL data_type string to our 5-category system
pg_type_to_cat <- function(pg_type) {
  pt <- tolower(trimws(pg_type))
  if (grepl("^(int|bigint|smallint|integer|int4|int8|int2|serial|bigserial)", pt))
    return("integer")
  if (grepl("^(numeric|decimal|real|double|float|money)", pt))
    return("numeric")
  if (grepl("^(timestamp|timestamptz)", pt))
    return("timestamp")
  if (grepl("^date$", pt))
    return("date")
  if (grepl("^(char|varchar|text|name|bpchar|character)", pt))
    return("text")
  if (grepl("^bool", pt))
    return("boolean")
  return("other")
}

# ── CDM COLUMN NAME RESOLVER ──────────────────────────────────────────────────
# Fetches actual column names for a set of CDM tables from information_schema,
# returning a lookup function col(table, logical_name) -> quoted "ACTUAL_NAME".
# This makes all queries case-agnostic across OMOP implementations.
get_cdm_cols <- function(schema, tables) {
  sql <- sprintf(
    "SELECT table_name, column_name
     FROM information_schema.columns
     WHERE table_schema = %s
       AND lower(table_name) IN (%s)",
    pg_str(schema),
    paste(sapply(tables, pg_str), collapse = ","))
  rows <- tryCatch(db_query(sql), error = function(e) NULL)
  if (is.null(rows) || nrow(rows) == 0) {
    return(function(tbl, col) col)
  }
  lookup <- list()
  for (i in seq_len(nrow(rows))) {
    t <- tolower(rows$table_name[i])
    c <- tolower(rows$column_name[i])
    if (is.null(lookup[[t]])) lookup[[t]] <- list()
    lookup[[t]][[c]] <- rows$column_name[i]
  }
  function(tbl, col) {
    actual <- lookup[[tolower(tbl)]][[tolower(col)]]
    if (is.null(actual)) actual <- col
    sprintf('"%s"', actual)
  }
}


# ── ATHENA API FALLBACK ───────────────────────────────────────────────────────
# Session-level cache: persists across queries so each concept is only fetched once.
# Keyed by concept_id (character). Value is concept_name string (or NA on miss).
ATHENA_CACHE <- new.env(hash = TRUE, parent = emptyenv())
ATHENA_BASE  <- "https://athena.ohdsi.org/api/v1/concepts"
ATHENA_MAX   <- 50L   # maximum IDs fetched per Athena call

# Probe Athena reachability with a single known concept (4180628 = SNOMED hierarchy node).
# Returns list(ok = TRUE/FALSE, status = HTTP code or NA, ms = round-trip ms, error = msg or NULL)
athena_ping <- function() {
  if (!requireNamespace("httr", quietly = TRUE))
    return(list(ok = FALSE, status = NA_integer_, ms = NA_real_,
                error = "Package 'httr' not available"))
  t0 <- proc.time()[["elapsed"]]
  result <- tryCatch({
    resp <- httr::GET(paste0(ATHENA_BASE, "/4180628"),
                      httr::timeout(8),
                      httr::add_headers(Accept = "application/json"))
    ms   <- round((proc.time()[["elapsed"]] - t0) * 1000)
    code <- httr::status_code(resp)
    list(ok = code == 200L, status = code, ms = ms, error = NULL)
  }, error = function(e) {
    ms <- round((proc.time()[["elapsed"]] - t0) * 1000)
    list(ok = FALSE, status = NA_integer_, ms = ms, error = conditionMessage(e))
  })
  result
}

# Fetch concept names from Athena for a vector of integer concept IDs.
# Returns a named character vector: concept_id -> concept_name.
# IDs already in ATHENA_CACHE are served from cache; only new ones are fetched.
# Requires the httr package (standard in DRE workspaces).
athena_lookup <- function(ids) {
  if (!requireNamespace("httr", quietly = TRUE))
    stop("Package 'httr' is required for Athena lookup. Install it in your workspace.")

  ids      <- unique(as.character(ids[!is.na(ids) & ids != 0]))
  cached   <- ids[ids %in% ls(ATHENA_CACHE)]
  to_fetch <- setdiff(ids, cached)

  # Honour cap: take first ATHENA_MAX unfetched IDs
  if (length(to_fetch) > ATHENA_MAX) to_fetch <- to_fetch[seq_len(ATHENA_MAX)]

  for (cid in to_fetch) {
    result <- tryCatch({
      resp <- httr::GET(paste0(ATHENA_BASE, "/", cid),
                        httr::timeout(8),
                        httr::add_headers(Accept = "application/json"))
      if (httr::status_code(resp) == 200L) {
        body <- httr::content(resp, as = "parsed", type = "application/json")
        nm <- body$name %||% body$conceptName %||% NA_character_
        as.character(nm)
      } else NA_character_
    }, error = function(e) NA_character_)

    assign(cid, result, envir = ATHENA_CACHE)
    Sys.sleep(0.05)   # gentle rate limit
  }

  # Build result from cache for all requested IDs
  out <- vapply(ids, function(cid) {
    v <- tryCatch(get(cid, envir = ATHENA_CACHE), error = function(e) NA_character_)
    if (is.null(v) || is.na(v)) NA_character_ else v
  }, character(1))
  out
}

# Given a local lookup map (named char vector id->name) and a data frame,
# find IDs that returned NA names and return them for Athena enrichment.
unmatched_ids <- function(df, local_map, cid_cols) {
  ids <- unique(unlist(lapply(cid_cols, function(col) {
    vals <- as.character(df[[col]])
    vals[!is.na(df[[col]]) & suppressWarnings(as.integer(df[[col]])) != 0L &
        is.na(local_map[vals])]
  })))
  ids <- ids[!is.na(ids)]
  suppressWarnings(as.integer(ids))
  ids
}

# ─────────────────────────────────────────────────────────────────────────────

# Given a data frame of query results and the OMOP schema name, resolve any
# column whose name ends in concept_id by joining against concept.concept_name.
# The resolved name column is inserted immediately after the source column.
# Returns list(df, resolved, cols_found, warn)

resolve_concept_ids <- function(df, omop_schema) {
  NO_COLS  <- list(df = df, resolved = character(0), cols_found = character(0), warn = NULL)
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(NO_COLS)

  # Match any column ending in concept_id (with or without underscore prefix),
  # including deduplicated variants like concept_id_1, concept_id_2 produced when
  # JOIN queries yield duplicate columns that get renamed by make.unique().
  # Pattern: anything ending in concept_id OR concept_id_<digits>
  concept_cols <- names(df)[grepl("concept_id(_\\d+)?$", names(df), ignore.case = TRUE) &
                            !grepl("source_concept_id(_\\d+)?$", names(df), ignore.case = TRUE)]
  source_cols  <- names(df)[grepl("source_concept_id(_\\d+)?$", names(df), ignore.case = TRUE)]
  all_cid_cols <- c(concept_cols, source_cols)

  if (length(all_cid_cols) == 0) return(NO_COLS)

  # Collect all unique non-zero concept IDs across all matching columns
  all_ids <- unique(unlist(lapply(all_cid_cols, function(col) {
    vals <- df[[col]]
    suppressWarnings(as.integer(na.omit(vals[!is.na(vals) & vals != 0])))
  })))
  all_ids <- all_ids[!is.na(all_ids)]

  if (length(all_ids) == 0) return(NO_COLS)

  # Discover the actual quoted column names in the concept table.
  # PostgreSQL stores column names exactly as created - if the DDL used
  # "CONCEPT_ID" they must be referenced as "CONCEPT_ID", not concept_id.
  col_names <- get_concept_col_names(omop_schema)
  qid   <- sprintf('"%s"', col_names$concept_id)
  qname <- sprintf('"%s"', col_names$concept_name)
  qdom  <- sprintf('"%s"', col_names$domain_id)
  qvoc  <- sprintf('"%s"', col_names$vocabulary_id)

  # Cap at 5000 IDs per query to avoid very large IN clauses
  id_sample <- if (length(all_ids) > 5000) all_ids[seq_len(5000)] else all_ids
  id_list   <- paste(id_sample, collapse = ",")

  lookup_err <- NULL
  concept_map <- tryCatch(
    db_query(sprintf(
      'SELECT %s, %s, %s, %s FROM "%s"."concept" WHERE %s IN (%s)',
      qid, qname, qdom, qvoc, omop_schema, qid, id_list)),
    error = function(e) { lookup_err <<- conditionMessage(e); NULL })

  if (is.null(concept_map) || nrow(concept_map) == 0) {
    warn <- if (!is.null(lookup_err))
      paste0("Concept lookup failed: ", lookup_err)
    else
      sprintf("%d concept ID column%s found but no matching rows in concept table",
              length(all_cid_cols), if (length(all_cid_cols) == 1) "" else "s")
    return(list(df = df, resolved = character(0),
                cols_found = all_cid_cols, warn = warn))
  }

  # Normalise result column names to lowercase so lookups are consistent
  names(concept_map) <- tolower(names(concept_map))

  # Build lookup: concept_id (as character) -> concept_name
  lookup <- setNames(concept_map$concept_name, as.character(concept_map$concept_id))

  resolved <- character(0)
  out      <- df

  # Process in reverse column order so insertions don't shift earlier indices
  for (col in rev(all_cid_cols)) {
    col_idx <- which(names(out) == col)
    if (length(col_idx) == 0) next

    # Derive the resolved column name: preserve prefix, replace concept_id suffix.
    # Handles deduplicated variants: CONCEPT_ID_1 -> CONCEPT_NAME_1
    # e.g. CONDITION_CONCEPT_ID   -> CONDITION_CONCEPT_NAME
    #      concept_id              -> concept_name
    #      CONCEPT_ID_1            -> CONCEPT_NAME_1
    name_col <- sub("(?i)concept_id(_\\d+)?$",
                    paste0("concept_name", sub(".*concept_id(_\\d+)?$", "\\1", col,
                                               ignore.case = TRUE, perl = TRUE)),
                    col, perl = TRUE, ignore.case = TRUE)

    ids_chr   <- as.character(out[[col]])
    names_vec <- lookup[ids_chr]
    names_vec[is.na(out[[col]]) | suppressWarnings(as.integer(out[[col]])) == 0L] <- NA_character_
    names(names_vec) <- NULL

    left  <- if (col_idx > 0)         out[, seq_len(col_idx),            drop = FALSE] else out[, integer(0), drop = FALSE]
    right <- if (col_idx < ncol(out)) out[, seq(col_idx + 1, ncol(out)), drop = FALSE] else out[, integer(0), drop = FALSE]

    out      <- cbind(left, setNames(data.frame(names_vec, stringsAsFactors = FALSE), name_col), right)
    resolved <- c(resolved, name_col)
  }

  # Count how many ID values actually resolved (not NA in resolved columns)
  total_id_vals   <- sum(sapply(all_cid_cols, function(c) sum(!is.na(out[[c]]) &
                    suppressWarnings(as.integer(out[[c]])) != 0L)), na.rm = TRUE)
  resolved_vals   <- sum(sapply(resolved, function(c) sum(!is.na(out[[c]]))), na.rm = TRUE)

  partial_warn <- if (length(resolved) > 0 && resolved_vals < total_id_vals)
    sprintf("Partial match: %d of %d concept ID values resolved (unmatched IDs not in concept table)",
            resolved_vals, total_id_vals)
  else NULL

  list(df = out, resolved = resolved, cols_found = all_cid_cols, warn = partial_warn)
}

# ── 6. TYPE CLASSIFICATION ────────────────────────────────────────────────────

# col_name is optional - when supplied, boolean columns whose name looks like a
# numeric ID field (e.g. *_concept_id, *_source_value, *_type_concept_id, year_*,
# *_count, *_number) are promoted to "numeric" to work around schemas where ID
# columns are incorrectly typed as boolean.
BOOL_AS_NUMERIC_PATTERN <- paste0(
  "(_concept_id|_source_concept_id|_type_concept_id|_status_concept_id",
  "|_source_value|_person_id|_visit_id|_provider_id|_care_site_id",
  "|_count|_number|_days|_quantity|_refills",
  "|^year_|^month_|^day_)$")

type_category <- function(data_type, col_name = NULL) {
  if (data_type %in% c("integer","bigint","smallint","numeric","decimal","real",
                        "double precision","float","float4","float8","int4","int8"))
    return("numeric")
  if (data_type %in% c("date","timestamp","timestamp without time zone",
                        "timestamp with time zone","time","interval"))
    return("date")
  if (data_type %in% c("boolean","bool")) {
    # Promote to numeric if column name pattern suggests it is an ID/count field
    if (!is.null(col_name) &&
        grepl(BOOL_AS_NUMERIC_PATTERN, col_name, ignore.case = TRUE, perl = TRUE))
      return("numeric")
    return("boolean")
  }
  if (data_type %in% c("character varying","varchar","text","char","character","name"))
    return("text")
  return("other")
}

type_pill_html <- function(data_type) {
  cat   <- type_category(data_type)
  label <- switch(cat, numeric = "num", text = "text",
                  date = "date", boolean = "bool", "other")
  sprintf('<span class="type-pill tp-%s">%s</span>', cat, label)
}


# ── 7. SCHEMA LINK ANALYSIS ───────────────────────────────────────────────────
#
# Returns a list:
#   $all_cols  : named list  table → data.frame(column_name, data_type, is_nullable)
#   $field_map : named list  field_name → character vector of tables containing it
#   $common    : character vector of field names appearing in 2+ tables
#   $pairs     : data.frame(table_a, table_b, key) - every cross-table join path

analyse_schema_links <- function(schema) {
  tbls <- get_tables(schema)
  if (length(tbls) == 0)
    return(list(all_cols  = list(),
                field_map = list(),
                common    = character(0),
                pairs     = data.frame(table_a = character(), table_b = character(),
                                       key = character(), stringsAsFactors = FALSE)))

  all_cols <- setNames(
    lapply(tbls, function(t) get_columns(schema, t)),
    tbls)

  # field → which tables contain it
  field_map <- list()
  for (t in tbls)
    for (col in all_cols[[t]]$column_name)
      field_map[[col]] <- unique(c(field_map[[col]], t))

  # fields present in 2+ tables
  common <- names(Filter(function(v) length(v) >= 2, field_map))

  # every unique table-pair sharing at least one common field
  pair_rows <- list()
  for (fld in common) {
    tv <- field_map[[fld]]
    if (length(tv) >= 2)
      for (i in seq_len(length(tv) - 1))
        for (j in seq(i + 1, length(tv)))
          pair_rows[[length(pair_rows) + 1]] <-
            data.frame(table_a = tv[i], table_b = tv[j],
                       key = fld, stringsAsFactors = FALSE)
  }

  pairs <- if (length(pair_rows) > 0) do.call(rbind, pair_rows)
           else data.frame(table_a = character(), table_b = character(),
                           key = character(), stringsAsFactors = FALSE)

  list(all_cols = all_cols, field_map = field_map, common = common, pairs = pairs)
}

# Tables the primary table can JOIN to, with their shared keys
join_targets <- function(primary_table, links) {
  if (is.null(links) || nrow(links$pairs) == 0) return(list())
  p   <- links$pairs
  out <- list()
  for (i in seq_len(nrow(p))) {
    if (p$table_a[i] == primary_table)
      out[[p$table_b[i]]] <- unique(c(out[[p$table_b[i]]], p$key[i]))
    if (p$table_b[i] == primary_table)
      out[[p$table_a[i]]] <- unique(c(out[[p$table_a[i]]], p$key[i]))
  }
  out
}


# ── 8. QUERY SUGGESTION ENGINE ────────────────────────────────────────────────

generate_suggestions <- function(schema, table, columns, links = NULL) {
  fq <- fq_tbl(schema, table)

  by_type <- list(
    numeric = columns$column_name[mapply(type_category, columns$data_type, columns$column_name) == "numeric"],
    text    = columns$column_name[mapply(type_category, columns$data_type, columns$column_name) == "text"],
    date    = columns$column_name[mapply(type_category, columns$data_type, columns$column_name) == "date"],
    boolean = columns$column_name[mapply(type_category, columns$data_type, columns$column_name) == "boolean"]
  )

  s <- list()

  # ── Single-table ────────────────────────────────────────────────────────────

  s[["Preview data"]] <- list(
    icon  = "🔍", group = "Single table", desc = "First 100 rows",
    sql   = paste0("SELECT *\nFROM ", fq, "\nLIMIT 100;"))

  s[["Row count"]] <- list(
    icon  = "🔢", group = "Single table", desc = "Total records",
    sql   = paste0("SELECT COUNT(*) AS total_rows\nFROM ", fq, ";"))

  if (nrow(columns) > 0) {
    lines <- paste0(
      '  SUM(CASE WHEN "', columns$column_name,
      '" IS NULL THEN 1 ELSE 0 END) AS "',
      columns$column_name, '_nulls"', collapse = ",\n")
    s[["Null audit"]] <- list(
      icon  = "⚠️", group = "Single table", desc = "NULL count per column",
      sql   = paste0("SELECT\n  COUNT(*) AS total_rows,\n", lines,
                     "\nFROM ", fq, ";"))
  }

  if (length(by_type$numeric) > 0) {
    nc <- by_type$numeric[1]
    s[[paste0("Stats: ", nc)]] <- list(
      icon = "📊", group = "Single table",
      desc = paste("Descriptive statistics for", nc),
      sql  = paste0(
        "SELECT\n",
        "  COUNT(*)                                              AS n,\n",
        '  ROUND(AVG("', nc, '")::numeric, 4)                   AS mean,\n',
        '  PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY "', nc, '") AS median,\n',
        '  MIN("', nc, '")                                       AS min,\n',
        '  MAX("', nc, '")                                       AS max,\n',
        '  ROUND(STDDEV("', nc, '")::numeric, 4)                AS std_dev\n',
        "FROM ", fq, '\nWHERE "', nc, '" IS NOT NULL;'))
  }

  if (length(by_type$numeric) > 1 && length(by_type$numeric) <= 6) {
    lines <- paste0(
      '  MIN("',  by_type$numeric, '") AS "', by_type$numeric, '_min",\n',
      '  ROUND(AVG("', by_type$numeric, '")::numeric, 2) AS "',
      by_type$numeric, '_avg",\n',
      '  MAX("',  by_type$numeric, '") AS "', by_type$numeric, '_max"',
      collapse = ",\n")
    s[["Numeric range summary"]] <- list(
      icon = "📈", group = "Single table",
      desc = "Min / avg / max for all numeric fields",
      sql  = paste0("SELECT\n", lines, "\nFROM ", fq, ";"))
  }

  if (length(by_type$text) > 0) {
    tc <- by_type$text[1]
    s[[paste0("Top values: ", tc)]] <- list(
      icon = "🏷️", group = "Single table",
      desc = paste("Most frequent values in", tc),
      sql  = paste0(
        'SELECT "', tc, '" AS value,\n',
        "  COUNT(*) AS frequency,\n",
        "  ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER(), 1) AS pct\n",
        "FROM ", fq, '\nWHERE "', tc, '" IS NOT NULL\n',
        'GROUP BY "', tc, '"\nORDER BY frequency DESC\nLIMIT 20;'))

    lines <- paste0('  COUNT(DISTINCT "', by_type$text, '") AS "',
                    by_type$text, '_distinct"', collapse = ",\n")
    s[["Distinct values per text column"]] <- list(
      icon = "🔠", group = "Single table",
      desc = "Cardinality of all categorical fields",
      sql  = paste0("SELECT\n", lines, "\nFROM ", fq, ";"))
  }

  if (length(by_type$date) > 0) {
    dc <- by_type$date[1]
    s[[paste0("Monthly trend: ", dc)]] <- list(
      icon = "📅", group = "Single table", desc = "Record count by month",
      sql  = paste0(
        "SELECT\n  DATE_TRUNC('month', \"", dc, "\") AS month,\n",
        "  COUNT(*) AS records\nFROM ", fq,
        '\nWHERE "', dc, '" IS NOT NULL\nGROUP BY month\nORDER BY month;'))

    s[[paste0("Date range: ", dc)]] <- list(
      icon = "📆", group = "Single table", desc = "Earliest, latest and span",
      sql  = paste0(
        "SELECT\n  MIN(\"", dc, "\") AS earliest,\n",
        '  MAX("', dc, '") AS latest,\n',
        '  MAX("', dc, '") - MIN("', dc, '") AS span\nFROM ', fq, ";"))
  }

  if (length(by_type$boolean) > 0) {
    bc <- by_type$boolean[1]
    s[[paste0("Boolean distribution: ", bc)]] <- list(
      icon = "☑️", group = "Single table",
      desc = paste("True/false breakdown for", bc),
      sql  = paste0(
        'SELECT "', bc, '" AS value,\n  COUNT(*) AS count,\n',
        "  ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER(), 1) AS pct\n",
        'FROM ', fq, '\nGROUP BY "', bc, '"\nORDER BY count DESC;'))
  }

  id_cols <- columns$column_name[
    grepl("_id$", columns$column_name, ignore.case = TRUE)]
  if (length(id_cols) > 0) {
    ic <- id_cols[1]
    s[[paste0("Key uniqueness: ", ic)]] <- list(
      icon = "🔑", group = "Single table",
      desc = paste("Distinct vs total for", ic),
      sql  = paste0(
        'SELECT\n  COUNT(DISTINCT "', ic, '") AS distinct_ids,\n',
        '  COUNT(*) AS total_rows,\n',
        '  ROUND(100.0 * COUNT(DISTINCT "', ic, '") / COUNT(*), 1) AS pct_unique\n',
        'FROM ', fq, ";"))
  }

  # ── Cross-table JOIN suggestions ────────────────────────────────────────────

  if (!is.null(links) && length(links$common) > 0) {

    targets <- join_targets(table, links)

    for (other_tbl in names(targets)) {
      keys <- targets[[other_tbl]]
      jk   <- keys[1]
      fq2  <- fq_tbl(schema, other_tbl)

      # INNER JOIN preview
      s[[paste0("Join preview: ", table, " ↔ ", other_tbl)]] <- list(
        icon  = "🔗", group = "Cross-table",
        desc  = sprintf('INNER JOIN on "%s" - first 100 rows from both tables', jk),
        sql   = paste0(
          'SELECT t1.*, t2.*\n',
          'FROM ', fq,  ' AS t1\n',
          'INNER JOIN ', fq2, ' AS t2\n',
          '  ON t1."', jk, '" = t2."', jk, '"\n',
          'LIMIT 100;'))

      # LEFT anti-join: records with no match
      s[[paste0("Unmatched: ", table, " → ", other_tbl)]] <- list(
        icon  = "🔎", group = "Cross-table",
        desc  = sprintf(
          'Rows in %s with no matching "%s" in %s', table, jk, other_tbl),
        sql   = paste0(
          'SELECT t1.*\n',
          'FROM ', fq,  ' AS t1\n',
          'LEFT JOIN ', fq2, ' AS t2\n',
          '  ON t1."', jk, '" = t2."', jk, '"\n',
          'WHERE t2."', jk, '" IS NULL;'))

      # Key coverage: how many rows each key value has in each table
      s[[paste0("Key coverage: ", jk, "  (", table, " vs ", other_tbl, ")")]] <- list(
        icon  = "📋", group = "Cross-table",
        desc  = sprintf(
          'Row count per "%s" value in each table (FULL OUTER JOIN)', jk),
        sql   = paste0(
          'SELECT\n',
          '  COALESCE(t1."', jk, '", t2."', jk, '") AS "', jk, '",\n',
          '  COUNT(DISTINCT t1.ctid) AS "rows_in_', table, '",\n',
          '  COUNT(DISTINCT t2.ctid) AS "rows_in_', other_tbl, '"\n',
          'FROM ', fq,  ' AS t1\n',
          'FULL OUTER JOIN ', fq2, ' AS t2\n',
          '  ON t1."', jk, '" = t2."', jk, '"\n',
          'GROUP BY 1\nORDER BY 1\nLIMIT 100;'))

      other_cols <- links$all_cols[[other_tbl]]

      if (!is.null(other_cols)) {

        # Aggregate numeric column in the joined table by key
        other_num <- setdiff(
          other_cols$column_name[
            mapply(type_category, other_cols$data_type, other_cols$column_name) == "numeric"],
          jk)
        if (length(other_num) > 0) {
          ac <- other_num[1]
          s[[paste0("Aggregate: ", ac, " by ", jk)]] <- list(
            icon  = "➕", group = "Cross-table",
            desc  = sprintf(
              'Sum / avg of %s.%s grouped by shared key "%s"',
              other_tbl, ac, jk),
            sql   = paste0(
              'SELECT\n',
              '  t1."', jk, '",\n',
              '  COUNT(*) AS joined_rows,\n',
              '  ROUND(SUM(t2."', ac, '")::numeric, 2)  AS "sum_', ac, '",\n',
              '  ROUND(AVG(t2."', ac, '")::numeric, 4)  AS "avg_', ac, '"\n',
              'FROM ', fq,  ' AS t1\n',
              'INNER JOIN ', fq2, ' AS t2\n',
              '  ON t1."', jk, '" = t2."', jk, '"\n',
              'GROUP BY t1."', jk, '"\nORDER BY joined_rows DESC\nLIMIT 50;'))
        }

        # Categorical breakdown from joined table
        other_txt <- setdiff(
          other_cols$column_name[
            mapply(type_category, other_cols$data_type, other_cols$column_name) == "text"],
          jk)
        if (length(other_txt) > 0) {
          tc2 <- other_txt[1]
          s[[paste0("Breakdown: ", table, " by ", tc2)]] <- list(
            icon  = "📊", group = "Cross-table",
            desc  = sprintf(
              'Count of %s rows per %s.%s category (joined on "%s")',
              table, other_tbl, tc2, jk),
            sql   = paste0(
              'SELECT\n',
              '  t2."', tc2, '" AS category,\n',
              '  COUNT(*) AS row_count\n',
              'FROM ', fq,  ' AS t1\n',
              'INNER JOIN ', fq2, ' AS t2\n',
              '  ON t1."', jk, '" = t2."', jk, '"\n',
              'WHERE t2."', tc2, '" IS NOT NULL\n',
              'GROUP BY t2."', tc2, '"\nORDER BY row_count DESC\nLIMIT 20;'))
        }

        # Date trend using a date column from the joined table
        other_dt <- setdiff(
          other_cols$column_name[
            mapply(type_category, other_cols$data_type, other_cols$column_name) == "date"],
          jk)
        if (length(other_dt) > 0) {
          dc2 <- other_dt[1]
          s[[paste0("Trend via join: ", dc2, " (", other_tbl, ")")]] <- list(
            icon  = "📅", group = "Cross-table",
            desc  = sprintf(
              'Monthly record count from %s using date column %s.%s',
              table, other_tbl, dc2),
            sql   = paste0(
              'SELECT\n',
              "  DATE_TRUNC('month', t2.\"", dc2, "\") AS month,\n",
              '  COUNT(*) AS records\n',
              'FROM ', fq,  ' AS t1\n',
              'INNER JOIN ', fq2, ' AS t2\n',
              '  ON t1."', jk, '" = t2."', jk, '"\n',
              'WHERE t2."', dc2, '" IS NOT NULL\n',
              'GROUP BY month\nORDER BY month;'))
        }
      }

      # Multi-key join when more than one shared field exists
      if (length(keys) > 1) {
        on_clause <- paste0(
          '  ON t1."', keys, '" = t2."', keys, '"', collapse = "\n  AND ")
        s[[paste0("Multi-key join: ", table, " ↔ ", other_tbl)]] <- list(
          icon  = "🔗", group = "Cross-table",
          desc  = sprintf("JOIN on multiple shared fields: %s",
                          paste(keys, collapse = ", ")),
          sql   = paste0(
            'SELECT t1.*, t2.*\n',
            'FROM ', fq,  ' AS t1\n',
            'INNER JOIN ', fq2, ' AS t2\n',
            on_clause, '\nLIMIT 100;'))
      }
    }

    # Value-overlap: which values of each common field appear in multiple tables
    for (fld in links$common[seq_len(min(3L, length(links$common)))]) {
      tbls_with <- links$field_map[[fld]]
      if (length(tbls_with) >= 2) {
        parts <- lapply(tbls_with, function(t)
          paste0('  SELECT "', fld, '" AS value, ',
                 pg_str(t), ' AS source_table\n  FROM ', fq_tbl(schema, t)))
        s[[paste0("Value overlap: ", fld)]] <- list(
          icon  = "🌐", group = "Cross-table",
          desc  = sprintf(
            'Which "%s" values appear in more than one table', fld),
          sql   = paste0(
            'SELECT value,\n',
            '  COUNT(DISTINCT source_table) AS in_n_tables,\n',
            "  STRING_AGG(DISTINCT source_table, ', ') AS tables\n",
            'FROM (\n',
            paste(parts, collapse = "\n  UNION ALL\n"),
            '\n) sub\n',
            'GROUP BY value\n',
            'HAVING COUNT(DISTINCT source_table) > 1\n',
            'ORDER BY in_n_tables DESC, value\nLIMIT 50;'))
      }
    }
  }

  s
}


# ── 9. NO-CODE SQL BUILDER ────────────────────────────────────────────────────

build_nocode_sql <- function(schema, primary_table,
                              select_cols,
                              joins,        # list of list(table, key, type)
                              conditions,   # list of list(alias, col, op, val)
                              groupby_cols,
                              orderby_col, order_dir,
                              row_limit,
                              all_col_info) {

  primary_fq    <- fq_tbl(schema, primary_table)
  primary_alias <- "t1"

  # alias → table name map
  alias_map            <- list()
  alias_map[["t1"]]    <- primary_table

  # FROM + explicit JOINs
  from_lines  <- paste0("FROM ", primary_fq, " AS t1")
  valid_joins <- Filter(function(j) !is.null(j$table) && nzchar(j$table %||% ""), joins)

  for (i in seq_along(valid_joins)) {
    j     <- valid_joins[[i]]
    alias <- tbl_alias(i + 1)
    alias_map[[alias]] <- j$table
    jtype <- j$type %||% "INNER JOIN"
    jkey  <- j$key  %||% ""
    if (!nzchar(jkey)) next
    from_lines <- paste0(
      from_lines, "\n",
      jtype, " ", fq_tbl(schema, j$table), " AS ", alias, "\n",
      '  ON t1."', jkey, '" = ', alias, '."', jkey, '"')
  }

  # SELECT
  # Strip "*" sentinel when specific columns are also selected - cannot mix
  has_star   <- "*" %in% (select_cols %||% character(0))
  named_cols <- (select_cols %||% character(0))[select_cols != "*"]

  if (is.null(select_cols) || length(select_cols) == 0 ||
      (has_star && length(named_cols) == 0)) {
    select_sql <- "SELECT *"
  } else {
    col_exprs <- sapply(named_cols, function(sc) {
      if (grepl(".", sc, fixed = TRUE)) {
        p <- strsplit(sc, ".", fixed = TRUE)[[1]]
        paste0(p[1], '."', paste(p[-1], collapse = "."), '"')
      } else paste0('"', sc, '"')
    })
    select_sql <- paste0("SELECT\n  ", paste(col_exprs, collapse = ",\n  "))
  }
  # WHERE
  where_parts <- c()
  for (cond in conditions) {
    alias <- cond$alias %||% "t1"
    col   <- cond$col   %||% ""
    op    <- cond$op    %||% "="
    val   <- cond$val   %||% ""
    if (!nzchar(col)) next
    lhs <- paste0(alias, '."', col, '"')
    if (op %in% c("IS NULL", "IS NOT NULL")) {
      where_parts <- c(where_parts, paste(lhs, op))
    } else if (op == "LIKE") {
      where_parts <- c(where_parts, paste0(lhs, " LIKE '", val, "'"))
    } else if (op == "IN") {
      vals <- paste0("'", trimws(unlist(strsplit(val, ","))), "'", collapse = ", ")
      where_parts <- c(where_parts, paste0(lhs, " IN (", vals, ")"))
    } else {
      tbl_name <- alias_map[[alias]] %||% primary_table
      col_info <- all_col_info[[tbl_name]]
      col_type <- if (!is.null(col_info))
        col_info$data_type[col_info$column_name == col] else character(0)
      if (length(col_type) > 0 && type_category(col_type[1], col) == "numeric")
        where_parts <- c(where_parts, paste(lhs, op, val))
      else
        where_parts <- c(where_parts, paste0(lhs, " ", op, " '", val, "'"))
    }
  }
  where_sql <- if (length(where_parts) > 0)
    paste("WHERE", paste(where_parts, collapse = "\n  AND "))
  else ""

  # GROUP BY
  # When GROUP BY is active, every SELECT column must appear in GROUP BY or be
  # an aggregate. Auto-promote any SELECT columns not already in GROUP BY into it
  # so the query is always valid.
  qualify <- function(sc) {
    if (grepl(".", sc, fixed = TRUE)) {
      p <- strsplit(sc, ".", fixed = TRUE)[[1]]
      paste0(p[1], '."', paste(p[-1], collapse = "."), '"')
    } else paste0('"', sc, '"')
  }

  # Bare column name for comparison (strip alias prefix if present)
  bare_col <- function(sc) {
    if (grepl(".", sc, fixed = TRUE))
      paste(strsplit(sc, ".", fixed = TRUE)[[1]][-1], collapse = ".")
    else sc
  }

  using_star    <- has_star && length(named_cols) == 0
  using_groupby <- !using_star &&
                   !is.null(groupby_cols) && length(groupby_cols) > 0 &&
                   any(nzchar(groupby_cols))
  # SELECT * with GROUP BY is always invalid - drop GROUP BY in that case

  effective_groupby <- groupby_cols

  if (using_groupby && length(named_cols) > 0) {
    # Auto-promote SELECT cols not already in GROUP BY so the query is valid
    grp_bare <- sapply(groupby_cols, bare_col)
    sel_bare  <- sapply(named_cols,   bare_col)
    missing   <- named_cols[!sel_bare %in% grp_bare]
    if (length(missing) > 0)
      effective_groupby <- c(groupby_cols, missing)
  }

  group_sql <- if (using_groupby)
    paste0("GROUP BY\n  ", paste(sapply(effective_groupby, qualify), collapse = ",\n  "))
  else ""
  # ORDER BY
  order_sql <- if (!is.null(orderby_col) && nzchar(orderby_col %||% "") &&
                   orderby_col != "none")
    paste0("ORDER BY ", qualify(orderby_col), " ", order_dir)
  else ""

  lim_val   <- suppressWarnings(as.integer(row_limit))
  limit_sql <- if (!is.na(lim_val) && lim_val > 0) paste("LIMIT", lim_val) else ""

  paste(Filter(nzchar, c(select_sql, from_lines, where_sql,
                          group_sql, order_sql, limit_sql)),
        collapse = "\n")
}


# ── 10. CSS ────────────────────────────────────────────────────────────────────

APP_CSS <- '
:root {
  --dre-dark:    #0B2341;
  --dre-surface: #0D2A4A;
  --dre-mid:     #133660;
  --dre-teal:    #00C5B5;
  --dre-teal-lt: #00E5D2;
  --dre-text:    #E8F4F8;
  --dre-muted:   #8BA3B5;
  --dre-border:  rgba(255,255,255,0.08);
  --dre-error:   #E05C6E;
  --dre-gold:    #FFB84D;
}
/* Base: 15px gives every rem unit a comfortable starting point */
html { font-size:15px; }
body { background:var(--dre-dark); color:var(--dre-text);
       font-family:"Segoe UI",system-ui,sans-serif;
       font-size:1rem; line-height:1.6; }
.skin-blue .main-header .logo,
.skin-blue .main-header .navbar { background:var(--dre-dark)!important;
  border-bottom:1px solid var(--dre-border); }
.skin-blue .main-header .logo   { color:var(--dre-teal)!important;
  font-weight:700; font-size:1.15rem; }
.skin-blue .main-sidebar         { background:var(--dre-surface)!important;
  border-right:1px solid var(--dre-border); }
.content-wrapper,.right-side    { background:var(--dre-dark)!important; }

/* ── Sidebar ── */
.conn-badge { display:flex;align-items:center;gap:8px;padding:9px 13px;border-radius:8px;
  font-size:1rem;font-weight:600;margin-bottom:14px;width:100%; }
.conn-ok   { background:rgba(0,197,181,0.12);border:1px solid rgba(0,197,181,0.3);color:var(--dre-teal); }
.conn-fail { background:rgba(224,92,110,0.12);border:1px solid rgba(224,92,110,0.3);color:var(--dre-error); }
.conn-dot  { width:9px;height:9px;border-radius:50%;background:currentColor;flex-shrink:0; }
.sb-label  { font-size:0.87rem;color:var(--dre-muted);text-transform:uppercase;
  letter-spacing:1.2px;margin:14px 0 7px;display:block; }
.tbl-list  { display:flex;flex-direction:column;gap:3px;max-height:240px;overflow-y:auto;margin-bottom:6px; }
.tbl-item  { padding:9px 11px;border-radius:6px;cursor:pointer;font-size:1rem;
  color:var(--dre-muted);display:flex;align-items:center;gap:9px;
  transition:all 0.2s;border:1px solid transparent; }
.tbl-item:hover   { background:rgba(0,197,181,0.08);color:var(--dre-text); }
.tbl-item.active  { background:rgba(0,197,181,0.14);border-color:rgba(0,197,181,0.3);
  color:var(--dre-teal);font-weight:600; }
.tbl-icon  { font-size:0.95rem;color:var(--dre-teal);opacity:0.7; }
.col-list  { display:flex;flex-direction:column;gap:3px;max-height:180px;overflow-y:auto; }
.col-item  { display:flex;align-items:center;gap:8px;padding:5px 7px;font-size:0.97rem; }
.col-item.is-key { border-left:3px solid var(--dre-gold);padding-left:6px; }
.col-name  { color:var(--dre-text);flex:1;overflow:hidden;text-overflow:ellipsis;white-space:nowrap; }
.type-pill { font-size:0.78rem;padding:2px 8px;border-radius:10px;font-weight:700;
  text-transform:uppercase;white-space:nowrap;flex-shrink:0; }
.tp-numeric { background:rgba(0,197,181,0.18);  color:#00C5B5; }
.tp-text    { background:rgba(139,163,181,0.2); color:#8BA3B5; }
.tp-date    { background:rgba(180,120,255,0.2); color:#b47bff; }
.tp-boolean { background:rgba(255,200,100,0.2); color:#ffc864; }
.tp-other   { background:rgba(255,255,255,0.1); color:#8BA3B5; }
.key-pill   { font-size:0.74rem;padding:2px 8px;border-radius:10px;font-weight:700;
  background:rgba(255,184,77,0.2);color:var(--dre-gold);flex-shrink:0;white-space:nowrap; }

/* Common-fields sidebar panel */
.links-box   { background:rgba(255,184,77,0.06);border:1px solid rgba(255,184,77,0.22);
  border-radius:8px;padding:11px 13px;margin-top:10px; }
.links-title { font-size:0.84rem;color:var(--dre-gold);text-transform:uppercase;
  letter-spacing:1px;font-weight:700;margin-bottom:0; }
.links-toggle { cursor:pointer;user-select:none;display:flex;align-items:center;gap:4px;
  padding:4px 0;transition:opacity .15s; }
.links-toggle:hover { opacity:0.8; }
.links-arrow { font-size:0.75rem;flex-shrink:0;color:var(--dre-gold); }
.links-body  { padding-top:10px; }
/* Each field: icon+name on row 1, tables indented on row 2 */
.link-field  { display:block;padding:5px 0;border-bottom:1px solid rgba(255,184,77,0.1); }
.link-field:last-child { border-bottom:none;padding-bottom:0; }
.link-field-header { display:flex;align-items:center;gap:6px; }
.link-icon   { color:var(--dre-gold);font-size:0.82rem;flex-shrink:0; }
.link-fname  { color:var(--dre-gold);font-weight:700;font-size:0.88rem;word-break:break-all;line-height:1.3; }
.link-tbls   { color:var(--dre-muted);font-size:0.79rem;line-height:1.5;padding-left:18px;margin-top:2px;word-break:break-word; }

/* ── SQL editor ── */
.sql-editor-wrap textarea.form-control {
  background:#060F1A!important;color:#A8D8EA!important;
  border:1px solid var(--dre-border)!important;border-radius:8px!important;
  font-family:"Fira Code","Courier New",monospace!important;
  font-size:1.1rem!important;line-height:1.75!important;
  padding:16px!important;resize:vertical!important;box-shadow:none!important; }
.sql-editor-wrap textarea.form-control:focus {
  border-color:rgba(0,197,181,0.5)!important;
  box-shadow:0 0 0 3px rgba(0,197,181,0.08)!important; }

/* ── Buttons ── */
.btn-run      { background:var(--dre-teal)!important;color:var(--dre-dark)!important;
  border:none!important;font-weight:700!important;font-size:1rem!important;
  padding:10px 28px!important;border-radius:8px!important; }
.btn-run:hover { background:var(--dre-teal-lt)!important; }
.btn-dre-sec  { background:var(--dre-mid)!important;color:var(--dre-text)!important;
  border:1px solid var(--dre-border)!important;border-radius:8px!important;
  font-size:0.97rem!important; }
.btn-dre-sec:hover { border-color:var(--dre-teal)!important;color:var(--dre-teal)!important; }
.btn-generate { background:var(--dre-teal)!important;color:var(--dre-dark)!important;
  border:none!important;font-weight:700!important;border-radius:8px!important;
  width:100%!important;padding:13px!important;font-size:1.05rem!important; }
.btn-add-cond { background:rgba(0,197,181,0.1)!important;color:var(--dre-teal)!important;
  border:1px solid rgba(0,197,181,0.25)!important;border-radius:6px!important;
  font-size:0.93rem!important;margin-top:8px!important; }
.btn-add-join { background:rgba(255,184,77,0.1)!important;color:var(--dre-gold)!important;
  border:1px solid rgba(255,184,77,0.3)!important;border-radius:6px!important;
  font-size:0.93rem!important;margin-top:8px!important; }
.btn-rm-cond  { background:rgba(224,92,110,0.1)!important;color:var(--dre-error)!important;
  border:1px solid rgba(224,92,110,0.25)!important;border-radius:6px!important;
  padding:5px 12px!important;font-size:0.97rem!important;line-height:1; }
.btn-use-query { background:rgba(0,197,181,0.12)!important;color:var(--dre-teal)!important;
  border:1px solid rgba(0,197,181,0.3)!important;border-radius:6px!important;
  font-size:0.93rem!important;font-weight:600!important;width:100%!important; }
.btn-use-query:hover { background:rgba(0,197,181,0.22)!important; }

/* ── Results ── */
.result-stats { display:flex;gap:14px;flex-wrap:wrap;margin-bottom:14px; }
.rs-card { background:var(--dre-surface);border:1px solid var(--dre-border);
  border-radius:8px;padding:11px 20px;text-align:center;min-width:110px; }
.rs-num { font-size:1.75rem;font-weight:700;color:var(--dre-teal);line-height:1.1; }
.rs-lbl { font-size:0.84rem;color:var(--dre-muted);margin-top:3px; }
.sql-error { background:rgba(224,92,110,0.09);border:1px solid rgba(224,92,110,0.3);
  border-radius:8px;padding:16px;color:#E05C6E;font-size:0.97rem;margin-bottom:16px; }
.sql-error pre { margin:0;color:inherit;font-size:0.93rem;white-space:pre-wrap; }
.sql-info { background:rgba(0,197,181,0.07);border:1px solid rgba(0,197,181,0.2);
  border-radius:8px;padding:11px 15px;color:var(--dre-teal);font-size:0.97rem;margin-bottom:14px; }

/* ── Suggestion cards ── */
.sug-filter-row { display:flex;gap:8px;margin-bottom:20px;flex-wrap:wrap; }
.sug-ftab   { padding:7px 18px;border-radius:20px;cursor:pointer;font-size:0.95rem;
  font-weight:600;border:1px solid var(--dre-border);color:var(--dre-muted);
  background:var(--dre-surface);transition:all 0.2s;user-select:none; }
.sug-ftab:hover { border-color:var(--dre-teal);color:var(--dre-text); }
.sug-ftab.active { background:rgba(0,197,181,0.15);border-color:var(--dre-teal);color:var(--dre-teal); }
.sug-ftab.join-ftab.active { background:rgba(255,184,77,0.15);
  border-color:var(--dre-gold);color:var(--dre-gold); }
.sug-grid   { display:grid;grid-template-columns:repeat(auto-fill,minmax(400px,1fr));gap:18px; }
.sug-card   { background:var(--dre-surface);border:1px solid var(--dre-border);
  border-radius:12px;padding:22px;transition:all 0.25s; }
.sug-card:hover { border-color:rgba(0,197,181,0.3);transform:translateY(-2px);
  box-shadow:0 8px 24px rgba(0,0,0,0.25); }
.sug-card.join-card { border-color:rgba(255,184,77,0.12); }
.sug-card.join-card:hover { border-color:rgba(255,184,77,0.4); }
.sug-header { display:flex;align-items:center;gap:10px;margin-bottom:7px; }
.sug-icon   { font-size:1.45rem; }
.sug-title  { font-weight:600;font-size:1.02rem;flex:1; }
.sug-desc   { font-size:0.91rem;color:var(--dre-muted);margin-bottom:13px;line-height:1.55; }
.sug-sql    { background:#060F1A;border-radius:6px;padding:13px;
  font-family:"Fira Code","Courier New",monospace;font-size:0.84rem;color:#A8D8EA;
  white-space:pre;overflow-x:auto;margin-bottom:13px;max-height:140px;overflow-y:auto;
  border:1px solid rgba(255,255,255,0.05); }
.sug-badge  { display:inline-block;font-size:0.74rem;padding:3px 9px;border-radius:10px;
  font-weight:700;margin-bottom:9px;text-transform:uppercase; }
.badge-single { background:rgba(0,197,181,0.15);color:var(--dre-teal); }
.badge-cross  { background:rgba(255,184,77,0.15);color:var(--dre-gold); }

/* ── No-code builder ── */
.nc-section { background:var(--dre-surface);border:1px solid var(--dre-border);
  border-radius:10px;padding:22px;margin-bottom:16px; }
.nc-section.nc-join-section { border-color:rgba(255,184,77,0.22); }
.nc-label  { font-size:0.82rem;color:var(--dre-teal);text-transform:uppercase;
  letter-spacing:1.5px;margin-bottom:13px;font-weight:700;display:block; }
.nc-join-label { color:var(--dre-gold)!important; }
.nc-preview { background:#060F1A;border:1px solid var(--dre-border);border-radius:8px;
  padding:16px;font-family:"Fira Code","Courier New",monospace;font-size:0.95rem;
  color:#A8D8EA;white-space:pre;overflow-x:auto;min-height:64px; }
.join-row   { background:rgba(255,184,77,0.04);border:1px solid rgba(255,184,77,0.15);
  border-radius:8px;padding:14px;margin-bottom:11px; }
.join-on-hint { font-size:0.84rem;color:var(--dre-gold);margin-top:7px;
  font-family:"Fira Code","Courier New",monospace;opacity:0.9; }

/* ── Schema explorer ── */
.se-schema { font-weight:700;color:var(--dre-teal);padding:9px 0;
  border-bottom:1px solid var(--dre-border);margin-bottom:11px;font-size:1rem; }
.se-table  { padding:7px 0 5px 14px;color:var(--dre-text);font-weight:600;font-size:0.95rem; }
.se-col    { padding:4px 0 4px 28px;display:flex;align-items:center;gap:8px;font-size:0.88rem; }
.se-col.se-key { color:var(--dre-text); }
.se-nullable { font-size:0.78rem;color:var(--dre-muted);opacity:0.75; }
.se-joinmap  { background:rgba(255,184,77,0.06);border:1px solid rgba(255,184,77,0.2);
  border-radius:8px;padding:13px 17px;margin-bottom:15px; }
.se-joinmap-title { font-size:0.88rem;color:var(--dre-gold);font-weight:700;
  text-transform:uppercase;letter-spacing:1px;margin-bottom:11px; }
.se-jrow    { display:flex;align-items:center;gap:9px;padding:5px 0;font-size:0.91rem; }
.se-jrow span.tname { color:var(--dre-text);font-weight:600; }
.se-jrow span.sep   { color:var(--dre-muted); }

/* ── Tabs & DataTables ── */
.nav-tabs { border-color:var(--dre-border)!important; }
.nav-tabs>li>a { color:var(--dre-muted)!important;background:transparent!important;
  border:none!important;font-size:1rem!important; }
.nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover {
  color:var(--dre-teal)!important;background:transparent!important;
  border-bottom:2px solid var(--dre-teal)!important;
  border-top:none!important;border-left:none!important;border-right:none!important; }
.tab-content { padding-top:20px; }
.dataTables_wrapper { color:var(--dre-text);font-size:0.97rem; }
table.dataTable { font-size:0.97rem; }
table.dataTable thead { background:var(--dre-mid); }
table.dataTable thead th { color:var(--dre-text)!important;
  border-bottom:1px solid var(--dre-border)!important;font-size:0.97rem!important; }

table.dataTable tbody tr { background:var(--dre-surface)!important; }
table.dataTable tbody tr:hover { background:var(--dre-mid)!important; }
table.dataTable tbody td { color:var(--dre-text);border-color:var(--dre-border);
  padding:8px 10px!important; }
/* Pagination & info */
.dataTables_info,.dataTables_paginate { color:var(--dre-muted)!important;font-size:0.93rem!important; }
.paginate_button { color:var(--dre-muted)!important;font-size:0.93rem!important; }
.paginate_button.current { background:var(--dre-teal)!important;color:var(--dre-dark)!important;
  border-color:var(--dre-teal)!important; }
/* Global search box & show N entries */
.dataTables_filter input,.dataTables_length select {
  background:var(--dre-mid)!important;color:var(--dre-text)!important;
  border:1px solid var(--dre-border)!important;border-radius:6px!important;
  font-size:0.93rem!important;padding:4px 8px!important; }
.dataTables_filter label,.dataTables_length label { color:var(--dre-muted)!important;
  font-size:0.93rem!important; }


/* ── Form controls ── */
.form-control { background:var(--dre-mid)!important;border:1px solid var(--dre-border)!important;
  color:var(--dre-text)!important;border-radius:6px!important;
  font-size:1rem!important; }
.form-control:focus { border-color:rgba(0,197,181,0.5)!important;box-shadow:none!important; }
.selectize-input { background:var(--dre-mid)!important;border:1px solid var(--dre-border)!important;
  color:var(--dre-text)!important;border-radius:6px!important;
  font-size:1rem!important; }
.selectize-dropdown { background:var(--dre-surface)!important;
  border:1px solid var(--dre-border)!important;color:var(--dre-text)!important;
  font-size:1rem!important; }
.selectize-dropdown .option:hover,.selectize-dropdown .option.active { background:var(--dre-mid)!important; }
.item { background:rgba(0,197,181,0.14)!important;color:var(--dre-teal)!important;
  border-radius:4px!important;font-size:0.97rem!important; }
.checkbox label { color:var(--dre-text)!important;font-size:1rem!important; }
input[type="checkbox"] { accent-color:var(--dre-teal);width:15px;height:15px; }
label { font-size:1rem; }
.control-label { font-size:1rem!important;color:var(--dre-text)!important; }

/* ── Save-to-table modal ── */
.save-modal-row { display:flex;align-items:center;gap:10px;margin-bottom:14px; }
.save-schema-tag { font-size:0.87rem;color:var(--dre-teal);text-transform:uppercase;
  letter-spacing:1px;font-weight:700;margin-bottom:7px;display:block; }
.save-warn  { background:rgba(255,184,77,0.1);border:1px solid rgba(255,184,77,0.35);
  border-radius:8px;padding:13px 17px;display:flex;align-items:flex-start;gap:10px;
  margin-bottom:14px; }
.save-warn-icon { font-size:1.25rem;flex-shrink:0;margin-top:1px; }
.save-warn-text { font-size:0.95rem;color:var(--dre-gold);line-height:1.55; }
.save-ok    { background:rgba(0,197,181,0.1);border:1px solid rgba(0,197,181,0.3);
  border-radius:8px;padding:13px 17px;display:flex;align-items:center;gap:10px;
  margin-bottom:14px; }
.save-ok-icon { font-size:1.25rem; }
.save-ok-text { font-size:0.95rem;color:var(--dre-teal);line-height:1.55; }
.save-err   { background:rgba(224,92,110,0.1);border:1px solid rgba(224,92,110,0.3);
  border-radius:8px;padding:13px 17px;margin-bottom:14px;color:var(--dre-error);
  font-size:0.95rem; }
.new-schema-badge { display:inline-flex;align-items:center;gap:5px;font-size:0.8rem;
  background:rgba(180,120,255,0.15);color:#b47bff;border:1px solid rgba(180,120,255,0.3);
  border-radius:10px;padding:3px 10px;font-weight:700;margin-left:8px; }
.modal-content { background:var(--dre-surface)!important;
  border:1px solid var(--dre-border)!important;color:var(--dre-text)!important;
  font-size:1rem!important; }
.modal-header  { background:var(--dre-mid)!important;
  border-bottom:1px solid var(--dre-border)!important; }
.modal-title   { color:var(--dre-text)!important;font-weight:600;font-size:1.1rem!important; }
.modal-footer  { background:var(--dre-mid)!important;
  border-top:1px solid var(--dre-border)!important; }
.modal-body    { font-size:1rem!important; }
.btn-save-confirm { background:var(--dre-teal)!important;color:var(--dre-dark)!important;
  border:none!important;font-weight:700!important;border-radius:6px!important;
  font-size:1rem!important; }
.btn-save-overwrite { background:rgba(255,184,77,0.2)!important;color:var(--dre-gold)!important;
  border:1px solid rgba(255,184,77,0.4)!important;font-weight:700!important;
  border-radius:6px!important;font-size:1rem!important; }


/* ── OMOP CDM badge (sidebar) ── */
.omop-badge { background:rgba(180,120,255,0.08);border:1px solid rgba(180,120,255,0.3);
  border-radius:8px;padding:10px 12px;margin-bottom:10px; }
.omop-badge-header { display:flex;align-items:center;gap:7px;margin-bottom:4px; }
.omop-dna   { font-size:1rem;flex-shrink:0; }
.omop-badge-title { color:#b47bff;font-weight:700;font-size:0.88rem;flex:1; }
.omop-badge-ver   { font-size:0.72rem;background:rgba(180,120,255,0.2);color:#b47bff;
  border:1px solid rgba(180,120,255,0.35);border-radius:10px;
  padding:1px 7px;font-weight:700;flex-shrink:0; }
.omop-badge-detail { font-size:0.78rem;color:var(--dre-muted);line-height:1.4; }

/* ── OMOP concept resolution bar (above results table) ── */
.omop-resolve-bar { display:flex;align-items:center;gap:10px;
  background:rgba(180,120,255,0.07);border:1px solid rgba(180,120,255,0.25);
  border-radius:8px;padding:9px 14px;margin-bottom:12px;flex-wrap:wrap; }
.omop-resolve-bar.omop-resolve-warn { background:rgba(255,80,80,0.07);
  border-color:rgba(255,80,80,0.30); }
.omop-resolve-bar.omop-resolve-warn .omop-resolve-text { color:#ff8080; }
.omop-resolve-bar.omop-resolve-partial { background:rgba(255,184,77,0.07);
  border-color:rgba(255,184,77,0.35); }
.omop-resolve-bar.omop-resolve-partial .omop-resolve-text { color:var(--dre-gold); }
.omop-resolve-icon { font-size:1.1rem;flex-shrink:0; }
.omop-resolve-text { color:#b47bff;font-size:0.9rem;flex:1;min-width:160px; }
.btn-omop-toggle { background:rgba(180,120,255,0.15)!important;
  color:#b47bff!important;border:1px solid rgba(180,120,255,0.35)!important;
  border-radius:6px!important;font-size:0.82rem!important;
.btn-athena-fetch { background:rgba(255,184,77,0.18)!important;
  color:var(--dre-gold)!important;border:1px solid rgba(255,184,77,0.45)!important;
  border-radius:6px!important;font-size:0.82rem!important;
  padding:4px 12px!important;white-space:nowrap;font-weight:600!important; }
.btn-athena-fetch:hover { background:rgba(255,184,77,0.30)!important;
  border-color:var(--dre-gold)!important; }
  padding:4px 12px!important;font-weight:600!important;flex-shrink:0; }
.btn-omop-toggle:hover { background:rgba(180,120,255,0.28)!important; }

/* ── OMOP Tools tab ── */
.omop-tools-wrap { max-width:960px; }
.omop-section-title { color:var(--dre-teal);font-size:0.78rem;font-weight:700;
  text-transform:uppercase;letter-spacing:0.07em;margin:24px 0 10px; }
.omop-query-grid { display:grid;grid-template-columns:repeat(auto-fill,minmax(260px,1fr));
  gap:10px;margin-bottom:6px; }
.omop-qcard { background:var(--dre-surface);border:1px solid var(--dre-border);
  border-radius:10px;padding:14px 16px;cursor:pointer;transition:border-color .15s,background .15s; }
.omop-qcard:hover { border-color:var(--dre-teal);background:var(--dre-mid); }
.omop-qcard-icon { font-size:1.4rem;margin-bottom:6px; }
.omop-qcard-title { color:var(--dre-text);font-weight:600;font-size:0.95rem;margin-bottom:3px; }
.omop-qcard-desc  { color:var(--dre-muted);font-size:0.82rem; }
.omop-concept-search-wrap { background:var(--dre-surface);border:1px solid var(--dre-border);
  border-radius:10px;padding:20px;margin-bottom:24px; }
.omop-concept-search-wrap h4 { color:var(--dre-teal);margin:0 0 14px;font-size:1rem; }
.omop-concept-result { background:var(--dre-mid);border:1px solid var(--dre-border);
  border-radius:8px;padding:10px 14px;margin-top:8px;cursor:pointer;transition:border-color .15s; }
.omop-concept-result:hover { border-color:var(--dre-teal); }
.omop-concept-id   { color:var(--dre-teal);font-weight:700;font-size:0.9rem;margin-right:10px; }
.omop-concept-name { color:var(--dre-text);font-size:0.93rem; }
.omop-concept-meta { color:var(--dre-muted);font-size:0.8rem;margin-top:3px; }

/* ── Misc ── */
.empty-state { text-align:center;padding:60px 40px;color:var(--dre-muted); }
.empty-icon  { font-size:3rem;margin-bottom:12px; }
.empty-state h4 { font-size:1.2rem;margin-bottom:8px; }
.empty-state p  { font-size:1rem; }
::-webkit-scrollbar { width:5px;height:5px; }
::-webkit-scrollbar-track { background:var(--dre-dark); }
::-webkit-scrollbar-thumb { background:var(--dre-mid);border-radius:3px; }
::-webkit-scrollbar-thumb:hover { background:var(--dre-teal); }
/* Results table - left-align all cells and headers */
table.dataTable tbody td { text-align:left!important; }
table.dataTable thead th { text-align:left!important; }
/* ── OMOP Tools ── */
.omop-section { margin-bottom:8px; }
.omop-section-title { font-size:1.05rem;font-weight:700;color:var(--dre-text);
  margin-bottom:6px;display:flex;align-items:center;gap:8px; }
.omop-section-desc { color:var(--dre-muted);font-size:0.88rem;margin-bottom:14px; }
/* Concept search table */
.concept-search-results { overflow-x:auto; }
.concept-table { width:100%;border-collapse:collapse;font-size:0.9rem; }
.concept-table thead th { background:var(--dre-mid);color:var(--dre-muted);
  font-weight:600;padding:8px 10px;text-align:left;
  border-bottom:1px solid var(--dre-border);white-space:nowrap; }
.concept-row { cursor:pointer;transition:background 0.15s; }
.concept-row:hover { background:rgba(0,197,181,0.08)!important; }
.concept-row td { padding:7px 10px;border-bottom:1px solid var(--dre-border);
  color:var(--dre-text);vertical-align:top; }
.concept-id-cell { font-family:monospace;color:var(--dre-teal);font-weight:600;
  white-space:nowrap; }
.domain-pill { background:rgba(0,197,181,0.12);color:var(--dre-teal);
  border-radius:10px;padding:2px 8px;font-size:0.8rem;white-space:nowrap; }
/* Quick query cards */
.omop-qcat { margin-bottom:20px; }
.omop-qcat-title { font-size:0.95rem;font-weight:700;color:var(--dre-muted);
  text-transform:uppercase;letter-spacing:0.05em;margin-bottom:10px; }
.omop-qcat-grid { display:grid;grid-template-columns:repeat(auto-fill,minmax(280px,1fr));
  gap:12px; }
.omop-qcard { background:var(--dre-mid);border:1px solid var(--dre-border);
  border-radius:10px;padding:14px 16px;display:flex;flex-direction:column;gap:8px;
  transition:border-color 0.15s; }
.omop-qcard:hover { border-color:rgba(0,197,181,0.4); }
.omop-qcard-title { font-size:0.95rem;font-weight:700;color:var(--dre-text); }
.omop-qcard-desc  { font-size:0.83rem;color:var(--dre-muted);flex:1;line-height:1.5; }
.btn-omop-load { background:rgba(0,197,181,0.12)!important;color:var(--dre-teal)!important;
  border:1px solid rgba(0,197,181,0.3)!important;border-radius:6px!important;
  font-size:0.82rem!important;padding:4px 12px!important;align-self:flex-start; }
.btn-omop-load:hover { background:rgba(0,197,181,0.22)!important;
  border-color:var(--dre-teal)!important; }

/* ── Query History tab ── */
.hist-empty { color:var(--dre-muted);font-style:italic;padding:20px 0; }
.hist-list   { display:flex;flex-direction:column;gap:10px; }
.hist-card   { background:var(--dre-surface);border:1px solid var(--dre-border);
  border-radius:10px;padding:14px 16px;transition:border-color .15s; }
.hist-card:hover { border-color:rgba(0,197,181,0.3); }
.hist-card.hist-error { border-color:rgba(224,92,110,0.25); }
.hist-meta   { display:flex;align-items:center;gap:10px;margin-bottom:8px;flex-wrap:wrap; }
.hist-ts     { color:var(--dre-muted);font-size:0.82rem; }
.hist-rows   { background:rgba(0,197,181,0.12);color:var(--dre-teal);border-radius:10px;
  padding:2px 9px;font-size:0.79rem;font-weight:700; }
.hist-err-badge { background:rgba(224,92,110,0.12);color:var(--dre-error);border-radius:10px;
  padding:2px 9px;font-size:0.79rem;font-weight:700; }
.hist-ms     { color:var(--dre-muted);font-size:0.79rem;margin-left:auto; }
.hist-sql    { background:#060F1A;border-radius:6px;padding:10px 12px;
  font-family:"Fira Code","Courier New",monospace;font-size:0.81rem;color:#A8D8EA;
  white-space:pre-wrap;overflow-x:auto;max-height:90px;overflow-y:auto;
  border:1px solid rgba(255,255,255,0.05);margin-bottom:10px; }
.btn-hist-restore { background:rgba(0,197,181,0.1)!important;color:var(--dre-teal)!important;
  border:1px solid rgba(0,197,181,0.28)!important;border-radius:6px!important;
  font-size:0.82rem!important;padding:4px 13px!important;font-weight:600!important; }
.btn-hist-restore:hover { background:rgba(0,197,181,0.22)!important; }

/* ── Column value preview (sidebar) ── */
.col-preview-wrap { margin-top:10px;background:rgba(0,197,181,0.05);
  border:1px solid rgba(0,197,181,0.18);border-radius:8px;padding:11px 13px; }
.col-preview-title { color:var(--dre-teal);font-size:0.78rem;font-weight:700;
  text-transform:uppercase;letter-spacing:1px;margin-bottom:8px; }
.col-preview-table { width:100%;border-collapse:collapse;font-size:0.83rem; }
.col-preview-table td { padding:3px 6px;color:var(--dre-text);border-bottom:1px solid var(--dre-border); }
.col-preview-table td:first-child { color:var(--dre-muted);max-width:120px;
  overflow:hidden;text-overflow:ellipsis;white-space:nowrap; }
.col-preview-table td:last-child { color:var(--dre-teal);font-weight:600;
  text-align:right;white-space:nowrap; }
.col-preview-stat { display:flex;justify-content:space-between;padding:3px 0;
  font-size:0.83rem;border-bottom:1px solid var(--dre-border); }
.col-preview-stat:last-child { border-bottom:none; }
.col-preview-stat-lbl { color:var(--dre-muted); }
.col-preview-stat-val { color:var(--dre-teal);font-weight:600; }
.col-item { cursor:pointer;border-radius:6px;transition:background .15s; }
.col-item:hover { background:rgba(0,197,181,0.07); }
.col-item.preview-active { background:rgba(0,197,181,0.12);border-radius:6px; }

/* ── Concept search in no-code WHERE ── */
.cond-concept-btn { background:rgba(180,120,255,0.12)!important;color:#b47bff!important;
  border:1px solid rgba(180,120,255,0.3)!important;border-radius:6px!important;
  padding:6px 10px!important;font-size:0.85rem!important;white-space:nowrap;
  margin-top:1px;flex-shrink:0; }
.cond-concept-btn:hover { background:rgba(180,120,255,0.25)!important; }
.nc-cond-val-wrap { display:flex;gap:6px;align-items:flex-start; }

/* ── OMOP Type Checker ── */
.tc-summary   { display:flex;gap:12px;flex-wrap:wrap;margin-bottom:20px; }
.tc-sum-card  { background:var(--dre-surface);border:1px solid var(--dre-border);
  border-radius:8px;padding:10px 18px;text-align:center;min-width:100px; }
.tc-sum-num   { font-size:1.65rem;font-weight:700;line-height:1.1; }
.tc-sum-lbl   { font-size:0.8rem;color:var(--dre-muted);margin-top:2px; }
.tc-ok    { color:var(--dre-teal); }
.tc-warn  { color:var(--dre-gold); }
.tc-err   { color:var(--dre-error); }
.tc-info  { color:#b47bff; }
.tc-filter-row { display:flex;gap:8px;margin-bottom:16px;flex-wrap:wrap; }
.tc-ftab  { padding:5px 15px;border-radius:20px;cursor:pointer;font-size:0.88rem;
  font-weight:600;border:1px solid var(--dre-border);color:var(--dre-muted);
  background:var(--dre-surface);transition:all 0.15s;user-select:none; }
.tc-ftab:hover { border-color:var(--dre-teal);color:var(--dre-text); }
.tc-ftab.active { background:rgba(0,197,181,0.15);border-color:var(--dre-teal);color:var(--dre-teal); }
.tc-ftab.tc-ftab-warn.active { background:rgba(255,184,77,0.15);
  border-color:var(--dre-gold);color:var(--dre-gold); }
.tc-ftab.tc-ftab-err.active  { background:rgba(224,92,110,0.15);
  border-color:var(--dre-error);color:var(--dre-error); }
.tc-table-block { margin-bottom:18px; }
.tc-tbl-header  { display:flex;align-items:center;gap:10px;padding:9px 14px;
  background:var(--dre-surface);border:1px solid var(--dre-border);
  border-radius:8px 8px 0 0;cursor:pointer;user-select:none; }
.tc-tbl-header:hover { background:var(--dre-mid); }
.tc-tbl-name  { font-weight:700;font-size:0.95rem;flex:1; }
.tc-tbl-badge { font-size:0.75rem;padding:2px 9px;border-radius:10px;
  font-weight:700;white-space:nowrap; }
.tc-badge-ok   { background:rgba(0,197,181,0.12); color:var(--dre-teal); }
.tc-badge-warn { background:rgba(255,184,77,0.12); color:var(--dre-gold); }
.tc-badge-info { background:rgba(180,120,255,0.12);color:#b47bff; }
.tc-tbl-arrow { font-size:0.8rem;color:var(--dre-muted); }
.tc-tbl-body  { border:1px solid var(--dre-border);border-top:none;
  border-radius:0 0 8px 8px;overflow:hidden; }
.tc-row       { display:grid;grid-template-columns:1fr 120px 120px 140px;
  gap:0;padding:7px 14px;font-size:0.875rem;
  border-bottom:1px solid var(--dre-border);align-items:center; }
.tc-row:last-child { border-bottom:none; }
.tc-row.tc-row-err  { background:rgba(224,92,110,0.06); }
.tc-row.tc-row-warn { background:rgba(255,184,77,0.05); }
.tc-row.tc-row-miss { background:rgba(180,120,255,0.05); }
.tc-row-header { background:var(--dre-mid)!important;font-weight:700;
  font-size:0.79rem;color:var(--dre-muted);text-transform:uppercase;letter-spacing:.05em; }
.tc-col-name  { color:var(--dre-text);font-family:"Fira Code",monospace;font-size:0.83rem; }
.tc-col-actual  { color:var(--dre-muted);font-size:0.82rem; }
.tc-col-expected { font-size:0.82rem; }
.tc-col-status  { font-size:0.82rem;font-weight:700; }
.tc-status-ok   { color:var(--dre-teal); }
.tc-status-err  { color:var(--dre-error); }
.tc-status-miss { color:#b47bff; }
.tc-status-extra { color:var(--dre-muted); }

/* ── DQ Dashboard ── */
.dq-wrap       { max-width:1100px; }
.dq-run-bar    { display:flex;align-items:center;gap:14px;margin-bottom:24px;flex-wrap:wrap; }
.dq-schema-tag { background:rgba(180,120,255,0.1);border:1px solid rgba(180,120,255,0.3);
  border-radius:6px;padding:4px 12px;font-size:0.82rem;color:#b47bff;font-weight:600; }
.dq-section    { margin-bottom:28px; }
.dq-section-title { font-size:0.78rem;font-weight:700;color:var(--dre-muted);
  text-transform:uppercase;letter-spacing:.1em;margin-bottom:12px;
  display:flex;align-items:center;gap:8px; }
.dq-section-title::after { content:"";flex:1;height:1px;background:var(--dre-border); }
/* KPI cards row */
.dq-kpi-row    { display:flex;gap:12px;flex-wrap:wrap;margin-bottom:4px; }
.dq-kpi        { background:var(--dre-surface);border:1px solid var(--dre-border);
  border-radius:10px;padding:14px 20px;min-width:140px;flex:1; }
.dq-kpi-val    { font-size:1.9rem;font-weight:700;color:var(--dre-teal);line-height:1.1; }
.dq-kpi-lbl    { font-size:0.8rem;color:var(--dre-muted);margin-top:3px; }
.dq-kpi-sub    { font-size:0.78rem;color:var(--dre-muted);margin-top:2px; }
/* Domain coverage table */
.dq-cov-table  { width:100%;border-collapse:collapse;font-size:0.875rem; }
.dq-cov-table thead th { background:var(--dre-mid);color:var(--dre-muted);
  font-weight:700;padding:8px 12px;text-align:left;font-size:0.79rem;
  text-transform:uppercase;letter-spacing:.05em;white-space:nowrap;
  border-bottom:1px solid var(--dre-border); }
.dq-cov-table tbody td { padding:8px 12px;border-bottom:1px solid var(--dre-border);
  color:var(--dre-text);vertical-align:middle; }
.dq-cov-table tbody tr:last-child td { border-bottom:none; }
.dq-cov-table tbody tr:hover td { background:var(--dre-mid); }
.dq-tbl-name   { font-weight:600;font-family:"Fira Code",monospace;font-size:0.83rem; }
.dq-num        { font-variant-numeric:tabular-nums;text-align:right; }
.dq-date-range { color:var(--dre-muted);font-size:0.8rem;white-space:nowrap; }
/* Coverage bar */
.dq-bar-wrap   { display:flex;align-items:center;gap:8px; }
.dq-bar-bg     { flex:1;height:8px;background:var(--dre-mid);border-radius:4px;overflow:hidden;min-width:60px; }
.dq-bar-fill   { height:100%;border-radius:4px;background:var(--dre-teal);transition:width .3s; }
.dq-bar-fill.dq-bar-warn  { background:var(--dre-gold); }
.dq-bar-fill.dq-bar-err   { background:var(--dre-error); }
.dq-bar-pct    { font-size:0.79rem;color:var(--dre-muted);white-space:nowrap;min-width:38px;text-align:right; }
/* Unmapped table - same base, colour-coded pct */
.dq-pct-ok   { color:var(--dre-teal);font-weight:700; }
.dq-pct-warn { color:var(--dre-gold);font-weight:700; }
.dq-pct-err  { color:var(--dre-error);font-weight:700; }
/* Temporal chart */
.dq-chart-wrap { overflow-x:auto; }
.dq-year-chart { display:flex;align-items:flex-end;gap:3px;height:80px;
  padding:0 4px 0;border-bottom:1px solid var(--dre-border);min-width:400px; }
.dq-year-bar   { display:flex;flex-direction:column;align-items:center;flex:1;min-width:8px;cursor:default; }
.dq-year-fill  { width:100%;background:var(--dre-teal);border-radius:2px 2px 0 0;
  opacity:0.75;transition:opacity .15s;min-height:2px; }
.dq-year-fill:hover { opacity:1; }
.dq-year-labels { display:flex;gap:3px;padding:3px 4px 0;min-width:400px; }
.dq-year-lbl   { flex:1;text-align:center;font-size:0.6rem;color:var(--dre-muted);
  min-width:8px;overflow:hidden;white-space:nowrap; }
.dq-chart-title { font-size:0.82rem;color:var(--dre-muted);margin-bottom:6px; }
.dq-charts-grid { display:grid;grid-template-columns:repeat(auto-fill,minmax(280px,1fr));gap:16px; }
.dq-chart-card  { background:var(--dre-surface);border:1px solid var(--dre-border);
  border-radius:8px;padding:14px 16px; }
.dq-chart-card-title { font-weight:600;font-size:0.88rem;margin-bottom:10px;color:var(--dre-text); }
/* Progress spinner */
.dq-spinner { display:flex;align-items:center;gap:12px;color:var(--dre-muted);
  padding:32px 0;font-size:0.95rem; }

'



# ── 11. UI ────────────────────────────────────────────────────────────────────

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title      = span(style = "color:#00C5B5;font-weight:700;letter-spacing:-0.5px;",
                      "ARIDHIA  ",
                      span(style = "color:#8BA3B5;font-weight:400;font-size:0.85rem;",
                           "SQL Workbench")),
    titleWidth = 300
  ),

  dashboardSidebar(
    width = 300,
    tags$head(
      tags$style(APP_CSS),
      tags$script(HTML('
        Shiny.addCustomMessageHandler("toggle_el", function(msg) {
          var el = document.getElementById(msg.id);
          if (el) el.style.display = msg.show ? "" : "none";
        });
        Shiny.addCustomMessageHandler("clear_conditions", function(msg) {
          var c = document.getElementById("conditions_container");
          if (!c) return;
          // Remove all cond_row_N divs
          var rows = c.querySelectorAll("[id^=cond_row_]");
          rows.forEach(function(r){ r.remove(); });
          // Show the empty message
          var msg_el = document.getElementById("no_conditions_msg");
          if (msg_el) msg_el.style.display = "";
        });
      ')),
      tags$style(HTML("
        /* Dark background on the entire filter row cell - no child element can show white */
        thead tr.filters th {
          background:#0B2341!important;
          overflow:hidden!important;
        }
        /* Hide every non-input child DT puts in filter cells */
        thead tr.filters th > *:not(input) { display:none!important; visibility:hidden!important; }
        /* Force all inputs to be readable */
        thead tr.filters th input {
          display:block!important;
          position:relative!important;
          z-index:10!important;
          background:#0B2341!important;
          color:#E8F4F8!important;
          -webkit-text-fill-color:#E8F4F8!important;
          caret-color:#00C5B5!important;
          border:1px solid rgba(0,197,181,0.25)!important;
          border-radius:6px!important;
          padding:5px 10px!important;
          font-size:0.93rem!important;
          width:100%!important;
          box-sizing:border-box!important;
          outline:none!important;
          margin:0!important;
        }
        thead tr.filters th input:focus {
          border-color:#00C5B5!important;
          box-shadow:0 0 0 2px rgba(0,197,181,0.15)!important;
        }
        thead tr.filters th input::placeholder { color:#8BA3B5!important; opacity:1!important; }
      "))
    ),
    tags$div(
      style = "padding:16px;",
      uiOutput("conn_status_ui"),
      div(class = "sb-label", "Schema"),
      uiOutput("schema_select_ui"),
      uiOutput("omop_badge_ui"),
      div(class = "sb-label", "Tables"),
      uiOutput("table_list_ui"),
      uiOutput("col_info_ui"),
      uiOutput("col_preview_ui"),
      uiOutput("schema_links_sidebar_ui"),
      tags$div(style = "margin-top:16px;",
               actionButton("refresh_db", "↻  Refresh", class = "btn-dre-sec",
                            style = "width:100%;font-size:0.82rem;"))
    )
  ),

  dashboardBody(uiOutput("main_body_ui"))
)


# ── 12. SERVER ────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Reactive state ──────────────────────────────────────────────────────────
  selected_schema  <- reactiveVal(NULL)
  selected_table   <- reactiveVal(NULL)
  schemas_rv       <- reactiveVal(character(0))
  col_data         <- reactiveVal(NULL)
  schema_links_rv  <- reactiveVal(NULL)
  query_result     <- reactiveVal(NULL)
  query_error      <- reactiveVal(NULL)
  query_time_ms    <- reactiveVal(NULL)
  last_run_sql     <- reactiveVal(NULL)   # verbatim SQL of the last successful run, used as view body
  cond_ids         <- reactiveVal(integer(0))
  cond_ctr         <- reactiveVal(0L)
  join_ids         <- reactiveVal(integer(0))
  join_ctr         <- reactiveVal(0L)
  sug_filter       <- reactiveVal("all")
  save_status      <- reactiveVal(NULL)   # NULL | "success" | "error"
  save_message     <- reactiveVal("")
  omop_rv          <- reactiveVal(NULL)   # NULL | list(is_omop, hit_count, cdm_info, schema)
  concept_resolve  <- reactiveVal(TRUE)   # user toggle: resolve concept IDs?
  resolved_result  <- reactiveVal(NULL)   # concept-resolved version of query_result
  athena_fetching  <- reactiveVal(FALSE)  # TRUE while Athena HTTP calls are in flight
  athena_extra     <- reactiveVal(NULL)   # named char vec: additional id->name from Athena
  query_history_rv <- reactiveVal(list()) # list of history entries, newest first
  col_preview_rv   <- reactiveVal(NULL)   # preview data for clicked column
  col_preview_col  <- reactiveVal(NULL)   # currently previewed column name
  nc_concept_cond  <- reactiveVal(NULL)   # which cond_row_N is waiting for concept search
  type_check_rv    <- reactiveVal(NULL)   # results of CDM type check: list of table results
  type_check_filter <- reactiveVal("all") # filter tab: "all"|"issues"|"missing"|"wrong"
  dq_rv            <- reactiveVal(NULL)   # DQ dashboard results


  # ── Connection badge ────────────────────────────────────────────────────────
  output$conn_status_ui <- renderUI({
    if (conn_valid())
      div(class = "conn-badge conn-ok",
          span(class = "conn-dot"), "Connected - workspace database")
    else
      div(class = "conn-badge conn-fail",
          span(class = "conn-dot"), "No database connection")
  })


  # ── Schema loading ──────────────────────────────────────────────────────────
  load_schemas <- function() {
    if (!conn_valid()) { schemas_rv(character(0)); return() }
    s <- get_schemas(); schemas_rv(s)
    if (length(s) > 0 && is.null(selected_schema())) selected_schema(s[1])
  }

  observe({ load_schemas() })

  observeEvent(input$refresh_db, {
    load_schemas()
    selected_table(NULL); col_data(NULL); schema_links_rv(NULL)
    query_result(NULL);   query_error(NULL); last_run_sql(NULL)
    col_preview_rv(NULL); col_preview_col(NULL)
    join_ids(integer(0)); join_ctr(0L)
    cond_ids(integer(0)); cond_ctr(0L)
      session$sendCustomMessage("clear_conditions", list())
  })


  # ── Schema selector ─────────────────────────────────────────────────────────
  output$schema_select_ui <- renderUI({
    s <- schemas_rv()
    if (length(s) == 0)
      return(p(style = "color:#8BA3B5;font-size:0.82rem;", "No schemas found"))
    selectInput("schema_select", label = NULL,
                choices = s, selected = selected_schema(), width = "100%")
  })

  observeEvent(input$schema_select, {
    req(input$schema_select)
    selected_schema(input$schema_select)
    selected_table(NULL)
    col_data(NULL)
    schema_links_rv(NULL)
    query_result(NULL)
    query_error(NULL)
    resolved_result(NULL)
    last_run_sql(NULL)
    athena_extra(NULL)
    join_ids(integer(0)); join_ctr(0L)
    cond_ids(integer(0)); cond_ctr(0L)
      session$sendCustomMessage("clear_conditions", list())
  }, ignoreNULL = TRUE, ignoreInit = FALSE)


  # ── Schema link analysis - fires whenever schema changes ───────────────────
  observe({
    sch <- selected_schema()
    req(sch, conn_valid())
    withProgress(message = "Analysing schema links...", value = 0.4, {
      links <- tryCatch(analyse_schema_links(sch), error = function(e) NULL)
      incProgress(0.6)
      schema_links_rv(links)
    })
  })


  # ── OMOP CDM detection - fires after schema links ───────────────────────────
  observe({
    sch <- selected_schema()
    req(sch, conn_valid())
    result <- tryCatch(detect_omop(sch), error = function(e) NULL)
    if (!is.null(result)) {
      result$schema <- sch
      omop_rv(result)
    } else {
      omop_rv(NULL)
    }
    # Reset concept-resolved result when schema changes
    resolved_result(NULL)
  })


  # ── OMOP badge sidebar renderer ─────────────────────────────────────────────
  output$omop_badge_ui <- renderUI({
    omop <- omop_rv()
    if (is.null(omop) || !omop$is_omop) return(NULL)

    cdm_ver <- NULL
    if (!is.null(omop$cdm_info) && nrow(omop$cdm_info) > 0) {
      if ("cdm_version" %in% names(omop$cdm_info))
        cdm_ver <- omop$cdm_info$cdm_version[1]
    }

    div(class = "omop-badge",
        div(class = "omop-badge-header",
            span(class = "omop-dna", "🧬"),
            span(class = "omop-badge-title", "OMOP CDM Detected"),
            if (!is.null(cdm_ver))
              span(class = "omop-badge-ver", paste0("v", cdm_ver))),
        div(class = "omop-badge-detail",
            sprintf("%d CDM tables found · concept names auto-resolved",
                    omop$hit_count)))
  })


  # ── Table list ──────────────────────────────────────────────────────────────
  tables_rv <- reactive({
    req(selected_schema(), conn_valid())
    get_tables(selected_schema())
  })

  output$table_list_ui <- renderUI({
    tbls  <- tables_rv()
    sel   <- selected_table()
    links <- schema_links_rv()
    if (length(tbls) == 0)
      return(p(style = "color:#8BA3B5;font-size:0.82rem;padding:6px 0;",
               "No tables in this schema"))
    div(class = "tbl-list",
        lapply(tbls, function(t) {
          has_links <- !is.null(links) && nrow(links$pairs) > 0 &&
            (any(links$pairs$table_a == t) || any(links$pairs$table_b == t))
          div(
            class   = paste("tbl-item", if (!is.null(sel) && t == sel) "active" else ""),
            onclick = sprintf(
              "Shiny.setInputValue('select_table','%s',{priority:'event'})", t),
            span(class = "tbl-icon", if (has_links) "🔗" else "▤"), t
          )
        })
    )
  })

  observeEvent(input$select_table, {
    tbl <- input$select_table; sch <- selected_schema()
    selected_table(tbl)
    if (!is.null(sch) && !is.null(tbl)) {
      cols <- get_columns(sch, tbl); col_data(cols)
      cond_ids(integer(0)); cond_ctr(0L)
      session$sendCustomMessage("clear_conditions", list())
      join_ids(integer(0)); join_ctr(0L)
      updateTextAreaInput(session, "sql_input",
        value = paste0("SELECT *\nFROM ", fq_tbl(sch, tbl), "\nLIMIT 100;"))
    }
  }, ignoreNULL = TRUE)


  # ── Sidebar: common fields panel (collapsible, collapsed by default) ────────
  output$schema_links_sidebar_ui <- renderUI({
    links <- schema_links_rv()
    if (is.null(links) || length(links$common) == 0) return(NULL)
    n <- length(links$common)
    div(class = "links-box",
        # Clickable header toggles body visibility
        div(class = "links-title links-toggle",
            onclick = "
              var body = this.nextElementSibling;
              var arrow = this.querySelector('.links-arrow');
              var open = body.style.display !== 'none';
              body.style.display = open ? 'none' : 'block';
              arrow.textContent  = open ? '▸' : '▾';
            ",
            span(class = "links-arrow", "▸"),   # ▸ = collapsed, ▾ = open
            sprintf(" 🔗 %d Common Field%s", n, if (n == 1) "" else "s")
        ),
        # Body hidden by default
        div(class = "links-body", style = "display:none;",
            lapply(links$common, function(fld) {
              tbls <- links$field_map[[fld]]
              div(class = "link-field",
                  div(class = "link-field-header",
                      span(class = "link-icon", "⬡"),
                      span(class = "link-fname", fld)),
                  div(class = "link-tbls",
                      paste(tbls, collapse = ", ")))
            })
        )
    )
  })


  # ── Sidebar: column list for selected table ─────────────────────────────────
  output$col_info_ui <- renderUI({
    cols    <- col_data(); tbl <- selected_table()
    links   <- schema_links_rv()
    active  <- col_preview_col()
    common  <- if (!is.null(links)) links$common else character(0)
    if (is.null(cols) || nrow(cols) == 0 || is.null(tbl)) return(NULL)
    tagList(
      div(class = "sb-label", paste0(tbl, " - ", nrow(cols), " columns")),
      div(class = "col-list",
          lapply(seq_len(nrow(cols)), function(i) {
            cname  <- cols$column_name[i]
            is_key <- cname %in% common
            is_act <- identical(cname, active)
            div(class = paste("col-item",
                              if (is_key) "is-key" else "",
                              if (is_act) "preview-active" else ""),
                onclick = sprintf(
                  "Shiny.setInputValue('preview_col','%s',{priority:'event'})",
                  gsub("'", "\\'", cname)),
                span(class = "col-name", cname),
                if (is_key) span(class = "key-pill", "key"),
                HTML(type_pill_html(cols$data_type[i])))
          })
      )
    )
  })

  # ── Column value preview ───────────────────────────────────────────────────
  observeEvent(input$preview_col, {
    cname <- input$preview_col
    tbl   <- selected_table()
    sch   <- selected_schema()
    cols  <- col_data()
    if (is.null(tbl) || is.null(sch) || is.null(cols)) return()

    # Toggle off if same column clicked twice
    if (identical(col_preview_col(), cname)) {
      col_preview_col(NULL); col_preview_rv(NULL); return()
    }
    col_preview_col(cname); col_preview_rv(list(loading = TRUE))

    dtype_row <- cols[tolower(cols$column_name) == tolower(cname), ]
    dtype     <- if (nrow(dtype_row) > 0) dtype_row$data_type[1] else "text"
    cat_type  <- type_category(dtype, cname)
    fqt       <- fq_tbl(sch, tbl)
    qcol      <- sprintf('"%s"', cname)

    result <- tryCatch({
      if (cat_type == "numeric") {
        r <- db_query(sprintf(
          "SELECT COUNT(*) AS n_total,
                  COUNT(%s) AS n_non_null,
                  ROUND(MIN(CAST(%s AS numeric))::numeric, 4)  AS min_val,
                  ROUND(MAX(CAST(%s AS numeric))::numeric, 4)  AS max_val,
                  ROUND(AVG(CAST(%s AS numeric))::numeric, 4)  AS avg_val
           FROM %s", qcol, qcol, qcol, qcol, fqt))
        list(type = "numeric", data = r)
      } else if (cat_type == "date") {
        r <- db_query(sprintf(
          "SELECT COUNT(*) AS n_total, COUNT(%s) AS n_non_null,
                  MIN(%s)::text AS min_val, MAX(%s)::text AS max_val
           FROM %s", qcol, qcol, qcol, fqt))
        list(type = "date", data = r)
      } else {
        # Low-cardinality text / boolean: top 10 distinct values by frequency
        r <- db_query(sprintf(
          "SELECT COALESCE(%s::text,'(NULL)') AS value,
                  COUNT(*) AS n
           FROM %s
           GROUP BY 1 ORDER BY 2 DESC LIMIT 10", qcol, fqt))
        total <- db_query(sprintf("SELECT COUNT(*) AS n FROM %s", fqt))
        list(type = "values", data = r,
             total = if (!is.null(total)) total$n[1] else NA)
      }
    }, error = function(e) list(type = "error", msg = conditionMessage(e)))

    col_preview_rv(result)
  })

  output$col_preview_ui <- renderUI({
    pv  <- col_preview_rv()
    col <- col_preview_col()
    if (is.null(pv) || is.null(col)) return(NULL)

    inner <- if (!is.null(pv$loading) && pv$loading) {
      p(style = "color:var(--dre-muted);font-size:0.83rem;", "Loading...")
    } else if (pv$type == "error") {
      p(style = "color:var(--dre-error);font-size:0.83rem;", pv$msg)
    } else if (pv$type == "numeric") {
      d <- pv$data
      nulls <- if (!is.na(d$n_total[1]) && !is.na(d$n_non_null[1]))
        d$n_total[1] - d$n_non_null[1] else NA
      tags$table(class = "col-preview-table",
        tags$tr(tags$td("min"),  tags$td(as.character(d$min_val[1]))),
        tags$tr(tags$td("max"),  tags$td(as.character(d$max_val[1]))),
        tags$tr(tags$td("avg"),  tags$td(as.character(d$avg_val[1]))),
        tags$tr(tags$td("rows"), tags$td(format(d$n_total[1], big.mark=","))),
        if (!is.na(nulls))
          tags$tr(tags$td("nulls"), tags$td(format(nulls, big.mark=",")))
      )
    } else if (pv$type == "date") {
      d <- pv$data
      tags$table(class = "col-preview-table",
        tags$tr(tags$td("earliest"), tags$td(as.character(d$min_val[1]))),
        tags$tr(tags$td("latest"),   tags$td(as.character(d$max_val[1]))),
        tags$tr(tags$td("rows"),     tags$td(format(d$n_total[1], big.mark=","))),
        tags$tr(tags$td("nulls"),    tags$td(format(d$n_total[1] - d$n_non_null[1], big.mark=",")))
      )
    } else if (pv$type == "values") {
      d <- pv$data
      if (is.null(d) || nrow(d) == 0)
        return(div(class = "col-preview-wrap",
                   div(class = "col-preview-title", col),
                   p(style = "color:var(--dre-muted);font-size:0.82rem;", "No data")))
      tags$table(class = "col-preview-table",
        lapply(seq_len(nrow(d)), function(i)
          tags$tr(
            tags$td(as.character(d$value[i])),
            tags$td(format(d$n[i], big.mark=","))))
      )
    }

    div(class = "col-preview-wrap",
        div(class = "col-preview-title", paste0("▸ ", col)),
        inner)
  })


  # ── Main body ───────────────────────────────────────────────────────────────
  output$main_body_ui <- renderUI({
    if (!conn_valid())
      return(fluidRow(column(12,
        div(class = "empty-state",
            div(class = "empty-icon", "🔌"),
            h4("No Database Connection"),
            p(style = "color:#8BA3B5;max-width:420px;margin:0 auto;",
              "This app requires xaputils, pre-installed in DRE Workspace R environments.")))))

    fluidRow(column(12,
      tabsetPanel(id = "main_tabs",

        tabPanel("SQL Editor", value = "editor",
          br(),
          div(class = "sql-editor-wrap",
              textAreaInput("sql_input", label = NULL, width = "100%", rows = 10,
                value = '-- Write your SQL query here\n-- Example: SELECT * FROM "public"."table" LIMIT 100;')),
          div(style = "display:flex;gap:12px;align-items:center;margin-bottom:20px;flex-wrap:wrap;",
              actionButton("run_query",   "▶  Run Query", class = "btn-run"),
              actionButton("clear_query", "✕  Clear",     class = "btn-dre-sec"),
              uiOutput("table_context_ui")),
          uiOutput("query_results_ui")),

        tabPanel("No-Code Builder",  value = "nocode",      br(), uiOutput("nocode_ui")),
        tabPanel("Query Suggestions",value = "suggestions",  br(), uiOutput("suggestions_ui")),
        tabPanel("Schema Explorer",  value = "explorer",     br(), uiOutput("explorer_ui")),
        tabPanel("OMOP Tools",       value = "omop_tools",   br(), uiOutput("omop_tools_ui")),
        tabPanel("DQ Dashboard",     value = "dq_dashboard", br(), uiOutput("dq_dashboard_ui")),
        tabPanel("History",           value = "history",      br(), uiOutput("history_ui")),

        tabPanel("Help", value = "help",
          fluidRow(column(8, offset = 2, br(),
            h3("SQL Workbench - User Guide"), hr(),
            h4("Schema Link Analysis"),
            p("When you select a schema, the app queries all tables for overlapping column names. Columns shared by two or more tables become join keys and are highlighted in gold throughout the app. Tables that participate in at least one join show a 🔗 icon in the sidebar."),
            h4("SQL Editor"),
            p("Write any PostgreSQL query and click Run Query. Selecting a table pre-populates a SELECT * LIMIT 100. Export CSV downloads the full result set. Save to Database writes the result set as a new database table."),
            h4("Save to Database"),
            tags$dl(
              tags$dt("Target schema"),
              tags$dd("Choose any existing schema, or select \"Create new schema\" to name a new one. New schemas are created automatically before the table is written."),
              tags$dt("Table name"),
              tags$dd("Must start with a letter or underscore and contain only letters, numbers and underscores."),
              tags$dt("Overwrite warning"),
              tags$dd("If a table with the same name already exists in the chosen schema, an amber warning appears and the confirm button changes to \"Overwrite Table\". The existing table and all its data will be replaced.")
            ),
            h4("No-Code Builder"),
            tags$dl(
              tags$dt("Primary table"),
              tags$dd("Set by clicking any table in the sidebar."),
              tags$dt("JOIN section"),
              tags$dd("Click + Add Join to add a linked table. The join key is auto-detected from the schema analysis. Choose INNER, LEFT, RIGHT, or FULL OUTER join type. Once a join is added, columns from the joined table appear in SELECT, WHERE, GROUP BY and ORDER BY as t2.column_name."),
              tags$dt("SELECT"),
              tags$dd("Tick columns to return. Joined table columns show with their alias prefix (t2.col, t3.col)."),
              tags$dt("WHERE / GROUP BY / ORDER BY / LIMIT"),
              tags$dd("Filter, aggregate and sort across all joined tables.")
            ),
            h4("Query Suggestions"),
            p("Use the filter tabs to view single-table or cross-table suggestions separately. Cross-table suggestions include INNER JOIN previews, LEFT anti-join unmatched detection, key coverage (FULL OUTER JOIN), aggregate summaries, categorical breakdowns using joined fields, date trends through joined tables, and value-overlap queries that reveal which key values appear in multiple tables."),
            h4("Schema Explorer"),
            p("A join map at the top of each schema summarises all detected table relationships. Common key fields are highlighted in gold with a 'key' badge on every column where they appear."),
            hr(),
            p(tags$em("Aridhia SQL Workbench v2 · aridhia.com · knowledgebase.aridhia.io"))
          ))
        )
      )
    ))
  })


  # ── Table context badge ─────────────────────────────────────────────────────
  output$table_context_ui <- renderUI({
    tbl <- selected_table(); sch <- selected_schema()
    if (is.null(tbl)) return(NULL)
    div(style = "font-size:0.8rem;color:#8BA3B5;padding:6px 12px;
                 background:var(--dre-mid);border-radius:6px;border:1px solid var(--dre-border);",
        span(style = "color:#00C5B5;", "Active: "),
        sprintf('"%s"."%s"', sch, tbl))
  })


  # ── Run / clear query ───────────────────────────────────────────────────────
  observeEvent(input$run_query, {
    sql_raw <- input$sql_input %||% ""        # verbatim, for view definition
    sql     <- trimws(sql_raw)                # trimmed, for execution
    if (!nzchar(sql) || grepl("^--", sql)) return()
    query_result(NULL); query_error(NULL); resolved_result(NULL); last_run_sql(NULL)
    start  <- proc.time()["elapsed"]
    result <- tryCatch(
      { list(data = db_query(sql), err = NULL) },
      error = function(e) list(data = NULL, err = conditionMessage(e)))
    query_result(result$data); query_error(result$err)
    if (is.null(result$err)) last_run_sql(sql_raw)
    elapsed <- round((proc.time()["elapsed"] - start) * 1000)
    query_time_ms(elapsed)

    # ── Push to query history (keep last 30, newest first) ──
    entry <- list(
      sql       = sql,
      ts        = format(Sys.time(), "%H:%M:%S"),
      rows      = if (!is.null(result$data)) nrow(result$data) else NA_integer_,
      ms        = elapsed,
      is_error  = !is.null(result$err),
      err_short = if (!is.null(result$err))
        substr(gsub("\n", " ", result$err), 1, 120) else NULL
    )
    hist <- c(list(entry), query_history_rv())
    if (length(hist) > 30) hist <- hist[seq_len(30)]
    query_history_rv(hist)

    # Concept ID resolution (OMOP schemas only)
    df   <- result$data
    omop <- omop_rv()
    athena_extra(NULL)   # reset Athena supplement on every new query
    if (!is.null(df) && !is.null(omop) && omop$is_omop && isTRUE(isolate(concept_resolve()))) {
      res <- tryCatch(
        resolve_concept_ids(df, omop$schema),
        error = function(e) list(df = df, resolved = character(0)))
      resolved_result(res)
    } else {
      resolved_result(NULL)
    }
  })


  # Re-resolve if toggle is flipped while results are displayed
  observeEvent(input$toggle_resolve, {
    concept_resolve(!concept_resolve())
    df   <- query_result()
    omop <- omop_rv()
    if (!is.null(df) && !is.null(omop) && omop$is_omop && isTRUE(concept_resolve())) {
      res <- tryCatch(
        resolve_concept_ids(df, omop$schema),
        error = function(e) list(df = df, resolved = character(0)))
      resolved_result(res)
    } else {
      resolved_result(NULL)
    }
  })


  # Athena API fallback - triggered by "Fetch from Athena" button in banner
  observeEvent(input$fetch_athena, {
    df   <- query_result(); req(df)
    omop <- omop_rv();      req(omop, omop$is_omop)
    res  <- resolved_result()

    # Identify the concept_id columns and which IDs are unmatched
    cid_cols <- if (!is.null(res) && length(res$cols_found) > 0)
      res$cols_found
    else {
      all_cols <- names(df)
      all_cols[grepl("concept_id(_\\d+)?$", all_cols, ignore.case = TRUE)]
    }
    if (length(cid_cols) == 0) return()

    # Build the local lookup map that was already applied (if any)
    local_map <- if (!is.null(res) && !is.null(res$df)) {
      # Infer from what already resolved - just use an empty map so unmatched_ids
      # picks up everything that has NA in resolved name columns.
      setNames(character(0), character(0))
    } else setNames(character(0), character(0))

    # Collect IDs that are still NA in the current resolved df
    current_df <- if (!is.null(res)) res$df else df
    miss_ids <- unique(unlist(lapply(cid_cols, function(col) {
      name_col <- sub("(?i)concept_id(_\\d+)?$",
                      paste0("concept_name",
                             sub(".*concept_id(_\\d+)?$", "\\1", col,
                                 ignore.case = TRUE, perl = TRUE)),
                      col, perl = TRUE, ignore.case = TRUE)
      id_vals   <- current_df[[col]]
      name_vals <- if (name_col %in% names(current_df)) current_df[[name_col]] else rep(NA, nrow(current_df))
      as.character(id_vals[!is.na(id_vals) &
                           suppressWarnings(as.integer(id_vals)) != 0L &
                           is.na(name_vals)])
    })))
    miss_ids <- miss_ids[!is.na(miss_ids) & nchar(miss_ids) > 0]

    if (length(miss_ids) == 0) {
      showNotification("All concept IDs already resolved - nothing to fetch.", type = "message", duration = 4)
      return()
    }

    n_fetch <- min(length(miss_ids), ATHENA_MAX)
    athena_fetching(TRUE)

    # ── Step 1: connectivity probe ──────────────────────────────────────────
    showNotification("Checking Athena connectivity...",
                     id = "athena_ping_notif", duration = NULL, type = "message")
    ping <- athena_ping()
    removeNotification("athena_ping_notif")

    if (!ping$ok) {
      athena_fetching(FALSE)
      err_detail <- if (!is.null(ping$error))
        ping$error
      else
        sprintf("HTTP %s", if (is.na(ping$status)) "no response" else ping$status)
      showNotification(
        tagList(
          tags$strong("Athena unreachable"),
          tags$br(),
          tags$span(style = "font-size:0.9rem;",
            "The workspace may not have outbound internet access to athena.ohdsi.org."),
          tags$br(),
          tags$span(style = "font-size:0.85rem;color:#ff8080;", err_detail)
        ),
        type = "error", duration = 10)
      return()
    }

    showNotification(
      tagList(
        tags$span(style = "color:#00E5D2;", "\u2713 Athena reachable"),
        tags$span(style = "color:#8BA3B5;font-size:0.85rem;",
                  sprintf(" (%dms) - fetching %d concept name%s...",
                          ping$ms, n_fetch, if (n_fetch == 1) "" else "s"))
      ),
      id = "athena_notif", duration = NULL, type = "message")

    result_map <- tryCatch({
      athena_lookup(miss_ids[seq_len(n_fetch)])
    }, error = function(e) {
      showNotification(paste("Athena lookup failed:", conditionMessage(e)),
                       type = "error", duration = 8)
      NULL
    })

    removeNotification("athena_notif")
    athena_fetching(FALSE)

    if (is.null(result_map)) return()

    # Filter to successfully fetched names only
    good <- result_map[!is.na(result_map)]
    if (length(good) == 0) {
      showNotification("Athena returned no matches for these concept IDs.", type = "warning", duration = 5)
      return()
    }

    # Merge into existing supplement map
    prev <- athena_extra()
    merged <- c(if (!is.null(prev)) prev else character(0), good)
    merged <- merged[!duplicated(names(merged))]
    athena_extra(merged)

    # Now re-apply resolution with the supplement baked in
    current_df2 <- if (!is.null(res)) res$df else df
    for (col in rev(cid_cols)) {
      name_col <- sub("(?i)concept_id(_\\d+)?$",
                      paste0("concept_name",
                             sub(".*concept_id(_\\d+)?$", "\\1", col,
                                 ignore.case = TRUE, perl = TRUE)),
                      col, perl = TRUE, ignore.case = TRUE)
      if (!(name_col %in% names(current_df2))) next
      id_vals   <- as.character(current_df2[[col]])
      still_na  <- is.na(current_df2[[name_col]])
      athena_hits <- merged[id_vals]
      current_df2[[name_col]] <- ifelse(still_na & !is.na(athena_hits),
                                        athena_hits,
                                        current_df2[[name_col]])
    }

    # Update resolved_result with Athena-enriched df
    new_res <- res
    new_res$df            <- current_df2
    new_res$athena_count  <- sum(!is.na(good))
    new_res$warn          <- NULL   # clear partial warning
    resolved_result(new_res)

    showNotification(
      sprintf("Athena: %d concept name%s added.", length(good),
              if (length(good) == 1) "" else "s"),
      type = "message", duration = 4)
  })

  observeEvent(input$clear_query, {
    updateTextAreaInput(session, "sql_input", value = "")
    query_result(NULL); query_error(NULL); last_run_sql(NULL)
  })


  # ── Query results ───────────────────────────────────────────────────────────
  output$query_results_ui <- renderUI({
    err  <- query_error(); df <- query_result(); t <- query_time_ms()
    res  <- resolved_result()
    omop <- omop_rv()
    if (!is.null(err))
      return(div(class = "sql-error",
                 tags$strong("Query error"), br(), tags$pre(err)))
    if (is.null(df)) return(NULL)
    if (nrow(df) == 0)
      return(div(class = "empty-state",
                 div(class = "empty-icon", "🔎"),
                 h4("Query returned no rows"),
                 p("The query executed successfully but matched no data. Check your WHERE conditions or concept IDs.")))

    display_df <- if (!is.null(res)) res$df else df
    n_resolved <- if (!is.null(res)) length(res$resolved) else 0

    tagList(
      div(class = "result-stats",
          div(class = "rs-card",
              div(class = "rs-num", format(nrow(df), big.mark = ",")),
              div(class = "rs-lbl", "Rows returned")),
          div(class = "rs-card",
              div(class = "rs-num", ncol(display_df)),
              div(class = "rs-lbl", "Columns")),
          div(class = "rs-card",
              div(class = "rs-num", paste0(t, "ms")),
              div(class = "rs-lbl", "Query time"))),

      # OMOP concept resolution banner
      if (!is.null(omop) && omop$is_omop) {
        warn_msg   <- if (!is.null(res)) res$warn else NULL
        cols_found <- if (!is.null(res) && !is.null(res$cols_found)) length(res$cols_found) else 0

        # Partial = resolved some cols but warn is set (match rate < 100%)
        is_partial  <- n_resolved > 0 && !is.null(warn_msg)
        is_warn     <- !is.null(warn_msg) && n_resolved == 0

        bar_class <- if (is_partial)    "omop-resolve-bar omop-resolve-partial"
                     else if (is_warn)  "omop-resolve-bar omop-resolve-warn"
                     else               "omop-resolve-bar"

        athena_cnt  <- if (!is.null(res)) res$athena_count %||% 0L else 0L
        is_fetching <- isTRUE(athena_fetching())

        banner_text <-
          if (!concept_resolve())
            "OMOP schema - concept ID resolution off"
          else if (is_partial)
            paste0(sprintf("OMOP: %d concept ID column%s resolved",
                           n_resolved, if (n_resolved == 1) "" else "s"),
                   " - ", warn_msg)
          else if (n_resolved > 0 && athena_cnt > 0)
            sprintf("OMOP: %d concept ID column%s resolved (%d name%s from Athena)",
                    n_resolved, if (n_resolved == 1) "" else "s",
                    athena_cnt, if (athena_cnt == 1) "" else "s")
          else if (n_resolved > 0)
            sprintf("OMOP: %d concept ID column%s resolved to names",
                    n_resolved, if (n_resolved == 1) "" else "s")
          else if (is_warn)
            warn_msg
          else if (cols_found > 0)
            sprintf("%d concept ID column%s detected - no matching concepts found",
                    cols_found, if (cols_found == 1) "" else "s")
          else
            "OMOP schema - no concept ID columns in this result"

        # Show "Fetch from Athena" button only when partial and resolution is on
        show_athena_btn <- concept_resolve() && is_partial && !is_fetching

        div(class = bar_class,
            span(class = "omop-resolve-icon", if (is_fetching) "⏳" else "🧬"),
            div(class = "omop-resolve-text", banner_text),
            if (show_athena_btn)
              actionButton("fetch_athena",
                           "Fetch missing from Athena",
                           class = "btn-athena-fetch"),
            actionButton("toggle_resolve",
                         if (concept_resolve()) "Turn off" else "Turn on",
                         class = "btn-omop-toggle"))
      },

      div(style = "display:flex;gap:10px;margin-bottom:12px;flex-wrap:wrap;",
          downloadButton("download_csv", "⬇  Export CSV", class = "btn-dre-sec"),
          actionButton("open_save_modal", "💾  Save to Database", class = "btn-dre-sec"),
          actionButton("open_view_modal", "👁  Create View",      class = "btn-dre-sec")),
      uiOutput("save_feedback_ui"),
      DTOutput("results_dt")
    )
  })

  output$results_dt <- renderDT({
    df  <- query_result(); req(df)
    res <- resolved_result()
    display_df    <- if (!is.null(res)) res$df else df
    resolved_cols <- if (!is.null(res)) res$resolved else character(0)

    # Build column names list - inject ◆ prefix into derived column headers
    col_names <- names(display_df)
    if (length(resolved_cols) > 0) {
      col_names <- ifelse(col_names %in% resolved_cols,
                          paste0("\u25c6 ", col_names),
                          col_names)
    }

    # JS: after table draws, style headers of derived columns with teal background
    resolved_indices_js <- if (length(resolved_cols) > 0) {
      # 0-based indices of resolved columns for JS
      idx0 <- which(names(display_df) %in% resolved_cols) - 1L
      paste0("[", paste(idx0, collapse = ","), "]")
    } else "[]"

    dt <- datatable(display_df, rownames = FALSE,
              colnames = col_names,
              filter   = "none",
              options  = list(
                pageLength = 25,
                scrollX    = TRUE,
                dom        = "lfrtip",
                drawCallback = JS(paste0(
                  "function(settings) {",
                  "  var api = new $.fn.dataTable.Api(settings);",
                  "  var derived = ", resolved_indices_js, ";",
                  "  derived.forEach(function(i) {",
                  "    $(api.column(i).header())",
                  "      .css({'background':'rgba(0,197,181,0.18)',",
                  "            'color':'#00E5D2','font-style':'italic',",
                  "            'border-bottom':'2px solid rgba(0,197,181,0.5)'});",
                  "  });",
                  "}"
                ))
              )) %>%
      formatStyle(seq_along(display_df), backgroundColor = "#0D2A4A", color = "#E8F4F8")

    if (length(resolved_cols) > 0) {
      idx <- which(names(display_df) %in% resolved_cols)
      dt  <- dt %>%
        formatStyle(idx,
                    backgroundColor = "rgba(0,197,181,0.06)",
                    color           = "#cffaf6",
                    fontStyle       = "italic",
                    borderLeft      = "3px solid rgba(0,197,181,0.45)")
    }
    dt
  })


  output$download_csv <- downloadHandler(
    filename = function()
      paste0("query_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      df <- query_result()
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )

  # ── Save feedback banner (shown below Export/Save buttons) ──────────────────
  output$save_feedback_ui <- renderUI({
    st  <- save_status()
    msg <- save_message()
    if (is.null(st)) return(NULL)
    if (st == "success")
      div(class = "save-ok",
          span(class = "save-ok-icon", "✓"),
          span(class = "save-ok-text", msg))
    else
      div(class = "save-err",
          div(style = "font-weight:700;margin-bottom:6px;", "⚠  Save failed"),
          tags$pre(style = "margin:0;font-size:0.88rem;color:var(--dre-error);
                            white-space:pre-wrap;word-break:break-word;
                            background:transparent;border:none;padding:0;", msg))
  })

  # ── Open Save modal ─────────────────────────────────────────────────────────
  observeEvent(input$open_save_modal, {
    df <- query_result()
    if (is.null(df)) {
      showNotification("Run a query first to get results to save.", type = "warning")
      return()
    }
    save_status(NULL); save_message("")
    schemas <- schemas_rv()

    showModal(modalDialog(
      title = tagList(span("💾  Save Results to Database Table")),
      size  = "m",
      easyClose = TRUE,

      # ── Schema picker ────────────────────────────────────────────────────────
      span(class = "save-schema-tag", "Target Schema"),
      fluidRow(
        column(8,
          selectInput("save_schema_select", label = NULL,
                      choices  = c(schemas, "── Create new schema ──" = "__new__"),
                      selected = selected_schema() %||% schemas[1],
                      width    = "100%")),
        column(4,
          uiOutput("save_schema_display"))
      ),
      uiOutput("save_new_schema_ui"),

      # ── Table name ───────────────────────────────────────────────────────────
      span(class = "save-schema-tag", "Table Name"),
      textInput("save_table_name", label = NULL,
                placeholder = "e.g. my_derived_results",
                width       = "100%"),
      uiOutput("save_exists_warning_ui"),

      # ── Row / column summary ─────────────────────────────────────────────────
      div(style = "font-size:0.8rem;color:#8BA3B5;margin-top:4px;",
          sprintf("Result set: %s rows × %d columns",
                  format(nrow(df), big.mark = ","), ncol(df))),

      footer = tagList(
        modalButton("Cancel"),
        uiOutput("save_confirm_btn_ui")
      )
    ))
  })

  # ── New schema text input (shown only when __new__ is selected) ─────────────
  output$save_new_schema_ui <- renderUI({
    req(input$save_schema_select == "__new__")
    div(style = "margin-top:8px;margin-bottom:6px;",
        span(class = "save-schema-tag",
             tagList("New Schema Name",
                     span(class = "new-schema-badge", "NEW"))),
        textInput("save_new_schema_name", label = NULL,
                  placeholder = "e.g. analysis_results",
                  width       = "100%"),
        div(style = "font-size:0.78rem;color:#8BA3B5;margin-top:2px;",
            "Schema names must be lowercase, start with a letter, and contain only letters, numbers and underscores."))
  })

  # ── Small badge showing resolved schema ─────────────────────────────────────
  output$save_schema_display <- renderUI({
    sel <- input$save_schema_select %||% ""
    if (sel == "__new__") return(NULL)
    div(style = "padding-top:6px;font-size:0.78rem;color:#8BA3B5;", sel)
  })

  # ── Existence warning ────────────────────────────────────────────────────────
  output$save_exists_warning_ui <- renderUI({
    tbl_name  <- trimws(input$save_table_name %||% "")
    sel_sch   <- input$save_schema_select %||% ""
    new_sch   <- trimws(input$save_new_schema_name %||% "")
    target_sch <- if (sel_sch == "__new__") new_sch else sel_sch

    if (!nzchar(tbl_name) || !nzchar(target_sch) || sel_sch == "__new__")
      return(NULL)

    existing <- tryCatch(get_tables(target_sch), error = function(e) character(0))
    if (tbl_name %in% existing)
      div(class = "save-warn",
          span(class = "save-warn-icon", "⚠️"),
          div(class = "save-warn-text",
              tags$strong(sprintf('"%s"."%s"', target_sch, tbl_name)),
              " already exists. Saving will overwrite the existing table and all its data."))
    else
      NULL
  })

  # ── Confirm button label changes when overwriting ────────────────────────────
  output$save_confirm_btn_ui <- renderUI({
    tbl_name  <- trimws(input$save_table_name %||% "")
    sel_sch   <- input$save_schema_select %||% ""
    new_sch   <- trimws(input$save_new_schema_name %||% "")
    target_sch <- if (sel_sch == "__new__") new_sch else sel_sch

    existing <- if (nzchar(target_sch) && sel_sch != "__new__")
      tryCatch(get_tables(target_sch), error = function(e) character(0))
    else character(0)

    is_overwrite <- nzchar(tbl_name) && tbl_name %in% existing

    if (is_overwrite)
      actionButton("confirm_save", "⚠️  Overwrite Table",
                   class = "btn-save-overwrite")
    else
      actionButton("confirm_save", "💾  Save Table",
                   class = "btn-save-confirm")
  })

  # ── Execute save ─────────────────────────────────────────────────────────────
  observeEvent(input$confirm_save, {
    df        <- query_result()
    tbl_name  <- trimws(input$save_table_name %||% "")
    sel_sch   <- input$save_schema_select %||% ""
    new_sch   <- trimws(input$save_new_schema_name %||% "")
    target_sch <- if (sel_sch == "__new__") new_sch else sel_sch

    # Validation
    if (is.null(df)) {
      showNotification("No result set to save.", type = "error"); return()
    }
    if (!nzchar(tbl_name)) {
      showNotification("Please enter a table name.", type = "warning"); return()
    }
    if (!grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", tbl_name)) {
      showNotification(
        "Table name must start with a letter or underscore and contain only letters, numbers and underscores.",
        type = "warning"); return()
    }
    if (!nzchar(target_sch)) {
      showNotification("Please enter a schema name.", type = "warning"); return()
    }
    if (sel_sch == "__new__" && !grepl("^[a-z_][a-z0-9_]*$", target_sch)) {
      showNotification(
        "Schema name must be lowercase, start with a letter or underscore, and contain only letters, numbers and underscores.",
        type = "warning"); return()
    }

    # Create schema if new
    if (sel_sch == "__new__") {
      result <- tryCatch({
        dbExecute(DRE_CONN, sprintf('CREATE SCHEMA IF NOT EXISTS "%s"', target_sch))
        list(ok = TRUE, err = NULL)
      }, error = function(e) list(ok = FALSE, err = conditionMessage(e)))
      if (!result$ok) {
        save_status("error")
        save_message(paste("Could not create schema:", result$err))
        removeModal()
        return()
      }
    }

    # Write table (overwrite if exists)
    # Use the plain "schema.table" string - xap.db.writeframe and the
    # underlying RPostgres/RPostgreSQL drivers both accept this form.
    # DBI::Id is intentionally avoided: it causes the quoted identifier to be
    # stored as a literal table name containing the quote characters.
    fq          <- sprintf("%s.%s", target_sch, tbl_name)
    fq_display  <- sprintf('"%s"."%s"', target_sch, tbl_name)

    # Deduplicate column names - JOIN queries with SELECT * commonly produce
    # duplicate names (e.g. VALID_START_DATE from both tables). PostgreSQL
    # refuses to create a table with duplicate column names.
    orig_names <- names(df)
    if (anyDuplicated(orig_names)) {
      names(df) <- make.unique(tolower(orig_names), sep = "_")
      dupes_fixed <- TRUE
    } else {
      names(df) <- tolower(orig_names)   # normalise to lowercase regardless
      dupes_fixed <- FALSE
    }

    # Drop first so overwrite is clean regardless of driver behaviour
    if (tbl_name %in% tryCatch(get_tables(target_sch), error = function(e) character(0))) {
      tryCatch(
        dbExecute(DRE_CONN,
                  sprintf('DROP TABLE IF EXISTS "%s"."%s"', target_sch, tbl_name)),
        error = function(e) NULL)
    }

    result <- tryCatch({
      # xap.db.writeframe is the preferred xaputils write path
      if (exists("xap.db.writeframe", mode = "function")) {
        xap.db.writeframe(df, fq)
      } else {
        dbWriteTable(DRE_CONN, fq, df, overwrite = TRUE)
      }
      list(ok = TRUE, err = NULL)
    }, error = function(e) list(ok = FALSE, err = conditionMessage(e)))

    removeModal()

    if (result$ok) {
      save_status("success")
      note <- if (dupes_fixed)
        " (duplicate column names were renamed automatically)"
      else ""
      save_message(sprintf(
        'Saved %s rows × %d columns to %s%s',
        format(nrow(df), big.mark = ","), ncol(df), fq_display, note))
      # Refresh schema list so new schema / table appears immediately
      new_schemas <- get_schemas()
      schemas_rv(new_schemas)
    } else {
      save_status("error")
      save_message(result$err)
    }
  })


  # ── Create View: helper to detect whether a name in a schema exists, and
  #     whether it's a view or a base table. Returns "view", "table", or NA.
  get_object_kind <- function(schema, name) {
    if (!conn_valid()) return(NA_character_)
    if (!nzchar(schema) || !nzchar(name)) return(NA_character_)
    r <- tryCatch(
      db_query(sprintf(
        "SELECT table_type FROM information_schema.tables
         WHERE  table_schema = %s AND table_name = %s
         LIMIT 1",
        pg_str(schema), pg_str(name))),
      error = function(e) NULL)
    if (is.null(r) || nrow(r) == 0) return(NA_character_)
    tt <- as.character(r$table_type[1])
    if (identical(tt, "VIEW"))            return("view")
    if (identical(tt, "BASE TABLE"))      return("table")
    NA_character_
  }


  # ── Open Create View modal ──────────────────────────────────────────────────
  observeEvent(input$open_view_modal, {
    df  <- query_result()
    sql <- last_run_sql()
    if (is.null(df)) {
      showNotification("Run a query first to create a view from its definition.",
                       type = "warning"); return()
    }
    if (is.null(sql) || !nzchar(trimws(sql))) {
      showNotification("No SQL was captured from the last successful run.",
                       type = "error"); return()
    }
    save_status(NULL); save_message("")
    schemas <- schemas_rv()

    showModal(modalDialog(
      title = tagList(span("👁  Create Database View")),
      size  = "m",
      easyClose = TRUE,

      span(class = "save-schema-tag", "Target Schema"),
      fluidRow(
        column(8,
          selectInput("view_schema_select", label = NULL,
                      choices  = schemas,
                      selected = selected_schema() %||% schemas[1],
                      width    = "100%")),
        column(4,
          uiOutput("view_schema_display"))
      ),

      span(class = "save-schema-tag", "View Name"),
      textInput("view_name", label = NULL,
                placeholder = "e.g. my_cohort_view",
                width       = "100%"),
      uiOutput("view_exists_warning_ui"),

      div(style = "font-size:0.8rem;color:#8BA3B5;margin-top:4px;",
          sprintf("View body: %s characters of SQL from the last run",
                  format(nchar(sql), big.mark = ","))),

      footer = tagList(
        modalButton("Cancel"),
        uiOutput("view_confirm_btn_ui")
      )
    ))
  })

  # Small badge mirroring the resolved schema (mirrors save_schema_display)
  output$view_schema_display <- renderUI({
    sel <- input$view_schema_select %||% ""
    if (!nzchar(sel)) return(NULL)
    div(style = "padding-top:6px;font-size:0.78rem;color:#8BA3B5;", sel)
  })

  # Live existence/kind feedback inside the modal
  output$view_exists_warning_ui <- renderUI({
    nm  <- trimws(input$view_name %||% "")
    sch <- input$view_schema_select %||% ""
    if (!nzchar(nm) || !nzchar(sch)) return(NULL)

    kind <- tryCatch(get_object_kind(sch, nm), error = function(e) NA_character_)
    if (is.na(kind)) return(NULL)

    if (identical(kind, "view")) {
      div(class = "save-warn",
          span(class = "save-warn-icon", "⚠️"),
          div(class = "save-warn-text",
              tags$strong(sprintf('"%s"."%s"', sch, nm)),
              " already exists as a view. Replacing it will overwrite the existing definition."))
    } else if (identical(kind, "table")) {
      div(class = "save-err",
          div(style = "font-weight:700;margin-bottom:6px;",
              "⚠  Name conflicts with an existing table"),
          tags$pre(style = "margin:0;font-size:0.88rem;color:var(--dre-error);
                            white-space:pre-wrap;word-break:break-word;
                            background:transparent;border:none;padding:0;",
                   sprintf('A table named "%s"."%s" already exists. Choose a different view name, or drop the table first.',
                           sch, nm)))
    } else {
      NULL
    }
  })

  # Confirm button - label and class change based on existence/kind
  output$view_confirm_btn_ui <- renderUI({
    nm  <- trimws(input$view_name %||% "")
    sch <- input$view_schema_select %||% ""

    name_ok    <- nzchar(nm) && grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", nm)
    schema_ok  <- nzchar(sch)

    kind <- if (name_ok && schema_ok)
      tryCatch(get_object_kind(sch, nm), error = function(e) NA_character_)
    else NA_character_

    blocked_by_table <- identical(kind, "table")
    is_replace       <- identical(kind, "view")

    if (!name_ok || !schema_ok || blocked_by_table) {
      # Render disabled-looking button. Shiny actionButton can take disabled attr.
      tags$button(
        type     = "button",
        class    = "btn btn-default action-button btn-save-confirm",
        disabled = "disabled",
        style    = "opacity:0.5;cursor:not-allowed;",
        "👁  Create View")
    } else if (is_replace) {
      actionButton("confirm_create_view", "⚠️  Replace View",
                   class = "btn-save-overwrite")
    } else {
      actionButton("confirm_create_view", "👁  Create View",
                   class = "btn-save-confirm")
    }
  })

  # Execute Create View
  observeEvent(input$confirm_create_view, {
    sql <- last_run_sql()
    nm  <- trimws(input$view_name %||% "")
    sch <- input$view_schema_select %||% ""

    # Validation
    if (is.null(sql) || !nzchar(trimws(sql))) {
      showNotification("No SQL captured from the last run.", type = "error"); return()
    }
    if (!nzchar(nm)) {
      showNotification("Please enter a view name.", type = "warning"); return()
    }
    if (!grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", nm)) {
      showNotification(
        "View name must start with a letter or underscore and contain only letters, numbers and underscores.",
        type = "warning"); return()
    }
    if (!nzchar(sch)) {
      showNotification("Please select a target schema.", type = "warning"); return()
    }

    # Re-check existence/kind at confirm time - guards against the modal being
    # open while another action created a table with the same name.
    kind <- tryCatch(get_object_kind(sch, nm), error = function(e) NA_character_)
    if (identical(kind, "table")) {
      removeModal()
      save_status("error")
      save_message(sprintf(
        'Cannot create view: a table named "%s"."%s" already exists. Drop the table or choose a different name.',
        sch, nm))
      return()
    }
    is_replace <- identical(kind, "view")

    # Build view body - strip a single trailing semicolon and trailing whitespace
    # only. Comments and everything else stay verbatim, per requirements.
    body <- sub("[[:space:]]*;[[:space:]]*$", "", sql)
    body <- sub("[[:space:]]+$", "", body)

    fq_display <- sprintf('"%s"."%s"', sch, nm)
    or_replace <- if (is_replace) "OR REPLACE " else ""

    ddl <- sprintf('CREATE %sVIEW "%s"."%s" AS %s',
                   or_replace, sch, nm, body)

    result <- tryCatch({
      dbExecute(DRE_CONN, ddl)
      list(ok = TRUE, err = NULL)
    }, error = function(e) list(ok = FALSE, err = conditionMessage(e)))

    removeModal()

    if (result$ok) {
      save_status("success")
      verb <- if (is_replace) "Replaced view" else "Created view"
      save_message(sprintf("%s %s", verb, fq_display))
      # Refresh schema list so the new view's schema reflects current state
      new_schemas <- get_schemas()
      schemas_rv(new_schemas)
    } else {
      save_status("error")
      save_message(result$err)
    }
  })


  # ── Helper: build full column choice list across primary + joined tables ────
  build_all_col_choices <- function(primary_tbl, primary_cols, jids, links) {
    has_joins <- length(jids) > 0 && !is.null(links)
    # Only prefix labels with alias when there are joins - otherwise it's noise
    primary_labels <- if (has_joins)
      paste0("t1.", primary_cols$column_name, "  [", primary_cols$data_type, "]")
    else
      paste0(primary_cols$column_name, "  [", primary_cols$data_type, "]")
    choices <- setNames(primary_cols$column_name, primary_labels)
    if (!has_joins) return(choices)
    for (i in seq_along(jids)) {
      alias <- tbl_alias(i + 1)
      tbl2  <- input[[paste0("join_tbl_", jids[i])]]
      if (is.null(tbl2) || !nzchar(tbl2)) next
      cols2 <- links$all_cols[[tbl2]]
      if (is.null(cols2)) next
      new_ch <- setNames(
        paste0(alias, ".", cols2$column_name),
        paste0(alias, ".", cols2$column_name, "  [", cols2$data_type, "]"))
      choices <- c(choices, new_ch)
    }
    choices
  }


  # ── No-Code Builder UI ──────────────────────────────────────────────────────
  output$nocode_ui <- renderUI({
    cols  <- col_data(); tbl <- selected_table(); sch <- selected_schema()
    links <- schema_links_rv()
    jids  <- join_ids()

    if (is.null(cols) || is.null(tbl))
      return(div(class = "empty-state",
                 div(class = "empty-icon", "🔧"),
                 h4("Select a table"),
                 p(style = "color:#8BA3B5;",
                   "Choose a table in the sidebar to open the no-code builder.")))

    targets        <- join_targets(tbl, links)
    join_tbl_opts  <- names(targets)
    all_choices    <- build_all_col_choices(tbl, cols, jids, links)
    order_choices  <- c("None" = "none", all_choices)
    select_choices <- c("* (all columns)" = "*", all_choices)

    tagList(
      div(class = "sql-info",
          tags$strong(sprintf('"%s"."%s"', sch, tbl)),
          sprintf("  -  %d columns", nrow(cols)),
          if (length(targets) > 0)
            tagList(
              span(style = "margin-left:10px;",
                   HTML(sprintf('<span class="key-pill">🔗 %d linked table%s</span>',
                                length(targets),
                                if (length(targets) == 1) "" else "s")))
            )
      ),

      # ── JOIN ───────────────────────────────────────────────────────────────
      div(class = "nc-section nc-join-section",
          span(class = "nc-label nc-join-label",
               "JOIN - link related tables via shared fields"),
          uiOutput("join_rows_ui"),
          if (length(join_tbl_opts) > 0)
            actionButton("add_join", "+ Add Join", class = "btn-add-join")
          else
            p(style = "color:#8BA3B5;font-size:0.85rem;margin:0;",
              "No linked tables detected in this schema.")
      ),

      # ── SELECT ─────────────────────────────────────────────────────────────
      div(class = "nc-section",
          span(class = "nc-label", "SELECT - columns to return"),
          checkboxGroupInput("nc_select_cols", label = NULL,
                             choices = select_choices, selected = "*",
                             inline = FALSE)),

      # ── WHERE ──────────────────────────────────────────────────────────────
      div(class = "nc-section",
          span(class = "nc-label", "WHERE - filter conditions (combined with AND)"),
          div(id = "conditions_container",
              p(id = "no_conditions_msg",
                style = "color:#8BA3B5;font-size:0.82rem;",
                "No conditions added. Click + Add Condition below.")),
          actionButton("add_condition", "+ Add Condition", class = "btn-add-cond")),

      # ── GROUP BY ───────────────────────────────────────────────────────────
      div(class = "nc-section",
          span(class = "nc-label", "GROUP BY"),
          selectizeInput("nc_groupby", label = NULL,
                         choices  = all_choices,
                         multiple = TRUE,
                         selected = NULL,
                         options  = list(placeholder = "Select columns to group by..."))),

      # ── ORDER BY / LIMIT ───────────────────────────────────────────────────
      div(class = "nc-section",
          span(class = "nc-label", "ORDER BY / LIMIT"),
          fluidRow(
            column(5, selectInput("nc_orderby", "Order by",
                                  choices = order_choices, selected = "none")),
            column(3, selectInput("nc_order_dir", "Direction",
                                  choices = c("ASC", "DESC"), selected = "ASC")),
            column(4, numericInput("nc_limit", "Limit rows",
                                   value = 100, min = 0, max = 10000000, step = 100))
          )),

      # ── SQL preview ────────────────────────────────────────────────────────
      div(class = "nc-section",
          span(class = "nc-label", "Generated SQL"),
          div(class = "nc-preview",
              textOutput("nc_sql_preview", inline = FALSE))),

      div(style = "display:flex;gap:10px;flex-wrap:wrap;",
        actionButton("nc_run_btn",
                     "▶  Run Query",
                     class = "btn-run"),
        actionButton("generate_sql_btn",
                     "Open in SQL Editor",
                     class = "btn-dre-sec")
      ),
      uiOutput("nc_error_ui")
    )
  })


  # ── JOIN row management ─────────────────────────────────────────────────────
  observeEvent(input$add_join, {
    n <- join_ctr() + 1L; join_ctr(n); join_ids(c(join_ids(), n))
  })

  observe({
    lapply(join_ids(), function(id) {
      local({
        lid <- id
        observeEvent(input[[paste0("rm_join_", lid)]], {
          join_ids(join_ids()[join_ids() != lid])
        }, ignoreInit = TRUE, ignoreNULL = TRUE, once = TRUE)
      })
    })
  })

  output$join_rows_ui <- renderUI({
    ids   <- join_ids(); tbl <- selected_table(); links <- schema_links_rv()
    if (length(ids) == 0)
      return(p(style = "color:#8BA3B5;font-size:0.85rem;",
               "No joins added. Click + Add Join to link a related table."))
    if (is.null(links)) return(NULL)
    targets <- join_targets(tbl, links)
    join_tbl_opts <- names(targets)

    lapply(seq_along(ids), function(i) {
      id    <- ids[i]
      alias <- tbl_alias(i + 1)
      tbl_input_id <- paste0("join_tbl_",  id)
      key_input_id <- paste0("join_key_",  id)
      typ_input_id <- paste0("join_type_", id)

      cur_tbl  <- input[[tbl_input_id]] %||% join_tbl_opts[1]
      key_opts <- if (!is.null(cur_tbl) && cur_tbl %in% names(targets))
        targets[[cur_tbl]] else links$common
      cur_key  <- input[[key_input_id]] %||% key_opts[1]

      div(class = "join-row",
          fluidRow(
            column(4, selectInput(tbl_input_id,
                                  paste0("Table (alias: ", alias, ")"),
                                  choices = join_tbl_opts, width = "100%")),
            column(3, selectInput(key_input_id,
                                  "Join key",
                                  choices = key_opts, width = "100%")),
            column(3, selectInput(typ_input_id,
                                  "Join type",
                                  choices = c("INNER JOIN", "LEFT JOIN",
                                              "RIGHT JOIN", "FULL OUTER JOIN"),
                                  selected = "INNER JOIN", width = "100%")),
            column(2, div(style = "margin-top:24px;",
                          actionButton(paste0("rm_join_", id), "×",
                                       class = "btn-rm-cond")))
          ),
          div(class = "join-on-hint",
              sprintf('ON t1."%s" = %s."%s"', cur_key, alias, cur_key))
      )
    })
  })


  # ── WHERE condition management - insertUI/removeUI to preserve existing values ─
  make_cond_row <- function(id, col_choices) {
    op_choices <- c("=", "!=", ">", "<", ">=", "<=",
                    "LIKE", "IN", "IS NULL", "IS NOT NULL")
    # Determine if OMOP schema active - if so, show concept search button on concept_id cols
    omop_active <- !is.null(omop_rv()) && isTRUE(omop_rv()$is_omop)
    omop_schema <- if (omop_active) omop_rv()$schema else NULL

    # The value input + optional concept search button
    val_ui <- if (omop_active) {
      div(class = "nc-cond-val-wrap",
          textInput(paste0("cond_val_", id), NULL, placeholder = "value", width = "100%"),
          actionButton(paste0("nc_csearch_", id), "🧬",
                       class = "cond-concept-btn",
                       title = "Search concept IDs",
                       onclick = sprintf(
                         "Shiny.setInputValue('nc_concept_open',{cid:%d},
                          {priority:'event'})", id)))
    } else {
      textInput(paste0("cond_val_", id), NULL, placeholder = "value", width = "100%")
    }

    div(id = paste0("cond_row_", id),
        fluidRow(style = "margin-bottom:2px;align-items:center;",
          column(4, selectInput(paste0("cond_col_", id), NULL,
                                choices = col_choices, width = "100%")),
          column(3, selectInput(paste0("cond_op_",  id), NULL,
                                choices = op_choices,  width = "100%")),
          column(4, val_ui),
          column(1, div(style = "padding-top:2px;",
                        actionButton(paste0("rm_cond_", id), "×",
                                     class = "btn-rm-cond")))
        )
    )
  }

  # ── Concept search modal for no-code WHERE conditions ──────────────────────
  observeEvent(input$nc_concept_open, {
    nc_concept_cond(input$nc_concept_open$cid)
    omop <- omop_rv()
    if (is.null(omop) || !omop$is_omop) return()

    showModal(modalDialog(
      title = tagList(span("🧬", style="margin-right:8px;"),
                      "Search Concept IDs"),
      div(style = "display:flex;gap:10px;margin-bottom:12px;",
          div(style = "flex:1;",
              textInput("nc_csearch_term", label = NULL,
                        placeholder = "e.g. diabetes, hypertension, HbA1c...",
                        width = "100%")),
          actionButton("nc_csearch_btn", "Search", class = "btn-dre-pri")),
      uiOutput("nc_csearch_results_ui"),
      size = "l", easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })

  nc_csearch_rv <- reactiveVal(NULL)

  observeEvent(input$nc_csearch_btn, {
    term  <- trimws(input$nc_csearch_term %||% "")
    omop  <- omop_rv()
    if (!nzchar(term) || is.null(omop)) return()
    nc_csearch_rv(NULL)
    r <- tryCatch({
      sch <- omop$schema
      cdm_info <- get_cdm_cols(sch, c("concept"))
      col_fn   <- cdm_info
      r <- db_query(sprintf(
        "SELECT %s AS concept_id, %s AS concept_name,
                %s AS domain_id,  %s AS vocabulary_id
         FROM \"%s\".\"concept\"
         WHERE %s ILIKE %s
         ORDER BY %s
         LIMIT 50",
        col_fn("concept","concept_id"),
        col_fn("concept","concept_name"),
        col_fn("concept","domain_id"),
        col_fn("concept","vocabulary_id"),
        sch,
        col_fn("concept","concept_name"),
        pg_str(paste0("%", term, "%")),
        col_fn("concept","concept_name")))
      r
    }, error = function(e) NULL)
    nc_csearch_rv(r)
  })

  output$nc_csearch_results_ui <- renderUI({
    res <- nc_csearch_rv()
    if (is.null(res)) return(NULL)
    if (nrow(res) == 0)
      return(p(style = "color:var(--dre-muted);font-style:italic;", "No concepts found."))
    cid_col  <- names(res)[1]
    cname_col <- names(res)[2]
    dom_col  <- names(res)[3]
    div(class = "concept-search-results",
        p(style = "color:var(--dre-muted);font-size:0.85rem;margin-bottom:8px;",
          sprintf("%d result%s - click a row to use the concept ID",
                  nrow(res), if (nrow(res)==1) "" else "s")),
        tags$table(class = "concept-table",
          tags$thead(tags$tr(
            tags$th("Concept ID"), tags$th("Name"), tags$th("Domain")
          )),
          tags$tbody(
            lapply(seq_len(nrow(res)), function(i) {
              cid   <- as.character(res[[cid_col]][i])
              cname <- as.character(res[[cname_col]][i])
              dom   <- as.character(res[[dom_col]][i])
              tags$tr(class = "concept-row",
                onclick = sprintf(
                  "Shiny.setInputValue('nc_concept_selected',
                   {cid:'%s',row_id:%d},{priority:'event'})",
                  cid, isolate(nc_concept_cond()) %||% 0),
                tags$td(class = "concept-id-cell", cid),
                tags$td(cname),
                tags$td(tags$span(class = "domain-pill", dom))
              )
            })
          )
        )
    )
  })

  observeEvent(input$nc_concept_selected, {
    sel    <- input$nc_concept_selected
    row_id <- nc_concept_cond()
    if (is.null(row_id) || is.null(sel$cid)) return()
    updateTextInput(session, paste0("cond_val_", row_id), value = sel$cid)
    removeModal()
    nc_concept_cond(NULL)
  })

  observeEvent(input$add_condition, {
    cols  <- col_data(); tbl <- selected_table()
    links <- schema_links_rv(); jids <- join_ids()
    if (is.null(cols) || is.null(tbl)) return()
    n <- cond_ctr() + 1L
    cond_ctr(n)
    cond_ids(c(cond_ids(), n))
    all_choices <- build_all_col_choices(tbl, cols, jids, links)
    # Hide the empty-state message once first row added
    session$sendCustomMessage("toggle_el", list(id="no_conditions_msg", show=FALSE))
    insertUI(
      selector = "#conditions_container",
      where    = "beforeEnd",
      ui       = make_cond_row(n, all_choices),
      immediate = TRUE
    )
    # Wire up remove button for this specific row
    local({
      lid <- n
      observeEvent(input[[paste0("rm_cond_", lid)]], {
        removeUI(selector = paste0("#cond_row_", lid), immediate = TRUE)
        ids <- cond_ids()[cond_ids() != lid]
        cond_ids(ids)
        if (length(ids) == 0) session$sendCustomMessage("toggle_el", list(id="no_conditions_msg", show=TRUE))
      }, ignoreInit = TRUE, ignoreNULL = TRUE, once = TRUE)
    })
  })

  get_conditions <- reactive({
    ids <- cond_ids()
    if (length(ids) == 0) return(list())
    Filter(function(c) !is.null(c$col) && nzchar(c$col %||% ""),
           lapply(ids, function(id) {
             raw <- input[[paste0("cond_col_", id)]] %||% ""
             if (grepl(".", raw, fixed = TRUE)) {
               p <- strsplit(raw, ".", fixed = TRUE)[[1]]
               list(alias = p[1],
                    col   = paste(p[-1], collapse = "."),
                    op    = input[[paste0("cond_op_",  id)]],
                    val   = input[[paste0("cond_val_", id)]])
             } else {
               list(alias = "t1",
                    col   = raw,
                    op    = input[[paste0("cond_op_",  id)]],
                    val   = input[[paste0("cond_val_", id)]])
             }
           }))
  })

  get_joins <- reactive({
    ids <- join_ids()
    if (length(ids) == 0) return(list())
    Filter(function(j) !is.null(j$table) && nzchar(j$table %||% ""),
           lapply(ids, function(id) list(
             table = input[[paste0("join_tbl_",  id)]],
             key   = input[[paste0("join_key_",  id)]],
             type  = input[[paste0("join_type_", id)]]
           )))
  })

  nc_sql_text <- reactive({
    cols  <- col_data(); tbl <- selected_table(); sch <- selected_schema()
    links <- schema_links_rv()
    req(cols, tbl, sch)

    valid_conds <- Filter(function(c) {
      !is.null(c$op) &&
        (c$op %in% c("IS NULL", "IS NOT NULL") ||
           (!is.null(c$val) && nzchar(c$val)))
    }, get_conditions())

    all_col_info <- if (!is.null(links)) links$all_cols else list()
    all_col_info[[tbl]] <- cols

    build_nocode_sql(
      schema        = sch,
      primary_table = tbl,
      select_cols   = input$nc_select_cols,
      joins         = get_joins(),
      conditions    = valid_conds,
      groupby_cols  = input$nc_groupby,
      orderby_col   = input$nc_orderby,
      order_dir     = input$nc_order_dir %||% "ASC",
      row_limit     = as.character(input$nc_limit %||% 100),
      all_col_info  = all_col_info
    )
  })

  output$nc_sql_preview <- renderText({
    tryCatch(nc_sql_text(), error = function(e) paste("Error:", conditionMessage(e)))
  })

  observeEvent(input$generate_sql_btn, {
    sql <- tryCatch(nc_sql_text(), error = function(e) NULL)
    if (!is.null(sql)) {
      updateTextAreaInput(session, "sql_input", value = sql)
      updateTabsetPanel(session, "main_tabs", selected = "editor")
    }
  })

  nc_run_error <- reactiveVal(NULL)

  output$nc_error_ui <- renderUI({
    err <- nc_run_error()
    if (is.null(err)) return(NULL)
    div(class = "sql-error", style = "margin-top:12px;",
        tags$strong("Query error"), br(),
        tags$pre(err))
  })

  observeEvent(input$nc_run_btn, {
    nc_run_error(NULL)
    sql <- tryCatch(nc_sql_text(), error = function(e) {
      nc_run_error(conditionMessage(e)); NULL })
    if (is.null(sql)) return()
    updateTextAreaInput(session, "sql_input", value = sql)
    query_result(NULL); query_error(NULL); resolved_result(NULL); last_run_sql(NULL)
    start  <- proc.time()["elapsed"]
    result <- tryCatch(
      { list(data = db_query(sql), err = NULL) },
      error = function(e) list(data = NULL, err = conditionMessage(e)))
    query_result(result$data); query_error(result$err)
    if (is.null(result$err)) last_run_sql(sql)
    elapsed <- round((proc.time()["elapsed"] - start) * 1000)
    query_time_ms(elapsed)

    # ── Push to query history (keep last 30, newest first) ──
    entry <- list(
      sql       = sql,
      ts        = format(Sys.time(), "%H:%M:%S"),
      rows      = if (!is.null(result$data)) nrow(result$data) else NA_integer_,
      ms        = elapsed,
      is_error  = !is.null(result$err),
      err_short = if (!is.null(result$err))
        substr(gsub("\n", " ", result$err), 1, 120) else NULL
    )
    hist <- c(list(entry), query_history_rv())
    if (length(hist) > 30) hist <- hist[seq_len(30)]
    query_history_rv(hist)
    if (!is.null(result$err)) {
      nc_run_error(result$err)
      return()
    }
    df   <- result$data
    omop <- omop_rv(); athena_extra(NULL)
    if (!is.null(df) && !is.null(omop) && omop$is_omop && isTRUE(isolate(concept_resolve()))) {
      res <- tryCatch(resolve_concept_ids(df, omop$schema),
                      error = function(e) list(df = df, resolved = character(0)))
      resolved_result(res)
    }
    updateTabsetPanel(session, "main_tabs", selected = "editor")
  })


  # ── Query Suggestions ───────────────────────────────────────────────────────
  all_suggestions <- reactive({
    cols  <- col_data(); tbl <- selected_table(); sch <- selected_schema()
    links <- schema_links_rv()
    req(cols, tbl, sch)
    generate_suggestions(sch, tbl, cols, links)
  })

  observeEvent(input$sug_filter_click, { sug_filter(input$sug_filter_click) })

  output$suggestions_ui <- renderUI({
    cols <- col_data(); tbl <- selected_table(); sch <- selected_schema()
    if (is.null(cols) || is.null(tbl))
      return(div(class = "empty-state",
                 div(class = "empty-icon", "💡"),
                 h4("Select a table"),
                 p(style = "color:#8BA3B5;",
                   "Choose a table in the sidebar to see generated queries.")))

    sugs   <- all_suggestions()
    filt   <- sug_filter()
    groups <- unique(sapply(sugs, function(s) s$group))
    shown  <- if (filt == "all") sugs
              else Filter(function(s) s$group == filt, sugs)

    tagList(
      # Filter tabs
      div(class = "sug-filter-row",
          div(class = paste("sug-ftab", if (filt == "all") "active" else ""),
              onclick = "Shiny.setInputValue('sug_filter_click','all',{priority:'event'})",
              sprintf("All  (%d)", length(sugs))),
          lapply(groups, function(g) {
            cnt   <- sum(sapply(sugs, function(s) s$group == g))
            is_jt <- g == "Cross-table"
            div(class = paste("sug-ftab", if (is_jt) "join-ftab" else "",
                              if (filt == g) "active" else ""),
                onclick = sprintf(
                  "Shiny.setInputValue('sug_filter_click','%s',{priority:'event'})", g),
                sprintf("%s  (%d)", g, cnt))
          })
      ),

      div(class = "sql-info",
          sprintf("%d quer%s shown for ", length(shown),
                  if (length(shown) == 1) "y" else "ies"),
          tags$strong(sprintf('"%s"."%s"', sch, tbl))),

      div(class = "sug-grid",
          lapply(names(shown), function(nm) {
            s      <- shown[[nm]]
            btn_id <- paste0("use_sug_", gsub("[^a-zA-Z0-9]", "_", nm))
            is_jt  <- !is.null(s$group) && s$group == "Cross-table"
            div(class = paste("sug-card", if (is_jt) "join-card" else ""),
                HTML(sprintf('<span class="sug-badge %s">%s</span>',
                             if (is_jt) "badge-cross" else "badge-single",
                             s$group %||% "Single table")),
                div(class = "sug-header",
                    span(class = "sug-icon", s$icon),
                    span(class = "sug-title", nm)),
                div(class = "sug-desc", s$desc),
                div(class = "sug-sql",  s$sql),
                actionButton(btn_id, "↗  Use Query", class = "btn-use-query"))
          }))
    )
  })

  # One observer per suggestion button
  observe({
    cols  <- col_data(); tbl <- selected_table(); sch <- selected_schema()
    links <- schema_links_rv()
    if (is.null(cols) || is.null(tbl)) return()
    sugs <- generate_suggestions(sch, tbl, cols, links)
    lapply(names(sugs), function(nm) {
      local({
        local_sql <- sugs[[nm]]$sql
        btn_id    <- paste0("use_sug_", gsub("[^a-zA-Z0-9]", "_", nm))
        observeEvent(input[[btn_id]], {
          updateTextAreaInput(session, "sql_input", value = local_sql)
          updateTabsetPanel(session, "main_tabs", selected = "editor")
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    })
  })
  
  # ── SCHEMA EXPLORER ───────────────────────────────────────────────────────────

  # Renders the Schema Explorer tab: a join map of detected table relationships
  # followed by a per-table column listing. Common key fields (shared by 2+
  # tables) are badged. All data comes from schema_links_rv(), already computed
  # by analyse_schema_links() when the schema was selected - no new queries.
  output$explorer_ui <- renderUI({
    sch   <- selected_schema()
    links <- schema_links_rv()

    if (is.null(sch) || !nzchar(sch))
      return(div(class = "empty-state",
                 div(class = "empty-icon", "🗂"),
                 h4("Select a schema"),
                 p(style = "color:#8BA3B5;",
                   "Choose a schema in the sidebar to explore its tables, columns and join keys.")))

    if (is.null(links))
      return(div(class = "empty-state",
                 div(class = "empty-icon", "⏳"),
                 h4("Analysing schema"),
                 p(style = "color:#8BA3B5;", "Schema link analysis is still running.")))

    all_cols <- links$all_cols
    common   <- links$common %||% character(0)
    pairs    <- links$pairs
    tbls     <- names(all_cols)

    if (length(tbls) == 0)
      return(div(class = "empty-state",
                 div(class = "empty-icon", "🗂"),
                 h4("No tables"),
                 p(style = "color:#8BA3B5;", "This schema contains no base tables.")))

    # ── Join map ──────────────────────────────────────────────────────────────
    joinmap <- if (!is.null(pairs) && nrow(pairs) > 0) {
      jrows <- lapply(seq_len(nrow(pairs)), function(i) {
        div(class = "se-jrow",
            span(class = "tname", pairs$table_a[i]),
            span(class = "sep", "↔"),
            span(class = "tname", pairs$table_b[i]),
            span(class = "sep", "on"),
            HTML(sprintf('<span class="key-pill">%s</span>', pairs$key[i])))
      })
      div(class = "se-joinmap",
          div(class = "se-joinmap-title",
              sprintf("Join map · %d relationship%s",
                      nrow(pairs), if (nrow(pairs) == 1) "" else "s")),
          jrows)
    } else {
      div(class = "se-joinmap",
          div(class = "se-joinmap-title", "Join map"),
          p(style = "color:#8BA3B5;font-size:0.88rem;margin:0;",
            "No shared key fields detected between tables in this schema."))
    }

    # ── Per-table column listing ──────────────────────────────────────────────
    table_blocks <- lapply(tbls, function(t) {
      cols <- all_cols[[t]]
      n    <- if (is.null(cols)) 0L else nrow(cols)

      col_rows <- if (n == 0) {
        list(div(class = "se-col",
                 span(style = "color:#8BA3B5;", "no columns")))
      } else {
        lapply(seq_len(n), function(i) {
          cn       <- cols$column_name[i]
          is_key   <- cn %in% common
          nullable <- "is_nullable" %in% names(cols) &&
                      identical(toupper(cols$is_nullable[i]), "YES")
          div(class = paste("se-col", if (is_key) "se-key" else ""),
              span(class = "col-name", cn),
              if (is_key) span(class = "key-pill", "key"),
              HTML(type_pill_html(cols$data_type[i])),
              if (nullable) span(class = "se-nullable", "nullable"))
        })
      }

      tagList(
        div(class = "se-table",
            t,
            span(style = "color:#8BA3B5;font-weight:400;",
                 sprintf("  ·  %d column%s", n, if (n == 1) "" else "s"))),
        col_rows
      )
    })

    div(
      div(class = "se-schema",
          sprintf('"%s"  ·  %d table%s', sch, length(tbls),
                  if (length(tbls) == 1) "" else "s")),
      joinmap,
      do.call(tagList, table_blocks)
    )
  })

# ── OMOP TOOLS ────────────────────────────────────────────────────────────────

  # Reactive: concept search results
  concept_search_rv <- reactiveVal(NULL)

  # OMOP quick-query definitions
  omop_quick_queries <- function(schema) {
    s <- schema

    # Resolve actual column names and types for all CDM tables used in queries
    cdm_tables <- c("person","observation_period","condition_occurrence",
                    "drug_exposure","measurement","concept")
    col <- get_cdm_cols(schema, cdm_tables)

    # Shorthand: fq(table) -> "schema"."table"
    fq <- function(t) sprintf('"%s"."%s"', s, t)

    list(
      list(
        category = "Demographics", icon = "👥",
        title    = "Person count by gender",
        desc     = "Total persons broken down by gender concept.",
        sql      = sprintf(
          'SELECT c.%s AS gender, COUNT(*) AS n\nFROM %s p\nLEFT JOIN %s c ON p.%s = c.%s\nGROUP BY c.%s\nORDER BY n DESC',
          col("concept","concept_name"),
          fq("person"),
          fq("concept"),
          col("person","gender_concept_id"),
          col("concept","concept_id"),
          col("concept","concept_name"))
      ),
      list(
        category = "Demographics", icon = "📅",
        title    = "Age distribution (5-year bands)",
        desc     = "Person counts grouped into 5-year age bands using year_of_birth.",
        sql      = sprintf(
          'SELECT\n  FLOOR((EXTRACT(YEAR FROM CURRENT_DATE) - %s) / 5) * 5 AS age_band_start,\n  COUNT(*) AS n\nFROM %s\nGROUP BY age_band_start\nORDER BY age_band_start',
          col("person","year_of_birth"),
          fq("person"))
      ),
      list(
        category = "Demographics", icon = "📆",
        title    = "Observation period coverage",
        desc     = "Min/max observation dates and average follow-up in days.",
        sql      = sprintf(
          'SELECT\n  MIN(%s) AS earliest,\n  MAX(%s) AS latest,\n  ROUND(AVG(%s - %s)) AS avg_days\nFROM %s',
          col("observation_period","observation_period_start_date"),
          col("observation_period","observation_period_end_date"),
          col("observation_period","observation_period_end_date"),
          col("observation_period","observation_period_start_date"),
          fq("observation_period"))
      ),
      list(
        category = "Conditions", icon = "🩺",
        title    = "Top 20 conditions by frequency",
        desc     = "Most common condition concepts recorded in condition_occurrence.",
        sql      = sprintf(
          'SELECT c.%s, COUNT(*) AS occurrences, COUNT(DISTINCT co.%s) AS persons\nFROM %s co\nLEFT JOIN %s c ON co.%s = c.%s\nGROUP BY c.%s\nORDER BY occurrences DESC\nLIMIT 20',
          col("concept","concept_name"),
          col("condition_occurrence","person_id"),
          fq("condition_occurrence"),
          fq("concept"),
          col("condition_occurrence","condition_concept_id"),
          col("concept","concept_id"),
          col("concept","concept_name"))
      ),
      list(
        category = "Conditions", icon = "📈",
        title    = "Condition occurrence timeline",
        desc     = "Monthly count of new condition records over time.",
        sql      = sprintf(
          'SELECT\n  DATE_TRUNC(\'month\', %s) AS month,\n  COUNT(*) AS occurrences\nFROM %s\nGROUP BY month\nORDER BY month',
          col("condition_occurrence","condition_start_date"),
          fq("condition_occurrence"))
      ),
      list(
        category = "Drugs", icon = "💊",
        title    = "Top 20 drugs by exposure count",
        desc     = "Most frequently prescribed drug concepts.",
        sql      = sprintf(
          'SELECT c.%s, COUNT(*) AS exposures, COUNT(DISTINCT de.%s) AS persons\nFROM %s de\nLEFT JOIN %s c ON de.%s = c.%s\nGROUP BY c.%s\nORDER BY exposures DESC\nLIMIT 20',
          col("concept","concept_name"),
          col("drug_exposure","person_id"),
          fq("drug_exposure"),
          fq("concept"),
          col("drug_exposure","drug_concept_id"),
          col("concept","concept_id"),
          col("concept","concept_name"))
      ),
      list(
        category = "Drugs", icon = "⏱",
        title    = "Drug exposure duration summary",
        desc     = "Average, min and max drug exposure duration in days per drug.",
        sql      = sprintf(
          'SELECT c.%s,\n  COUNT(*) AS exposures,\n  ROUND(AVG(%s)) AS avg_days,\n  MIN(%s) AS min_days,\n  MAX(%s) AS max_days\nFROM %s de\nLEFT JOIN %s c ON de.%s = c.%s\nWHERE %s IS NOT NULL\nGROUP BY c.%s\nORDER BY exposures DESC\nLIMIT 20',
          col("concept","concept_name"),
          col("drug_exposure","days_supply"),
          col("drug_exposure","days_supply"),
          col("drug_exposure","days_supply"),
          fq("drug_exposure"),
          fq("concept"),
          col("drug_exposure","drug_concept_id"),
          col("concept","concept_id"),
          col("drug_exposure","days_supply"),
          col("concept","concept_name"))
      ),
      local({
        # Inspect unit_concept_id and value_as_number column types at runtime
        col_types <- tryCatch({
          r <- db_query(sprintf(
            "SELECT lower(column_name) AS col, lower(data_type) AS dtype
             FROM information_schema.columns
             WHERE table_schema = %s AND lower(table_name) = 'measurement'
               AND lower(column_name) IN ('unit_concept_id','value_as_number')",
            pg_str(s)))
          if (!is.null(r) && nrow(r) > 0) setNames(r$dtype, r$col) else c()
        }, error = function(e) c())

        unit_is_usable <- !grepl("bool", col_types[["unit_concept_id"]] %||% "integer",
                                 fixed = TRUE)
        val_is_text    <- grepl("char|text",  col_types[["value_as_number"]] %||% "numeric",
                                ignore.case = TRUE)

        # WHERE clause: for text-typed columns filter empty strings too
        val_col  <- col("measurement","value_as_number")
        val_filter <- if (val_is_text)
          sprintf("m.%s IS NOT NULL AND m.%s <> ''\n  AND m.%s ~ '^[+-]?[0-9]+(\\.[0-9]+)?$'",
                  val_col, val_col, val_col)
        else
          sprintf("m.%s IS NOT NULL", val_col)

        # Safe numeric cast - use NULLIF to avoid casting empty/non-numeric text
        val_cast <- if (val_is_text)
          sprintf("NULLIF(REGEXP_REPLACE(m.%s, '[^0-9.+-]', '', 'g'), '')::numeric", val_col)
        else
          sprintf("CAST(m.%s AS numeric)", val_col)

        meas_sql <- if (unit_is_usable) {
          sprintf(
            'SELECT c.%s AS measurement,\n  COUNT(*) AS records,\n  ROUND(AVG(%s), 2) AS avg_value,\n  uc.%s AS unit\nFROM %s m\nLEFT JOIN %s c  ON m.%s = c.%s\nLEFT JOIN %s uc ON m.%s = uc.%s\nWHERE %s\nGROUP BY c.%s, uc.%s\nORDER BY records DESC\nLIMIT 20',
            col("concept","concept_name"),
            val_cast,
            col("concept","concept_name"),
            fq("measurement"),
            fq("concept"), col("measurement","measurement_concept_id"), col("concept","concept_id"),
            fq("concept"), col("measurement","unit_concept_id"),        col("concept","concept_id"),
            val_filter,
            col("concept","concept_name"),
            col("concept","concept_name"))
        } else {
          sprintf(
            'SELECT c.%s AS measurement,\n  COUNT(*) AS records,\n  ROUND(AVG(%s), 2) AS avg_value\nFROM %s m\nLEFT JOIN %s c ON m.%s = c.%s\nWHERE %s\nGROUP BY c.%s\nORDER BY records DESC\nLIMIT 20',
            col("concept","concept_name"),
            val_cast,
            fq("measurement"),
            fq("concept"), col("measurement","measurement_concept_id"), col("concept","concept_id"),
            val_filter,
            col("concept","concept_name"))
        }
        list(category = "Measurements", icon = "📏",
             title = "Top 20 measurements by frequency",
             desc  = "Most common measurement concepts with average numeric value.",
             sql   = meas_sql)
      }),
      local({
        all_cdm <- c("person","observation_period","visit_occurrence","condition_occurrence",
                     "drug_exposure","measurement","observation","procedure_occurrence",
                     "device_exposure","death","note","specimen","location","care_site",
                     "provider","payer_plan_period","cost","drug_era","dose_era",
                     "condition_era","cdm_source","concept","vocabulary","domain",
                     "concept_class","concept_relationship","relationship",
                     "concept_synonym","concept_ancestor","source_to_concept_map",
                     "drug_strength","cohort","fact_relationship")
        existing <- tryCatch({
          r <- db_query(sprintf(
            "SELECT lower(table_name) FROM information_schema.tables
             WHERE table_schema = %s AND table_type = 'BASE TABLE'",
            pg_str(s)))
          if (!is.null(r) && nrow(r) > 0) tolower(r[[1]]) else character(0)
        }, error = function(e) character(0))
        present <- all_cdm[all_cdm %in% existing]
        if (length(present) == 0) present <- c("person")
        list(
          category = "Data Quality", icon = "✅",
          title    = "CDM table row counts",
          desc     = sprintf("Row counts for %d CDM tables found in this schema.", length(present)),
          sql      = paste(
            sapply(present, function(t)
              sprintf("SELECT '%s' AS cdm_table, COUNT(*) AS row_count FROM \"%s\".\"%s\"",
                      t, s, t)),
            collapse = "\nUNION ALL\n"))
      }),
      list(
        category = "Data Quality", icon = "⚠",
        title    = "Unmapped concept IDs",
        desc     = "Records with concept_id = 0 (unmapped) per domain.",
        sql      = sprintf(
          'SELECT \'condition\' AS domain, COUNT(*) AS unmapped FROM %s WHERE %s = 0\nUNION ALL\nSELECT \'drug\',       COUNT(*) FROM %s WHERE %s = 0\nUNION ALL\nSELECT \'measurement\',COUNT(*) FROM %s WHERE %s = 0\nORDER BY unmapped DESC',
          fq("condition_occurrence"), col("condition_occurrence","condition_concept_id"),
          fq("drug_exposure"),        col("drug_exposure","drug_concept_id"),
          fq("measurement"),          col("measurement","measurement_concept_id"))
      )
    )
  }

  # ── Query History tab ─────────────────────────────────────────────────────
  output$history_ui <- renderUI({
    hist <- query_history_rv()
    if (length(hist) == 0)
      return(div(class = "empty-state",
                 div(class = "empty-icon", "🕑"),
                 h4("No query history yet"),
                 p(style = "color:#8BA3B5;", "Queries you run will appear here.")))

    tagList(
      div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:16px;",
          h4(style = "margin:0;", sprintf("%d quer%s", length(hist),
                                          if (length(hist) == 1) "y" else "ies")),
          actionButton("clear_history_btn", "Clear history",
                       class = "btn-dre-sec",
                       style = "font-size:0.82rem;padding:5px 12px;")),
      div(class = "hist-list",
          lapply(seq_along(hist), function(i) {
            e   <- hist[[i]]
            btn_id <- paste0("hist_restore_", i)
            div(class = paste("hist-card", if (isTRUE(e$is_error)) "hist-error" else ""),
                div(class = "hist-meta",
                    span(class = "hist-ts", e$ts),
                    if (isTRUE(e$is_error))
                      span(class = "hist-err-badge", "Error")
                    else if (!is.na(e$rows))
                      span(class = "hist-rows", paste0(format(e$rows, big.mark=","), " rows"))
                    else NULL,
                    if (!is.null(e$ms))
                      span(class = "hist-ms", paste0(e$ms, " ms"))
                ),
                if (isTRUE(e$is_error) && !is.null(e$err_short))
                  div(style = "font-size:0.82rem;color:var(--dre-error);margin-bottom:8px;",
                      e$err_short),
                div(class = "hist-sql", e$sql),
                actionButton(btn_id, "↩ Restore",
                             class = "btn-hist-restore")
            )
          })
      )
    )
  })

  observeEvent(input$clear_history_btn, { query_history_rv(list()) })

  # Dynamic restore observers - one per visible history card
  observe({
    hist <- query_history_rv()
    lapply(seq_along(hist), function(i) {
      btn_id <- paste0("hist_restore_", i)
      local({
        li <- i
        observeEvent(input[[btn_id]], {
          h <- query_history_rv()
          if (li <= length(h)) {
            updateTextAreaInput(session, "sql_input", value = h[[li]]$sql)
            updateTabsetPanel(session, "main_tabs", selected = "editor")
          }
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    })
  })


  # ── OMOP Tools UI ─────────────────────────────────────────────────────────────
  output$omop_tools_ui <- renderUI({
    omop <- omop_rv()
    if (is.null(omop) || !omop$is_omop)
      return(div(class = "empty-state",
                 div(class = "empty-icon", "🧬"),
                 h4("No OMOP schema selected"),
                 p("Select an OMOP CDM schema from the sidebar to use these tools.")))

    sch <- omop$schema

    tagList(
      # ── Concept Search ───────────────────────────────────────────────────────
      div(class = "omop-section",
        div(class = "omop-section-title", "🔍  Concept Search"),
        p(class = "omop-section-desc",
          "Search the concept table by name. Results show concept ID, domain, and vocabulary - click any row to copy the concept ID into the SQL editor."),
        div(style = "display:flex;gap:10px;margin-bottom:12px;",
          div(style = "flex:1;",
            textInput("concept_search_term", label = NULL,
                      placeholder = "e.g. celecoxib, atrial fibrillation, HbA1c...",
                      width = "100%")),
          actionButton("concept_search_btn", "Search", class = "btn-dre-pri")
        ),
        uiOutput("concept_search_results_ui")
      ),

      tags$hr(style = "border-color:var(--dre-border);margin:24px 0;"),

      # ── Type Checker ─────────────────────────────────────────────────────────
      div(class = "omop-section",
        div(class = "omop-section-title", "🔬  CDM Type Checker"),
        p(class = "omop-section-desc",
          "Compares every column in this schema against the OMOP CDM v5.4 specification. ",
          "Flags mismatched types, missing expected columns, and columns with no CDM definition."),
        div(style = "margin-bottom:14px;",
          actionButton("run_type_check", "▶  Run Type Check",
                       class = "btn-run",
                       style = "font-size:0.95rem;padding:9px 24px;")
        ),
        uiOutput("type_check_ui")
      ),

      tags$hr(style = "border-color:var(--dre-border);margin:24px 0;"),

      # ── Quick Queries ────────────────────────────────────────────────────────
      div(class = "omop-section",
        div(class = "omop-section-title", "⚡  Quick Queries"),
        p(class = "omop-section-desc",
          "Pre-built CDM queries for this schema. Click any card to load it into the SQL editor."),
        uiOutput("omop_quick_query_ui")
      )
    )
  })

  # ── CDM Type Checker ──────────────────────────────────────────────────────
  observeEvent(input$run_type_check, {
    omop <- omop_rv()
    if (is.null(omop) || !omop$is_omop) return()
    type_check_rv(list(running = TRUE))
    type_check_filter("all")

    sch <- omop$schema

    # Fetch all columns from information_schema for every OMOP table present
    actual_raw <- tryCatch(
      db_query(sprintf(
        "SELECT lower(table_name) AS tbl, lower(column_name) AS col, data_type
         FROM information_schema.columns
         WHERE table_schema = %s
           AND lower(table_name) IN (%s)
         ORDER BY table_name, ordinal_position",
        pg_str(sch),
        paste(sapply(names(OMOP_CDM_EXPECTED), pg_str), collapse = ","))),
      error = function(e) NULL)

    if (is.null(actual_raw)) {
      type_check_rv(list(error = "Could not query information_schema"))
      return()
    }

    # Build per-table results
    results <- lapply(names(OMOP_CDM_EXPECTED), function(tbl_name) {
      expected  <- OMOP_CDM_EXPECTED[[tbl_name]]
      tbl_rows  <- actual_raw[actual_raw$tbl == tbl_name, ]

      if (nrow(tbl_rows) == 0)
        return(list(table = tbl_name, present = FALSE, rows = data.frame()))

      # Map actual col -> pg category
      actual_cats <- setNames(
        sapply(tbl_rows$data_type, pg_type_to_cat),
        tbl_rows$col)

      # Build comparison rows
      all_cols <- union(names(expected), names(actual_cats))
      rows <- lapply(all_cols, function(col) {
        exp_cat <- expected[[col]] %||% NA_character_
        act_cat <- actual_cats[[col]] %||% NA_character_
        act_raw <- if (!is.na(act_cat)) {
          tbl_rows$data_type[tbl_rows$col == col][1]
        } else NA_character_

        status <- if (is.na(act_cat) && !is.na(exp_cat)) {
          "missing"       # in spec, not in schema
        } else if (is.na(exp_cat)) {
          "extra"         # in schema, not in spec
        } else if (act_cat == exp_cat) {
          "ok"
        } else {
          "wrong"
        }
        list(col = col, expected = exp_cat %||% "-",
             actual = act_raw %||% "-", actual_cat = act_cat %||% "-",
             status = status)
      })

      n_wrong   <- sum(sapply(rows, function(r) r$status == "wrong"))
      n_missing <- sum(sapply(rows, function(r) r$status == "missing"))
      n_ok      <- sum(sapply(rows, function(r) r$status == "ok"))
      n_extra   <- sum(sapply(rows, function(r) r$status == "extra"))

      list(table = tbl_name, present = TRUE, rows = rows,
           n_wrong = n_wrong, n_missing = n_missing,
           n_ok = n_ok, n_extra = n_extra,
           has_issues = (n_wrong + n_missing) > 0)
    })

    type_check_rv(list(results = results))
  })

  observeEvent(input$tc_filter, { type_check_filter(input$tc_filter) })

  # ── Generate ALTER TABLE fix script ───────────────────────────────────────
  observeEvent(input$tc_gen_fix, {
    tc   <- type_check_rv()
    omop <- omop_rv()
    if (is.null(tc) || is.null(tc$results) || is.null(omop)) return()

    sch     <- omop$schema
    present <- Filter(function(r) r$present, tc$results)

    # For each wrong-type column, build the appropriate ALTER TABLE statement.
    # The USING clause varies by conversion direction.
    make_alter <- function(sch, tbl, col, actual_cat, expected_cat, actual_raw) {
      qcol <- sprintf('"%s"', col)
      qtbl <- sprintf('"%s"."%s"', sch, tbl)

      # Target SQL type for each expected category
      target_type <- switch(expected_cat,
        integer   = "integer",
        numeric   = "numeric",
        text      = "text",
        date      = "date",
        timestamp = "timestamp without time zone",
        "text"    # safe fallback
      )

      # USING clause - must handle known problematic conversions explicitly
      using_expr <- if (actual_cat == "boolean" && expected_cat == "integer") {
        # boolean -> integer: true/false can't be meaningfully cast to concept IDs.
        # Generate a commented-out statement with a warning.
        return(paste0(
          "-- ⚠ WARNING: ", col, " is boolean but expected integer.\n",
          "--   Boolean values (true/false) cannot represent valid OMOP concept IDs.\n",
          "--   This likely indicates an upstream ETL problem - review source data before fixing.\n",
          "--   If you still want to cast: true→1, false→0 (NOT valid concept IDs)\n",
          "-- ALTER TABLE ", qtbl, "\n",
          "--   ALTER COLUMN ", qcol, " TYPE integer\n",
          "--   USING CASE WHEN ", qcol, " THEN 1 ELSE 0 END;"))
      } else if (actual_cat == "text" && expected_cat == "integer") {
        paste0("NULLIF(REGEXP_REPLACE(", qcol, ", '[^0-9-]', '', 'g'), '')::integer")
      } else if (actual_cat == "text" && expected_cat == "numeric") {
        paste0("NULLIF(TRIM(", qcol, "), '')::numeric")
      } else if (actual_cat == "text" && expected_cat == "date") {
        paste0(qcol, "::date")
      } else if (actual_cat == "text" && expected_cat == "timestamp") {
        paste0(qcol, "::timestamp without time zone")
      } else if (actual_cat == "integer" && expected_cat == "numeric") {
        paste0(qcol, "::numeric")
      } else if (actual_cat == "numeric" && expected_cat == "integer") {
        paste0("ROUND(", qcol, ")::integer")
      } else {
        # Generic cast - may fail at runtime for some type pairs
        paste0(qcol, "::", target_type)
      }

      paste0(
        "ALTER TABLE ", qtbl, "\n",
        "  ALTER COLUMN ", qcol, " TYPE ", target_type, "\n",
        "  USING ", using_expr, ";"
      )
    }

    # Collect all ALTER statements grouped by table
    lines <- c(
      "-- ============================================================",
      sprintf("-- OMOP CDM Type Fix Script - schema: %s", sch),
      sprintf("-- Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "-- Review each statement carefully before executing.",
      "-- Run a SELECT with the USING expression first to check for",
      "-- conversion errors (e.g. non-numeric text in a numeric column).",
      "-- ============================================================",
      ""
    )

    any_fixes <- FALSE
    for (tbl_res in present) {
      wrong_rows <- Filter(function(r) r$status == "wrong", tbl_res$rows)
      if (length(wrong_rows) == 0) next

      lines <- c(lines,
        sprintf("-- ── %s (%d column%s) ──────────────────────────────",
                tbl_res$table, length(wrong_rows),
                if (length(wrong_rows)==1) "" else "s"),
        ""
      )
      for (r in wrong_rows) {
        stmt <- make_alter(sch, tbl_res$table, r$col,
                           r$actual_cat, r$expected, r$actual)
        lines <- c(lines, stmt, "")
      }
      any_fixes <- TRUE
    }

    if (!any_fixes) {
      showNotification("No wrong-type columns found to fix.", type = "message")
      return()
    }

    sql_script <- paste(lines, collapse = "\n")

    # Load into SQL editor and switch to it
    updateTextAreaInput(session, "sql_input", value = sql_script)
    updateTabsetPanel(session, "main_tabs", selected = "editor")
    showNotification(
      "Fix script loaded into SQL editor. Review carefully before running.",
      type = "message", duration = 6)
  })

  output$type_check_ui <- renderUI({
    tc <- type_check_rv()
    if (is.null(tc)) return(NULL)

    if (!is.null(tc$running) && tc$running)
      return(p(style = "color:var(--dre-muted);", "Running type check..."))
    if (!is.null(tc$error))
      return(div(class = "sql-error", tc$error))

    results  <- tc$results
    present  <- Filter(function(r) r$present, results)
    absent   <- Filter(function(r) !r$present, results)

    # Summary counts across all present tables
    total_ok      <- sum(sapply(present, function(r) r$n_ok))
    total_wrong   <- sum(sapply(present, function(r) r$n_wrong))
    total_missing <- sum(sapply(present, function(r) r$n_missing))
    total_extra   <- sum(sapply(present, function(r) r$n_extra))
    n_tables_issues <- sum(sapply(present, function(r) r$has_issues))

    filt <- type_check_filter()

    tagList(
      # Summary cards
      div(class = "tc-summary",
          div(class = "tc-sum-card",
              div(class = "tc-sum-num tc-ok",  total_ok),
              div(class = "tc-sum-lbl", "Correct")),
          div(class = "tc-sum-card",
              div(class = "tc-sum-num tc-err",  total_wrong),
              div(class = "tc-sum-lbl", "Wrong type")),
          div(class = "tc-sum-card",
              div(class = "tc-sum-num tc-warn", total_missing),
              div(class = "tc-sum-lbl", "Missing col")),
          div(class = "tc-sum-card",
              div(class = "tc-sum-num tc-info", total_extra),
              div(class = "tc-sum-lbl", "Extra col")),
          div(class = "tc-sum-card",
              div(class = "tc-sum-num",
                  style = "color:var(--dre-muted);",
                  length(absent)),
              div(class = "tc-sum-lbl", "Tables absent"))
      ),

      # Generate fix script button - only shown when there are wrong-type columns
      if (total_wrong > 0)
        div(style = "margin-bottom:16px;",
            actionButton("tc_gen_fix", "🔧  Generate Fix Script",
                         class = "btn-dre-sec",
                         style = "font-size:0.9rem;"),
            span(style = "color:var(--dre-muted);font-size:0.82rem;margin-left:12px;",
                 sprintf("Generates ALTER TABLE SQL for %d wrong-type column%s",
                         total_wrong, if (total_wrong==1) "" else "s"))
        ),

      # Filter tabs
      div(class = "tc-filter-row",
          tags$span(class = paste("tc-ftab", if (filt=="all") "active" else ""),
                    onclick = "Shiny.setInputValue('tc_filter','all',{priority:'event'})",
                    "All tables"),
          tags$span(class = paste("tc-ftab tc-ftab-err",
                                  if (filt=="wrong") "active" else ""),
                    onclick = "Shiny.setInputValue('tc_filter','wrong',{priority:'event'})",
                    sprintf("Wrong type (%d)", total_wrong)),
          tags$span(class = paste("tc-ftab tc-ftab-warn",
                                  if (filt=="missing") "active" else ""),
                    onclick = "Shiny.setInputValue('tc_filter','missing',{priority:'event'})",
                    sprintf("Missing (%d)", total_missing)),
          tags$span(class = paste("tc-ftab",
                                  if (filt=="issues") "active" else ""),
                    onclick = "Shiny.setInputValue('tc_filter','issues',{priority:'event'})",
                    sprintf("Issues only (%d tables)", n_tables_issues))
      ),

      # Per-table collapsible blocks
      lapply(present, function(tbl_res) {
        rows <- tbl_res$rows
        # Apply filter
        visible_rows <- switch(filt,
          wrong   = Filter(function(r) r$status == "wrong",   rows),
          missing = Filter(function(r) r$status == "missing", rows),
          issues  = Filter(function(r) r$status %in% c("wrong","missing"), rows),
          rows   # "all"
        )
        if (filt != "all" && length(visible_rows) == 0) return(NULL)

        badge_class <- if (tbl_res$n_wrong > 0) "tc-badge-warn"
                       else if (tbl_res$n_missing > 0) "tc-badge-info"
                       else "tc-badge-ok"
        badge_txt <- if (tbl_res$n_wrong > 0)
          sprintf("%d type mismatch%s", tbl_res$n_wrong,
                  if (tbl_res$n_wrong==1) "" else "es")
        else if (tbl_res$n_missing > 0)
          sprintf("%d missing col%s", tbl_res$n_missing,
                  if (tbl_res$n_missing==1) "" else "s")
        else
          sprintf("✓ %d cols OK", tbl_res$n_ok)

        block_id <- paste0("tc_block_", tbl_res$table)
        div(class = "tc-table-block",
          # Clickable header
          div(class = "tc-tbl-header",
              onclick = sprintf("
                var b = document.getElementById('%s');
                var a = this.querySelector('.tc-tbl-arrow');
                if (b.style.display === 'none') {
                  b.style.display=''; a.textContent='▾';
                } else {
                  b.style.display='none'; a.textContent='▸';
                }
              ", block_id),
              span(class = "tc-tbl-name", tbl_res$table),
              span(class = paste("tc-tbl-badge", badge_class), badge_txt),
              span(class = "tc-tbl-arrow", "▸")
          ),
          # Collapsible body - hidden by default
          div(id = block_id, style = "display:none;",
              div(class = "tc-tbl-body",
                  # Header row
                  div(class = "tc-row tc-row-header",
                      div("Column"), div("Actual type"),
                      div("Expected"), div("Status")),
                  # Data rows
                  lapply(visible_rows, function(r) {
                    row_class <- paste("tc-row", switch(r$status,
                      wrong   = "tc-row-err",
                      missing = "tc-row-miss",
                      extra   = "",
                      ""))
                    status_ui <- switch(r$status,
                      ok      = span(class = "tc-col-status tc-status-ok",   "✓ OK"),
                      wrong   = span(class = "tc-col-status tc-status-err",
                                     paste0("✗ got ", r$actual_cat)),
                      missing = span(class = "tc-col-status tc-status-miss",  "⚠ missing"),
                      extra   = span(class = "tc-col-status tc-status-extra", "- not in spec"),
                      span(r$status)
                    )
                    div(class = row_class,
                        div(class = "tc-col-name",     r$col),
                        div(class = "tc-col-actual",   r$actual),
                        div(class = "tc-col-expected", r$expected),
                        status_ui)
                  })
              )
          )
        )
      }),

      # Tables not present in schema
      if (length(absent) > 0 && filt == "all") {
        div(style = "margin-top:10px;",
            p(style = "color:var(--dre-muted);font-size:0.85rem;font-weight:600;
                        text-transform:uppercase;letter-spacing:.05em;margin-bottom:8px;",
              sprintf("%d CDM tables not found in schema:", length(absent))),
            p(style = "color:var(--dre-muted);font-size:0.85rem;line-height:1.8;",
              paste(sapply(absent, function(r) r$table), collapse = ", ")))
      }
    )
  })


  # ── Concept search results ─────────────────────────────────────────────────
  output$concept_search_results_ui <- renderUI({
    res <- concept_search_rv()
    if (is.null(res)) return(NULL)
    if (nrow(res) == 0)
      return(p(style = "color:var(--dre-muted);font-style:italic;", "No concepts found."))

    div(class = "concept-search-results",
      p(style = "color:var(--dre-muted);font-size:0.85rem;margin-bottom:8px;",
        sprintf("%d result%s - click a row to use the concept ID in the SQL editor",
                nrow(res), if (nrow(res) == 1) "" else "s")),
      tags$table(class = "concept-table",
        tags$thead(tags$tr(
          tags$th("Concept ID"), tags$th("Name"),
          tags$th("Domain"), tags$th("Vocabulary"), tags$th("Standard")
        )),
        tags$tbody(
          lapply(seq_len(min(nrow(res), 50L)), function(i) {
            r      <- res[i, ]
            safe_col <- function(df_row, nm, pos) {
              v <- if (nm %in% names(df_row)) df_row[[nm]] else
                   if (ncol(df_row) >= pos) df_row[[pos]] else NA
              if (is.null(v) || length(v) == 0 || is.na(v)) "" else as.character(v)
            }
            cid    <- safe_col(r, "concept_id",       1)
            cname  <- safe_col(r, "concept_name",     2)
            domain <- safe_col(r, "domain_id",        3)
            vocab  <- safe_col(r, "vocabulary_id",    4)
            std    <- safe_col(r, "standard_concept", 5)
            std_lbl <- if (std == "S") "✓ Standard" else if (std == "C") "Classification" else "Non-standard"
            tags$tr(class = "concept-row",
              onclick = sprintf(
                "Shiny.setInputValue('concept_id_clicked','%s',{priority:'event'})", cid),
              tags$td(class = "concept-id-cell", cid),
              tags$td(cname),
              tags$td(tags$span(class = "domain-pill", domain)),
              tags$td(style = "color:var(--dre-muted);", vocab),
              tags$td(style = paste0("color:", if (std == "S") "#00E5D2" else "var(--dre-muted)"),
                      std_lbl)
            )
          })
        )
      )
    )
  })

  # Run concept search
  observeEvent(input$concept_search_btn, {
    omop <- omop_rv(); req(omop, omop$is_omop)
    term <- trimws(input$concept_search_term %||% "")
    req(nchar(term) >= 2)

    col_names_raw <- get_concept_col_names(omop$schema)
    qid   <- sprintf('"%s"', col_names_raw$concept_id)
    qname <- sprintf('"%s"', col_names_raw$concept_name)

    # Also fetch domain, vocabulary, standard_concept if available
    extra_cols <- tryCatch({
      sql_c <- sprintf(
        "SELECT column_name FROM information_schema.columns
         WHERE table_schema = %s AND table_name = 'concept'
           AND lower(column_name) IN ('domain_id','vocabulary_id','standard_concept')
         ORDER BY ordinal_position",
        pg_str(omop$schema))
      ec <- db_query(sql_c)$column_name
      if (length(ec) > 0) paste0(', ', paste(sprintf('"%s"', ec), collapse = ", ")) else ""
    }, error = function(e) "")

    sql <- sprintf(
      'SELECT %s, %s%s FROM "%s"."concept"
       WHERE %s ILIKE %s
       ORDER BY %s LIMIT 50',
      qid, qname, extra_cols, omop$schema,
      qname, pg_str(paste0("%", term, "%")),
      qname)

    res <- tryCatch(db_query(sql), error = function(e) {
      showNotification(paste("Concept search failed:", conditionMessage(e)),
                       type = "error", duration = 6)
      NULL
    })
    if (!is.null(res)) {
      names(res) <- tolower(names(res))
      concept_search_rv(res)
    }
  })

  # Concept row click → insert concept ID into SQL editor
  observeEvent(input$concept_id_clicked, {
    cid <- input$concept_id_clicked
    req(cid)
    updateTabsetPanel(session, "main_tabs", selected = "editor")
    current <- isolate(input$sql_input %||% "")
    updateTextAreaInput(session, "sql_input",
                        value = if (nchar(trimws(current)) == 0) cid
                                else paste0(current, "
-- concept_id: ", cid))
    showNotification(paste0("Concept ID ", cid, " added to SQL editor"),
                     type = "message", duration = 3)
  })

  # ── Quick query cards ──────────────────────────────────────────────────────
  output$omop_quick_query_ui <- renderUI({
    omop <- omop_rv(); req(omop, omop$is_omop)
    queries <- omop_quick_queries(omop$schema)
    categories <- unique(sapply(queries, `[[`, "category"))

    lapply(categories, function(cat) {
      cat_queries <- Filter(function(q) q$category == cat, queries)
      div(class = "omop-qcat",
        div(class = "omop-qcat-title",
            cat_queries[[1]]$icon, " ", cat),
        div(class = "omop-qcat-grid",
          lapply(seq_along(cat_queries), function(j) {
            q      <- cat_queries[[j]]
            btn_id <- paste0("omop_q_", gsub("[^a-z0-9]", "_", tolower(cat)), "_", j)
            div(class = "omop-qcard",
              div(class = "omop-qcard-title", q$title),
              div(class = "omop-qcard-desc",  q$desc),
              actionButton(btn_id, "↗  Load Query", class = "btn-omop-load")
            )
          })
        )
      )
    })
  })

  # Quick query load buttons
  observe({
    omop <- omop_rv()
    if (is.null(omop) || !omop$is_omop) return()
    queries    <- omop_quick_queries(omop$schema)
    categories <- unique(sapply(queries, `[[`, "category"))
    for (cat in categories) {
      cat_queries <- Filter(function(q) q$category == cat, queries)
      local({
        cqs <- cat_queries
        cat_safe <- gsub("[^a-z0-9]", "_", tolower(cat))
        for (j in seq_along(cqs)) {
          local({
            q      <- cqs[[j]]
            btn_id <- paste0("omop_q_", cat_safe, "_", j)
            observeEvent(input[[btn_id]], {
              updateTextAreaInput(session, "sql_input", value = q$sql)
              updateTabsetPanel(session, "main_tabs", selected = "editor")
              showNotification(paste0("Loaded: ", q$title),
                               type = "message", duration = 3)
            }, ignoreNULL = TRUE, ignoreInit = TRUE)
          })
        }
      })
    }
  })


  # ══════════════════════════════════════════════════════════════════════════
  # DATA QUALITY DASHBOARD
  # ══════════════════════════════════════════════════════════════════════════

  # ── Shell UI ──────────────────────────────────────────────────────────────
  output$dq_dashboard_ui <- renderUI({
    omop <- omop_rv()
    not_omop <- div(class = "empty-state",
                    div(class = "empty-icon", "📊"),
                    h4("No OMOP schema selected"),
                    p("Select an OMOP CDM schema from the sidebar to run the data quality dashboard."))
    if (is.null(omop) || !omop$is_omop) return(not_omop)

    dq <- dq_rv()

    div(class = "dq-wrap",
      div(class = "dq-run-bar",
        actionButton("run_dq", "▶  Run DQ Analysis", class = "btn-run",
                     style = "font-size:0.95rem;padding:9px 24px;"),
        span(class = "dq-schema-tag", omop$schema),
        if (!is.null(dq) && !is.null(dq$ran_at))
          span(style = "color:var(--dre-muted);font-size:0.82rem;",
               paste("Last run:", dq$ran_at))
      ),
      uiOutput("dq_results_ui")
    )
  })

  # ── Run observer ──────────────────────────────────────────────────────────
  observeEvent(input$run_dq, {
    omop <- omop_rv()
    if (is.null(omop) || !omop$is_omop) return()
    dq_rv(list(running = TRUE))
    sch <- omop$schema

    withProgress(message = "Running DQ analysis...", value = 0, {

      # ── 1. Person-level counts ────────────────────────────────────────────
      incProgress(0.05, detail = "Counting persons...")
      person_count <- tryCatch({
        r <- db_query(sprintf(
          'SELECT COUNT(*) AS n FROM "%s"."person"', sch))
        as.integer(r$n[1])
      }, error = function(e) NA_integer_)

      # ── 2. Domain coverage: row counts, distinct persons, date range ──────
      incProgress(0.10, detail = "Scanning clinical domains...")

      # Tables and their date / person columns
      domain_defs <- list(
        list(tbl="condition_occurrence",  date="condition_start_date",    label="Conditions"),
        list(tbl="drug_exposure",         date="drug_exposure_start_date",label="Drug exposures"),
        list(tbl="procedure_occurrence",  date="procedure_date",          label="Procedures"),
        list(tbl="measurement",           date="measurement_date",        label="Measurements"),
        list(tbl="observation",           date="observation_date",        label="Observations"),
        list(tbl="visit_occurrence",      date="visit_start_date",        label="Visits"),
        list(tbl="visit_detail",          date="visit_detail_start_date", label="Visit details"),
        list(tbl="device_exposure",       date="device_exposure_start_date",label="Device exposures"),
        list(tbl="death",                 date="death_date",              label="Deaths"),
        list(tbl="observation_period",    date="observation_period_start_date",label="Obs. periods"),
        list(tbl="drug_era",              date="drug_era_start_date",     label="Drug eras"),
        list(tbl="condition_era",         date="condition_era_start_date",label="Condition eras")
      )

      avail_tbls <- tolower(get_tables(sch))
      domain_rows <- lapply(domain_defs, function(d) {
        if (!d$tbl %in% avail_tbls) return(NULL)
        tryCatch({
          sql <- sprintf(
            'SELECT COUNT(*) AS n_rows,
                    COUNT(DISTINCT person_id) AS n_persons,
                    MIN("%s")::text AS min_date,
                    MAX("%s")::text AS max_date
             FROM "%s"."%s"',
            d$date, d$date, sch, d$tbl)
          r <- db_query(sql)
          list(label    = d$label,
               tbl      = d$tbl,
               n_rows   = as.integer(r$n_rows[1]),
               n_persons = as.integer(r$n_persons[1]),
               min_date = r$min_date[1],
               max_date = r$max_date[1],
               person_pct = if (!is.na(person_count) && person_count > 0)
                 round(100 * as.integer(r$n_persons[1]) / person_count, 1)
                 else NA_real_)
        }, error = function(e) NULL)
      })
      domain_rows <- Filter(Negate(is.null), domain_rows)

      # ── 3. Temporal distribution - events per year for key domains ────────
      incProgress(0.20, detail = "Temporal distribution...")

      temporal_domains <- list(
        list(tbl="condition_occurrence", date="condition_start_date",     label="Conditions"),
        list(tbl="drug_exposure",        date="drug_exposure_start_date", label="Drug exposures"),
        list(tbl="measurement",          date="measurement_date",         label="Measurements"),
        list(tbl="visit_occurrence",     date="visit_start_date",         label="Visits")
      )

      temporal_data <- lapply(temporal_domains, function(d) {
        if (!d$tbl %in% avail_tbls) return(NULL)
        tryCatch({
          sql <- sprintf(
            'SELECT EXTRACT(YEAR FROM "%s")::integer AS yr,
                    COUNT(*) AS n
             FROM "%s"."%s"
             WHERE "%s" IS NOT NULL
             GROUP BY 1 ORDER BY 1',
            d$date, sch, d$tbl, d$date)
          r <- db_query(sql)
          if (nrow(r) == 0) return(NULL)
          list(label = d$label, tbl = d$tbl,
               years = as.integer(r$yr),
               counts = as.integer(r$n))
        }, error = function(e) NULL)
      })
      temporal_data <- Filter(Negate(is.null), temporal_data)

      # ── 4. Concept zero (unmapped) rates ──────────────────────────────────
      incProgress(0.30, detail = "Checking concept mappings...")

      unmapped_defs <- list(
        list(tbl="condition_occurrence",  col="condition_concept_id",   label="Conditions"),
        list(tbl="drug_exposure",         col="drug_concept_id",        label="Drug exposures"),
        list(tbl="procedure_occurrence",  col="procedure_concept_id",   label="Procedures"),
        list(tbl="measurement",           col="measurement_concept_id", label="Measurements"),
        list(tbl="observation",           col="observation_concept_id", label="Observations"),
        list(tbl="visit_occurrence",      col="visit_concept_id",       label="Visits")
      )

      # Safe cast helper - handles boolean concept_id columns (Eunomia quirk)
      unmapped_rows <- lapply(unmapped_defs, function(d) {
        if (!d$tbl %in% avail_tbls) return(NULL)
        tryCatch({
          # Check actual column type first
          type_q <- sprintf(
            "SELECT data_type FROM information_schema.columns
             WHERE table_schema = %s AND lower(table_name) = %s
               AND lower(column_name) = %s LIMIT 1",
            pg_str(sch), pg_str(d$tbl), pg_str(d$col))
          type_r  <- tryCatch(db_query(type_q), error = function(e) NULL)
          is_bool <- !is.null(type_r) && nrow(type_r) > 0 &&
                     grepl("bool", tolower(type_r$data_type[1]))

          zero_cond <- if (is_bool)
            sprintf('NOT "%s"', d$col)
          else
            sprintf('("%s" IS NULL OR "%s" = 0)', d$col, d$col)

          sql <- sprintf(
            'SELECT COUNT(*) AS n_total,
                    SUM(CASE WHEN %s THEN 1 ELSE 0 END) AS n_zero
             FROM "%s"."%s"',
            zero_cond, sch, d$tbl)
          r <- db_query(sql)
          n_total <- as.integer(r$n_total[1])
          n_zero  <- as.integer(r$n_zero[1])
          pct <- if (n_total > 0) round(100 * n_zero / n_total, 1) else 0
          list(label = d$label, tbl = d$tbl, col = d$col,
               n_total = n_total, n_zero = n_zero, pct_zero = pct)
        }, error = function(e) NULL)
      })
      unmapped_rows <- Filter(Negate(is.null), unmapped_rows)

      # ── 5. Null rates for key clinical fields ─────────────────────────────
      incProgress(0.45, detail = "Null rate analysis...")

      null_defs <- list(
        list(tbl="measurement",     col="value_as_number",   label="Measurement - value_as_number"),
        list(tbl="measurement",     col="unit_concept_id",   label="Measurement - unit_concept_id"),
        list(tbl="drug_exposure",   col="days_supply",       label="Drug exposure - days_supply"),
        list(tbl="drug_exposure",   col="quantity",          label="Drug exposure - quantity"),
        list(tbl="condition_occurrence", col="condition_end_date", label="Condition - end_date"),
        list(tbl="visit_occurrence",col="care_site_id",      label="Visit - care_site_id"),
        list(tbl="person",          col="birth_datetime",    label="Person - birth_datetime"),
        list(tbl="person",          col="location_id",       label="Person - location_id"),
        list(tbl="death",           col="cause_concept_id",  label="Death - cause_concept_id")
      )

      null_rows <- lapply(null_defs, function(d) {
        if (!d$tbl %in% avail_tbls) return(NULL)
        # Check col exists
        col_exists <- tryCatch({
          r <- db_query(sprintf(
            "SELECT 1 FROM information_schema.columns
             WHERE table_schema = %s AND lower(table_name) = %s
               AND lower(column_name) = %s LIMIT 1",
            pg_str(sch), pg_str(d$tbl), pg_str(d$col)))
          nrow(r) > 0
        }, error = function(e) FALSE)
        if (!col_exists) return(NULL)
        tryCatch({
          sql <- sprintf(
            'SELECT COUNT(*) AS n_total,
                    SUM(CASE WHEN "%s" IS NULL THEN 1 ELSE 0 END) AS n_null
             FROM "%s"."%s"',
            d$col, sch, d$tbl)
          r <- db_query(sql)
          n_total <- as.integer(r$n_total[1])
          n_null  <- as.integer(r$n_null[1])
          pct <- if (n_total > 0) round(100 * n_null / n_total, 1) else 0
          list(label = d$label, n_total = n_total,
               n_null = n_null, pct_null = pct)
        }, error = function(e) NULL)
      })
      null_rows <- Filter(Negate(is.null), null_rows)

      # ── 6. Observation period continuity ─────────────────────────────────
      incProgress(0.60, detail = "Observation period checks...")

      obs_stats <- if ("observation_period" %in% avail_tbls) {
        tryCatch({
          sql <- sprintf(
            'SELECT
               COUNT(*) AS n_periods,
               COUNT(DISTINCT person_id) AS n_persons,
               ROUND(AVG(observation_period_end_date -
                         observation_period_start_date)) AS avg_days,
               MIN(observation_period_start_date)::text AS earliest,
               MAX(observation_period_end_date)::text   AS latest
             FROM "%s"."observation_period"', sch)
          r <- db_query(sql)
          list(n_periods  = as.integer(r$n_periods[1]),
               n_persons  = as.integer(r$n_persons[1]),
               avg_days   = as.integer(r$avg_days[1]),
               earliest   = r$earliest[1],
               latest     = r$latest[1])
        }, error = function(e) NULL)
      } else NULL

      # ── 7. Gender & birth-year distribution ──────────────────────────────
      incProgress(0.70, detail = "Demographics...")

      gender_dist <- if ("person" %in% avail_tbls) {
        tryCatch({
          sql <- sprintf(
            'SELECT gender_concept_id::text AS gid,
                    COUNT(*) AS n
             FROM "%s"."person"
             GROUP BY 1 ORDER BY 2 DESC', sch)
          r <- db_query(sql)
          # Try to resolve concept IDs to labels
          concept_tbl <- if ("concept" %in% avail_tbls) {
            tryCatch({
              ids <- paste(unique(r$gid), collapse=",")
              cq  <- sprintf(
                'SELECT concept_id::text AS gid, concept_name AS label
                 FROM "%s"."concept"
                 WHERE concept_id::text IN (%s)',
                sch, paste(sapply(unique(r$gid), pg_str), collapse=","))
              db_query(cq)
            }, error = function(e) data.frame(gid=character(),label=character()))
          } else data.frame(gid=character(), label=character())

          r$label <- ifelse(
            r$gid %in% concept_tbl$gid,
            concept_tbl$label[match(r$gid, concept_tbl$gid)],
            paste0("concept_id ", r$gid))
          r
        }, error = function(e) NULL)
      } else NULL

      birth_dist <- if ("person" %in% avail_tbls) {
        tryCatch({
          sql <- sprintf(
            'SELECT year_of_birth AS yr, COUNT(*) AS n
             FROM "%s"."person"
             WHERE year_of_birth IS NOT NULL
             GROUP BY 1 ORDER BY 1', sch)
          r <- db_query(sql)
          list(years  = as.integer(r$yr),
               counts = as.integer(r$n))
        }, error = function(e) NULL)
      } else NULL

      # ── 8. CDM source metadata ────────────────────────────────────────────
      incProgress(0.85, detail = "Reading CDM source...")
      cdm_info <- omop$cdm_info

      incProgress(1.0, detail = "Done")

      dq_rv(list(
        ran_at       = format(Sys.time(), "%Y-%m-%d %H:%M"),
        schema       = sch,
        person_count = person_count,
        domain_rows  = domain_rows,
        temporal     = temporal_data,
        unmapped     = unmapped_rows,
        nulls        = null_rows,
        obs_stats    = obs_stats,
        gender_dist  = gender_dist,
        birth_dist   = birth_dist,
        cdm_info     = cdm_info
      ))
    }) # withProgress
  })

  # ── Results renderer ──────────────────────────────────────────────────────
  output$dq_results_ui <- renderUI({
    dq <- dq_rv()
    if (is.null(dq)) return(
      div(style = "color:var(--dre-muted);padding:32px 0;font-size:0.95rem;",
          "Click  ▶ Run DQ Analysis  to begin."))
    if (!is.null(dq$running) && dq$running)
      return(div(class = "dq-spinner",
                 tags$span(style = "font-size:1.4rem;", "⏳"),
                 "Running analysis - this may take a moment for large schemas..."))

    fmt_n <- function(x) if (is.na(x) || is.null(x)) "-" else
      format(as.integer(x), big.mark = ",")

    # ── Helper: mini bar ────────────────────────────────────────────────────
    pct_bar <- function(pct, invert = FALSE) {
      if (is.na(pct) || is.null(pct)) return(span("-"))
      fill_pct <- max(0, min(100, pct))
      cls <- if (invert) {  # higher = worse (unmapped, nulls)
        if (pct >= 20) "dq-bar-fill dq-bar-err"
        else if (pct >= 5) "dq-bar-fill dq-bar-warn"
        else "dq-bar-fill"
      } else {              # higher = better (person coverage)
        if (pct < 20) "dq-bar-fill dq-bar-err"
        else if (pct < 60) "dq-bar-fill dq-bar-warn"
        else "dq-bar-fill"
      }
      div(class = "dq-bar-wrap",
          div(class = "dq-bar-bg",
              div(class = cls, style = sprintf("width:%.1f%%", fill_pct))),
          span(class = "dq-bar-pct", paste0(pct, "%")))
    }

    # ── Helper: percent text colour ─────────────────────────────────────────
    pct_cls <- function(pct, invert = FALSE) {
      if (is.na(pct)) return("-")
      cls <- if (invert) {
        if (pct >= 20) "dq-pct-err" else if (pct >= 5) "dq-pct-warn" else "dq-pct-ok"
      } else {
        if (pct < 20) "dq-pct-err" else if (pct < 60) "dq-pct-warn" else "dq-pct-ok"
      }
      span(class = cls, paste0(pct, "%"))
    }

    # ── Helper: year bar chart ───────────────────────────────────────────────
    year_chart <- function(years, counts, title_txt) {
      if (length(years) == 0) return(NULL)
      max_n <- max(counts, na.rm = TRUE)
      div(class = "dq-chart-card",
        div(class = "dq-chart-card-title", title_txt),
        div(class = "dq-chart-wrap",
          div(class = "dq-year-chart",
            lapply(seq_along(years), function(i) {
              h_pct <- if (max_n > 0) round(100 * counts[i] / max_n) else 0
              div(class = "dq-year-bar",
                  div(class = "dq-year-fill",
                      style = sprintf("height:%d%%;", max(2, h_pct)),
                      title = paste0(years[i], ": ", fmt_n(counts[i]))))
            })
          ),
          div(class = "dq-year-labels",
            lapply(seq_along(years), function(i) {
              # Show label only every N years to avoid overcrowding
              n_yrs <- length(years)
              step  <- if (n_yrs > 40) 10 else if (n_yrs > 20) 5 else if (n_yrs > 10) 2 else 1
              lbl   <- if ((i - 1) %% step == 0) as.character(years[i]) else ""
              div(class = "dq-year-lbl", lbl)
            })
          )
        )
      )
    }

    tagList(

      # ── CDM Source banner ────────────────────────────────────────────────
      if (!is.null(dq$cdm_info) && nrow(dq$cdm_info) > 0) {
        ci <- dq$cdm_info[1,]
        div(style = "background:var(--dre-surface);border:1px solid var(--dre-border);
                     border-radius:8px;padding:12px 18px;margin-bottom:20px;
                     display:flex;gap:20px;flex-wrap:wrap;align-items:center;",
          if (!is.null(ci$cdm_source_name) && !is.na(ci$cdm_source_name))
            div(tags$strong("Source: "), ci$cdm_source_name),
          if (!is.null(ci$cdm_version) && !is.na(ci$cdm_version))
            div(tags$strong("CDM version: "), ci$cdm_version),
          if (!is.null(ci$vocabulary_version) && !is.na(ci$vocabulary_version))
            div(tags$strong("Vocabulary: "), ci$vocabulary_version),
          if (!is.null(ci$source_release_date) && !is.na(ci$source_release_date))
            div(tags$strong("Source release: "),
                as.character(ci$source_release_date))
        )
      },

      # ── Top KPI row ──────────────────────────────────────────────────────
      div(class = "dq-section",
        div(class = "dq-section-title", "Overview"),
        div(class = "dq-kpi-row",
          div(class = "dq-kpi",
              div(class = "dq-kpi-val", fmt_n(dq$person_count)),
              div(class = "dq-kpi-lbl", "Total patients")),
          div(class = "dq-kpi",
              div(class = "dq-kpi-val",
                  length(dq$domain_rows)),
              div(class = "dq-kpi-lbl", "Clinical domains present")),
          if (!is.null(dq$obs_stats)) {
            os <- dq$obs_stats
            div(class = "dq-kpi",
                div(class = "dq-kpi-val", fmt_n(os$avg_days)),
                div(class = "dq-kpi-lbl", "Avg. observation days"),
                div(class = "dq-kpi-sub",
                    paste0(os$earliest, " → ", os$latest)))
          },
          if (!is.null(dq$obs_stats)) {
            os <- dq$obs_stats
            multi_pct <- if (!is.na(dq$person_count) && dq$person_count > 0)
              round(100 * (os$n_periods - os$n_persons) / dq$person_count, 1)
            else NA_real_
            div(class = "dq-kpi",
                div(class = "dq-kpi-val",
                    if (!is.na(multi_pct))
                      paste0(multi_pct, "%")
                    else fmt_n(os$n_periods)),
                div(class = "dq-kpi-lbl",
                    if (!is.na(multi_pct)) "Patients with >1 obs. period"
                    else "Observation period records"))
          }
        )
      ),

      # ── Domain coverage table ─────────────────────────────────────────────
      div(class = "dq-section",
        div(class = "dq-section-title", "Domain Coverage"),
        if (length(dq$domain_rows) == 0)
          p(style = "color:var(--dre-muted);", "No clinical domain tables found.")
        else
          div(style = "background:var(--dre-surface);border:1px solid var(--dre-border);border-radius:8px;overflow:hidden;",
            tags$table(class = "dq-cov-table",
              tags$thead(tags$tr(
                tags$th("Domain"),
                tags$th(style = "text-align:right;", "Rows"),
                tags$th(style = "text-align:right;", "Distinct patients"),
                tags$th("Patient coverage"),
                tags$th("Date range")
              )),
              tags$tbody(
                lapply(dq$domain_rows, function(d) {
                  tags$tr(
                    tags$td(class = "dq-tbl-name", d$label),
                    tags$td(class = "dq-num", fmt_n(d$n_rows)),
                    tags$td(class = "dq-num", fmt_n(d$n_persons)),
                    tags$td(pct_bar(d$person_pct, invert = FALSE)),
                    tags$td(class = "dq-date-range",
                            if (!is.na(d$min_date) && !is.null(d$min_date))
                              paste0(substr(d$min_date,1,10), " → ",
                                     substr(d$max_date,1,10))
                            else "-")
                  )
                })
              )
            )
          )
      ),

      # ── Unmapped concept rates ────────────────────────────────────────────
      div(class = "dq-section",
        div(class = "dq-section-title", "Concept Mapping (concept_id = 0 or NULL)"),
        if (length(dq$unmapped) == 0)
          p(style = "color:var(--dre-muted);", "No domain tables found.")
        else
          div(style = "background:var(--dre-surface);border:1px solid var(--dre-border);border-radius:8px;overflow:hidden;",
            tags$table(class = "dq-cov-table",
              tags$thead(tags$tr(
                tags$th("Domain"),
                tags$th(style = "text-align:right;", "Total rows"),
                tags$th(style = "text-align:right;", "Unmapped"),
                tags$th("Unmapped %"),
                tags$th("Coverage bar")
              )),
              tags$tbody(
                lapply(dq$unmapped, function(d) {
                  tags$tr(
                    tags$td(class = "dq-tbl-name", d$label),
                    tags$td(class = "dq-num", fmt_n(d$n_total)),
                    tags$td(class = "dq-num", fmt_n(d$n_zero)),
                    tags$td(pct_cls(d$pct_zero, invert = TRUE)),
                    tags$td(pct_bar(d$pct_zero, invert = TRUE))
                  )
                })
              )
            )
          )
      ),

      # ── Null rate table ───────────────────────────────────────────────────
      div(class = "dq-section",
        div(class = "dq-section-title", "Null Rates - Key Clinical Fields"),
        if (length(dq$nulls) == 0)
          p(style = "color:var(--dre-muted);", "No data found.")
        else
          div(style = "background:var(--dre-surface);border:1px solid var(--dre-border);border-radius:8px;overflow:hidden;",
            tags$table(class = "dq-cov-table",
              tags$thead(tags$tr(
                tags$th("Field"),
                tags$th(style = "text-align:right;", "Total rows"),
                tags$th(style = "text-align:right;", "Null"),
                tags$th("Null %"),
                tags$th("Bar")
              )),
              tags$tbody(
                lapply(dq$nulls, function(d) {
                  tags$tr(
                    tags$td(class = "dq-tbl-name", d$label),
                    tags$td(class = "dq-num", fmt_n(d$n_total)),
                    tags$td(class = "dq-num", fmt_n(d$n_null)),
                    tags$td(pct_cls(d$pct_null, invert = TRUE)),
                    tags$td(pct_bar(d$pct_null, invert = TRUE))
                  )
                })
              )
            )
          )
      ),

      # ── Temporal charts ───────────────────────────────────────────────────
      if (length(dq$temporal) > 0)
        div(class = "dq-section",
          div(class = "dq-section-title", "Event Volume by Year"),
          div(class = "dq-charts-grid",
            lapply(dq$temporal, function(d)
              year_chart(d$years, d$counts, d$label))
          )
        ),

      # ── Birth year chart ─────────────────────────────────────────────────
      if (!is.null(dq$birth_dist) && length(dq$birth_dist$years) > 0)
        div(class = "dq-section",
          div(class = "dq-section-title", "Demographics"),
          div(class = "dq-charts-grid",
            year_chart(dq$birth_dist$years, dq$birth_dist$counts,
                       "Patient year of birth"),
            # Gender table
            if (!is.null(dq$gender_dist) && nrow(dq$gender_dist) > 0)
              div(class = "dq-chart-card",
                div(class = "dq-chart-card-title", "Gender distribution"),
                tags$table(class = "dq-cov-table",
                  tags$thead(tags$tr(
                    tags$th("Gender"), tags$th(style="text-align:right;","Count"), tags$th("Share")
                  )),
                  tags$tbody(lapply(seq_len(nrow(dq$gender_dist)), function(i) {
                    row   <- dq$gender_dist[i,]
                    total <- sum(dq$gender_dist$n)
                    pct   <- round(100 * row$n / total, 1)
                    tags$tr(
                      tags$td(row$label),
                      tags$td(class="dq-num", fmt_n(row$n)),
                      tags$td(pct_bar(pct, invert = FALSE))
                    )
                  }))
                )
              )
          )
        )
    ) # tagList
  })

}

# ── 12. LAUNCH ────────────────────────────────────────────────────────────────

message("[ sql workbench ] launching...")
shinyApp(ui = ui, server = server)
