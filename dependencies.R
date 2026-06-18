# ============================================================
# dependencies.R - SQL Browser (Aridhia DRE Workspace)
# ============================================================
# Run this script once in a workspace RStudio session to verify
# that all required packages are available before launching
# the SQL Browser app.
#
# In a DRE workspace R environment, packages are installed into
# /home/workspace/files/R/<version>/ and persist across sessions.
# The workspace is NETWORK-ISOLATED - do NOT call install.packages()
# here. Contact your workspace administrator if a package is missing.
# ============================================================

# ── 1. Package inventory ───────────────────────────────────────────────────────
#
# Package       Source              Role
# -----------   ------------------  ----------------------------------------
# shiny         CRAN (pre-installed) Core Shiny web framework
# shinydashboard CRAN (pre-installed) Dashboard layout (sidebar, boxes)
# DT            CRAN (pre-installed) Interactive DataTables output
# DBI           CRAN (pre-installed) Database interface (dbGetQuery, dbExecute)
# RPostgreSQL   CRAN (pre-installed) PostgreSQL driver used by DBI
# jsonlite      CRAN (pre-installed) JSON serialisation for error clipboard copy
# httr          CRAN (pre-installed) HTTP client for optional Athena concept lookup
# xaputils      DRE-native           xap.conn - the workspace PostgreSQL connection
#                                    Pre-installed in all DRE workspace R environments.
#                                    Not available on CRAN. Do not attempt to install.
# ============================================================

required <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "DBI",
  "RPostgreSQL",
  "jsonlite",
  "httr"
)

# ── 2. Check each package ──────────────────────────────────────────────────────
cat("Checking SQL Browser dependencies...\n\n")

all_ok <- TRUE
for (pkg in required) {
  ok <- requireNamespace(pkg, quietly = TRUE)
  status <- if (ok) "\u2713 OK     " else "\u2717 MISSING"
  cat(sprintf("  %s  %s\n", status, pkg))
  if (!ok) all_ok <- FALSE
}

# ── 3. xaputils (DRE-native, checked separately) ──────────────────────────────
xap_ok <- requireNamespace("xaputils", quietly = TRUE)
cat(sprintf("  %s  %s  [DRE-native - pre-installed in all workspaces]\n",
            if (xap_ok) "\u2713 OK     " else "\u2717 MISSING",
            "xaputils"))
if (!xap_ok) all_ok <- FALSE

# ── 4. Summary ────────────────────────────────────────────────────────────────
cat("\n")
if (all_ok) {
  cat("All dependencies are satisfied. The SQL Browser app can be launched.\n\n")
  cat("  Launch command:\n")
  cat("    shiny::runApp('/home/workspace/files/SQL Browser')\n\n")
} else {
  cat("One or more dependencies are missing.\n")
  cat("In a DRE workspace the package library is at:\n")
  cat("  /home/workspace/files/R/<R-version>/\n\n")
  cat("Contact your workspace administrator to install missing CRAN packages.\n")
  cat("xaputils is DRE-native and cannot be installed from CRAN.\n\n")
}

# ── 5. Version report ─────────────────────────────────────────────────────────
cat("Package versions:\n")
for (pkg in c(required, "xaputils")) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    ver <- tryCatch(
      as.character(utils::packageVersion(pkg)),
      error = function(e) "unknown"
    )
    cat(sprintf("  %-18s %s\n", pkg, ver))
  }
}

cat(sprintf("\nR version: %s\n", R.version.string))
cat(sprintf("Library paths:\n"))
for (p in .libPaths()) cat(sprintf("  %s\n", p))
