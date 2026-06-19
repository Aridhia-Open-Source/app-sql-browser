# SQL Workbench for Aridhia DRE

A single-file R Shiny application for browsing and querying any PostgreSQL schema inside
an Aridhia Digital Research Environment (DRE) Project Workspace. It connects to the
workspace database, lets a researcher pick a schema and table, and offers four ways to
work with it: a free-form SQL editor, a no-code query builder, one-click query
suggestions, and a table inspector. Exports go to CSV or ZIP.

The workbench is the general-purpose tool of the DRE app set. It makes no assumptions about
what is in the database, so it works on any schema, OMOP or otherwise. OMOP-specific
features (CDM conformance checking, type validation, remediation) live in the separate OMOP
CDM Validator app, and OMOP cohort building and analysis live in the OMOP Cohort Builder.

---

## What it does

Pick a schema, then a table, and four tabs open over it:

| Tab | Purpose |
|-----|---------|
| **SQL editor** | Free-form SQL against the workspace database. Read-only by default: only `SELECT`, `EXPLAIN`, and similar query statements run. Writes are opt-in via an explicit "Enable writes (DDL/DML)" checkbox that surfaces a clear banner while active. Results render in a paged table with an elapsed-time readout. |
| **Builder** | A no-code query builder: choose `SELECT` columns, add `JOIN`s (candidate tables are auto-detected by shared column name), build `WHERE` conditions, add `GROUP BY` and `ORDER BY`, and set a row limit. A live SQL preview updates as you go, and one button drops the generated query into the SQL editor for further editing. |
| **Suggestions** | One-click starter queries scoped to the selected table, for getting a quick feel for its contents without writing anything. |
| **Inspector** | Structural view of the table: columns and types, sample rows, and basic per-column statistics. |

Exports are available as CSV or ZIP, with a row-count guard on large extracts.

---

## How it runs in the DRE

The workbench is written around the workspace runtime, which differs from a standard R
installation in ways that make several conventional patterns silently fail:

- **Network-isolated.** No CRAN, no GitHub, no external APIs reach a workspace. The app
  never calls `install.packages()`; it checks for its required packages with
  `requireNamespace()` and stops with a clear message if any are missing.
- **Database connection via `xaputils::xap.conn`.** This active binding exposes the
  workspace PostgreSQL connection. It is referenced inline at every call, never captured to
  a local variable, because the underlying pointer can go stale between evaluations.
  Schema discovery uses the live binding directly rather than a startup snapshot.
- **PostgreSQL column-case handling.** RPostgreSQL returns column headers in their stored
  case, which varies between datasets. Results are lowercased on the way back so downstream
  rendering is predictable, and identifiers are double-quoted in generated SQL so mixed-case
  table and column names resolve correctly.
- **Server errors surface as warnings.** RPostgreSQL reports server-side query errors as a
  warning plus a `NULL` result rather than an R error. The app catches that, extracts the
  PostgreSQL `ERROR:` text, and raises it as a clear message instead of a silent empty grid.
- **Quoting that copes with messy input.** Table names that already carry quotes or an
  embedded schema prefix are normalised before being requoted, so workspace metadata that
  returns pre-formatted identifiers does not break query construction.
- **Single-file `app.R`.** The platform deploys a single file named `app.R`.
- **File outputs under `/home/workspace/files/`.** Every export is mirrored to
  `/home/workspace/files/Downloads/` in addition to the in-browser download, because the
  workspace file manager only shows files on disk.

---

## Requirements

All packages are pre-installed at workspace creation. Nothing is installed at runtime. If a
required package is missing the app stops at startup with a clear message.

| Package | Purpose |
|---------|---------|
| shiny, shinydashboard | Application framework and layout |
| DT | Result grids, inspector views, paged tables |
| DBI | Database interface |

There are no optional layers. The workbench runs entirely on native SQL against the
workspace database.

---

## Read-only by default, writes by deliberate choice

The workbench defaults to read-only and makes writing a conscious act:

- **Read-only is the default.** Without the writes toggle, statements are limited to queries.
  The editor shows a banner confirming read-only mode.
- **Writes are explicit and visible.** Enabling writes flips the banner to a warning that
  names exactly which statement types (`INSERT`, `UPDATE`, `DELETE`, `CREATE`, `ALTER`,
  `DROP`, `TRUNCATE`, and similar) will now execute against the database.
- **The toggle is a guard, not the authority.** A heuristic classifies each statement as a
  read or a write to enforce the toggle, but PostgreSQL role permissions remain
  authoritative. A query that the toggle would allow still fails if the connected role lacks
  the privilege, and the server error is reported back clearly.
- **Sensible default limits.** Ad-hoc `SELECT`s pick up a default row limit unless you
  supply your own, and exports are capped to keep large extracts from overwhelming the
  browser or the file manager.

---

## Deployment

1. Develop in the workspace with `shiny::runApp("app.R")`.
2. The app must be a single file named `app.R` for platform-managed deployment.
3. The platform launches it with `R -e 'shiny::runApp("app.R", port=8080, host="0.0.0.0")'`,
   with no inherited R options.
4. Logs are viewable through the workspace app-management UI; `message(...)` output appears
   there.

The connected database role needs read access to `information_schema` and the schemas you
want to browse. Write operations additionally need the relevant privileges on the target
tables, and only run when the writes toggle is enabled.

---

## Local development

Outside a workspace there is no `xaputils`, so the app falls back to a "no connection" state
and still launches, which is enough to check the UI and confirm the file parses. Querying
needs a real workspace database, so do that work inside a workspace.

---

## Companion apps

The workbench is one of a small set of single-file DRE apps that share the same conventions:

| App | Role |
|-----|------|
| **SQL Workbench** (this app) | Generic PostgreSQL schema browser, table inspector, and SQL editor for any workspace schema. |
| **OMOP CDM Validator** | OMOP CDM conformance checking and column type remediation. |
| **OMOP Cohort Builder** | Build, validate, and analyse cohorts over a conformant OMOP schema. |

The division is deliberate: the workbench is the general-purpose query tool, the validator
gets an OMOP schema into shape, and the cohort builder does the OMOP-specific analysis once
the schema is sound.

---

## Repository layout

| File | Purpose |
|------|---------|
| `app.R` | The application. Single-file Shiny app, deployed as-is to the workspace. |
| `reference-*.md` | Internal reference notes (DRE constraints, Shiny patterns). |

---

## Governance and compliance

The workbench is a community app running inside the Aridhia DRE. The environment carries a
96% SATRE score and ISO 27001, ISO 27701, HITRUST, and Cyber Essentials Plus certification.
All querying happens against the workspace database; nothing leaves the governed environment
except through the standard airlock and egress review. Writes are off by default and gated
behind an explicit toggle, with PostgreSQL permissions as the final authority.
