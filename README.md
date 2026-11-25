# dlman

<!-- badges: start -->
<!-- badges: end -->

`dlman` provides DataOps utilities for managing local, server-free
[DuckLake](https://ducklake.select/) lakehouses in R with DuckDB and spatial
extensions. It separates reusable lakehouse operations—such as connection
management, catalog contract validation, lookup CSV encoding/delimiter
resolution, and safe read-only analysis access—from dataset-specific ingestion
scripts and lake storage.

## Installation

You can install the development version of dlman from
[GitHub](https://github.com/) with:

```r
# install.packages("pak")
pak::pak("beausoleilmo/dlman")
```

## Features

- **Session & connection lifecycle:** Open read-write ingestion sessions
  (`init_ingestion()`) or safe read-only analysis connections
  (`connect_lake_readonly()`), and close connections cleanly
  (`disconnect_lake()`).
- **Catalog contract validation:** Validate YAML dataset contracts
  (`catalog/dataset.yml`) before ingesting or mutating data.
- **Lookup CSV helpers:** Resolve file encoding (`resolve_lookup_encoding()`)
  and column delimiters (`resolve_lookup_delimiter()`) for reference tables
  under `catalog/lookups/`, with `ASCII` treated as compatible with `UTF-8` and
  clear `cli` diagnostics.
- **Project-agnostic lake paths:** Pass `lake_dir` explicitly to functions so
  the same package supports production lakehouses, scripts, and isolated
  `testthat` temporary lakes.

## Workflow

```mermaid
flowchart TD
    subgraph Guards["Input & Contract Guards"]
        A1["assert_non_empty_string() / assert_flag()"]
        A2["check_catalog()<br/>(catalog/dataset.yml)"]
    end

    subgraph Ingest["Ingestion Pipeline (read_only = FALSE)"]
        B1["init_ingestion()<br/>(explicit lake_dir)"]
        B2["resolve_lookup_encoding()<br/>& resolve_lookup_delimiter()"]
        B3["Stage & Materialize<br/>(tempdir() -> data/lake/ Parquet)"]
        B4["Quality Checks &<br/>Transactional Catalog Upsert"]
        B5["disconnect_lake()<br/>(on.exit() in script)"]
    end

    subgraph Maint["Snapshot Maintenance"]
        C1["ducklake_snapshots()<br/>Expire & Clean (dry_run = TRUE first)"]
    end

    subgraph Analyze["Self-Service & Analysis (read_only = TRUE)"]
        D1["connect_lake_readonly()<br/>(no write lock)"]
        D2["Query Lake & Lookups<br/>(catalog/lookups/ UTF-8 CSVs)"]
    end

    A1 --> B1
    A2 --> B1
    B1 --> B2 --> B3 --> B4 --> B5
    B4 -.-> C1
    B5 -.-> D1
    B2 -.-> D2
    D1 --> D2
```
