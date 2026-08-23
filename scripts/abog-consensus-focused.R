#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(testthat)
})

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

DBI::dbExecute(con, "
  CREATE TABLE retirement_signals_unified AS
  SELECT * FROM (VALUES
    ('1000000001', 'medicare_part_b', 2022, 0.85, 0.25),
    ('1000000001', 'abog_explicit_retirement', NULL, 0.95, 0.20),
    ('1000000002', 'nppes_deactivation', 2021, 1.00, 0.35),
    ('1000000002', 'medicare_part_b', 2021, 0.85, 0.25),
    ('1000000003', 'medicare_part_d', NULL, 0.85, 0.22),
    ('1000000003', 'abog_explicit_retirement', NULL, 0.95, 0.20)
  ) AS t(
    npi,
    signal_source,
    retirement_year,
    confidence,
    source_weight
  )
")

DBI::dbExecute(con, "
  CREATE TABLE retirement_consensus_preliminary AS
  WITH weighted_calc AS (
    SELECT
      npi,
      CASE
        WHEN SUM(
          CASE WHEN retirement_year IS NOT NULL
               THEN source_weight * confidence ELSE 0 END
        ) = 0 THEN NULL
        ELSE
          SUM(
            CASE WHEN retirement_year IS NOT NULL
                 THEN retirement_year * source_weight * confidence ELSE 0 END
          ) /
          SUM(
            CASE WHEN retirement_year IS NOT NULL
                 THEN source_weight * confidence ELSE 0 END
          )
      END AS weighted_retirement_year,
      COUNT(DISTINCT signal_source) AS source_count,
      COUNT(DISTINCT CASE
        WHEN retirement_year IS NOT NULL THEN signal_source
      END) AS year_source_count,
      STRING_AGG(DISTINCT signal_source, '+') AS sources,
      MIN(retirement_year) AS earliest_year,
      MAX(retirement_year) AS latest_year,
      STDDEV(CAST(retirement_year AS DOUBLE)) AS year_variance,
      AVG(confidence) AS base_confidence,
      CASE WHEN SUM(source_weight) = 0 THEN NULL
           ELSE SUM(source_weight * confidence) / SUM(source_weight)
      END AS weighted_confidence
    FROM retirement_signals_unified
    GROUP BY npi
  ),
  consensus AS (
    SELECT
      npi,
      CASE
        WHEN CAST(FLOOR(weighted_retirement_year + 0.5) AS INTEGER) < 1970
          THEN 1970
        WHEN CAST(FLOOR(weighted_retirement_year + 0.5) AS INTEGER) > 2025
          THEN 2025
        ELSE CAST(FLOOR(weighted_retirement_year + 0.5) AS INTEGER)
      END AS retirement_year_consensus,
      weighted_retirement_year AS retirement_year_exact,
      source_count,
      year_source_count,
      sources,
      earliest_year,
      latest_year,
      year_variance,
      CASE
        WHEN year_variance IS NOT NULL AND year_variance > 5 THEN
          GREATEST(0.30, weighted_confidence - 0.15)
        ELSE
          LEAST(1.0,
            weighted_confidence +
            CASE
              WHEN source_count >= 5 THEN 0.20
              WHEN source_count >= 3 THEN 0.15
              WHEN source_count >= 2 THEN 0.10
              ELSE 0.00
            END
          )
      END AS final_confidence
    FROM weighted_calc
  )
  SELECT * FROM consensus
  WHERE source_count >= 2
    AND year_source_count >= 1
")

result <- DBI::dbGetQuery(
  con,
  "SELECT * FROM retirement_consensus_preliminary ORDER BY npi"
)

one <- result[result$npi == "1000000001", , drop = FALSE]
testthat::expect_equal(nrow(one), 1L)
testthat::expect_equal(one$retirement_year_consensus, 2022L)
testthat::expect_equal(one$source_count, 2L)
testthat::expect_equal(one$year_source_count, 1L)
testthat::expect_gt(one$final_confidence, 0.85)
testthat::expect_match(one$sources, "abog_explicit_retirement")

two <- result[result$npi == "1000000002", , drop = FALSE]
testthat::expect_equal(nrow(two), 1L)
testthat::expect_equal(two$retirement_year_consensus, 2021L)
testthat::expect_equal(two$year_source_count, 2L)

three <- result[result$npi == "1000000003", , drop = FALSE]
testthat::expect_equal(nrow(three), 0L)

cat("ABOG status-aware retirement consensus focused test: PASS\n")
