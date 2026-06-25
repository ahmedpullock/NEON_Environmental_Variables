################# NEON TEMPERATURE DOWNLOAD + SITE AVERAGE + UNITS #################

library(neonUtilities)
library(dplyr)
library(purrr)
library(lubridate)
library(neonOS)
library(tibble)

# ============================================================
# 1. OUTPUT FOLDER
# ============================================================

output_dir <- file.path(
  path.expand("~"),
  "Library",
  "CloudStorage",
  "OneDrive-MississippiStateUniversity",
  "NEON Code",
  "siteaverages_temperature_with_units"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("\nOutput folder:\n")
cat(output_dir, "\n\n")

# ============================================================
# 2. NEON TOKEN
# ============================================================

NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJzaGFtZWVtYWhtZWRwdWxsb2NrQGdtYWlsLmNvbSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkxODc2NDM0NiwiaWF0IjoxNzYxMDg0MzQ2LCJlbWFpbCI6InNoYW1lZW1haG1lZHB1bGxvY2tAZ21haWwuY29tIn0.0OEJ47LDC8Ub9ki9-Kdnu64UzHphE1-JVMeBbZEp55YOyPk8mmlBR3B7nrloUIb39ysTe6W2WaYVNLjg59fIng"

# ============================================================
# 3. DP CODE, TABLES, SITES, YEARS
# ============================================================

DP1_codes <- c("DP1.00002.001")

tables_to_use <- c("SAAT_1min", "SAAT_30min")

sites <- c(
  "HARV", "BART", "SCBI", "BLAN", "SERC", "OSBS", "DSNY", "JERC",
  "GUAN", "LAJA", "UNDE", "STEI", "TREE", "KONZ", "KONA", "UKFS",
  "ORNL", "GRSM", "MLBS", "TALL", "DELA", "LENO", "WOOD", "DCFS",
  "NOGP", "CPER", "RMNP", "STER", "CLBJ", "OAES", "YELL", "NIWO",
  "MOAB", "SRER", "JORN", "ONAQ", "WREF", "ABBY", "SJER", "SOAP",
  "TEAK", "TOOL", "BARR", "BONA", "DEJU", "HEAL", "PUUM"
)

years <- 2018:2024

# ============================================================
# 4. MAIN LOOP
# ============================================================

for (code in DP1_codes) {
  
  cat("\n====================================================\n")
  cat("STARTING DP CODE:", code, "\n")
  cat("====================================================\n")
  
  master_units_all_sites <- list()
  
  for (site in sites) {
    
    cat("\n----------------------------------------------------\n")
    cat("STARTING SITE:", site, "\n")
    cat("DP CODE:", code, "\n")
    cat("----------------------------------------------------\n")
    
    avg_outfile <- file.path(
      output_dir,
      paste(code, site, "AVERAGE_TEMPERATURE_WITH_UNITS.csv", sep = ".")
    )
    
    units_outfile <- file.path(
      output_dir,
      paste(code, site, "VARIABLE_UNITS.csv", sep = ".")
    )
    
    if (file.exists(avg_outfile) && file.exists(units_outfile)) {
      cat("Skipping completed site:", site, "\n")
      next
    }
    
    all_rows <- list()
    variables_all_years <- list()
    
    # ========================================================
    # 5. DOWNLOAD ONE YEAR AT A TIME
    # ========================================================
    
    for (yr in years) {
      
      cat("\nDownloading:", code, site, yr, "\n")
      
      neon_data <- tryCatch(
        loadByProduct(
          dpID = code,
          site = site,
          startdate = paste0(yr, "-01"),
          enddate   = paste0(yr, "-12"),
          include.provisional = TRUE,
          progress = TRUE,
          check.size = FALSE,
          token = NEON_TOKEN
        ),
        error = function(e) {
          cat("ERROR:", site, yr, conditionMessage(e), "\n")
          return(NULL)
        }
      )
      
      if (is.null(neon_data)) next
      
      cat("\nTables returned by NEON for", site, yr, ":\n")
      print(names(neon_data))
      
      # ======================================================
      # 6. FIND VARIABLE METADATA TABLE FLEXIBLY
      # ======================================================
      
      var_table_name <- names(neon_data)[
        grepl("variables", names(neon_data), ignore.case = TRUE)
      ][1]
      
      if (!is.na(var_table_name) && is.data.frame(neon_data[[var_table_name]])) {
        
        variables_this_year <- neon_data[[var_table_name]] %>%
          as_tibble() %>%
          mutate(
            dpID = code,
            siteID_download = site,
            year = yr
          )
        
        variables_all_years[[as.character(yr)]] <- variables_this_year
        
        cat("\nVARIABLE METADATA TABLE FOUND:", var_table_name, "\n")
        cat("VARIABLES AND UNITS FOUND FOR", site, yr, "\n")
        cat("----------------------------------------------------\n")
        
        variables_this_year %>%
          filter(table %in% tables_to_use) %>%
          select(table, fieldName, units, description) %>%
          as_tibble() %>%
          print(n = Inf)
        
      } else {
        cat("\nNo variables metadata table found for", site, yr, "\n")
        cat("Available tables were:\n")
        print(names(neon_data))
      }
      
      # ======================================================
      # 7. EXTRACT SELECTED TEMPERATURE TABLES
      # ======================================================
      
      data_tables_year <- neon_data[tables_to_use]
      data_tables_year <- data_tables_year[!sapply(data_tables_year, is.null)]
      data_tables_year <- data_tables_year[sapply(data_tables_year, is.data.frame)]
      
      if (length(data_tables_year) == 0) {
        cat("No selected SAAT tables found for", site, yr, "\n")
        next
      }
      
      cat("\nSAAT tables found for", site, yr, ":\n")
      print(names(data_tables_year))
      
      data_tables_year <- imap(
        data_tables_year,
        function(df, table_name) {
          df %>%
            as_tibble() %>%
            mutate(
              year = yr,
              source_table = table_name
            )
        }
      )
      
      all_rows <- c(all_rows, data_tables_year)
    }
    
    # ========================================================
    # 8. CHECK DOWNLOADED DATA
    # ========================================================
    
    if (length(all_rows) == 0) {
      cat("\nNo SAAT data downloaded for site:", site, "\n")
      next
    }
    
    if (length(variables_all_years) == 0) {
      cat("\nNo variable metadata found for site:", site, "\n")
      cat("Proceeding without NEON units for this site.\n")
      
      variables_meta <- tibble(
        table = character(),
        fieldName = character(),
        units = character(),
        description = character()
      )
    } else {
      variables_meta <- bind_rows(variables_all_years) %>%
        distinct(table, fieldName, units, description, .keep_all = TRUE)
    }
    
    master_units_all_sites[[site]] <- variables_meta
    
    # ========================================================
    # 9. COMBINE SAME TABLE ACROSS YEARS
    # ========================================================
    
    tables_combined <- split(all_rows, names(all_rows)) %>%
      map(bind_rows)
    
    tables_reduced <- tables_combined[
      names(tables_combined) %in% tables_to_use
    ]
    
    if (length(tables_reduced) == 0) {
      cat("\nNo SAAT_1min or SAAT_30min tables found after combining for:", site, "\n")
      next
    }
    
    cat("\nTABLES USED FOR AVERAGING:\n")
    print(names(tables_reduced))
    
    # ========================================================
    # 10. AVERAGE EACH TABLE BY siteID
    # ========================================================
    
    table_avgs <- imap(
      tables_reduced,
      function(df, table_name) {
        
        cat("\nSummarizing table:", table_name, "\n")
        
        if (!"siteID" %in% colnames(df)) {
          cat("Skipping", table_name, "- no siteID column\n")
          return(NULL)
        }
        
        if ("finalQF" %in% colnames(df)) {
          df <- df %>% filter(finalQF == 0)
        }
        
        numeric_cols <- df %>%
          select(where(is.numeric)) %>%
          colnames()
        
        numeric_cols <- setdiff(
          numeric_cols,
          c("year")
        )
        
        if (length(numeric_cols) == 0) {
          cat("Skipping", table_name, "- no numeric columns\n")
          return(NULL)
        }
        
        cat("\nNUMERIC VARIABLES AND UNITS IN", table_name, "\n")
        cat("----------------------------------------------------\n")
        
        if (nrow(variables_meta) > 0) {
          variables_meta %>%
            filter(
              table == table_name,
              fieldName %in% numeric_cols
            ) %>%
            select(table, fieldName, units, description) %>%
            as_tibble() %>%
            print(n = Inf)
        } else {
          cat("No units metadata available for this table.\n")
        }
        
        df %>%
          group_by(siteID) %>%
          summarise(
            across(
              all_of(numeric_cols),
              ~ mean(.x, na.rm = TRUE),
              .names = paste0(table_name, "_{.col}_mean")
            ),
            total_obs = n(),
            n_years = n_distinct(year),
            .groups = "drop"
          )
      }
    )
    
    table_avgs <- compact(table_avgs)
    
    if (length(table_avgs) == 0) {
      cat("\nNo summarized tables for site:", site, "\n")
      next
    }
    
    # ========================================================
    # 11. MERGE TABLE SUMMARIES
    # ========================================================
    
    site_avg <- reduce(
      table_avgs,
      full_join,
      by = "siteID"
    )
    
    # ========================================================
    # 12. MAKE UNITS SUMMARY
    # ========================================================
    
    final_columns <- colnames(site_avg)
    
    units_summary <- tibble(
      final_column = final_columns
    ) %>%
      mutate(
        table = case_when(
          grepl("^SAAT_1min_", final_column) ~ "SAAT_1min",
          grepl("^SAAT_30min_", final_column) ~ "SAAT_30min",
          TRUE ~ NA_character_
        ),
        original_fieldName = final_column %>%
          gsub("^SAAT_1min_", "", .) %>%
          gsub("^SAAT_30min_", "", .) %>%
          gsub("_mean$", "", .)
      ) %>%
      left_join(
        variables_meta %>%
          select(
            table,
            original_fieldName = fieldName,
            units,
            description
          ) %>%
          distinct(),
        by = c("table", "original_fieldName")
      )
    
    # ========================================================
    # 13. ADD UNIT COLUMNS TO SAME CSV
    # ========================================================
    
    site_avg_with_units <- site_avg
    
    for (col in colnames(site_avg)) {
      
      if (col == "siteID") next
      
      unit_value <- units_summary %>%
        filter(final_column == col) %>%
        pull(units)
      
      if (length(unit_value) == 0 || is.na(unit_value[1])) {
        unit_value <- NA_character_
      } else {
        unit_value <- unit_value[1]
      }
      
      site_avg_with_units[[paste0(col, "_unit")]] <- unit_value
    }
    
    # ========================================================
    # 14. SAVE OUTPUTS
    # ========================================================
    
    write.csv(
      site_avg_with_units,
      avg_outfile,
      row.names = FALSE
    )
    
    write.csv(
      units_summary,
      units_outfile,
      row.names = FALSE
    )
    
    cat("\nSAVED AVERAGED DATA WITH UNITS:\n")
    cat(avg_outfile, "\n")
    cat("Average file exists:", file.exists(avg_outfile), "\n")
    
    cat("\nSAVED VARIABLE UNITS FILE:\n")
    cat(units_outfile, "\n")
    cat("Units file exists:", file.exists(units_outfile), "\n")
  }
  
  # ==========================================================
  # 15. SAVE MASTER UNITS FILE FOR THIS DP CODE
  # ==========================================================
  
  if (length(master_units_all_sites) > 0) {
    
    master_units <- bind_rows(master_units_all_sites) %>%
      distinct(table, fieldName, units, description, .keep_all = TRUE)
    
    master_units_file <- file.path(
      output_dir,
      paste0(code, "_MASTER_VARIABLE_UNITS.csv")
    )
    
    write.csv(
      master_units,
      master_units_file,
      row.names = FALSE
    )
    
    cat("\nMASTER UNITS FILE SAVED:\n")
    cat(master_units_file, "\n")
    cat("Master units file exists:", file.exists(master_units_file), "\n")
  }
}

cat("\nALL DONE.\n")