################# NEON PRECIPITATION DOWNLOAD + SITE AVERAGE + UNITS #################

library(neonUtilities)
library(dplyr)
library(purrr)
library(tibble)

# ============================================================
# OUTPUT FOLDER INSIDE NEON Code
# ============================================================

output_dir <- file.path(
  path.expand("~"),
  "Library",
  "CloudStorage",
  "OneDrive-MississippiStateUniversity",
  "NEON Code",
  "siteaverages_precipitation_DP1_00044_with_units"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("\nOutput folder:\n", output_dir, "\n\n")

# ============================================================
# NEON TOKEN
# ============================================================

NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJzaGFtZWVtYWhtZWRwdWxsb2NrQGdtYWlsLmNvbSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkxODc2NDM0NiwiaWF0IjoxNzYxMDg0MzQ2LCJlbWFpbCI6InNoYW1lZW1haG1lZHB1bGxvY2tAZ21haWwuY29tIn0.0OEJ47LDC8Ub9ki9-Kdnu64UzHphE1-JVMeBbZEp55YOyPk8mmlBR3B7nrloUIb39ysTe6W2WaYVNLjg59fIng"

# ============================================================
# DP CODE AND SITES
# ============================================================

code <- "DP1.00044.001"   # Precipitation

sites <- c(
  "HARV","BART","SCBI","BLAN","SERC","OSBS","DSNY","JERC","GUAN","LAJA",
  "UNDE","STEI","TREE","KONZ","KONA","UKFS","ORNL","GRSM","MLBS","TALL",
  "DELA","LENO","WOOD","DCFS","NOGP","CPER","RMNP","STER","CLBJ","OAES",
  "YELL","NIWO","MOAB","SRER","JORN","ONAQ","WREF","ABBY","SJER","SOAP",
  "TEAK","TOOL","BARR","BONA","DEJU","HEAL","PUUM"
)

years <- 2018:2024

master_all_sites <- list()
master_all_units <- list()

# ============================================================
# MAIN LOOP
# ============================================================

for(site in sites){
  
  cat("\n====================================================\n")
  cat("Processing site:", site, "\n")
  cat("DP code:", code, "\n")
  cat("====================================================\n")
  
  site_outfile <- file.path(
    output_dir,
    paste0(code, ".", site, ".AVERAGE_PRECIPITATION_WITH_UNITS.csv")
  )
  
  site_units_outfile <- file.path(
    output_dir,
    paste0(code, ".", site, ".VARIABLE_UNITS.csv")
  )
  
  if(file.exists(site_outfile) && file.exists(site_units_outfile)){
    cat("Skipping completed site:", site, "\n")
    master_all_sites[[site]] <- read.csv(site_outfile, stringsAsFactors = FALSE)
    next
  }
  
  all_rows <- list()
  variables_all_years <- list()
  
  for(yr in years){
    
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
      error = function(e){
        cat("Download failed/no data:", site, yr, "\n")
        cat("Error:", conditionMessage(e), "\n")
        return(NULL)
      }
    )
    
    if(is.null(neon_data)) next
    
    cat("\nTables returned by NEON for", site, yr, ":\n")
    print(names(neon_data))
    
    # Variable metadata
    var_table_name <- names(neon_data)[
      grepl("variables", names(neon_data), ignore.case = TRUE)
    ][1]
    
    if(!is.na(var_table_name) && is.data.frame(neon_data[[var_table_name]])){
      
      variables_this_year <- neon_data[[var_table_name]] %>%
        as_tibble() %>%
        mutate(
          dpID = code,
          siteID_download = site,
          year = yr
        )
      
      variables_all_years[[as.character(yr)]] <- variables_this_year
      
      cat("\nVARIABLE UNITS FOUND FOR", site, yr, "\n")
      variables_this_year %>%
        select(table, fieldName, units, description) %>%
        as_tibble() %>%
        print(n = Inf)
    }
    
    # Real data tables only
    data_tables <- names(neon_data)[sapply(neon_data, is.data.frame)]
    
    data_tables <- data_tables[
      !grepl(
        "citation|issueLog|readme|variables|sensor_positions|science_review",
        data_tables,
        ignore.case = TRUE
      )
    ]
    
    if(length(data_tables) == 0){
      cat("No usable data tables for", site, yr, "\n")
      next
    }
    
    data_tables_year <- neon_data[data_tables]
    
    data_tables_year <- imap(
      data_tables_year,
      function(df, table_name){
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
  
  if(length(all_rows) == 0){
    cat("\nNo precipitation data downloaded for site:", site, "\n")
    next
  }
  
  if(length(variables_all_years) == 0){
    variables_meta <- tibble(
      table = character(),
      fieldName = character(),
      units = character(),
      description = character(),
      dpID = character(),
      siteID_download = character(),
      year = integer()
    )
  } else {
    variables_meta <- bind_rows(variables_all_years) %>%
      distinct(table, fieldName, units, description, .keep_all = TRUE)
  }
  
  master_all_units[[site]] <- variables_meta
  
  # Combine same table across years
  tables_combined <- split(all_rows, names(all_rows)) %>%
    map(bind_rows)
  
  cat("\nTables used for averaging:\n")
  print(names(tables_combined))
  
  tables_processed <- imap(tables_combined, function(df, tbl){
    
    cat("\nSummarizing table:", tbl, "\n")
    
    df <- df %>% as_tibble()
    
    if("finalQF" %in% colnames(df)){
      df <- df %>% filter(finalQF == 0)
    }
    
    group_cols <- intersect(
      c("siteID", "plotID", "horizontalPosition", "verticalPosition"),
      colnames(df)
    )
    
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    numeric_cols <- setdiff(
      numeric_cols,
      c(group_cols, "finalQF", "year")
    )
    
    if(length(numeric_cols) == 0){
      cat("No numeric variables in", tbl, "\n")
      return(NULL)
    }
    
    cat("\nNumeric variables and units in", tbl, ":\n")
    
    variables_meta %>%
      filter(table == tbl, fieldName %in% numeric_cols) %>%
      select(table, fieldName, units, description) %>%
      as_tibble() %>%
      print(n = Inf)
    
    if(length(group_cols) == 0){
      
      summary_df <- df %>%
        summarise(
          across(
            all_of(numeric_cols),
            ~ mean(.x, na.rm = TRUE)
          )
        )
      
    } else {
      
      summary_df <- df %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(
          across(
            all_of(numeric_cols),
            ~ mean(.x, na.rm = TRUE)
          ),
          .groups = "drop"
        )
    }
    
    summary_df <- summary_df %>%
      mutate(
        dpID = code,
        site = site,
        table = tbl
      ) %>%
      relocate(dpID, site, table)
    
    for(col in numeric_cols){
      
      unit_value <- variables_meta %>%
        filter(table == tbl, fieldName == col) %>%
        pull(units)
      
      if(length(unit_value) == 0 || is.na(unit_value[1])){
        unit_value <- NA_character_
      } else {
        unit_value <- unit_value[1]
      }
      
      summary_df[[paste0(col, "_unit")]] <- unit_value
    }
    
    return(summary_df)
  })
  
  tables_processed <- compact(tables_processed)
  
  if(length(tables_processed) == 0){
    cat("No numeric variables processed for", site, "\n")
    next
  }
  
  final_output <- bind_rows(tables_processed)
  
  write.csv(final_output, site_outfile, row.names = FALSE)
  write.csv(variables_meta, site_units_outfile, row.names = FALSE)
  
  cat("\nSaved site data:\n", site_outfile, "\n")
  cat("Site data exists:", file.exists(site_outfile), "\n")
  
  cat("\nSaved site units:\n", site_units_outfile, "\n")
  cat("Site units exists:", file.exists(site_units_outfile), "\n")
  
  master_all_sites[[site]] <- final_output
  
  cat("\nFinished:", site, "\n")
}

# ============================================================
# MASTER DATA FILE
# ============================================================

if(length(master_all_sites) > 0){
  
  master_output <- bind_rows(master_all_sites)
  
  master_outfile <- file.path(
    output_dir,
    paste0(code, ".MASTER_ALL_SITES_AVERAGE_PRECIPITATION_WITH_UNITS.csv")
  )
  
  write.csv(master_output, master_outfile, row.names = FALSE)
  
  cat("\nMASTER DATA FILE SAVED:\n")
  cat(master_outfile, "\n")
  cat("Master data exists:", file.exists(master_outfile), "\n")
}

# ============================================================
# MASTER UNITS FILE
# ============================================================

if(length(master_all_units) > 0){
  
  master_units <- bind_rows(master_all_units) %>%
    distinct(table, fieldName, units, description, .keep_all = TRUE)
  
  master_units_outfile <- file.path(
    output_dir,
    paste0(code, ".MASTER_VARIABLE_UNITS.csv")
  )
  
  write.csv(master_units, master_units_outfile, row.names = FALSE)
  
  cat("\nMASTER UNITS FILE SAVED:\n")
  cat(master_units_outfile, "\n")
  cat("Master units exists:", file.exists(master_units_outfile), "\n")
}

cat("\nALL DONE.\n")