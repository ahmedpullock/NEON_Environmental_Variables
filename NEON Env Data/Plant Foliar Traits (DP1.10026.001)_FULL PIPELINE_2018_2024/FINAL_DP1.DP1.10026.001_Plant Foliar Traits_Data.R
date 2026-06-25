############################################################
# DP1.10026.001
# FULL FIXED PIPELINE
# RAW DATA + UNITS + SITE/PLOT AVERAGES + MASTER FILE
# USES VALID SITE-YEARS CSV
############################################################

library(neonUtilities)
library(dplyr)
library(tibble)
library(readr)
library(purrr)
library(stringr)
library(lubridate)

# ==========================================================
# SETTINGS
# ==========================================================

code <- "DP1.10026.001"

NEON_TOKEN <- Sys.getenv("eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJzaGFtZWVtYWhtZWRwdWxsb2NrQGdtYWlsLmNvbSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkxODc2NDM0NiwiaWF0IjoxNzYxMDg0MzQ2LCJlbWFpbCI6InNoYW1lZW1haG1lZHB1bGxvY2tAZ21haWwuY29tIn0.0OEJ47LDC8Ub9ki9-Kdnu64UzHphE1-JVMeBbZEp55YOyPk8mmlBR3B7nrloUIb39ysTe6W2WaYVNLjg59fIng")

# ==========================================================
# READ VALID SITE-YEARS FILE YOU ALREADY CREATED
# ==========================================================

valid_site_years_file <- file.path(
  path.expand("~"),
  "Library",
  "CloudStorage",
  "OneDrive-MississippiStateUniversity",
  "NEON Code",
  "DP1_10026_availability_check_2018_2024",
  "DP1.10026.001_VALID_SITE_YEARS_TO_DOWNLOAD.csv"
)

valid_site_years <- read_csv(
  valid_site_years_file,
  show_col_types = FALSE
)

valid_site_years <- valid_site_years %>%
  filter(has_data == TRUE) %>%
  select(dpID, site, year)

cat("\nValid site-years loaded:\n")
print(valid_site_years, n = Inf)

# ==========================================================
# MAIN OUTPUT FOLDER
# ==========================================================

main_dir <- file.path(
  path.expand("~"),
  "Library",
  "CloudStorage",
  "OneDrive-MississippiStateUniversity",
  "NEON Code",
  "DP1_10026_FULL_PIPELINE_2018_2024"
)

raw_main_dir <- file.path(main_dir, "01_raw_data_by_site_plot")
avg_main_dir <- file.path(main_dir, "02_site_plot_average_2018_2024")
master_dir   <- file.path(main_dir, "03_master_all_sites_plot_average_2018_2024")

dir.create(raw_main_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(avg_main_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(master_dir, recursive = TRUE, showWarnings = FALSE)

cat("\nMAIN OUTPUT FOLDER:\n", main_dir, "\n")

# ==========================================================
# HELPER FUNCTIONS
# ==========================================================

safe_name <- function(x){
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "NO_VALUE"
  x <- gsub("[^A-Za-z0-9_\\-\\.]", "_", x)
  x
}

get_unit <- function(units_df, tbl, var){
  
  unit_value <- units_df %>%
    filter(table == tbl, fieldName == var) %>%
    pull(units)
  
  if(length(unit_value) == 0 || is.na(unit_value[1])){
    return(NA_character_)
  } else {
    return(unit_value[1])
  }
}

# ==========================================================
# STORAGE
# ==========================================================

master_avg_all_sites <- list()
master_units_all_sites <- list()
master_table_presence <- list()

# ==========================================================
# MAIN SITE LOOP
# ==========================================================

sites_to_process <- sort(unique(valid_site_years$site))

for(site in sites_to_process){
  
  cat("\n====================================================\n")
  cat("STARTING SITE:", site, "\n")
  cat("====================================================\n")
  
  site_years <- valid_site_years %>%
    filter(site == !!site) %>%
    pull(year)
  
  site_raw_dir <- file.path(raw_main_dir, site)
  site_avg_dir <- file.path(avg_main_dir, site)
  site_unit_dir <- file.path(site_raw_dir, "00_units")
  
  dir.create(site_raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(site_avg_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(site_unit_dir, recursive = TRUE, showWarnings = FALSE)
  
  site_raw_tables <- list()
  site_units <- list()
  site_table_presence <- list()
  
  # ========================================================
  # DOWNLOAD VALID YEARS ONLY
  # ========================================================
  
  for(yr in site_years){
    
    cat("\n----------------------------------------------------\n")
    cat("Downloading:", code, site, yr, "\n")
    cat("----------------------------------------------------\n")
    
    neon_data <- tryCatch(
      loadByProduct(
        dpID = code,
        site = site,
        startdate = paste0(yr, "-01"),
        enddate   = paste0(yr, "-12"),
        include.provisional = TRUE,
        check.size = FALSE,
        progress = TRUE,
        token = NEON_TOKEN
      ),
      error = function(e){
        cat("FAILED:", site, yr, "\n")
        cat("Error:", conditionMessage(e), "\n")
        return(NULL)
      }
    )
    
    if(is.null(neon_data)){
      next
    }
    
    cat("\nTables returned:\n")
    print(names(neon_data))
    
    site_table_presence[[as.character(yr)]] <- tibble(
      dpID = code,
      site = site,
      year = yr,
      table = names(neon_data)
    )
    
    # ------------------------------------------------------
    # SAVE VARIABLES / UNITS
    # ------------------------------------------------------
    
    var_table_name <- names(neon_data)[
      grepl("variables", names(neon_data), ignore.case = TRUE)
    ][1]
    
    if(!is.na(var_table_name)){
      
      units_this_year <- neon_data[[var_table_name]] %>%
        as_tibble() %>%
        mutate(
          dpID = code,
          site = site,
          year = yr
        )
      
      site_units[[as.character(yr)]] <- units_this_year
      
      write_csv(
        units_this_year,
        file.path(
          site_unit_dir,
          paste0(code, "_", site, "_", yr, "_VARIABLE_UNITS.csv")
        )
      )
    }
    
    # ------------------------------------------------------
    # REAL DATA TABLES ONLY
    # ------------------------------------------------------
    
    data_tables <- names(neon_data)[sapply(neon_data, is.data.frame)]
    
    data_tables <- data_tables[
      !grepl(
        "citation|issueLog|readme|variables|validation|categoricalCodes",
        data_tables,
        ignore.case = TRUE
      )
    ]
    
    if(length(data_tables) == 0){
      cat("No usable data tables for:", site, yr, "\n")
      next
    }
    
    # ------------------------------------------------------
    # SAVE RAW DATA BY SITE / YEAR / TABLE / PLOT
    # ------------------------------------------------------
    
    for(tbl in data_tables){
      
      raw_df <- neon_data[[tbl]] %>%
        as_tibble() %>%
        mutate(
          dpID = code,
          site_download = site,
          year_download = yr,
          source_table = tbl
        )
      
      site_raw_tables[[paste(site, yr, tbl, sep = "__")]] <- raw_df
      
      table_dir <- file.path(site_raw_dir, as.character(yr), tbl)
      dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
      
      full_table_file <- file.path(
        table_dir,
        paste0(code, "_", site, "_", yr, "_RAW_", tbl, "_ALL_PLOTS.csv")
      )
      
      write_csv(raw_df, full_table_file)
      
      if("plotID" %in% names(raw_df)){
        
        plots <- sort(unique(raw_df$plotID))
        plots <- plots[!is.na(plots)]
        
        for(pl in plots){
          
          plot_df <- raw_df %>%
            filter(plotID == pl)
          
          plot_file <- file.path(
            table_dir,
            paste0(
              code, "_", site, "_", yr, "_RAW_", tbl,
              "_PLOT_", safe_name(pl), ".csv"
            )
          )
          
          write_csv(plot_df, plot_file)
        }
      }
      
      cat("Saved raw table:", full_table_file, "\n")
    }
  }
  
  # ========================================================
  # SKIP SITE IF NO RAW DATA
  # ========================================================
  
  if(length(site_raw_tables) == 0){
    cat("\nNo downloaded raw data for site:", site, "\n")
    next
  }
  
  # ========================================================
  # SITE MASTER UNITS
  # ========================================================
  
  if(length(site_units) > 0){
    
    site_units_master <- bind_rows(site_units) %>%
      distinct(table, fieldName, units, description, .keep_all = TRUE)
    
  } else {
    
    site_units_master <- tibble(
      table = character(),
      fieldName = character(),
      units = character(),
      description = character()
    )
  }
  
  write_csv(
    site_units_master,
    file.path(
      site_unit_dir,
      paste0(code, "_", site, "_MASTER_VARIABLE_UNITS_2018_2024.csv")
    )
  )
  
  master_units_all_sites[[site]] <- site_units_master %>%
    mutate(site = site)
  
  # ========================================================
  # COMBINE SAME TABLE ACROSS VALID YEARS
  # ========================================================
  
  tables_combined_site <- site_raw_tables %>%
    split(map_chr(., ~ unique(.x$source_table)[1])) %>%
    map(bind_rows)
  
  site_avg_outputs <- list()
  
  # ========================================================
  # SITE/PLOT AVERAGES ACROSS AVAILABLE YEARS
  # ========================================================
  
  for(tbl in names(tables_combined_site)){
    
    cat("\nAveraging site:", site, "table:", tbl, "\n")
    
    df <- tables_combined_site[[tbl]] %>%
      as_tibble()
    
    if("collectDate" %in% names(df)){
      df <- df %>%
        mutate(
          collectDate_clean = as.Date(collectDate),
          collectYear = year(collectDate_clean)
        )
    }
    
    group_cols <- intersect(
      c(
        "siteID",
        "plotID",
        "namedLocation",
        "plotType",
        "subplotID",
        "individualID",
        "sampleID",
        "sampleCode",
        "plantStatus",
        "growthForm",
        "taxonID",
        "scientificName"
      ),
      names(df)
    )
    
    if(length(group_cols) == 0){
      group_cols <- intersect(c("siteID"), names(df))
    }
    
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, group_cols)
    
    if(length(numeric_cols) == 0){
      cat("No numeric columns to average for:", site, tbl, "\n")
      next
    }
    
    if(length(group_cols) > 0){
      
      avg_df <- df %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(
          n_raw_records = n(),
          across(
            all_of(numeric_cols),
            ~ mean(.x, na.rm = TRUE),
            .names = "{.col}_mean_2018_2024"
          ),
          .groups = "drop"
        )
      
    } else {
      
      avg_df <- df %>%
        summarise(
          n_raw_records = n(),
          across(
            all_of(numeric_cols),
            ~ mean(.x, na.rm = TRUE),
            .names = "{.col}_mean_2018_2024"
          )
        )
    }
    
    avg_df <- avg_df %>%
      mutate(
        dpID = code,
        site = site,
        table = tbl,
        years_available_averaged = paste(sort(unique(df$year_download)), collapse = ",")
      ) %>%
      relocate(dpID, site, table, years_available_averaged)
    
    for(v in numeric_cols){
      
      unit_value <- get_unit(site_units_master, tbl, v)
      
      avg_df[[paste0(v, "_mean_2018_2024_unit")]] <- unit_value
    }
    
    site_avg_outputs[[tbl]] <- avg_df
    
    site_avg_file <- file.path(
      site_avg_dir,
      paste0(code, "_", site, "_", tbl, "_SITE_PLOT_AVERAGE_2018_2024_WITH_UNITS.csv")
    )
    
    write_csv(avg_df, site_avg_file)
    
    cat("Saved site average:", site_avg_file, "\n")
  }
  
  # ========================================================
  # COMBINED SITE AVERAGE
  # ========================================================
  
  if(length(site_avg_outputs) > 0){
    
    site_avg_combined <- bind_rows(site_avg_outputs)
    
    site_avg_combined_file <- file.path(
      site_avg_dir,
      paste0(code, "_", site, "_ALL_TABLES_SITE_PLOT_AVERAGE_2018_2024_WITH_UNITS.csv")
    )
    
    write_csv(site_avg_combined, site_avg_combined_file)
    
    master_avg_all_sites[[site]] <- site_avg_combined
    
    cat("\nSaved combined site average:\n", site_avg_combined_file, "\n")
  }
  
  # ========================================================
  # TABLE PRESENCE FOR SITE
  # ========================================================
  
  if(length(site_table_presence) > 0){
    
    site_table_presence_df <- bind_rows(site_table_presence)
    
    write_csv(
      site_table_presence_df,
      file.path(
        site_raw_dir,
        paste0(code, "_", site, "_TABLE_PRESENCE_2018_2024.csv")
      )
    )
    
    master_table_presence[[site]] <- site_table_presence_df
  }
  
  cat("\nFINISHED SITE:", site, "\n")
}

# ==========================================================
# MASTER FILES
# ==========================================================

cat("\n====================================================\n")
cat("CREATING MASTER FILES\n")
cat("====================================================\n")

if(length(master_avg_all_sites) > 0){
  
  master_avg <- bind_rows(master_avg_all_sites)
  
  master_avg_file <- file.path(
    master_dir,
    paste0(code, "_MASTER_ALL_SITES_ALL_PLOTS_AVERAGE_2018_2024_WITH_UNITS.csv")
  )
  
  write_csv(master_avg, master_avg_file)
  
  cat("\nSaved master average:\n", master_avg_file, "\n")
  
  table_names <- sort(unique(master_avg$table))
  
  for(tbl in table_names){
    
    tbl_master <- master_avg %>%
      filter(table == tbl)
    
    tbl_file <- file.path(
      master_dir,
      paste0(code, "_MASTER_", tbl, "_ALL_SITES_PLOTS_AVERAGE_2018_2024_WITH_UNITS.csv")
    )
    
    write_csv(tbl_master, tbl_file)
    
    cat("Saved per-table master:", tbl_file, "\n")
  }
}

if(length(master_units_all_sites) > 0){
  
  master_units <- bind_rows(master_units_all_sites) %>%
    distinct(table, fieldName, units, description, .keep_all = TRUE)
  
  master_units_file <- file.path(
    master_dir,
    paste0(code, "_MASTER_VARIABLE_UNITS_ALL_SITES.csv")
  )
  
  write_csv(master_units, master_units_file)
  
  cat("\nSaved master units:\n", master_units_file, "\n")
}

if(length(master_table_presence) > 0){
  
  master_presence <- bind_rows(master_table_presence)
  
  master_presence_file <- file.path(
    master_dir,
    paste0(code, "_MASTER_TABLE_PRESENCE_2018_2024.csv")
  )
  
  write_csv(master_presence, master_presence_file)
  
  cat("\nSaved master table presence:\n", master_presence_file, "\n")
}

cat("\nALL DONE.\n")
