# ============================================================
# NEON DP1.00094.001
# Soil moisture one-row site average, processed year-by-year
# Years: 2018–2024
#
# Strategy:
# 1. One site
# 2. One year at a time
# 3. Summarize that year immediately
# 4. Save site-year checkpoint CSV
# 5. After all 7 years, combine into ONE ROW per site
# 6. Move to next site
# ============================================================

library(neonUtilities)
library(dplyr)
library(lubridate)

# -----------------------------
# Output folders
# -----------------------------

year_dir <- "DP1_00094_yearly_checkpoints_2018_2024"
site_dir <- "DP1_00094_ONE_ROW_site_average_year_by_year_2018_2024"

dir.create(year_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(site_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Sites and years
# -----------------------------

sites <- c(
  "HARV","BART","SCBI","BLAN","SERC","OSBS","DSNY","JERC","GUAN","LAJA",
  "UNDE","STEI","TREE","KONZ","KONA","UKFS","ORNL","GRSM","MLBS","TALL",
  "DELA","LENO","WOOD","DCFS","NOGP","CPER","RMNP","STER","CLBJ","OAES",
  "YELL","NIWO","MOAB","SRER","JORN","ONAQ","WREF","ABBY","SJER","SOAP",
  "TEAK","TOOL","BARR","BONA","DEJU","HEAL","PUUM"
)

years <- 2018:2024

# ============================================================
# Main loop
# ============================================================

for(site in sites) {
  
  cat("\n====================================================\n")
  cat("STARTING SITE:", site, "\n")
  cat("====================================================\n")
  
  final_site_file <- file.path(
    site_dir,
    paste0("DP1_00094_", site, "_ONE_ROW_soil_moisture_average_2018_2024.csv")
  )
  
  if(file.exists(final_site_file)) {
    cat("Skipping completed site:", site, "\n")
    next
  }
  
  # ----------------------------------------------------------
  # Process each year separately
  # ----------------------------------------------------------
  
  for(yr in years) {
    
    cat("\n-----------------------------\n")
    cat("SITE:", site, "| YEAR:", yr, "\n")
    cat("-----------------------------\n")
    
    yearly_file <- file.path(
      year_dir,
      paste0("DP1_00094_", site, "_", yr, "_soil_moisture_yearly_summary.csv")
    )
    
    if(file.exists(yearly_file)) {
      cat("Skipping completed site-year:", site, yr, "\n")
      next
    }
    
    tryCatch({
      
      start_month <- paste0(yr, "-01")
      end_month   <- paste0(yr, "-12")
      
      soil_data <- loadByProduct(
        dpID = "DP1.00094.001",
        site = site,
        startdate = start_month,
        enddate = end_month,
        package = "basic",
        check.size = FALSE,
        include.provisional = TRUE,
        nCores = 2
      )
      
      cat("Download complete for:", site, yr, "\n")
      
      df <- soil_data$SWS_30_minute
      
      if(is.null(df)) {
        cat("SWS_30_minute missing for:", site, yr, "\n")
        
        empty_summary <- data.frame(
          siteID = site,
          year = yr,
          soil_moisture_mean = NA_real_,
          soil_moisture_sd = NA_real_,
          soil_moisture_min = NA_real_,
          soil_moisture_max = NA_real_,
          total_obs = 0,
          first_date = NA,
          last_date = NA
        )
        
        write.csv(empty_summary, yearly_file, row.names = FALSE)
        next
      }
      
      df <- df %>%
        select(
          siteID,
          startDateTime,
          VSWCMean,
          VSWCFinalQF
        ) %>%
        filter(
          VSWCFinalQF == 0,
          !is.na(VSWCMean)
        ) %>%
        mutate(
          year = year(startDateTime)
        ) %>%
        filter(year == yr)
      
      if(nrow(df) == 0) {
        
        yearly_summary <- data.frame(
          siteID = site,
          year = yr,
          soil_moisture_mean = NA_real_,
          soil_moisture_sd = NA_real_,
          soil_moisture_min = NA_real_,
          soil_moisture_max = NA_real_,
          total_obs = 0,
          first_date = NA,
          last_date = NA
        )
        
      } else {
        
        yearly_summary <- df %>%
          group_by(siteID, year) %>%
          summarise(
            soil_moisture_mean = mean(VSWCMean, na.rm = TRUE),
            soil_moisture_sd   = sd(VSWCMean, na.rm = TRUE),
            soil_moisture_min  = min(VSWCMean, na.rm = TRUE),
            soil_moisture_max  = max(VSWCMean, na.rm = TRUE),
            total_obs          = n(),
            first_date         = min(startDateTime, na.rm = TRUE),
            last_date          = max(startDateTime, na.rm = TRUE),
            .groups = "drop"
          )
      }
      
      write.csv(yearly_summary, yearly_file, row.names = FALSE)
      
      cat("Saved yearly file:", yearly_file, "\n")
      
      rm(soil_data, df, yearly_summary)
      gc()
      
    }, error = function(e) {
      
      cat("\nERROR PROCESSING:", site, yr, "\n")
      cat("MESSAGE:", e$message, "\n")
      
      error_file <- file.path(
        year_dir,
        paste0("ERROR_DP1_00094_", site, "_", yr, ".txt")
      )
      
      writeLines(
        c(
          paste("SITE:", site),
          paste("YEAR:", yr),
          paste("ERROR:", e$message)
        ),
        error_file
      )
      
    })
  }
  
  # ----------------------------------------------------------
  # Combine 7 yearly summaries into ONE site-level row
  # ----------------------------------------------------------
  
  cat("\nCombining years for site:", site, "\n")
  
  yearly_files <- file.path(
    year_dir,
    paste0("DP1_00094_", site, "_", years, "_soil_moisture_yearly_summary.csv")
  )
  
  existing_yearly_files <- yearly_files[file.exists(yearly_files)]
  
  if(length(existing_yearly_files) == 0) {
    cat("No yearly files found for:", site, "\n")
    next
  }
  
  site_years <- bind_rows(
    lapply(existing_yearly_files, read.csv)
  )
  
  site_years <- site_years %>%
    filter(total_obs > 0)
  
  if(nrow(site_years) == 0) {
    
    final_site_summary <- data.frame(
      siteID = site,
      years_available = 0,
      soil_moisture_mean_2018_2024 = NA_real_,
      soil_moisture_sd_2018_2024 = NA_real_,
      soil_moisture_min_2018_2024 = NA_real_,
      soil_moisture_max_2018_2024 = NA_real_,
      total_obs_2018_2024 = 0,
      first_date = NA,
      last_date = NA
    )
    
  } else {
    
    final_site_summary <- site_years %>%
      summarise(
        siteID = site,
        years_available = n_distinct(year),
        
        soil_moisture_mean_2018_2024 =
          weighted.mean(soil_moisture_mean, total_obs, na.rm = TRUE),
        
        soil_moisture_sd_2018_2024 =
          sd(soil_moisture_mean, na.rm = TRUE),
        
        soil_moisture_min_2018_2024 =
          min(soil_moisture_min, na.rm = TRUE),
        
        soil_moisture_max_2018_2024 =
          max(soil_moisture_max, na.rm = TRUE),
        
        total_obs_2018_2024 =
          sum(total_obs, na.rm = TRUE),
        
        first_date =
          min(first_date, na.rm = TRUE),
        
        last_date =
          max(last_date, na.rm = TRUE)
      )
  }
  
  write.csv(final_site_summary, final_site_file, row.names = FALSE)
  
  cat("Saved final ONE-ROW site file:", final_site_file, "\n")
  
  rm(site_years, final_site_summary)
  gc()
}

# ============================================================
# Combine all final site-level files
# ============================================================

cat("\nCombining all site-level final files...\n")

final_files <- list.files(
  site_dir,
  pattern = "_ONE_ROW_soil_moisture_average_2018_2024.csv$",
  full.names = TRUE
)

if(length(final_files) > 0) {
  
  all_sites <- bind_rows(
    lapply(final_files, read.csv)
  )
  
  all_sites_file <- file.path(
    site_dir,
    "DP1_00094_ALL_SITES_ONE_ROW_soil_moisture_average_2018_2024.csv"
  )
  
  write.csv(all_sites, all_sites_file, row.names = FALSE)
  
  cat("Saved final all-sites file:", all_sites_file, "\n")
}

cat("\n====================================================\n")
cat("ALL PROCESSING FINISHED\n")
cat("====================================================\n")


