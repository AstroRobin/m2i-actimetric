# ============================================================
# Title:        Automated Script for Running Actimetric on M2I Actigraph Data
#
# Author:       Robin van de Meeberg
# Created:      2026-03-19
# Last Updated: 2026-03-19

# ============================================================

### You won't need to include the following three lines in your code anymore.
### These only need to be run once in order to install the packages.
### They're commented out for now, so feel free to remove once installed.
#library(remotes)  # `install.packages('remotes')` if not already installed
#install_github("PhysicalActivityOpenTools/actimetricModels")
#install_github("PhysicalActivityOpenTools/actimetric")

library(actimetricModels)
library(actimetric)

library(tidyverse)  # `install.packages('tidyverse')` if not already installed


# ---- SETUP ---- #
# Define all the information needed to run the script
setwd("C:/Users/RvandeMeeberg/OneDrive - The Kids Research Institute Australia/Documents/Projects/BSTAT-1188 Emily Davey/data-raw/Actigraph")  # Set your working directory as the base point of your analysis
study_name = "M2I"  # used for bookkeeping of outputs. Might be useful if you want to keep separate versions of results (e.g. M2I_jan2026, M2I_noCP, etc.). Using the same study_name (i.e. M2I) will just overwrite the previous results
input_dir = 'data-raw'  # location of .gt3x files
output_dir = 'Processed_Data'  # location of where to store outputs
classifier = 'School age Wrist Random Forest'
create_visual_reports = TRUE  # TRUE/FALSE
clobber = FALSE  # TRUE: overwrite already-processed files; FALSE: skip


# Define some useful functions
log_message <- function(..., file = "log.txt", append = TRUE) {
  msg <- paste0(...)
  message(msg)
  clean_msg <- gsub("\x1B\\[[0-9;]*[A-Za-z]", "", msg) # Strip ANSI colour codes for file  
  cat(clean_msg, file = file, append = append)
}

# Check if necessary directories exist in working directory
if (!file.exists(input_dir)) {
  message("\033[31m", "[ERROR] The input directory '", input_dir, "' does not exist in your current working directory:\n  '", getwd(), "'.", "\033[0m")
  stop(call. = FALSE)
}

if (!file.exists(output_dir)) {
  message("\033[33m", "[WARNING] The output directory '", output_dir, "' does not exist in your current working directory:\n", "\033[0m", "  '", getwd(), "'")
  response <- readline(prompt = paste0("\033[35m", "\nWould you like to create it now? (y/n): ", "\033[0m"))
  
  if (tolower(response) %in% c("y", "yes")) {
    dir.create(output_dir)
    message("\033[32m", "[INFO] Directory created: '", output_dir, "'", "\033[0m")
  } else {
    message("\033[38m", "[INFO] Directory not created.", "\033[0m")
  }
}

logfile <- file.path(output_dir, paste0("log_", study_name, ".txt"))
logfile_created <- file.create(logfile)

# ----- CHECK FILES ----- #
## Check what files are actually available in the input directory

all_files <- list.files(input_dir, pattern = "\\.gt3x$")  # only files that end in .gt3x
message("\033[34m", "[INFO] The directory '", input_dir, "' has ", length(all_files), " '.gt3x' files.", "\033[34m")

# The information about the files will be stored here for later
file_info <- data.frame(
  file_name = character(),  # the name of the file
  patient_id = character(),   # the patient ID part of the file name
  diagnosis = character(),   # the 'XX' in 'M2IXX###'
  timepoint = character(),   # everything else in the filename, default is 'pre'
  file_size = character()
)

## This part loops over all the files and extracts the relevant info (ID, diagnosis, timpoint) from the file name
message("\033[34m", "[INFO] Extracting information from file names.", "\033[0m")
for (file in all_files) {
  file_parts <- str_split(file, "[-. ]", n = 2)

  ## Get the Patient ID
  patient_id <- file_parts[[1]][1] |> str_to_upper()
  patient_id <- ifelse(str_starts(patient_id, "M"), patient_id, paste0("M2I", patient_id))  # fix for some files not starting with M2I

  ## Get the Timepoint info
  if (file_parts[[1]][2] == 'gt3x') {  # no extra info in file, must be Pre timepoint
    timepoint <- 'Pre'
  } else {
    timepoint0 <- file_parts[[1]][2] |> str_remove('.gt3x') |> str_remove_all("[-._()\\s]") |> str_trim() |> str_to_lower()
    timepoint <- case_when(
      str_starts(timepoint0, 'initial') ~ "Initial",
      str_starts(timepoint0, 'pre') ~ "Pre",
      str_starts(timepoint0, 'post') ~ "Post",
      str_starts(timepoint0, '6m') ~ "6 month",
      str_starts(timepoint0, '12') ~ "12 month",
      str_starts(timepoint0, '202') ~ as.character(as.Date(timepoint0, format="%Y%m%d")),
      TRUE ~ timepoint0
    )
  }

  # Get the Diagnosis
  if (str_starts(patient_id, 'M2I')) {
    diagnosis = str_sub(patient_id, 4, 5)
  } else {
    diagnosis = "NA"
  }

  filesize <- paste0(round(file.info(file.path(input_dir, file))$size / (1024^2), 1), " MB")

  # Save the information
  file_info <- rbind(
    file_info,
    data.frame(
      file_name = file,
      patient_id = patient_id,
      diagnosis = diagnosis,
      timepoint = timepoint,
      file_size = filesize
    )
  )
}


# ---- RUN ACTIMETRIC ---- #
## Actually run the Actimetric code on each of the files found. 
cat(paste0("\n", paste0(rep("_", 50), collapse=''), "\n\nRunning M2I Actimetric on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '\n\n'), file = logfile, append = TRUE)
study_dirname = paste0('output_', study_name)

file_info <- file_info |> 
  arrange(
    patient_id,
    factor(timepoint, levels = c("Initial", "Pre", "Post", "6 month", "12 month")),
    timepoint   # optional: sorts adhoc values alphabetically at the end
  )

for (ii in seq(1, nrow(file_info))) {
  file_path <- paste0(input_dir, '/', file_info$file_name[ii])
  
  result_filepath <- file.path(output_dir, file_info$patient_id[ii], study_dirname, 'results', 'personlevel_report.csv')

  # IF a results file already exist and no overwriting (i.e. no clobber), then check whether we need to skip current patient+timepoint
  if (file.exists(result_filepath) && clobber == FALSE) {

    results_temp <- read_csv(result_filepath, show_col_types = FALSE)
    if (str_replace_all(file_info$file_name[ii], '.gt3x', '') %in% results_temp$ID) {
      log_message("\033[32m", "[INFO] Skipping file '", file_info$file_name[ii], "' because output already exists in 'personlevel_report.csv' and clobber = FALSE\n", "\033[0m", file=logfile)
      next
    }

    rm(results_temp)
  }
  
  message("\033[34m", "[INFO] Processing '", file_info$file_name[ii], "' (", file_info$file_size[ii], ").", "\033[0m")
  
  tryCatch({
    if (file_info$diagnosis[ii] == 'CP') {
      log_message("\033[33m", "[WARNING] Skipping file '", file_info$file_name[ii], "' - CP Model not yet implemented.\n", "\033[0m", file=logfile)
      # message("\033[36m", "[INFO] >>> Running Actimetric - Classifier: ", classifier , " (CP)", "\033[0m")
      # runActimetric(
      #   input_directory = file_path,
      #   output_directory = paste0(output_dir, '/', file_info$patient_id[ii]),
      #   classifier = classifier,
      #   visualreport = create_visual_reports,
      #   studyname = study_name,
      #   verbose = FALSE  # suppress extra information from runActimetric
      # )
    } else {
      message("\033[0m", "[INFO] >>> ID: ", file_info$patient_id[ii], " (", file_info$diagnosis[ii] ,") - ", file_info$timepoint[ii], "\033[0m")
      message("\033[0m", "[INFO] >>> Running Actimetric (Classifier: '", classifier, "')", "\033[0m")
      runActimetric(
        input_directory = paste0(input_dir, '/', file_info$file_name[ii]),
        output_directory = paste0(output_dir, '/', file_info$patient_id[ii]),
        classifier = classifier,
        visualreport = create_visual_reports,
        studyname = study_name,
        verbose = FALSE 
      )
      log_message("\033[32m", "[INFO] ✔ Successfully processed '", file_info$file_name[ii], "'.", "\033[0m", '\n', file=logfile)
    }

  }, error = function(e) {
    if (grepl('could not find function "runActimetric"', e$message)) {
      log_message(paste0("\033[31m", "[ERROR] ✖ Failed to process '", file_info$file_name[ii], "'.", "\n", "   ", e$message, "\033[0m\n", sep = ""), file=logfile)
      stop(e)  # stop if it's the one you care about
    } else {
      log_message(paste0("\033[31m", "[ERROR] ✖ Failed to process '", file_info$file_name[ii], "'.", "\033[0m ", "\n", "   ", e$message, "\n", sep = ""), file=logfile)
    }
  })

}

# ---- COLLATE RESULTS ---- #
## From all the result files generated, now collate these all together into a single file.

results_list <- list()
all_dirs <- list.dirs(output_dir, recursive = FALSE)
for (ii in seq_along(all_dirs)) {
  dirname <- all_dirs[ii]
  result_filepath <- file.path(dirname, study_dirname, 'results', 'personlevel_report.csv') 
  if (file.exists(result_filepath)) {
    message("\033[34m", "[INFO] Results file found in '", dirname, "'.", "\033[0m")
    tryCatch({
      results_list[[ii]] <- read_csv(result_filepath, show_col_types = FALSE)
      message("\033[32m", "[INFO] File contained ", nrow(results_list[[ii]]), " rows.\n", "\033[0m")
    }, error=function(e) {
      cat("\033[31m", "[ERROR] ✖ Could not read file '", result_filepath, "'.", "\033[0m ", "\n", "   ", e$message, "\n", sep = "")
    })
  } else {
    cat("\033[33m", "[WARNING] No report found in '", dirname, "'.\n", "\033[0m", sep = "")
  }

}

log_message("\033[32m", "\n[INFO] Found ", length(results_list), " results files in '", output_dir, "' folder.", "\033[0m", file = logfile)

# Collect each result into a single table
data_results <- bind_rows(results_list) |> 
  mutate(file_name = paste0(ID, '.gt3x')) |> 
  left_join(file_info |> select(file_name, patient_id, timepoint, diagnosis), by=join_by(file_name)) |> 
  select(patient_id, timepoint, diagnosis, file_name, everything(), -c(ID))

log_message("\033[32m", "\n[INFO] Final dataset for '", study_name,"' contains ", nrow(data_results), " rows'.", "\033[0m", file = logfile)


# save the data in a .csv file
output_filepath <- file.path(output_dir, paste0('actigraph_data_', study_name, '_', format(Sys.Date(), "%Y-%m-%d"), '.csv'))

tryCatch( {
  write_csv(data_results, file = output_filepath)
  log_message("\033[32m", "\n\n[INFO] ✔ Successfully wrote results to '", output_filepath, "'.\n", "\033[0m", file = logfile)
},
  error = function(e) {
    if (grepl("Cannot open file", e$message)) {
      backup_filepath <- sub("\\.csv$", paste0("_backup_", format(Sys.time(), "%H.%M%P"), ".csv"), output_filepath)
      log_message("\033[33m", "[WARNING] Cannot access file because it is locked by another application. Writing to: '", backup_filepath, "'.\n", "\033[0m", file=logfile)
      write_csv(data_results, backup_filepath)
    } else {
      stop(e)
    }
  }
)

# save the data in an .rds file
rds_filepath <- file.path(output_dir, paste0('actigraph_data_', study_name, '_', format(Sys.Date(), "%Y-%m-%d"), '.rds'))
saveRDS(data_results, file = rds_filepath)

log_message("\033[32m", "\n\n[INFO] ** Finished running M2I Actimetric on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ' ** \n\n', file = logfile)
