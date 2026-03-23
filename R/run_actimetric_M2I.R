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

# TODO: Create report of failed/successes
# TODO: Skip already run files.

library(actimetricModels)
library(actimetric)

library(tidyverse)  # `install.packages('tidyverse')` if not already installed


# ---- SETUP ---- #
# Define all the information needed to run the script

study_name = "M2I"  # used for bookkeeping of outputs. Might be useful if you want to keep separate versions of results (e.g. M2I_jan2026, M2I_noCP, etc.). Using the same study_name (i.e. M2I) will just overwrite the previous results
input_dir = 'data-raw'  # location of .gt3x files
output_dir = 'data-raw/Actigraph/Outputs'  # location of where to store outputs
classifier = 'School age Wrist Random Forest'
create_visual_report = FALSE  # TRUE/FALSE


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
message("\033[34m", "[INFO] Extracting information from file names.", "\033[34m")
for (file in all_files) {
  ## Get the Patient ID
  patient_id0 <- sub("^((M2I)?[A-Z][A-Z0-9]*).*", "\\1", file) # just extract the patient number
  patient_id <- ifelse(startsWith(patient_id0, "M"), patient_id0, paste0("M2I", patient_id0))
  
  ## Get the Timepoint info
  timepoint0 <- sub("^[^ -]+[ -]?", "", file) |> sub("^-", "", x = _) |> trimws()
  if (timepoint0 == ''){
    timepoint = 'pre' # if empty, this is the initial file
  } else {
    timepoint = sub("\\.[^.]*$", "", timepoint0) # removes the file extension
  }

  # Get the Diagnosis
  if (startsWith(patient_id, 'M2I')) {
    diagnosis = substr(patient_id, 4, 5)
  } else {
    diagnosis = NA_character_
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

file_info <- arrange(file_info, patient_id)
for (ii in seq(1, nrow(file_info))) {
  file_path <- paste0(input_dir, '/', file_info$file_name[ii])
  message("\033[34m", "[INFO] Processing '", file_info$file_name[ii], "' (", file_info$file_size[ii], ").", "\033[0m")
    
  tryCatch({
    if (file_info$diagnosis[ii] == 'CP') {
      message("\033[33m", "[WARNING] CP Model not yet implemented - skipping file.\n", "\033[0m")
      # message("\033[36m", "[INFO] >>> Running Actimetric - Classifier: ", classifier , " (CP)", "\033[0m")
      # runActimetric(
      #   input_directory = file_path,
      #   output_directory = paste0(output_dir, '/', file_info$patient_id[ii]),
      #   classifier = classifier,
      #   visualreport = create_visual_report,
      #   studyname = study_name,
      #   verbose = FALSE  # suppress extra information from runActimetric
      # )
    } else {
      message("\033[0m", "[INFO] >>> ID: ", file_info$patient_id[ii], " (", file_info$diagnosis[ii] ,") - ", file_info$timepoint[ii], "\033[0m")
      message("\033[0m", "[INFO] >>> Running Actimetric - Classifier: '", classifier, "'", "\033[0m")
      runActimetric(
        input_directory = paste0(input_dir, '/', file_info$file_name[ii]),
        output_directory = paste0(output_dir, '/', file_info$patient_id[ii]),
        classifier = classifier,
        visualreport = create_visual_report,
        studyname = study_name,
        verbose = FALSE 
      )
      message("\033[32m", "\n[INFO] ✔ Successfully processed '", file_info$file_name[ii], "'.", "\033[0m", '\n\n')
    }

  }, error = function(e) {
    cat("\033[31m", "[ERROR] ✖ Failed to process '", file_info$file_name[ii], "'.", "\033[0m ", "\n", "   ", e$message, "\n\n", sep = "")
  })

}

# ---- COLLATE RESULTS ---- #
## From all the result files generated, now collate these all together into a single file.
study_dirname = paste0('output_', study_name)
results_list <- list()
all_dirs <- list.dirs(output_dir, recursive = FALSE)
for (ii in seq_along(all_dirs)) {
  dirname <- all_dirs[ii]
  result_filepath <- file.path(dirname, study_dirname, 'results', 'personlevel_report.csv') 
  if (file.exists(result_filepath)) {
    message("\033[34m", "[INFO] Results file found in '", dirname, "'.", "\033[0m")
    tryCatch({
      results_list[[ii]] <- read_csv(result_filepath, show_col_types = FALSE)
      message("\033[32m", "[INFO] File contained ", nrow(results_list[[ii]]), " rows.", "\033[0m")
    }, error=function(e) {
      cat("\033[31m", "[ERROR] ✖ Could not read file '", result_filepath, "'.", "\033[0m ", "\n", "   ", e$message, "\n", sep = "")
    })
  } else {
    cat("\033[33m", "[WARNING] No report found in '", dirname, "'.\n", "\033[0m", sep = "")
  }

}

data_results <- bind_rows(results_list)  # actually collect each result into a single table

# save the file
output_filepath <- file.path(output_dir, paste0('actigraph_data_', study_name, '_', format(Sys.Date(), "%Y-%m-%d"), '.csv'))
write_csv(data_results, file = output_filepath)
