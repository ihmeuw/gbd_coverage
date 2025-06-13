



#' @param section [chr] Section of the vaccination model database to load (exact, case-sensitive)
#' @param path_vm_db [path] Path to the vaccination model database
#' @param assert_best [lgl] Select `is_best == 1` and assert one row per me_name? Default: `TRUE`

#' @return [data.table] A data.table of a chosen section of the vaccination model database
#' @export

#' @examples
load_vacc_model_db <- function(
      section     = NULL,
      path_vm_db  = file.path(code_root, "FILEPATH/vaccination_model_db.csv"), 
      assert_best = TRUE
){
   
   
   stopifnot(is.character(path_vm_db))
   stopifnot(file.exists(path_vm_db))
   stopifnot(is.logical(assert_best))
   valid_sections <- c(
      "ADMIN_BIAS_MODEL_LAUNCH_GBD2019",
      "ADMIN_BIAS_MODEL_LAUNCH",
      "ADMIN_BIAS_MODEL_LAUNCH_VIA_BUNDLE",
      "DPT_DROPOUT_MODEL_LAUNCH",
      "MAIN_COVERAGE_MODEL_LAUNCH",
      "MAIN_COVERAGE_MODEL_LAUNCH_VIA_BUNDLE",
      "AGE_SPECIFIC_MODEL_LAUNCH",
      "FORECASTING_MODEL_LAUNCH",
      "OTHER_MODEL_LAUNCH",
      "OTHER_MODEL_LAUNCH_VIA_BUNDLE",
      "STOCKOUT_MODEL_LAUNCH_STRAIGHT",
      "STOCKOUT_MODEL_LAUNCH_RATIOS"
   )
   
   invalid_sections <- c(
      "FORECASTING_MODEL_LAUNCH_DEPRECATED"
   )
   
   err_msg_sections <- paste0("`section` must be one of (copy and paste exactly):\n\n", paste(valid_sections, collapse = "\n"),
                              "\n\n",
                              "Note:\n\n", "The following sections (if any) are ambiguous and not supported: \n   ",
                              paste(invalid_sections, collapse = "\n   "))
   
   if(is.null(section)) stop(err_msg_sections)
   stopifnot(is.character(section))
   if(!section %in% valid_sections) stop(err_msg_sections)
   
   message("Loading ", section, " models from ", path_vm_db)
   vm_db <- data.table::fread(path_vm_db)
   
   
   round_column <- vm_db$round
   sections     <- round_column[grepl("^[A-Z]", round_column)]
   
   
   if(any(duplicated(sections))){
      stop("The following sections are duplicated in the 'round' column:\n",
           "  - Each section must be unique\n\n   ",
           paste(sections[duplicated(sections)], collapse = "\n   "))
   }
   
   if(any(!grepl("^[A-Z][A-Z0-9_]*$", sections))){
      stop("The following sections are invalid in the 'round' column:\n",
           "  - Each section starts with a capital letter and consists of only caps letters, numbers and underscores e.g. ADMIN_BIAS_MODEL_LAUNCH_GBD2023.\n\n   ",
           paste(sections[!grepl("^[A-Z][A-Z0-9_]*$", sections)], collapse = "\n   "))
   }
   
   
   vm_db <- rbind(vm_db, data.table::data.table(round = ""), fill = TRUE)
   
   idx_sections <- which(vm_db$round %in% sections)
   
   idx_blanks   <- which(grepl("^\\s*$", vm_db$round))
   
   
   for(i in idx_sections[-length(idx_sections)]){
      
      idx_next_blank   <- idx_blanks[idx_blanks > i][1]
      idx_next_section <- idx_sections[idx_sections > i][1]
      
      if(idx_next_blank > idx_next_section){
         
         stop("There must be at least one blank row between each section end and the next section start.\n",
              "Please check: ", path_vm_db, "\n",  
              "Place a blank row before: \n\n",
              sections[which(idx_next_section == idx_sections)])
      }
   }
   
   idx_section_start <- which(vm_db$round == section)[1] + 1
   idx_section_end   <- idx_blanks[idx_blanks > idx_section_start][1] - 1
   if(is.na(idx_section_start) || is.na(idx_section_end)){
      stop("Cannot find start and/or end index for: ", section, "\n",
           "Please check: ", path_vm_db)
   }
   
   vm_section <- vm_db[idx_section_start:idx_section_end, ]
   
   if(assert_best){
      vm_section <- vm_section[is_best == 1, ]
      if (nrow(vm_section[duplicated(me_name), ]) > 0) {
         stop(paste0("Multiple models marked 'best' for the same me_name: ",
                     toString(vm_section[duplicated(me_name), me_name])
         ))
      }
   }
   
   
   if(nrow(vm_section) < 1){
      stop("No rows found for section: ", section, "\n",
           "Please check (may need to check is_best column): ", path_vm_db)
   }
   
   return(vm_section)
}
