





file_roots = list(
   
   j_root             = "/snfs1"
   , bmgf_temp        = "FILEPATH"
   , mnt_vacc_root    = "FILEPATH"
   , draws_root       = "FILEPATH"
   , extract_root     = "FILEPATH"
   , ref_data_repo    = "FILEPATH"
   , repo_root        = "FILEPATH"
   , ubcov_central    = "FILEPATH"
   , ubcov_model_root = "FILEPATH"
   , dx_share_root    = "FILEPATH"
   , cc_r_lib_root    = "FILEPATH"
   , data_root        = "FILEPATH"
   , diagnostics_root = "FILEPATH"
   , root             = "FILEPATH"
   , sourcing_root    = "FILEPATH"
   , bundle_root      = "FILEPATH"
)

constants = list(
   username                  = Sys.info()[["user"]]
   , cooper_tracking         = "NUMBER"
   , gbd_cycle               = "gbd2023"
   , location_set_id         = 22L
   , location_set_version_id = 561L
   
   , release_id              = 33L 
   , year_start              = 1980L
   , year_end                = 2024L 
   , year_end_gbd            = 2023L 
)


file_paths = list(
   db_tools = "FILEPATH/db_tools.r"
   , fpath_stgpr_api = "FILEPATH/public.R"
   , fpath_stgpr_utility = "FILEPATH/utility.r"
   , fpath_cc_plot_gpr = "FILEPATH/plot_gpr.R"
   , fpath_cc_gpr_helpers = "FILEPATH/helpers.R"
   , fpath_vc_read_excel = "FILEPATH/read_excel.R"
   , fpath_ubcov_cluster_tools = "FILEPATH/cluster_tools.r"
)












file_roots$to_model_root      <- file.path(file_roots$data_root, "exp/to_model", constants$gbd_cycle)
file_roots$to_model_root_base <- file.path(file_roots$data_root, "exp/to_model")
file_roots$modeled_root       <- file.path(file_roots$data_root, "exp/modeled", constants$gbd_cycle)
file_roots$modeled_root_base  <- file.path(file_roots$data_root, "exp/modeled")
file_roots$draws_root_gbdxx   <- file.path(file_roots$draws_root, "exp", constants$gbd_cycle)
file_roots$secondary_analyses_root <- file.path(file_roots$mnt_vacc_root, "secondary_analyses")
file_roots$publications_root       <- file.path(file_roots$mnt_vacc_root, "publications")
file_roots$uploads_root            <- file.path(file_roots$data_root, "exp/upload")

init_list <- list(
   file_roots = file_roots,
   constants  = constants,
   file_paths = file_paths
   
)



rm(file_roots, constants, file_paths)










#' @param init_list [list] A named list of lists containing objects to load into


#' @return [none] Objects are loaded into global environment
#' @export

#' @examples
init_list_load <- function(init_list){
   message("init.r : Loading `init_list` objects into global environment")
   init_catch <- lapply(
      seq_along(init_list), 
      function(idx){
         
         message("\n", names(init_list)[idx], "\n")
         
         for(name in names(init_list[[idx]])){
            
            message("   ", name, " : ", init_list[[idx]][[name]])
            assign(name, init_list[[idx]][[name]], envir = globalenv())
            
         }
      }
   )
   message("\n") 
   
   
   rm(init_catch)
}