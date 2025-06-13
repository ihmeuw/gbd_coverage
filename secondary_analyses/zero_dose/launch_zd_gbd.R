


r_lib_team_dir <- "FILEPATH"
withr::with_libpaths(new = r_lib_team_dir, code = {se <- asNamespace("INDIVIDUAL_NAME")})
if(!interactive()){
   
   message("Starting arg-parser.")
   se$parse_all_named_cli_args(required_args = list(code_root = "character"))
} else {
   if(!is.na(Sys.getenv()['CODE_ROOT'])){ 
      code_root <- Sys.getenv()['CODE_ROOT']
   } else {
      code_root <- file.path("FILEPATH", Sys.info()[["user"]], "vaccines") 
   }
   message(code_root)
}
message(code_root)

se$submit_job(
   script_path         = file.path(code_root, "FILEPATH/zd_gbd.R")
   , threads              = 5L
   , mem                  = "16G"
   , runtime_min          = 30L
   , partition            = "all.q,long.q"
   , account              = "proj_cov_vpd"
   , console_style_log_tf = TRUE
   , send_email           = TRUE
   , args_list            = list(
      code_root = code_root
   )
)
