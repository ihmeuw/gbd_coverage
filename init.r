


if(!exists("code_root", envir = globalenv())){
   
   message(
      "
      init.r expects `code_root` to exist in global environment.
      We're not redefining it at the moment to avoid potentially conflicting definitions.
      If desireable in the future, please set in init.r as:

      `code_root <- file.path('FILEPATH', Sys.info()[['user']], 'vaccines')`"
   )
   
}

message("
Initializing pipeline paths and constants.
        
   code_root = ", code_root, "\n"
)



source(file.path(code_root, "init_list.r"))



init_list_load(init_list)



message("init.r : Loading packages and functions.\n")
.libs <- c(
   "data.table"
   , "parallel"
   , "jsonlite"
   , "dplyr"
   , "tidyr"
   , "magrittr"
   , "purrr"
   , "stringr"
   , "glue"
   , "ggplot2"
   , "ggrepel"
   , "patchwork"
)
for(.i in .libs){
   message("   ", .i)
   library(.i, character.only = TRUE)
}
if(exists("r_lib_team_dir", envir = globalenv())) {
   message("   vmTools")
   library(vmTools, lib.loc = r_lib_team_dir) 
} 

message("\nCC Functions\n")
.cc_funcs <- c(
   "get_location_metadata.R"
   , "get_ids.R"
   , "get_population.R"
   , "get_covariate_estimates.R"
)
for(.i in .cc_funcs){
   message("   ", .i)
   source(file.path(cc_r_lib_root, .i))
}
message("\nPipeline Functions\n")
.pipeline_funcs <- c(
   "get_location_hierarchy.R"
   , "path_clean.R"
   , "nin.r"
   , "assertions.R"
   , "defactor_vector.R"
   , "logit_calcs.R"
   , "utils.R"
   , "utils_io.R"
)
for(.i in .pipeline_funcs){
   message("   ", .i)
   source(file.path(code_root, "vaccination_pipeline_functions", .i))
}

rm(.i, .libs, .cc_funcs, .pipeline_funcs)
message("\ninit.r : Done")

