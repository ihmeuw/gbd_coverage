r_shell <- "/ihme/singularity-images/rstudio/shells/execRscript.sh -s"



resources_fhs_resample <- list(
   "cores"   = 8L,
   "queue"   = 'all.q',
   "runtime" = "800", 
   "memory"  = "20G"
)

resources_fhs_straight <- list(
   "cores"   = 8L,
   "queue"   = 'all.q',
   "runtime" = "800", 
   "memory"  = "20G"
)

resources_fhs_fixes <- list(
   "cores"   = 10L,
   "queue"   = 'all.q',
   "runtime" = "800",
   "memory"  = "20G"
)

resources_fhs_ratio <- list(
   "cores"   = 4L,
   "queue"   = 'all.q',
   "runtime" = "800",
   "memory"  = "12G"
)




template_task_fhs_resample <- jobmonr::task_template(
   tool                = tool,
   template_name       = paste0("task_fhs_resample"),
   command_template    = ("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task fhs_resample --code_root {code_root} --results_root {results_root} --me {me} --draws_read_root {draws_read_root} --draws_save_root {draws_save_root} --fhs_path_dpt3 {fhs_path_dpt3} --fhs_path_mcv1 {fhs_path_mcv1} --fhs_n_draws {fhs_n_draws} --n_draws_vc {n_draws_vc} --output_file {output_file}"),
   node_args           = list("code_root", "results_root", "me", "draws_read_root", "draws_save_root", "fhs_path_dpt3", "fhs_path_mcv1", "fhs_n_draws", "n_draws_vc", "output_file"),
   op_args             = list("nthreads", "r_shell", "scriptname") 
)

template_task_fhs_straight <- jobmonr::task_template(
   tool                = tool,
   template_name       = paste0("task_fhs_straight"),
   command_template    = ("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task fhs_straight --code_root {code_root} --results_root {results_root} --draws_read_root {draws_read_root} --draws_save_root {draws_save_root} --run_date {run_date} --year_end_gbd {year_end_gbd} --fhs_path_dpt3 {fhs_path_dpt3} --fhs_path_mcv1 {fhs_path_mcv1} --fhs_release_id {fhs_release_id} --fhs_location_set_id {fhs_location_set_id} --release_id_gbd {release_id_gbd} --location_set_id_gbd {location_set_id_gbd} --n_cores {n_cores} --output_file {output_file}"),
   node_args           = list("code_root", "results_root", "draws_read_root", "draws_save_root", "run_date", "year_end_gbd", "fhs_path_dpt3", "fhs_path_mcv1", "fhs_release_id", "fhs_location_set_id", "release_id_gbd", "location_set_id_gbd", "n_cores", "output_file"),
   op_args             = list("nthreads", "r_shell", "scriptname") 
)

template_task_fhs_fixes <- jobmonr::task_template(
   tool                = tool,
   template_name       = paste0("task_fhs_fixes"),
   command_template    = ("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task fhs_fixes --code_root {code_root} --results_root {results_root} --draws_read_root {draws_read_root} --draws_save_root {draws_save_root} --run_date {run_date} --year_end_gbd {year_end_gbd} --fhs_release_id {fhs_release_id} --fhs_location_set_id {fhs_location_set_id} --release_id_gbd {release_id_gbd} --location_set_id_gbd {location_set_id_gbd} --output_file {output_file}"),
   node_args           = list("code_root", "results_root", "draws_read_root", "draws_save_root", "run_date", "year_end_gbd", "fhs_release_id", "fhs_location_set_id", "release_id_gbd", "location_set_id_gbd", "output_file"),
   op_args             = list("nthreads", "r_shell", "scriptname")
)

template_task_fhs_ratio <- jobmonr::task_template(
   tool                = tool,
   template_name       = paste0("task_fhs_ratio"),
   command_template    = ("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task fhs_ratio --me {me} --run_date {run_date} --output_file {output_file}"),
   node_args           = list("me", "run_date", "output_file"),
   op_args             = list("nthreads", "r_shell", "scriptname") 
)

