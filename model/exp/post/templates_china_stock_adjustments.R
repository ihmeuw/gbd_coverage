r_shell <- "/ihme/singularity-images/rstudio/shells/execRscript.sh -s"



resources_task_apply_china_stock <- list(
  "cores"  =1L,
  "queue"  ='all.q',
  "runtime"='60',
  "memory" ="300M"
)


resources_task_rake_to_stock_adjusted <- list(
  "cores"  =8L,
  "queue"  ='all.q',
  "runtime"='900',
  "memory" ="3G"
)


resources_task_back_calculate_ratio_draws <- list(
  "cores"  =1L,
  "queue"  ='all.q',
  "runtime"='60', 
  "memory" ="300M"
)

resources_task_delete_numerator_draws <- list(
  "cores"  =1L,
  "queue"  ='all.q',
  "runtime"='60', 
  "memory" ="200M"
)

resources_task_save_confirmation <- list(
  "cores"  =1L,
  "queue"  ='all.q',
  "runtime"='200', 
  "memory" ="200M"
)


resources_task_china_prep_draws <- list(
  "cores"  =2L,
  "queue"  ='all.q',
  "runtime"='360', 
  "memory" ="15G"
)


template_task_apply_china_stock <- jobmonr::task_template(
  tool            =tool,
  template_name   =paste0("task_apply_china_stock"),
  command_template=("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task apply_china_stock_adjustment --code_root {code_root} --results_root {results_root} --gbd_cycle {gbd_cycle} --vaccine {vaccine} --run_date {run_date} --location_id {location_id} --year_end {year_end} --output_file {output_file}"), 
  node_args       = list("code_root", "results_root", "gbd_cycle", "vaccine", "run_date", "location_id", "year_end", "output_file"),
  op_args         =list("nthreads", "r_shell", "scriptname") 
)

template_task_rake_to_stock_adjusted <- jobmonr::task_template(
  tool            =tool,
  template_name   =paste0("task_rake_to_stock_adjusted"),
  command_template=("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task rake_to_stock_adjusted --code_root {code_root} --results_root {results_root} --gbd_cycle {gbd_cycle} --vaccine {vaccine} --run_date {run_date} --year_end {year_end} --output_file {output_file} --fhs_run_TF {fhs_run_TF} --location_set_id {location_set_id} --release_id {release_id}"), 
  node_args       = list("code_root", "results_root", "gbd_cycle", "vaccine", "run_date", "year_end", "output_file", "fhs_run_TF", "location_set_id", "release_id"),
  op_args         =list("nthreads", "r_shell", "scriptname") 
)

template_task_back_calculate_ratio_draws <- jobmonr::task_template(
  tool            =tool,
  template_name   =paste0("task_back_calculate_ratio_draws"),
  command_template=("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task back_calculate_ratio --code_root {code_root} --results_root {results_root} --vaccine {vaccine} --vaccine_ratio {vaccine_ratio} --run_date {run_date} --location_id {location_id} --output_file {output_file}"), 
  node_args       = list("code_root", "results_root", "vaccine", "vaccine_ratio", "run_date", "location_id", "output_file"),
  op_args         =list("nthreads", "r_shell", "scriptname") 
)

template_task_delete_numerator_draws <- jobmonr::task_template(
  tool            =tool,
  template_name   =paste0("task_delete_numerator_draws"),
  command_template=("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task delete_numerator_draws --code_root {code_root} --results_root {results_root} --gbd_cycle {gbd_cycle} --vaccine {vaccine} --run_date {run_date} --output_file {output_file}"), 
  node_args       = list("code_root", "results_root", "gbd_cycle", "vaccine", "run_date", "output_file"),
  op_args         =list("nthreads", "r_shell", "scriptname") 
)

template_task_save_confirmation <- jobmonr::task_template(
  tool            =tool,
  template_name   =paste0("task_save_confirmation"),
  command_template=("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task save_confirmation --code_root {code_root} --results_root {results_root} --confirmation_file {confirmation_file} --output_file {output_file}"), 
  node_args       = list("code_root", "results_root", "confirmation_file", "output_file"),
  op_args         =list("nthreads", "r_shell", "scriptname") 
)

template_task_china_prep_draws <- jobmonr::task_template(
  tool            =tool,
  template_name   =paste0("task_china_prep_draws"),
  command_template=("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task prep_draws --code_root {code_root} --results_root {results_root} --me {me} --draw_root {draw_root} --save_draws {save_draws} --save_collapsed {save_collapsed} --me_db {me_db} --offset_remove {offset_remove} --run_date {run_date} --files_to_check {files_to_check} --output_file {output_file}"), 
  node_args       = list("code_root", "results_root", "me", "draw_root", "save_draws", "save_collapsed", "me_db", "offset_remove", "run_date", "files_to_check", "output_file"),
  op_args         =list("nthreads", "r_shell", "scriptname") 
)