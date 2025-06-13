r_shell <- "/ihme/singularity-images/rstudio/shells/execRscript.sh -s"


resources_task_unrake_draws <- list(
  "cores"  =2L,
  "queue"  ='all.q',
  "runtime"="300", 
  "memory" ="1G"
)


resources_task_unrake_failed <- list(
  "cores"  =1L,
  "queue"  ='all.q',
  "runtime"="400", 
  "memory" ="600M"
)


resources_task_rake_draws <- list(
  "cores"  =8L,
  "queue"  ="all.q",
  "runtime"="27000", 
  "memory" ="15G"
)



resources_task_move_draws <- list(
  "cores"  =5L,
  "queue"  ="all.q",
  "runtime"="10000", 
  "memory" ="60G"
)


resources_task_ysi_to_years <- list(
  "cores"  =1L,
  "queue"  ="all.q",
  "runtime"="1000", 
  "memory" ="10G"
)




template_task_unrake_failed <- jobmonr::task_template(
  tool                =tool,
  template_name       ="task_unrake_failed",
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task unrake_failed_draws --code_root {code_root} --results_root {results_root} --run_id {run_id} --location_id {location_id} --intro_year {intro_year} --me_outdir {me_outdir} --output_file {output_file}"), 
  node_args           = list("code_root", "results_root", "run_id", "location_id", "intro_year", "me_outdir", "output_file"),
  op_args             =list("nthreads", "r_shell", "scriptname") 
)


template_task_rake_draws <- jobmonr::task_template(
  tool                =tool,
  template_name       ="task_rake_draws",
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task rake_draws --code_root {code_root} --results_root {results_root} --run_id {run_id} --location_id {location_id} --max_intro_year {max_intro_year} --min_intro_year {min_intro_year} --draw_path {draw_path} --me_outdir {me_outdir} --output_file {output_file}"),
  node_args           = list("code_root", "results_root", "run_id", "location_id", "max_intro_year", "min_intro_year", "draw_path", "me_outdir", "output_file"),
  op_args             =list("nthreads", "r_shell", "scriptname") 
)


template_task_move_draws <- jobmonr::task_template(
  tool                =tool,
  template_name       ="task_move_draws",
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task move_draws --code_root {code_root} --results_root {results_root} --run_id {run_id}  --raked_successfully {raked_successfully} --me_outdir {me_outdir} --input_root {input_root} --output_file {output_file}"),
  node_args           = list("code_root", "results_root", "run_id", "raked_successfully", "me_outdir", "input_root", "output_file"),
  op_args             =list("nthreads", "r_shell", "scriptname") 
)


template_task_ysi_to_years <- jobmonr::task_template(
  tool                =tool,
  template_name       ="task_ysi_to_years",
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task ysi_to_years --code_root {code_root} --results_root {results_root} --vaccine {vaccine} --me_outdir {me_outdir} --ndraws {ndraws} --year_start {year_start} --year_end {year_end} --output_file {output_file}"),
  node_args           = list("code_root", "results_root", "vaccine", "me_outdir", "ndraws", "year_start", "year_end", "output_file"),
  op_args             =list("nthreads", "r_shell", "scriptname")
)



set_default_template_resources(
  resources           =resources_task_rake_draws,
  task_template       =template_task_rake_draws,
  default_cluster_name="slurm"
)