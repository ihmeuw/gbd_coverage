r_shell <- "/ihme/singularity-images/rstudio/shells/execRscript.sh -s"



resources_task_prep_ratios <- list(
  "cores"  =8L,
  "queue"  ='all.q',
  "runtime"="800",
  "memory" ="35G"
)


resources_task_prep_draws <- list(
  "cores"  =8L,
  "queue"  ='all.q',
  "runtime"="400",
  "memory" ="15G"
)


resources_covid_adjustment <- list(
  "cores"  =8L,
  "queue"  ='all.q',
  "runtime"="600",
  "memory" ="15G"
)


resources_covid_adj_ratios <- list(
  "cores"  =8L,
  "queue"  ='all.q',
  "runtime"="1000",
  "memory" ="30G"
)


resources_dpt1_coverage <- list(
  "cores"  =8L,
  "queue"  ='all.q',
  "runtime"="400",
  "memory" ="20G"
)


resources_task_force_zero <- list(
  "cores"  =1L,
  "queue"  ='all.q',
  "runtime"="60", 
  "memory" ="300M"
)


resources_task_send_message <- list(
  "cores"  =1L,
  "queue"  ="all.q",
  "runtime"="30",
  "memory" ="150M"
)


template_task_prep_draws <- jobmonr::task_template(
  tool                =tool,
  template_name       =paste0("task_prep_draws"),
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task prep_draws --code_root {code_root} --results_root {results_root} --me {me} --me_names_epi_intro {me_names_epi_intro} --draw_root {draw_root} --save_draws {save_draws} --save_collapsed {save_collapsed} --me_db {me_db} --offset_remove {offset_remove} --run_date {run_date} --output_file {output_file}"),
  node_args           = list("code_root", "results_root", "me", "me_names_epi_intro", "draw_root", "save_draws", "save_collapsed", "me_db", "offset_remove", "run_date", "output_file"),
  op_args             =list("nthreads", "r_shell", "scriptname") 
)

template_task_covid_adjustment <- jobmonr::task_template(
  tool                =tool,
  template_name       =paste0("task_covid_adjustment"),
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task covid_adjustment --code_root {code_root} --me {me} --results_root {results_root} --offset_remove {offset_remove} --stockout_ratios {stockout_ratios} --run_date {run_date} --output_file {output_file} --ndraws {ndraws} --input_root {input_root}"),
  node_args           = list("code_root", "me", "results_root", "offset_remove", "stockout_ratios", "run_date", "output_file", "ndraws", "input_root"),
  op_args             =list("nthreads", "r_shell", "scriptname") 
)

template_task_prep_dpt_ratio <- jobmonr::task_template(
  tool                =tool,
  template_name       =paste0("task_prep_dpt_ratio"),
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task prep_ratios --code_root {code_root} --results_root {results_root} --me {me} --run_date {run_date} --draw_root {draw_root} --save_draws {save_draws} --save_collapsed {save_collapsed} --save_ratio_draws {save_ratio_draws} --offset_remove {offset_remove} --stockout_ratios {stockout_ratios} --output_file {output_file}"),
  node_args           = list("code_root", "results_root", "me", "run_date", "draw_root", "save_draws", "save_collapsed", "save_ratio_draws", "offset_remove", "stockout_ratios", "output_file"),
  op_args             =list("nthreads", "r_shell", "scriptname") 
)

template_task_prep_ratios <- jobmonr::task_template(
  tool                =tool,
  template_name       =paste0("task_prep_ratios"),
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task prep_ratios --code_root {code_root} --results_root {results_root} --me {me} --run_date {run_date} --draw_root {draw_root} --save_draws {save_draws} --save_collapsed {save_collapsed} --save_ratio_draws {save_ratio_draws} --offset_remove {offset_remove} --stockout_ratios {stockout_ratios} --output_file {output_file}"),
  node_args           = list("code_root", "results_root", "me", "run_date", "draw_root", "save_draws", "save_collapsed", "save_ratio_draws", "offset_remove", "stockout_ratios", "output_file"),
  op_args             =list("nthreads", "r_shell", "scriptname") 
)

template_task_covid_adj_dpt_ratios <- jobmonr::task_template(
  tool                =tool,
  template_name       =paste0("task_covid_adj_dpt_ratios"),
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task covid_adj_dpt_ratios --code_root {code_root}  --results_root {results_root} --me {me} --ndraws {ndraws} --save_collapsed {save_collapsed} --offset_remove {offset_remove} --run_date {run_date} --s1_stockout_ratios {s1_stockout_ratios} --s1_cascade_run_date {s1_cascade_run_date} --output_file {output_file} --input_root {input_root}"),
  node_args           = list("code_root", "results_root", "me", "ndraws", "save_collapsed", "offset_remove", "run_date",  "s1_stockout_ratios", "s1_cascade_run_date", "output_file", "input_root"),
  op_args             =list("nthreads", "r_shell", "scriptname") 
)

template_task_covid_adj_ratios <- jobmonr::task_template(
  tool                =tool,
  template_name       =paste0("task_covid_adj_ratios"),
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task covid_adj_ratios --code_root {code_root}  --results_root {results_root} --me {me} --ndraws {ndraws} --save_collapsed {save_collapsed} --offset_remove {offset_remove} --run_date {run_date} --s1_stockout_ratios {s1_stockout_ratios} --s1_cascade_run_date {s1_cascade_run_date} --output_file {output_file} --input_root {input_root}"),
  node_args           = list("code_root", "results_root", "me", "ndraws", "save_collapsed", "offset_remove", "run_date",  "s1_stockout_ratios", "s1_cascade_run_date", "output_file", "input_root"),
  op_args             =list("nthreads", "r_shell", "scriptname") 
)


template_task_dpt1_coverage <- jobmonr::task_template(
  tool                =tool,
  template_name       =paste0("task_dpt1_coverage"),
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task dpt1_coverage --code_root {code_root} --gbd_cycle {gbd_cycle} --me {me} --results_root {results_root} --run_date {run_date} --output_file {output_file}"),
  node_args           = list("code_root", "results_root", "gbd_cycle", "run_date", "me", "output_file"),
  op_args             =list("nthreads", "r_shell", "scriptname") 
)

template_task_force_zero <- jobmonr::task_template(
  tool                =tool,
  template_name       =paste0("task_force_zero"),
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task force_zero --code_root {code_root} --results_root {results_root} --me {me} --type {type} --collapsed_estimate_path_root {collapsed_estimate_path_root} --output_file {output_file}"),
  node_args           = list("code_root", "results_root", "me", "type", "collapsed_estimate_path_root", "output_file"),
  op_args             =list("nthreads", "r_shell", "scriptname") 
)

template_send_message <- jobmonr::task_template(
  tool                =tool,
  template_name       =paste0("send_message"),
  command_template    =("PYTHONPATH= OMP_NUM_THREADS={nthreads} {r_shell} {scriptname} --task send_message --code_root {code_root} --results_root {results_root} --message {message} --output_file {output_file}"),
  node_args           = list("code_root", "results_root", "message", "output_file"),
  op_args             =list("nthreads", "r_shell", "scriptname") 
)


set_default_template_resources(
  resources           =resources_task_rake_draws,
  task_template       =template_task_rake_draws,
  default_cluster_name="slurm"
)
