














#' @param me [chr] e.g. "vacc_dpt1"
#' @param draws_read_root [chr] e.g. "FILEPATH"
#' @param draws_save_root [chr] e.g. "FILEPATH"
#' @param fhs_path_dpt3 [chr] full path to FHS draws for DPT3
#' @param fhs_path_mcv1 [chr] full path to FHS draws for MCV1
#' @param fhs_n_draws [int] number of FHS draws
#' @param n_draws_vc [int] number of GBD draws

#' @return [none] draws are saved to `draws_save_root`, after up/downsampling 
#' @export

#' @examples
fhs_resample_task <- function(
    me,
    draws_read_root,
    draws_save_root,
    fhs_path_dpt3,
    fhs_path_mcv1,
    fhs_n_draws,
    n_draws_vc
){
  
  
  library(parallel)
  
  source("FILEPATH/get_location_metadata.R")
  locs  <- get_location_metadata(location_set_id = location_set_id, release_id = release_id)[level >= 3, ]
  
  library(ncdf4)
  library(ncdf4.helpers)
  library(data.table)
  
  
  
  
  
  fhs_paths <- c(fhs_path_dpt3, fhs_path_mcv1)
  names(fhs_paths) <- basename(fhs_paths)
  fhs_draws <- unlist(
    lapply(fhs_paths, function(fhs_path){
      nc <- nc_open(fhs_path)
      draws <- ncvar_get(nc, "draw")
      nc_close(nc)
      return(length(draws))
    })
  )
  draw_check <- c(fhs_draws, fhs_n_draws = fhs_n_draws)
  if(length(unique(draw_check)) != 1) stop("FHS draw counts do not match each other or config - inspect:\n", toString(prt_multiline(draw_check)))
  
  draw_dir_me <- file.path(draws_read_root, me)
  fpaths_read <- list.files(draw_dir_me, full.names = TRUE)
  names(fpaths_read) <- basename(fpaths_read)
  
  ncores <- Sys.getenv("OMP_NUM_THREADS")
  if (is.null(ncores) || ncores==""){
    ncores <- 2
  }
  
  
  
  dt_list <- mclapply(fpaths_read, fread, mc.cores=ncores)
  draw_check <- c(unlist(lapply(dt_list, function(x) length(grep("draw_", names(x))))), n_draws_vc)
  if(length(unique(draw_check)) != 1) stop("GBD draw counts do not match each other or config - inspect:\n", toString(prt_multiline(c(n_draws_vc = n_draws_vc))))
  
  dt_vc <- rbindlist(dt_list, use.names = TRUE)
  
  
  
  
  
  dt_vc <- fhs_draws_upsample_downsample(dt_vc = dt_vc, fhs_n_draws = fhs_n_draws, vc_n_draws = n_draws_vc)
  
  
  save_draws_general(df = dt_vc, me = me, root = draws_save_root)
  
  message("Done resampling draws for : ", me)
  
}