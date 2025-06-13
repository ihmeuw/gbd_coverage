










#' @param dt_vc [data.table] VC team 'best' estimates wide by draws
#' @param fhs_n_draws [int] number of FHS team draws
#' @param vc_n_draws [int] number of VC team draws

#' @return [data.table] VC team draws, up or down-sampled to match the FHS team
#' @export

#' @examples
fhs_draws_upsample_downsample <- function(dt_vc, fhs_n_draws, vc_n_draws){
  
  
  stopifnot(is.integer(fhs_n_draws))
  stopifnot(is.integer(vc_n_draws))
  
  varnames_draws <- grep("draw_", names(dt_vc), value = TRUE)
  varnames_other <- setdiff(names(dt_vc), varnames_draws)
  
  dt_draws <- as.data.table(dt_vc[, ..varnames_draws])
  dt_other <- as.data.table(dt_vc[, ..varnames_other])
  
  vc_draws_int <- as.integer(sub("draw_", "", varnames_draws))
  setcolorder(dt_draws, order(vc_draws_int))
  dt_vc <- as.data.table(cbind(dt_other, dt_draws), sorted = FALSE)
  
  
  draws_mat <- as.matrix(dt_vc[, ..varnames_draws])[, order(vc_draws_int)]
  
  
  set.seed(4321)
  
  if(fhs_n_draws == vc_n_draws) {
    
    
    dt_vc_sampled <- dt_vc
    
  } else if(fhs_n_draws > vc_n_draws){
    
    
    stopifnot(fhs_n_draws %% vc_n_draws == 0)
    message("Upsampling draws from ", vc_n_draws, " to ", fhs_n_draws, " draws.")
    fhs_draw_ratio <- as.integer(fhs_n_draws / vc_n_draws)
    
    sample_cols         <- sample(ncol(draws_mat), size = (fhs_draw_ratio - 1) * ncol(draws_mat), replace = TRUE)
    upsample_mat        <- draws_mat[, sample_cols]
    colnames(upsample_mat) <- paste0("draw_", (ncol(draws_mat)):(ncol(draws_mat) + ncol(upsample_mat) - 1))
    dt_vc_sampled       <- as.data.table(cbind(dt_vc[, ..varnames_other],
                                               draws_mat,
                                               upsample_mat),
                                         sorted = FALSE)
    
  } else {
    
    
    stopifnot(vc_n_draws %% fhs_n_draws == 0)
    message("Downsampling draws from ", vc_n_draws, " to ", fhs_n_draws, " draws.")
    sample_cols <- sample(ncol(draws_mat), size = fhs_n_draws, replace = FALSE)
    
    downsample_dt <- as.data.table(draws_mat[, sample_cols[order(sample_cols)]])
    
    setnames(downsample_dt, paste0("draw_", 0:(fhs_n_draws - 1)))
    dt_vc_sampled <- as.data.table(list(dt_other, downsample_dt), sorted = FALSE)
  }
  
    return(dt_vc_sampled)
  
  }
