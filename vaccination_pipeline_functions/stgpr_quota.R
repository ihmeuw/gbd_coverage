










































#' @param me_ids [int] e.g. 25722L - modelable entity ids (call `load_vacc_model_db()` by section to access these easily)
#' @param release_id [int] e.g. 16L - GBD release id 
#' @param n_draws [int] e.g. c(100, 1000) - subset output list to number of draws you care about

#' @return [list] 2-level list of data.tables; level 1 = me_id, 2 = n_draws
#' @export

#' @examples
stgpr_quota_query <- function(me_ids, release_id, n_draws = NULL){
   
   
   stopifnot(is.numeric(me_ids))
   stopifnot(is.numeric(release_id))
   stopifnot(is.null(n_draws) | is.numeric(n_draws))
   
   n_draws <- sort(unique(n_draws))
   
   
   
   
   quota_table <- data.table::data.table(
      gpr_draws = c( 0, 100, 1000)
      , quota   = c(50,  50,   15)
   )
   
   if(!all(n_draws %in% quota_table$gpr_draws)){
      stop("all n_draws must match internal quota table \n\n",
           paste(capture.output(quota_table), collapse = "\n")
      )
   }
   
   
   if (
      !exists("stgpr", envir = globalenv()) ||
      !is.environment(stgpr) ||
      !is.function(stgpr$get_stgpr_versions)
   ) {
      source("FILEPATH/public.R")
   }
   
   
   names(me_ids) <- me_ids
   
   vars_keep <- c(
      "modelable_entity_id",
      "modelable_entity_name",
      "stgpr_version_id",
      "release_id",
      "gpr_draws",
      "model_status_name",
      "user",
      "date_inserted_as_created"
   )
   
   mods <- lapply(me_ids, function(me_id){
      tryCatch(
         {
            data.table::as.data.table(
               stgpr$get_stgpr_versions(modelable_entity_id = me_id, release_id = release_id)
            )[, ..vars_keep]
         }, error = function(e) {
            message("No models found for modelable entity id: ", me_id)
         }
      )
   })
   
   
   mods <- Filter(data.table::is.data.table, mods)
   
   
   mods_by_draw <- lapply(mods, function(mod){
      N_draws        <- sort(unique(mod$gpr_draws))
      names(N_draws) <- as.character(N_draws)
      ret_list       <- lapply(N_draws, function(Ndraw){
         
         quota_row   <- quota_table[gpr_draws == Ndraw, ]
         quota_ndraw <- quota_row$quota
         
         mod <- mod[gpr_draws == Ndraw, ]
         mod[, mod_count := .N]
         mod[, quota_limit := quota_ndraw]
         
         max_i <- max(mod$mod_count)
         
         merge(mod, quota_row, by = "gpr_draws", all.x = TRUE)
         mod[, quota_reached := max_i == quota_ndraw]
      })
   })
   
   
   n_draw_labels <- c("0_100", "1000")
   
   mods_by_draw <- lapply(mods_by_draw, function(mod){
      mod[["0_100"]] <- rbindlist( list(mod[["0"]], mod[["100"]]) )
      if (nrow(mod[["0_100"]]) == 0) {
         mod[["0_100"]] <- NULL
      } else {
         mod[["0_100"]][, mod_count := .N]
         mod[["0_100"]][, quota_reached := mod_count == quota_limit][] 
      }
      return(mod[n_draw_labels])
   })
   
   mods_by_draw <- lapply(mods_by_draw, function(mod){
      names(mod) <- n_draw_labels
      return(mod)
   })
   
   return(mods_by_draw)
   
}





#' @param me_ids [int] e.g. 25722L - modelable entity ids (call `load_vacc_model_db()` by section to access these easily)
#' @param release_id [int] e.g. 16L - GBD release id 
#' @param n_draws [int] e.g. c(100, 1000) - subset output list to number of draws you care about

#' @return invisible - results of `stgpr_quota_query()`
#' @export

#' @examples
stgpr_quota_assert <- function(me_ids, release_id, n_draws){
   
   if(length(me_ids) == 0 || is.null(me_ids)) stop("No me_ids submitted - inspect.")
   if(length(n_draws) != 1L) stop("n_draws must be a single integer")
   valid_n_draws <- c(0L, 100L, 1000L)
   if(!n_draws %in% valid_n_draws) stop("n_draws must be one of: ", toString(valid_n_draws))
   
   
   mods_by_draw <- stgpr_quota_query(me_ids = me_ids, release_id = release_id, n_draws = n_draws)
   
   
   if(n_draws %in% c(0, 100)) n_draws <- "0_100"
   mods_by_draw_to_check              <- lapply(mods_by_draw, function(mod) mod[n_draws])
   
   
   if(n_draws %in% c(0, 100)) n_draws <- "0_100"
   mods_by_draw_to_check              <- lapply(mods_by_draw, function(mod) mod[n_draws])
   
   
   
   quota_check_list <- purrr::map_depth(mods_by_draw_to_check, .depth = 2, ~ unique(.x$quota_reached))
   quota_reached    <- unlist(quota_check_list)
   quota_reached    <- quota_reached[quota_reached]
   
   if(any(quota_reached)){
      names(quota_reached) <- paste0(names(quota_reached), "_draws")
      assign(x = "STGPR_QUOTA_LIST", value = mods_by_draw, envir = globalenv())
      
      stop("You have reached the stgpr quota for the following 'me_id.n_draw' modelable entity ids.\n  ",  
           "See `STGPR_QUOTA_LIST` which was just assigned to the global environment. \n\n",
           paste(capture.output(quota_reached), collapse = "\n"))
   }
   
   message("me_ids passing ST-GPR quota check: ", paste(unique(names(mods_by_draw)), collapse = ", "))
   
   return(mods_by_draw)
   
}








#' @param launch_bias_correction [lgl] config param
#' @param run.stockout [lgl] config param
#' @param ratio_stockouts [dbl] config param
#' @param launch_dpt1_dpt3_dropout [lgl] config param
#' @param mes_to_launch [chr] list of vaccine names e.g. c("vacc_mcv1", "vacc_dpt3")
#' @param release_id [int] GBD release id from init.r
#' @param n_draws_coverage [int] config param - number of draws to check
#' @param root_quota_log [path] path to save quota log to, e.g. 'FILEPATH/log_stgpr_quota.csv'

#' @return [list] - list produced by `stgpr_quota_assert()`, plus a final


#' @export

#' @examples
stgpr_check_launch_quotas <- function(launch_bias_correction
                                      , run.stockout
                                      , ratio_stockouts
                                      , launch_dpt1_dpt3_dropout
                                      , mes_to_launch
                                      , release_id
                                      , n_draws_coverage
                                      , root_quota_log){
   
   source("FILEPATH/get_ids.R")
   
   vmdb <- data.table()
   vmdb_0 <- data.table() 
   
   suppressMessages({
      if(launch_bias_correction) {
         vmdb_0 <- rbind(vmdb_0, load_vacc_model_db(section = "ADMIN_BIAS_MODEL_LAUNCH"))
      }
      if(run.stockout) {
         vmdb_0 <- rbind(vmdb_0, load_vacc_model_db(section = "STOCKOUT_MODEL_LAUNCH_STRAIGHT"))
      }
      if(run.stockout & ratio_stockouts) {
         vmdb_0 <- rbind(vmdb_0, load_vacc_model_db(section = "STOCKOUT_MODEL_LAUNCH_RATIOS"))
      }
      if(launch_dpt1_dpt3_dropout) {
         vmdb_0 <- rbind(vmdb_0, load_vacc_model_db(section = "DPT_DROPOUT_MODEL_LAUNCH"))
      }
      if(length(mes_to_launch)){
         vmdb <- rbind(vmdb, load_vacc_model_db(section = "MAIN_COVERAGE_MODEL_LAUNCH")[me_name %in% mes_to_launch, ])
      }
   })
   
   
   
   
   
   if(nrow(vmdb_0))  {
      message("Asserting 0-100 draw model quotas")
      quota_list_0   <- stgpr_quota_assert(me_ids = vmdb_0$modelable_entity_id, release_id = release_id, n_draws = 0L)
   } else {
      quota_list_0   <- NULL
   }
   
   if(nrow(vmdb)){ 
      message("\nAsserting 1000 draw model quotas")
      quota_list_mod <- stgpr_quota_assert(me_ids = vmdb$modelable_entity_id, release_id = release_id, n_draws = n_draws_coverage)
   } else {
      quota_list_mod <- NULL
   }
   quota_list <- c(quota_list_0, quota_list_mod)
   if(!length(quota_list)) return()
   
   to_delete_vec <- integer()
   quota_table   <- data.table(
      me_id           = integer(),
      quota_remaining = integer(),
      quota_draws     = integer()
   )
   message("") 
   
   
   for(idx in seq_along(quota_list)){
      me_id_x <- as.integer(names(quota_list[idx]))
      quota_list[[idx]]
      
      for(n_draws in names(quota_list[[idx]])){
         
         x <- quota_list[[idx]][[n_draws]]
         
         
         
         if(is.null(x)) {
            message(me_id_x, " is empty for ", n_draws, " draws")
            next
         }
         
         remainder   <- unique(x$quota_limit - x$mod_count)
         
         quota_table <- rbind(
            quota_table,
            data.table(me_id           = me_id_x,
                       quota_remaining = remainder,
                       quota_draws     = n_draws),
            fill = TRUE
         )
         
         if(remainder <= 5){
            
            to_delete_vec <- c(to_delete_vec, me_id_x)
            
            quota_msg <- paste0(
               "You have ", remainder, " quota spots remaining for "
               , me_id_x 
               , " at ", n_draws, " draws : "
               , unique(x$modelable_entity_name)
            )
            message(quota_msg)
            
         }
      }
      
   }
   
   dt_me_id    <- get_ids("modelable_entity")
   quota_table <- merge(quota_table, dt_me_id[, .(modelable_entity_id, modelable_entity_name)], by.x = "me_id", by.y = "modelable_entity_id")
   setnames(quota_table, "modelable_entity_name", "me_name")
   setorderv(quota_table, c("quota_draws", "quota_remaining"))
   setcolorder(quota_table, c("me_id", "me_name", "quota_remaining", "quota_draws"))
   
   path_quota_log <- file.path(root_quota_log, "log_stgpr_quotas.csv")
   fwrite(quota_table, path_quota_log)
   
   if(length(to_delete_vec)) message("\nSee [['me_ids_near_quota']] in returned list for modelable entity ids to delete.")
   
   return(append(quota_list, list(me_ids_near_quota = to_delete_vec)))
   
}
