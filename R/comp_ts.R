#' @title irts
#'
#' @description 
#' computes a two dimensional trace surface
#'
#' @param theta_gen
#' theta values for the general dimension
#'
#' @param theta_spec
#' theta values for the specific dimension
#'
#' @param item_params
#' contains: general factor slopes (slope_gen), specific factor slopes (slope_spec),
#' and item threshold(thr), for both grm and dichotomous items.
#'
#' @param ic_index
#' Numeric vector that delineates the correspondence between item and
#' item cluster.
#'
#' @return ts_ic
#'
#' @examples
#'item_params <- list()   
#'item_params[[1]] <- list()   
#'item_params[[1]]["slope_gen"] <- 1.2   
#'item_params[[1]]["slope_spec"] <- 1   
#'item_params[[1]][["thr"]] <- list()   
#'item_params[[1]][["thr"]] <- c(-1)   
#'item_params[[2]] <- list()   
#'item_params[[2]]["slope_gen"] <- 1.2   
#'item_params[[2]]["slope_spec"] <- 1   
#'item_params[[2]][["thr"]] <- list()   
#'item_params[[2]][["thr"]] <- c(-.6)   
#'item_params[[3]] <- list()   
#'item_params[[3]]["slope_gen"] <- 1   
#'item_params[[3]]["slope_spec"] <- .8   
#'item_params[[3]][["thr"]] <- list()   
#'item_params[[3]][["thr"]] <- c(-.2)   
#'item_params[[4]] <- list()   
#'item_params[[4]]["slope_gen"] <- 1   
#'item_params[[4]]["slope_spec"] <- .8   
#'item_params[[4]][["thr"]] <- list()   
#'item_params[[4]][["thr"]] <- c(.2)   
#'item_params[[5]] <- list()   
#'item_params[[5]]["slope_gen"] <- .8   
#'item_params[[5]]["slope_spec"] <- 1.2   
#'item_params[[5]][["thr"]] <- list()   
#'item_params[[5]][["thr"]] <- c(.6)   
#'item_params[[6]] <- list()   
#'item_params[[6]]["slope_gen"] <- .8   
#'item_params[[6]]["slope_spec"] <- 1.2   
#'item_params[[6]][["thr"]] <- list()   
#'item_params[[6]][["thr"]] <- c(1)   
#'example_ts <- comp_ts2(theta_gen = c(-1,0,1),   
#'                        theta_spec = c(-1,0,1),   
#'                        item_params = item_params,   
#'                        ic_index = ic_index <- c(1,1,2,2,3,3)) 
#'
#' @export comp_ts

comp_ts <- function(theta_gen,   
                    theta_spec,   
                    item_params,   
                    ic_index) {   
  n_items <- length(item_params)   
  ts <- list()   
  for (i in 1:n_items) {   
    n_thr <- length(item_params[[i]][["thr"]])   
    ts[[i]] <- list()   
    
    if(n_thr == 1) {   
      ts[[i]][[1]] <- matrix(nrow = length(theta_spec),   
                             ncol = length(theta_gen))   
      ts[[i]][[2]] <- matrix(nrow = length(theta_spec),   
                             ncol = length(theta_gen))   
      for(q1 in 1:length(theta_gen)) {   
        for(q2 in 1:length(theta_spec)) {   
          # p    
          ts[[i]][[2]][q1,q2] <- 1/(1+exp(-(item_params[[i]][["slope_gen"]] * theta_gen[q1] +
                                              item_params[[i]][["slope_spec"]] * theta_spec[q2] +
                                              (item_params[[i]][["thr"]][[1]]))))   
        }   
      }   
      # q   
      ts[[i]][[1]] <- 1-ts[[i]][[2]]   
    }   
    else{   
      length(ts[[i]]) <- n_thr+1   
      ts[[i]][[1]] <- matrix(nrow = length(theta_spec),   
                             ncol = length(theta_gen))   
      ts[[i]][[n_thr+1]] <- matrix(nrow = length(theta_spec),   
                                   ncol = length(theta_gen))   
      for(q1 in 1:length(theta_gen)) {   
        for(q2 in 1:length(theta_spec)) {   
          ts[[i]][[1]][q1,q2] <- 1-(1/(1+exp(-(item_params[[i]][["slope_gen"]]*theta_gen[q1] +   
                                                 item_params[[i]][["slope_spec"]]*theta_spec[q2] +   
                                                 (item_params[[i]][["thr"]][[1]])))))   
          ts[[i]][[n_thr+1]][q1,q2] <- 1-(1/(1+exp(-(item_params[[i]][["slope_gen"]]*theta_gen[q1] +   
                                                       item_params[[i]][["slope_spec"]]*theta_spec[q2] +   
                                                       (item_params[[i]][["thr"]][[n_thr]])))))   
        }   
      }   
      for(j in 2:n_thr) {   
        ts[[i]][[j]] <- matrix(nrow = length(theta_spec),   
                               ncol = length(theta_gen))   
        for(q1 in 1:length(theta_gen)) {   
          for(q2 in 1:length(theta_spec)) {   
            ts[[i]][[j]][q1,q2] <- (1-(1/(1+exp(-(item_params[[i]][["slope_gen"]]*theta_gen[q1] +   
                                                    item_params[[i]][["slope_spec"]]*theta_spec[q2] +   
                                                    (item_params[[i]][["thr"]][j-1])))))) -   
              (1-(1/(1+exp(-(item_params[[i]][["slope_gen"]]*theta_gen[q1] +   
                               item_params[[i]][["slope_spec"]]*theta_spec[q2] +   
                               (item_params[[i]][["thr"]][j]))))))   
            
          }   
        }   
      }   
    }   
  }   
  
  ts_ic <- list() # List similar to ts, only that the trace surfaces will be indexed by item clusters   
  ic_unique <- unique(ic_index) # Unique item cluster values   
  #item_index <- rep(seq(1,nr, by = 1), length(unique(ic_index)))   
  n_iter <- 0   
  for(i in 1:length(ic_unique)) {   
    ts_ic[[i]] <- list()   
    TF_index <- ic_index == ic_unique[i]   
    for(j in 1:length(TF_index[TF_index == TRUE])) {   
      n_iter <- n_iter+1   
      ts_ic[[i]][[j]] <- matrix(nrow = length(theta_spec),   
                                ncol = length(theta_gen))   
      ts_ic[[ic_unique[i]]][[j]] <- ts[[n_iter]] #p   
    }   
  }   
  return(ts_ic)   
}
