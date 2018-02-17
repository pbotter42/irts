quad_gen <- function(n_quad, theta_min, theta_max) {
  q_points <- seq(from = theta_min, to = theta_max,
                  by = (theta_max-theta_min)/(n_quad-1))
  return(q_points)
}

norm_dist_2d <- function(theta_gen, theta_spec) {
  dist_m <- matrix(0,length(theta_gen),length(theta_gen))
  phi <- 0#toDo
  for (i in 1:length(theta_gen)) {
    for (j in 1:length(theta_spec)) {
      dist_m[i,j] <- exp(-0.5*(theta_gen[i]^2+theta_spec[j]^2-2*phi*theta_gen[i]*theta_spec[j])/(1-phi*phi))
    }
  }
  dist_m <- dist_m/sum(dist_m)
  return(dist_m)
  
}

marg_dist_2d <- function(dist_2d) {
  n_quad <- nrow(dist_2d)
  dist_m <- rep(0, n_quad)
  for (i in 1:n_quad) {
    for (j in 1:n_quad) {
      dist_m[i] <- dist_m[i] + dist_2d[i,j]
    }
  }
  return(dist_m)
}

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

normal_distribution <- function(n_quad,
                                theta_min,
                                theta_max) {
  q_points <- seq(from = theta_min,
                  to = theta_max,
                  by = (theta_max-theta_min)/(n_quad-1))
  
  dist_m <- exp(-(q_points^2)/2)
  dist_m <- dist_m/sum(dist_m)
  return(dist_m)
  
}