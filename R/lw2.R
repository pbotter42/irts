#' @title irts
#'
#' @description This package computes the original Lord-Wingersky Algorithm for unidimensional IRT models, as well as the Lord-Wingersky Algorithm 2.0.
#'
#' @param n_quad
#' @param theta_min
#' @param theta_max
#' @param item_params
#' @param ic_index
#' 
#' @return aggregated_SS
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
#'
#'example_lw2 <- lw2(n_quad = 5,   
#'                        theta_min = -2,
#'                        theta_max = 2,   
#'                        item_params = item_params,   
#'                        ic_index = ic_index <- c(1,1,2,2,3,3))
#'
#' @export lw2

lw2 <- function(n_quad,
                   theta_min,
                   theta_max,
                   item_params,
                   ic_index) {  
  theta_gen <- quad_gen(n_quad,theta_min,theta_max)
  theta_spec <- quad_gen(n_quad,theta_min,theta_max)
  dist_2d <- norm_dist_2d(theta_gen, theta_spec)  
  marg_2d <- marg_dist_2d(dist_2d)  
  ts_list <- comp_ts(theta_gen = theta_gen,   
                      theta_spec = theta_spec,   
                      item_params = item_params,   
                      ic_index = ic_index)
  
  ts_score <- list() # esentually the same as ts_list, just has the scores added 
  lw_iter <- list()  
  lw_final_iter <- list()  
  # Adding item scores  
  for(k in 1:length(ts_list)) { 
    ts_score[[k]] <- list() 
    for(j in 1:length(ts_list[[k]])) { 
      ts_score[[k]][[j]] <- list() 
      score_iter <- -1 
      for(i in 1:length(ts_list[[k]][[j]])) { 
        score_iter <- score_iter+1 
        ts_score[[k]][[j]][[i]] <- list() 
        ts_score[[k]][[j]][[i]][["ts"]] <- ts_list[[k]][[j]][[i]] 
        ts_score[[k]][[j]][[i]][["SS"]] <- numeric() 
        ts_score[[k]][[j]][[i]][["SS"]] <- score_iter 
      } 
    } 
  } 
  for(k in 1:length(ts_score)) {  
    lw_iter[[k]] <- list() 
    lw_iter[[k]][[1]] <- ts_score[[k]]  
  }  
  # Above (i.e. [[k]][[1]] only) just set into place the the first item in   
  # the k clusters ts and SS  
  for(k in 1:length(ts_score)) {  # i.e. for the number of ic 
    # no need to make [[k]] a list bc was already done in previous loop  
    for(j in 2:length(ts_score[[k]])) {  # for 2 - the num of items in ic k
      lw_iter[[k]][[j]] <- rep(ts_score[[k]][[j-1]], length(ts_score[[k]][[j]]))  
      n_iter <- 0  
      for(i in 1:length(ts_score[[k]][[j]])) {  
        for(w in 1:length(lw_iter[[k]][[j-1]])) {  
          n_iter <- n_iter + 1  
          lw_iter[[k]][[j]][[n_iter]][["ts"]] <- lw_iter[[k]][[j]][[n_iter]][["ts"]]*  
            ts_score[[k]][[j]][[i]][["ts"]]  
          
          lw_iter[[k]][[j]][[n_iter]][["SS"]] <- 
            lw_iter[[k]][[j]][[n_iter]][["SS"]]+ts_score[[k]][[j]][[i]][["SS"]] 
        }  
      }  
    }  
    lw_final_iter[[k]] <- lw_iter[[k]][[length(lw_iter[[k]])]]
  } 
  temp <- list()
  # looping over lw_final_iter to get temp[[k]][[2]] to contain unique sum scores  
  for(k in 1:length(lw_final_iter)) {  
    temp[[k]] <- list()  
    temp[[k]][[1]] <- numeric() # Vector for all Sum Scores across item clusters  
    for(i in 1:length(lw_final_iter[[k]])) {  
      temp[[k]][[1]][i] <- lw_final_iter[[k]][[i]][[2]]  
    }  
    temp[[k]][[2]] <- numeric()  
    temp[[k]][[3]] <- numeric()  
    temp[[k]][[2]] <- unique(temp[[k]][[1]])  
    temp[[k]][[2]] <- sort(unique(temp[[k]][[1]]))  
    temp[[k]][[3]] <- data.frame(SS = temp[[k]][[2]],  
                                 index = seq(1, length(temp[[k]][[2]])))  
  } 
  
  # Creating a matrix of 0's that is where the combined lik will be 
  unique_SS_lik <- list()
  for(k in 1:length(temp)){  
    unique_SS_lik[[k]] <- list()  
    for(i in 1:length(temp[[k]][[2]])) { # indexing on SS +1 bc r cannot index on 0  
      unique_SS_lik[[k]][[i]] <- list()  
      unique_SS_lik[[k]][[i]][[1]] <- matrix(rep(0,  
                                                 n_quad*n_quad),  
                                             nrow = n_quad,  
                                             ncol = n_quad)  
      unique_SS_lik[[k]][[i]][[2]] <- numeric()  
      unique_SS_lik[[k]][[i]][[2]] <- temp[[k]][[2]][i]  
      
    }  
  }
  # Now summing ts by unique sum score(i.e. SS)
  # This will result in, for each ic, x i lists containing a unique SS and a sumed ts
  for(k in 1:length(lw_final_iter)) {  
    for(i in 1:length(lw_final_iter[[k]])) {  
      for(j in 1:nrow(temp[[k]][[3]])) {  
        if(lw_final_iter[[k]][[i]][[2]] == temp[[k]][[3]][[j,"SS"]])  
        {  
          unique_SS_lik[[k]][[j]][[1]] <- unique_SS_lik[[k]][[j]][[1]]+lw_final_iter[[k]][[i]][[1]]  
        }  
      }  
    }  
  }
  
  # now the unique ts by ic and unique ss will be marginalized   
  for(k in 1:length(unique_SS_lik)) {  
    for(i in 1:length(unique_SS_lik[[k]])) {  
      unique_SS_lik[[k]][[i]][["mar_tLine"]] <- rep(0,n_quad) # creating a numeric vector that will be the marginal trace line  
      for(QP1 in 1:n_quad) {  
        for(QP2 in 1:n_quad) {  
          unique_SS_lik[[k]][[i]][["mar_tLine"]][QP1] <- unique_SS_lik[[k]][[i]][["mar_tLine"]][QP1]+unique_SS_lik[[k]][[i]][[1]][QP1,QP2]*dist_2d[QP1,QP2]  
        }  
        unique_SS_lik[[k]][[i]][["mar_tLine"]][QP1] <-  unique_SS_lik[[k]][[i]][["mar_tLine"]][QP1]/marg_2d[QP1]  
      }  
    }  
  }
  unique_SS_lik2 <- list()  
  for(k in 1:length(unique_SS_lik)) {  
    unique_SS_lik2[[k]] <- list()  
    for(i in 1:length(unique_SS_lik[[k]])) {  
      unique_SS_lik2[[k]][[i]] <- list()  
      unique_SS_lik2[[k]][[i]][["mar_tLine"]] <- unique_SS_lik[[k]][[i]]$mar_tLine  
      unique_SS_lik2[[k]][[i]][["ic_SS"]] <- unique_SS_lik[[k]][[i]][[2]]  
    }  
  }  
  lw_iter <- list()  
  lw_iter[[1]] <- unique_SS_lik2[[1]]# the list for iteration1 of the part 2 recursions  
  for(k in 2:length(unique_SS_lik2)) { # this loops through the 2nd item cluster to the last  
    lw_iter[[k]] <- rep(lw_iter[[k-1]], length(unique_SS_lik2[[k]])) # notice how lw_iter is being repeated by the lengh of k  
    n_iter <- 0  
    for(i in 1:length(unique_SS_lik2[[k]])) {  
      for(j in 1:length(lw_iter[[k-1]])) {  
        n_iter <- n_iter+1  
        lw_iter[[k]][[n_iter]]$mar_tLine <- lw_iter[[k]][[n_iter]]$mar_tLine*unique_SS_lik2[[k]][[i]]$mar_tLine # summing the lik  
        lw_iter[[k]][[n_iter]]$ic_SS <- lw_iter[[k]][[n_iter]]$ic_SS+unique_SS_lik2[[k]][[i]]$ic_SS # summing the sum scores  
      }  
    }  
  }  
  lw_final_iter <- lw_iter[[length(lw_iter)]] # Returning only the final iteration of combining lik within item cluster  
  
  lw2.0_Lik <- matrix(nrow = length(lw_final_iter),  
                      ncol = n_quad)# Matrix for all the lik before they are combined via SS  
  lw2.0_SS <- numeric() # vector to contain the SS  
  
  for(k in 1:length(lw_final_iter)) {  
    lw2.0_Lik[k,] <- lw_final_iter[[k]]$mar_tLine  
    lw2.0_SS[k] <- lw_final_iter[[k]]$ic_SS  
  }  
  
  qPoint_names <- vector() # vector to store quad points names  
  for(i in 1:n_quad) {  
    qPoint_names[i] <- paste("Quad point", i, sep = " ")  
  }  
  colnames(lw2.0_Lik) <- qPoint_names  
  lw2.0_Lik_SS <- cbind(lw2.0_Lik, lw2.0_SS) # contains the lik and SS  
  lw2.0_Lik_SS <- as.data.frame(lw2.0_Lik_SS)  
  aggregated_SS <- aggregate(.~lw2.0_SS, lw2.0_Lik_SS, sum)  
  colnames(aggregated_SS)[1] <- "Sum Score"
   return(aggregated_SS)
}  
