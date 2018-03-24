#' @title irts print function
#'
#' @description Prints irt sum scores
#'
#' @param x
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
#'example1 <- lw2(n_quad = 5,   
#'                        theta_min = -2,
#'                        theta_max = 2,   
#'                        item_params = item_params,   
#'                        ic_index = c(1,1,2,2,3,3))
#'print(example1)
#'

print.irts <- function(x) {
  return(print(x[["Sum Scores"]]))
}
