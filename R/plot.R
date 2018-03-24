#' @title irts object plots 
#'
#' @description Sum Score ploting of irts objects
#'
#' @param x
#' @param which.scores
#' 
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
#'plot(example1)
#'

plot.irts <- function(x,
                      which.scores = "all") {
  if(which.scores == "all") {
    t_min <- min(x[["theta"]])+.2
    t_max <- max(x[["theta"]])-.2
    
    text_x <- seq(t_min,
                  t_max,
                  by = abs(t_min-t_max)/ncol(x[["Sum Scores"]]))
    
    text_y <- numeric()
    for(i in 1:nrow(x[["Sum Scores"]])) {
      text_y[i] <- max(x[["Sum Scores"]][i,2:ncol(x[["Sum Scores"]])])
    }
    
    plot(
      x = x[["theta"]],
      y = x[["Sum Scores"]][1,2:ncol(x[["Sum Scores"]])],
      main = paste("All Sum Score Likelihoods"),
      ylab = "",
      xlab = "Theta",
      ylim = c(0,
               min(max(x[["Sum Scores"]][,ncol(x[["Sum Scores"]])])*1.2,
                   1)),
      type = "l"
    )
    
    for(i in 1:nrow(x[["Sum Scores"]])) {
      lines(
        x = x[["theta"]],
        y = x[["Sum Scores"]][i,2:ncol(x[["Sum Scores"]])],
        type = "l",
        col = i
      )
    }
    for(i in 1:length(x[["theta"]])) {
      text(
        x = text_x[i],
        y = text_y[i],
        labels = x[["Sum Scores"]][i,1],
        col = i
      )
    }
  }
  if(which.scores == "ind") {
    for(i in 1:nrow(x[["Sum Scores"]])) {
      plot(
        x = x[["theta"]],
        y = x[["Sum Scores"]][i,2:ncol(x[["Sum Scores"]])],
        main = paste("Sum Score Likelihood","=",x[["Sum Scores"]][i,1]),
        ylab = "",
        xlab = "Theta",
        ylim = c(0,
                 min(max(x[i,length(x[["Sum Scores"]])])*1.2,
                     1)),
        type = "l"
      )
    }
  }
}

