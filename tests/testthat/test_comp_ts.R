context("testing comp_ts") 

item_params <- list()   
item_params[[1]] <- list()   
item_params[[1]]["slope_gen"] <- 1.2   
item_params[[1]]["slope_spec"] <- 1   
item_params[[1]][["thr"]] <- list()   
item_params[[1]][["thr"]] <- c(-1)   
item_params[[2]] <- list()   
item_params[[2]]["slope_gen"] <- 1.2   
item_params[[2]]["slope_spec"] <- 1   
item_params[[2]][["thr"]] <- list()   
item_params[[2]][["thr"]] <- c(-.6)   
item_params[[3]] <- list()   
item_params[[3]]["slope_gen"] <- 1   
item_params[[3]]["slope_spec"] <- .8   
item_params[[3]][["thr"]] <- list()   
item_params[[3]][["thr"]] <- c(-.2)   
item_params[[4]] <- list()   
item_params[[4]]["slope_gen"] <- 1   
item_params[[4]]["slope_spec"] <- .8   
item_params[[4]][["thr"]] <- list()   
item_params[[4]][["thr"]] <- c(.2)   
item_params[[5]] <- list()   
item_params[[5]]["slope_gen"] <- .8   
item_params[[5]]["slope_spec"] <- 1.2   
item_params[[5]][["thr"]] <- list()   
item_params[[5]][["thr"]] <- c(.6)   
item_params[[6]] <- list()   
item_params[[6]]["slope_gen"] <- .8   
item_params[[6]]["slope_spec"] <- 1.2   
item_params[[6]][["thr"]] <- list()   
item_params[[6]][["thr"]] <- c(1)   

ts_test <- comp_ts(theta_gen = c(-1,0,1),   
                   theta_spec = c(-1,0,1),   
                   item_params = item_params,   
                   ic_index = ic_index <- c(1,1,2,2,3,3)) 

test_that("comp ts object properties", {
  expect_output(str(ts_test), "List of 3")
})


