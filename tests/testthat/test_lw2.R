context("testing lw2") 

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

example_lw2_rounded <- round(lw2(n_quad = 5,   
                    theta_min = -2,
                    theta_max = 2,   
                    item_params = item_params,   
                    ic_index = ic_index <- c(1,1,2,2,3,3)),
                    digits = 5)

test_that("lw2 produces the correct estimates", {
  expect_equal(example_lw2_rounded[,1],
               c(0,1,2,3,4,5,6))
  expect_equal(example_lw2_rounded[,2],
               c(0.31024,0.37296,0.23734,0.06816,0.01049,0.00077,0.00003))
  expect_equal(example_lw2_rounded[,3],
               c(0.11411,0.26901,0.32625,0.20398,0.07220,0.01327,0.00119)) 
  expect_equal(example_lw2_rounded[,4],
               c(0.02161,0.09974,0.23263,0.30097,0.22992,0.09581,0.01932)) 
  expect_equal(example_lw2_rounded[,5],
               c(0.00172,0.01584,0.07275,0.19249,0.31026,0.28209,0.12484)) 
  expect_equal(colnames(example_lw2_rounded),
               c("Sum Score", "Quad point 1", "Quad point 2",
                 "Quad point 3", "Quad point 4", "Quad point 5"))
})
