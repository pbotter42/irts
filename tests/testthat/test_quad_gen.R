context("testing quad_gen") 

theta <- quad_gen(n_quad = 5, 
                  theta_min = -2, 
                  theta_max = 2) 

test_that("quad_gen produces 5 quad points", { 
  expect_equal(theta, c(-2, -1, 0, 1, 2)) 
}) 

