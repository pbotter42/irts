context("testing comp_ts") 

ts_test <- comp_ts(theta_gen = c(-2,-1,0,1,2),
                   theta_spec = c(-2,-1,0,1,2),
                   a_gen = c(1.2,1.2,1,1,.8,.8),
                   a_spec = c(1,1,.8,.8,1.2,1.2),
                   c = c(-1,-.6,-.2,.2,.6,1),
                   ic_index = c(1,1,2,2,3,3),
                   nr = 2)

test_that("comp ts object properties", {
  expect_output(str(ts_test), "List of 3")
})

