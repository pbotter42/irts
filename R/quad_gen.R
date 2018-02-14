#' @title irts
#'
#' @description This package computes the original Lord-Wingersky Algorithm for unidimensional IRT models, as well as the Lord-Wingersky Algorithm 2.0.
#'
#' @param n_quad
#' Number of quadrature  points to generate 
#' 
#' @param theta_max
#' the maximum quadrature value
#' 
#' @param theta_min
#' the minimum quadrature value
#' 
#' @return q_points
#'
#' @examples quad_gen(n_quad=21, theta_min=-5, theta_max=5)
#'
#' @export quad_gen


quad_gen <- function(n_quad, theta_min, theta_max) {
  if(theta_max <= 0 |
     theta_min >= 0 |
     theta_max/theta_min != -1) {
    stop("Improper theta_min and or theta_max values are
         provided. Ensure values have the proper sign and
         are proportional.")
  }
  if((n_quad %% 2) != 1) {
    warning("An odd number of quadrature points was not provided.")
  }
  q_points <- seq(from = theta_min, to = theta_max,
                  by = (theta_max-theta_min)/(n_quad-1))
  return(q_points)
}