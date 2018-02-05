#' @title irts
#'
#' @description This package computes the original Lord-Wingersky Algorithm for unidimensional IRT models, as well as the Lord-Wingersky Algorithm 2.0.
#'
#' @param n_quad
#'
#' @param theta_max
#'
#' @param theta_min
#'
#' @return q_points
#'
#' @examples 
#'
#' @export quad_gen


quad_gen <- function(n_quad, theta_min, theta_max) {

  q_points <- seq(from = theta_min, to = theta_max,
                  by = (theta_max-theta_min)/(n_quad-1))
  return(q_points)
}

