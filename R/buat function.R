#' 3D Geometry Package: Volume and Surface Area
#'
#' @docType package
#' @name geometry3d
#'
#' @importFrom stats pi
#' @import rgl

#' Calculate volume and surface area of a cube
#'
#' @param s Length of the cube's side
#'
#' @return A list with volume and surface area
#' @export
cube <- function(s) {
  volume <- s^3
  surface_area <- 6 * s^2
  list(volume = volume, surface_area = surface_area)
}

#' Calculate volume and surface area of a rectangular prism
#'
#' @param p Length of the prism
#' @param l Width of the prism
#' @param h Height of the prism
#'
#' @return A list with volume and surface area
#' @export
rectangular_prism <- function(p, l, h) {
  volume <- p * l * h
  surface_area <- 2 * (p * l + p * h + l * h)
  list(volume = volume, surface_area = surface_area)
}

#' Calculate volume and surface area of a cylinder
#'
#' @param r Radius of the cylinder's base
#' @param h Height of the cylinder
#'
#' @return A list with volume and surface area
#' @export
cylinder <- function(r, h) {
  volume <- pi * r^2 * h
  surface_area <- 2 * pi * r * (r + h)
  list(volume = volume, surface_area = surface_area)
}

