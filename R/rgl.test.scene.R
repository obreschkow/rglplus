#' @title Plot 3D test image
#'
#' @description Draws a cubic 3D test image with three Cartesian axes, a sphere and three light sources with RGB colors.
#'
#' @importFrom rgl rgl.clear rgl.light lines3d points3d text3d
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @examples
#' \donttest{
#' rgl.test.scene()
#' }
#'
#' @export rgl.test.scene

rgl.test.scene = function() {

  # open new plot
  rgl.new(width=0.5, aspect=1)

  # add light sources
  rgl::rgl.clear(type="lights")
  rgl::rgl.light(0, 90, specular = 'green', diffuse='green', viewpoint.rel = FALSE)
  rgl::rgl.light(90, 0, specular = 'red', diffuse='red', viewpoint.rel = FALSE)
  rgl::rgl.light(0, 0, specular = 'blue', diffuse='blue', viewpoint.rel = FALSE)

  # draw axes
  rgl::lines3d(c(0,1),c(0,0),c(0,0),col='#aa0000',lwd=2)
  rgl::lines3d(c(0,0),c(0,1),c(0,0),col='#00aa00',lwd=2)
  rgl::lines3d(c(0,0),c(0,0),c(0,1),col='#0000aa',lwd=2)

  # draw sphere
  rgl.ball(0.5, 0.5, 0.5, 0.25, col='grey', depth=5, alpha=0.8)

  # add a point in the middle of the sphere
  rgl::points3d(0.5, 0.5, 0.5)

  # add axes labels
  rgl::text3d(1.05,0,0,'x')
  rgl::text3d(0,1.05,0,'y')
  rgl::text3d(0,0,1.05,'z')

}
