#' @title Plot 3D test image
#'
#' @description Draws a c3D test image with three Cartesian axes, a sphere of radius 0.5 and three light sources with RGB colors.
#'
#' @param center 3-vector specifying the centre of the 3D plot.
#' @param width either an integer (>1) specifying the number of pixels in the horizontal direction, or a real value (>0 and <=1) specifying the fraction of the available pixels. If the selected aspect ratio causes the number of vertical pixels to exceed the available number, the width is reduced as much as necessary.
#'
#' @importFrom rgl clear3d light3d lines3d points3d text3d axis3d
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @export rgl.test.scene

rgl.test.scene = function(center=c(0,0,0), width=0.5) {

  # open new plot
  rgl.new(xlim=center[1]+c(-1,1), ylim=center[2]+c(-1,1), zlim=center[3]+c(-1,1), width=width, aspect=1, fov=0)

  # add light sources
  rgl::clear3d(type="lights")
  rgl::light3d(0, 90, specular = 'green', diffuse='green', viewpoint.rel = FALSE)
  rgl::light3d(90, 0, specular = 'red', diffuse='red', viewpoint.rel = FALSE)
  rgl::light3d(0, 0, specular = 'blue', diffuse='blue', viewpoint.rel = FALSE)

  # draw axes
  rgl::lines3d(center[1]+c(-1,1),center[2]+c(0,0),center[3]+c(0,0),col='#aa0000',lwd=2)
  rgl::lines3d(center[1]+c(0,0),center[2]+c(-1,1),center[3]+c(0,0),col='#00aa00',lwd=2)
  rgl::lines3d(center[1]+c(0,0),center[2]+c(0,0),center[3]+c(-1,1),col='#0000aa',lwd=2)

  # draw sphere
  rgl.ball(center[1], center[2], center[3], 0.5, col='grey', depth=4, alpha=0.8)

  # add coordinates
  rgl::axis3d('x',at=c(-1,-0.5,0.5,1)+center[1],pos=center)
  rgl::axis3d('y',at=c(-1,-0.5,0.5,1)+center[2],pos=center)
  rgl::axis3d('z',at=c(-1,-0.5,0.5,1)+center[3],pos=center)

  # add axes labels
  rgl::text3d(center[1]+1.05,center[2],center[3],'x')
  rgl::text3d(center[1],center[2]+1.05,center[3],'y')
  rgl::text3d(center[1],center[2],center[3]+1.05,'z')

}

