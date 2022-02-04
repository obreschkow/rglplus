#' @title Display orthogonal projection
#'
#' @description Display orthogonal projection on principal Cartesian planes, with scene centre in the image centre.
#'
#' @importFrom rgl identityMatrix rotationMatrix rgl.viewpoint
#'
#' @param plane character string, which can be either of `xy`, `yx`, `yz`, `zy`, `zx`, `xz`, where the  first letter is the axis displayed from left to right and the second letter is the axis displayed from bottom to top. The third axis points either out of the screen or into the screen following the right-hand convention.
#' @param fov field of view in degrees, as used in \code{\link[rgl]{rgl.viewpoint}}.
#' @param ... additional arguments for \code{\link[rgl]{rgl.viewpoint}}.
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @examples
#' \donttest{
#' rgl.new(width=0.5, aspect=1.0, col='black', xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1))
#' rgl.ball(0, 0, 0, 1.4, png=system.file('earth.png', package='rglplus'), specular='#333333')
#' rgl.orthoview('yz', fov=20)
#' }
#'
#' @seealso \code{\link{rgl.camera}}
#'
#' @export rgl.orthoview

rgl.orthoview = function(plane='xy', fov=0, ...) {
  if (plane=='xy') {
    um = rgl::identityMatrix()
  } else if (plane=='yx') {
    um = rgl::rotationMatrix(3*pi/2,0,0,1)%*%rgl::rotationMatrix(pi,0,1,0)
  } else if (plane=='yz') {
    um = rgl::rotationMatrix(3*pi/2,0,1,0)%*%rgl::rotationMatrix(3*pi/2,1,0,0)
  } else if (plane=='zy') {
    um = rgl::rotationMatrix(pi/2,0,1,0)
  } else if (plane=='xz') {
    um = rgl::rotationMatrix(3*pi/2,1,0,0)
  } else if (plane=='zx') {
    um = rgl::rotationMatrix(pi/2,0,1,0)%*%rgl::rotationMatrix(pi/2,0,0,1)
  } else {
    stop('unknown plane')
  }
  rgl::rgl.viewpoint(userMatrix=um, fov=fov, ...)
}
