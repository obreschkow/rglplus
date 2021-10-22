#' @title Display orthogonal projection
#'
#' @description Display orthogonal projection on principal Cartesian axes
#'
#' @importFrom rgl identityMatrix rotationMatrix rgl.viewpoint
#'
#' @param plane character string, which can be either of `xy`, `yx`, `yz`, `zy`, `zx`, `xz`, where the  first letter is the axis displayed from left to right and the second letter is the axis displayed from bottom to top. The third axis point either out of the screen or into the screen following the standard right-hand convention.
#' @param fov field of view in degrees, as used in \code{\link[rgl]{rgl.viewpoint}}.
#'
#' @author Danail Obreschkow
#'
#' @examples
#' rgl.new(width=0.5, aspect=1.0, col='black')
#' rgl.ball(0.5, 0.5, 0.5, 0.5, png='earth.png')
#' rgl.orthoview('xy')
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
