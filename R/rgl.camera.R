#' @title Place observer
#'
#' @description Set the position, orientation and field-of-view of the observer/camera.
#'
#' @importFrom rgl par3d rotationMatrix rgl.viewpoint translationMatrix observer3d
#'
#' @param position either a single number, a 3-vector or NULL. A single number is interpreted as the distance between the observer and the center of the scene. The observer is placed at distance along the line-of-sight of the current/specified viewing direction. A 3-vector is interpreted as the actual position of the observer in the coordinates of the 3D plot. If \code{NULL}, the position is adjusted automatically.
#' @param direction optional 3-vector specifying the direction in which the observer is looking. The norm of the vector is irrelevant, but has to be larger than zero.
#' @param angle (only used if \code{direction} is specified) position angle in degrees, defining the rotation of the camera around the line-of-sight. This angle is defined as the angle formed between the horizontal line on the screen and the projected z-axis. Only the z-axis gets very close to the line-of-sight, the angle refers to the projected y-axis.
#' @param fov (only used if \code{direction} is specified) field of view in degrees, as used in \code{\link[rgl]{rgl.viewpoint}}. This is roughly the field-of-view seen along the shortest axis of the window.
#'
#' @author Danail Obreschkow
#'
#' @examples
#' rgl.test.scene()
#' rgl.camera(position=c(1.4,1.4,1.4), direction=c(-1,-1,-1), fov=60, angle=0)
#'
#' @export rgl.camera

rgl.camera = function(position=10, direction=NULL, angle=0, fov=0) {

  skip = rgl::par3d()$skipRedraw
  rgl::par3d(skipRedraw=TRUE)

  # compute overall rotation matrix from direction and angle
  if (!is.null(direction)) {
    if (sum(direction^2)<=0) stop('direction has to be a vector of positive length.')
    e3 = -direction/sqrt(sum(direction^2))
    fz = ifelse(e3[3]^2<0.6,1,cos(min(1,1*((e3[3]^2-0.6)/0.4)^5)*pi/2)^2)
    er = c(0,sqrt(1-fz),sqrt(fz)) # reference unit-vector which is *never* parallel to e3
    e1 = c(er[2]*e3[3]-er[3]*e3[2],er[3]*e3[1]-er[1]*e3[3],er[1]*e3[2]-er[2]*e3[1])
    e1 = e1/sqrt(sum(e1^2))
    e2 = c(e3[2]*e1[3]-e3[3]*e1[2],e3[3]*e1[1]-e3[1]*e1[3],e3[1]*e1[2]-e3[2]*e1[1])
    um = rgl::rotationMatrix(matrix=rbind(e1,e2,e3))%*%rgl::rotationMatrix(angle/180*pi,e3[1],e3[2],e3[3])
    rgl::rgl.viewpoint(userMatrix=um,fov=fov)
  }

  # adjust observer position
  if (!is.null(position)) {
    if (length(position)==1) {
      rgl::observer3d(0,0,position)
    } else if (length(position)==3) {
      um = rgl::par3d()$userMatrix
      ehorz = um[1,1:3] # unit vector displayed left to right
      evert = um[2,1:3] # unit vector displayed bottom to top
      eperp = um[3,1:3] # unit vector displayed out of screen
      b = rgl::par3d()$bbox
      center = c(mean(b[1:2]), mean(b[3:4]), mean(b[5:6]))
      shorz = sum((position-center[1])*ehorz)
      svert = sum((position-center[2])*evert)
      sperp = sum((position-center[3])*eperp)
      userMatrix = rgl::par3d("userMatrix")
      rgl::par3d(userMatrix = userMatrix %*% t(rgl::translationMatrix(-shorz*ehorz[1]-svert*evert[1],
                                                                      -shorz*ehorz[2]-svert*evert[2],
                                                                      -shorz*ehorz[3]-svert*evert[3])))

      rgl::observer3d(0,0,sperp)
    } else {
      stop('unknown position type')
    }
  }

  rgl::par3d(skipRedraw=skip)

}
