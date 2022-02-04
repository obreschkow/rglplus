#' @title Place observer
#'
#' @description Set the position, orientation and field-of-view of the observer
#'
#' @importFrom rgl par3d rotationMatrix rgl.viewpoint translationMatrix observer3d
#'
#' @param position either a single number, a 3-vector or NULL. A single number is interpreted as the distance between the observer and the center of the scene. The observer is placed at distance along the current optical axis or that specified by the optional argument \code{direction}. A 3-vector is interpreted as the actual position of the observer in the coordinates of the 3D plot. If \code{position=NULL}, the position is adjusted automatically following the rules of \code{\link[rgl]{rgl.viewpoint}}.
#' @param direction optional 3-vector specifying the direction in which the observer is looking, that is the optical axis of the virtual camera. The norm of the vector is irrelevant, but has to be non-zero. If not given, the camera is pointed at the center of the scene bounding box returned by \code{\link[rgl]{par3d}}.
#' @param up (only used if \code{direction} is specified) single number or 3-vector, specifying the rotation of the camera around the optical axis (as defined with the argument \code{direction}). If a single number is provided, it is normally interpreted as the angle in degrees between the up-direction of the 2d camera image and the projected z-axis of the 3d scene. To avoid the singularity that occurs if the optical axis lies very close to the z-axis, "\code{up}" is, in this case, interpreted as the angle between the up-direction and the y-axis. If a 3-vector is provided, it is interpreted such that its projection points upwards on the projected image seen by the camera. Thus, this 3-vector must *not* be parallel to the direction.
#' @param fov field of view in degrees, as used in \code{\link[rgl]{rgl.viewpoint}}. This is roughly the field-of-view seen along the shortest axis of the window.
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @examples
#' \donttest{
#' rgl.test.scene()
#' for (a in seq(0,pi/2,length=200)) {
#'   rgl.camera(position=3, direction=c(-cos(a),0,-sin(a)), up=c(-sin(a),0,cos(a)), fov=60)
#' }
#' }
#'
#' @export rgl.camera

rgl.camera = function(position=10, direction=NULL, up=0, fov=0) {

  skip = rgl::par3d()$skipRedraw
  rgl::par3d(skipRedraw=TRUE)

  # compute overall rotation matrix from direction and orientation
  if (!is.null(direction)) {
    if (sum(direction^2)<=0) stop('direction has to be a vector of positive length.')
    e3 = -direction/sqrt(sum(direction^2))
    if (length(up)==1) {
      fz = ifelse(e3[3]^2<0.6,1,cos(min(1,1*((e3[3]^2-0.6)/0.4)^5)*pi/2)^2)
      er = c(0,sqrt(1-fz),sqrt(fz)) # reference unit-vector which is *never* parallel to e3
      e1 = c(er[2]*e3[3]-er[3]*e3[2],er[3]*e3[1]-er[1]*e3[3],er[1]*e3[2]-er[2]*e3[1])
      e1 = e1/sqrt(sum(e1^2))
      e2 = c(e3[2]*e1[3]-e3[3]*e1[2],e3[3]*e1[1]-e3[1]*e1[3],e3[1]*e1[2]-e3[2]*e1[1])
      um = rgl::rotationMatrix(matrix=rbind(e1,e2,e3))%*%rgl::rotationMatrix(up/180*pi,e3[1],e3[2],e3[3])
    } else if (length(up)==3) {
      e1 = c(e3[2]*up[3]-e3[3]*up[2],
             e3[3]*up[1]-e3[1]*up[3],
             e3[1]*up[2]-e3[2]*up[1])
      e1 = -e1/sqrt(sum(e1^2))
      e2 = c(e3[2]*e1[3]-e3[3]*e1[2],e3[3]*e1[1]-e3[1]*e1[3],e3[1]*e1[2]-e3[2]*e1[1])
      um = rgl::rotationMatrix(matrix=rbind(e1,e2,e3))
    } else {
      stop('unknown up type')
    }
    if (is.null(fov)) {
      rgl::rgl.viewpoint(userMatrix=um)
    } else {
      rgl::rgl.viewpoint(userMatrix=um,fov=fov)
    }
  } else {
    if (!is.null(fov)) {
      um = rgl::par3d()$userMatrix
      rgl::rgl.viewpoint(userMatrix=um,fov=fov)
    }
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
