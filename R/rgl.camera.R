#' @title Place observer
#'
#' @description Set the position, orientation and field-of-view of the observer
#'
#' @importFrom rgl open3d cylinder3d shade3d addNormals subdivision3d par3d rotationMatrix view3d translationMatrix observer3d
#'
#' @param position either a 3-vector, a single number or NULL. A vector directly specifies the location of camera. A single number specifies the distances of the camera along the z-axis, relative to the center of the scene (= center of the bounding box returned by \code{\link[rgl]{par3d}}). If no positions is given, then the camera is placed automatically.
#' @param direction optional 3-vector specifying the direction in which the observer is looking, that is the optical axis of the virtual camera. The norm of the vector is irrelevant, but has to be non-zero. If not given, the camera is pointed at the center of the scene.
#' @param up optional single number or 3-vector, specifying the rotation of the camera around the optical axis (as defined with the argument direction). If a single number is provided, it is normally interpreted as the angle in degrees between the up-direction of the 2d camera image and the projected z-axis of the 3d scene. To avoid the singularity that occurs if the optical axis lies very close to the z-axis, "up" is, in this case, interpreted as the angle between the up-direction and the y-axis. If a 3-vector is provided, it is interpreted such that its projection points upwards on the projected image seen by the camera. Thus, this 3-vector must *not* be parallel to the direction.
#' @param fov field of view in degrees, as used in \code{\link[rgl]{view3d}}. This is roughly the field-of-view seen along the shortest axis of the window.
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @examples
#' # Draw knot
#' rgl::open3d()
#' a = seq(0,2*pi,len=25)
#' knot = rgl::cylinder3d(center=cbind(sin(a)+2*sin(2*a), 2*sin(3*a), cos(a)-2*cos(2*a)),
#'                        e1 = cbind(cos(a)+4*cos(2*a), 6*cos(3*a), sin(a)+4*sin(2*a)),
#'                        radius = 0.8, closed = TRUE)
#' rgl::shade3d(rgl::addNormals(rgl::subdivision3d(knot,depth=2)), col="purple")
#'
#' # Place static camera
#' rgl.camera(c(10,0,0),fov=50)
#'
#' # Animate camera
#' \dontrun{
#'   for(alpha in seq(0,2*pi,len=100)) {
#'     rgl.camera(10*c(cos(alpha),sin(alpha),0),fov=50)
#'   }
#' }
#'
#' @export rgl.camera

rgl.camera = function(position=NULL, direction=NULL, up=NULL, fov=0) {

  # determine centre and size of bounding box
  b = rgl::par3d()$bbox
  x0 = c(mean(b[1:2]),mean(b[3:4]),mean(b[5:6])) # centre
  d = sqrt(diff(par3d()$bbox[1:2])^2+diff(par3d()$bbox[3:4])^2+diff(par3d()$bbox[5:6])^2) # space diagonal

  # handle position
  if (is.null(position)) {
    position = x0+c(0,0,d)
  } else {
    if (length(position)==1) {
      position = x0+c(0,0,position)
    }
  }

  # handle direction
  if (is.null(direction)) direction = x0-position
  if (sum(direction^2)<=0) direction = c(0,0,-1)
  e3 = -direction/sqrt(sum(direction^2))

  # evaluate rotation matrix
  if (is.null(up)) up = 0
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
    if (sum(e1^2)<=.Machine$double.eps) stop('up too close to direction')
    e1 = -e1/sqrt(sum(e1^2))
    e2 = c(e3[2]*e1[3]-e3[3]*e1[2],e3[3]*e1[1]-e3[1]*e1[3],e3[1]*e1[2]-e3[2]*e1[1])
    um = rgl::rotationMatrix(matrix=rbind(e1,e2,e3))
  } else {
    stop('unknown up type')
  }

  # add translation to correct position
  ehorz = um[1,1:3] # unit vector displayed left to right
  evert = um[2,1:3] # unit vector displayed bottom to top
  eperp = um[3,1:3] # unit vector displayed out of screen
  displacement = position-x0
  shorz = sum(displacement*ehorz)
  svert = sum(displacement*evert)
  sperp = sum(displacement*eperp)
  um = um %*% t(rgl::translationMatrix(-shorz*ehorz[1]-svert*evert[1],
                                       -shorz*ehorz[2]-svert*evert[2],
                                       -shorz*ehorz[3]-svert*evert[3]))

   # place camera
  skip = rgl::par3d()$skipRedraw
  on.exit(rgl::par3d(skipRedraw=skip))
  rgl::par3d(skipRedraw=TRUE)
  rgl::view3d(userMatrix=um,fov=fov)
  rgl::observer3d(0,0,sperp)

}
