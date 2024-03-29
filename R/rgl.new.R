#' @title Open and initialize new 3D plot
#'
#' @description Calls \code{\link[rgl]{open3d}} and various additional functions to initialize a 3d plot.
#'
#' @importFrom rgl open3d par3d decorate3d bg3d rotationMatrix view3d clear3d
#'
#' @param width either an integer (>1) specifying the number of pixels in the horizontal direction, or a real value (>0 and <=1) specifying the fraction of the available pixels. If the selected aspect ratio causes the number of vertical pixels to exceed the available number, the width is reduced as much as necessary.
#' @param aspect aspect ratio of window, defined as the ratio of vertical-to-horizontal size.
#' @param orientation 3-by-3 rotation matrix or 2-character string specifying the orientation of the camera. For character string the allowed values are `xy`, `yx`, `yz`, `zy`, `zx`, `xz`, where the  first letter is the axis displayed from left to right and the second letter is the axis displayed from bottom to top. The third axis points either out of the screen or into the screen following the right-hand convention. This is the same as the \code{plane} argument of \code{\link{rgl.orthoview}}.
#' @param fov field of view in degrees, as used in \code{\link[rgl]{view3d}}
#' @param col background color
#' @param light logical flag. If \code{TRUE}, the standard light source created by \code{\link[rgl]{open3d}} will be light up the scene. If \code{FALSE}, no light source is added and the user must create custom light sources manually by calling \code{\link[rgl]{light3d}}.
#' @param xlim 2-vector specifying the range along the x-axis
#' @param ylim 2-vector specifying the range along the y-axis
#' @param zlim 2-vector specifying the range along the z-axis
#' @param xlab character string specifying the label of the x-axis
#' @param ylab character string specifying the label of the y-axis
#' @param zlab character string specifying the label of the z-axis
#' @param axes logical flag specifying whether axes are displayed
#' @param fixed logical flag. If \code{TRUE} (default), the range of the axes is *not* adjusted as objects are drawn.
#' @param close.all logical flag. If \code{TRUE} (default), all existing rgl windows are closed before the new window is opened.
#' @param ... additional arguments for \code{\link[rgl]{view3d}}.
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @export rgl.new

rgl.new = function(width=0.5, aspect=16/9,
                   orientation='xy', fov=30,
                   col='white', light=TRUE,
                   xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),
                   xlab=NULL, ylab=NULL, zlab=NULL, axes=FALSE,
                   fixed = TRUE, # do not adjust view when plotting objects
                   close.all = TRUE, ...) {

  # close previous windows
  if (close.all) rgl.close.all()

  # open new window of custom size
  if (width<=1) {
    rgl::open3d(windowRect = c(0, 0, 1e4, 1e4))
    rgl::par3d(skipRedraw=TRUE)
    width = min(rgl::par3d()$viewport['width']*width, floor(rgl::par3d()$viewport['height']*aspect))
    height = round(width/aspect)
    rgl::par3d(windowRect=c(0,0,width,height))
  } else {
    height = round(width/aspect)
    rgl::open3d(windowRect = c(0, 0, width, height))
    rgl::par3d(skipRedraw=TRUE)
    if (rgl::par3d()$viewport['width']!=width) stop('requested width too low or too large for screen')
    if (rgl::par3d()$viewport['height']!=height) stop('requested height too low or too large for screen')
  }

  # set light source
  if (!light) rgl::clear3d(type="lights")

  # set scales and axes
  rgl::decorate3d(xlim=xlim, ylim=ylim, zlim=zlim, axes=axes,
                  xlab=xlab, ylab=ylab, zlab=zlab, box=FALSE)
  rgl::par3d(ignoreExtent=fixed)

  # reset rotation of scene
  if (is.matrix(orientation)) {
    if (dim(orientation)[1]!=3 | dim(orientation)[2]!=3) stop('orientation must be a 3-by-3 matrix or a 2-character string')
    userMatrix = rgl::rotationMatrix(matrix=orientation)
    rgl::view3d(userMatrix=rgl::identityMatrix(), fov=fov, ...)
  } else if (is.character(orientation)) {
    if (nchar(orientation)!=2) stop('orientation must be a 3-by-3 matrix or a 2-character string')
    rgl.orthoview(plane=orientation, fov=fov, ...)
  } else {
    stop('orientation must be a 3-by-3 matrix or a 2-character string')
  }

  # set background color
  rgl::bg3d(col=col)

  # update everything
  rgl::par3d(skipRedraw=FALSE)

}
