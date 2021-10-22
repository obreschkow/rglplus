#' @title Open and initialize new 3D plot
#'
#' @description Calls \code{\link[rgl]{open3d}} and various additional functions to initialize a 3d plot.
#'
#' @importFrom rgl open3d par3d decorate3d bg3d identityMatrix rgl.viewpoint
#'
#' @param width either an integer (>1) specifying the number of pixels in the horizontal direction, or a real value (>0 and <=1) specifying the fraction of the available pixels. If the selected aspect ratio causes the number of vertical pixels to exceed the available number, the width is reduced as much as necessary.
#' @param aspect aspect ratio of window, defined as the ratio of vertical-to-horizontal size.
#' @param fov field of view in degrees, as used in \code{\link[rgl]{rgl.viewpoint}}
#' @param col background color
#' @param xlim 2-vector specifying the range along the x-axis
#' @param ylim 2-vector specifying the range along the y-axis
#' @param zlim 2-vector specifying the range along the z-axis
#' @param xlab character string specifying the label of the x-axis
#' @param ylab character string specifying the label of the y-axis
#' @param zlab character string specifying the label of the z-axis
#' @param axes logical flag specifying whether axes are displayed
#' @param fixed logical flag. If \code{TRUE} (default), the range of the axes is *not* adjusted as objects are drawn.
#' @param close.all logical flag. If \code{TRUE} (default), all existing rgl windows are closed before the new window is opened.
#'
#' @author Danail Obreschkow
#'
#' @examples
#' rgl.new(width=0.3, aspect=1, col='grey')
#' rgl.spheres(array(runif(30),c(10,3)), radius=0.1, col=rainbow(10))
#'
#' @export rgl.new

rgl.new = function(width=1, aspect=16/9, fov=30, col='white',
                   xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),
                   xlab='', ylab='', zlab='', axes=FALSE,
                   fixed = TRUE, # do not adjust view when plotting objects
                   close.all = TRUE) {

  # close previous windows
  if (close.all) rgl.close.all()

  # open new window of custom size
  if (width<=1) {
    rgl::open3d(windowRect = c(0, 0, 1e4, 1e4))
    width = min(rgl::par3d()$viewport['width']*width, floor(rgl::par3d()$viewport['height']*aspect))
    height = round(width/aspect)
    rgl::par3d(windowRect=c(0,0,width,height))
  } else {
    height = round(width/aspect)
    rgl::open3d(windowRect = c(0, 0, width, height))
    if (rgl::par3d()$viewport['width']!=width) stop('requested width too large for screen')
    if (rgl::par3d()$viewport['height']!=height) stop('requested height too large for screen')
  }

  # set scales and axes
  rgl::decorate3d(xlim=xlim, ylim=ylim, zlim=zlim, axes=axes,
                  xlab=xlab, ylab=ylab, zlab=zlab)
  rgl::par3d(ignoreExtent=fixed)

  # reset rotation of scene
  rgl::rgl.viewpoint(userMatrix=rgl::identityMatrix(), fov=fov)

  # set background color
  rgl::bg3d(col=col)

}
