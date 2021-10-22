#' @title Stop drawing on screen
#'
#' @description Prevents the following rgl functions from drawing on the screen, until the function \code{\link{rgl.draw}} is called. This is used to accelerate complex drawings.
#'
#' @importFrom rgl par3d
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{rgl.draw}}
#'
#' @export rgl.hold

rgl.hold = function() {
  rgl::par3d(skipRedraw=TRUE)
}
