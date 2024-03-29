#' @title Stop drawing on screen
#'
#' @description Prevents the following rgl functions from drawing on the screen, until the function \code{\link{rgl.draw}} is called. This is used to accelerate complex drawings. This routine is identical to calling \code{par3d(skipRedraw=TRUE)}.
#'
#' @importFrom rgl par3d
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{rgl.draw}}
#'
#' @export rgl.hold

rgl.hold = function() {
  rgl::par3d(skipRedraw=TRUE)
}
