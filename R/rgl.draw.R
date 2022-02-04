#' @title Continue drawing on screen
#'
#' @description Updates screen display after this was stopped using the function \code{\link{rgl.hold}}. This is identical to calling \code{par3d(skipRedraw=FALSE)}.
#'
#' @importFrom rgl par3d
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{rgl.draw}}
#'
#' @export rgl.draw

rgl.draw = function() {
  rgl::par3d(skipRedraw=FALSE)
}
