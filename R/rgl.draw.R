#' @title Continue drawing on screen
#'
#' @description Updates screen display after this was stopped using the function \code{\link{rgl.hold}}.
#'
#' @importFrom rgl par3d
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{rgl.draw}}
#'
#' @export rgl.draw

rgl.draw = function() {
  rgl::par3d(skipRedraw=FALSE)
}
