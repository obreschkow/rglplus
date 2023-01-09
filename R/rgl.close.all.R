#' @title Close all open rgl windows
#'
#' @description Checks if any rgl windows are currently open and, if so, closes them.
#'
#' @importFrom rgl cur3d close3d
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @export rgl.close.all

rgl.close.all = function() {

  while (rgl::cur3d()[[1]]>0) {rgl::close3d()}

}
