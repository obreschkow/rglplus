#' @title Close all open rgl windows
#'
#' @description Checks if any rgl windows are currently open and, if so, closes them.
#'
#' @importFrom rgl close3d rgl.dev.list
#'
#' @author Danail Obreschkow
#'
#' @export rgl.close.all

rgl.close.all = function() {

  close3d(rgl.dev.list())

}
