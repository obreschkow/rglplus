#' @title Close all open rgl windows
#'
#' @description Checks if any rgl windows are currently open and, if so, closes them.
#'
#' @importFrom rgl rgl.cur rgl.close
#'
#' @author Danail Obreschkow
#'
#' @export rgl.close.all

rgl.close.all = function() {

  while (rgl::rgl.cur()[[1]]>0) {rgl::rgl.close()}

}
