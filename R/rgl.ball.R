#' @title Plot high-resolution sphere / globe
#'
#' @description Draws a sphere in custom resolution with custom surface image.
#'
#' @importFrom rgl cube3d subdivision3d asHomogeneous2 asEuclidean2 addNormals shade3d surface3d
#'
#' @param x x-coordinate of the center of the sphere
#' @param y y-coordinate of the center of the sphere
#' @param z z-coordinate of the center of the sphere
#' @param radius radius of the sphere
#' @param depth integer (1...7) specifying the number of rectangles (=6*4^depth)
#' @param png optional character string specifying the filename of a png-image to be rendered on the sphere. This file must contain the map to be displayed in an equirectangular projection (also known as equidistant cylindrical projection).
#' @param ... additional parameter to refine the material properties (see \code{\link[rgl]{rgl.material}}).
#'
#' @author Danail Obreschkow (thanks to input from Aaron Robotham's \code{sphereplot} package)
#'
#' @examples
#' # Show Earth with core
#' rgl.new(width=0.5, aspect=1.0, col='black', xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1))
#' rgl.ball(0, 0, 0, 1, png='earth.png', alpha=0.5)
#' rgl.ball(0, 0, 0, 0.6, col='red')
#'
#' @export rgl.ball

rgl.ball = function(x=0, y=0, z=0, radius=1, depth=4, png=NULL, ...) {

  if (depth<1 | depth>7 | depth!=round(depth)) stop('depth must be a positive integer <=7.')
  # depth = integer specifying the number of quads (=6*4^depth)
  # ... material properties

  if (is.null(png)) {

    # produce sphere from cube
    cube = rgl::cube3d()
    sphere = rgl::subdivision3d(cube,depth=depth)
    sphere$vb[4,] <- apply(sphere$vb[1:3,], 2, function(x) sqrt(sum(x^2)))

    # scale to custom radius
    sphere$vb[4,] = sphere$vb[4,]/radius

    # translate
    sphere$vb = rgl::asHomogeneous2(rgl::asEuclidean2(sphere$vb)+c(x,y,z))

    # render sphere
    sphere = rgl::addNormals(sphere)
    rgl::shade3d(sphere, ...)

  } else {

    # adopted from sphereplot::rgl.sphglobe by A. Robotham

    # grid of spherical coordinates
    nlong = 3*2^depth
    nlat = 2*2^depth
    long = seq(-180, 180, len = nlong) * pi/180
    lat = seq(90, -90, len = nlat) * pi/180
    longmat = matrix(long, nlong, nlat)
    latmat = matrix(lat, nlong, nlat, byrow = TRUE)

    # grid of cartesian coordinates
    px = x + radius * cos(latmat) * cos(longmat)
    py = y + radius * cos(latmat) * sin(longmat)
    pz = z + radius * sin(latmat)

    # render sphere
    rgl::surface3d(x, y, z, col = "white", texture = png, axes = FALSE,
                   box = FALSE, xlab = "", ylab = "", zlab = "", normal_x = x,
                   normal_y = y, normal_z = z, textype = "rgb", ...)
  }
}
