#' @title Plot high-resolution sphere / globe
#'
#' @description Draws a sphere in custom resolution with custom surface image.
#'
#' @importFrom rgl cube3d subdivision3d asHomogeneous2 asEuclidean2 addNormals shade3d surface3d rgl.light
#'
#' @param x x-coordinate of the center of the sphere
#' @param y y-coordinate of the center of the sphere
#' @param z z-coordinate of the center of the sphere
#' @param radius radius of the sphere
#' @param depth integer (1...8) specifying the number of rectangles (=6*4^depth)
#' @param png optional character string specifying the file name of a png-image to be rendered on the sphere. This file must contain the map to be displayed in an equirectangular projection (also known as equidistant cylindrical projection).
#' @param rotation optional 3-by-3 or or 4-by-4 rotation matrix applied to the whole globe; only used of \code{png} is specified.
#' @param normals character string specifying the way the normal vectors of the surface are internally passed to \code{\link[rgl]{surface3d}}. This argument is available because \code{\link[rgl]{surface3d}} (or rather the underlying routine \code{\link[rgl]{rgl.surface}}) sometimes handles the sign of normal vectors incorrectly, causing light sources to appear in the wrong direction if a rotation matrix is provided. The argument can take three values: "none" does not pass any normal vectors to \code{\link[rgl]{rgl.surface}}, hence avoiding any issues with the direction of light sources, but this can cause glitches at the 180-degree meridian (choose a high depth around 7 in this case); "standard" passes correct normal vectors to \code{\link[rgl]{rgl.surface}}, which can cause wrong lighting for certain rotation matrices; "improved" is similar to "standard", but corrects the lighting errors in most cases.
#' @param ... additional parameter to refine the material properties (see \code{\link[rgl]{rgl.material}}).
#'
#' @author Danail Obreschkow (thanks to input from Aaron Robotham's \code{sphereplot} package)
#'
#' @examples
#' # Show Earth with core
#' rgl.new(width=0.5, aspect=1.0, orientation='yz', light=FALSE, col='black',
#'         xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1))
#' rgl.ball(0, 0, 0, 1, png=system.file('earth.png', package='rglplus'), emission='grey', alpha=0.5)
#' rgl.ball(0, 0, 0, 0.6, col='red')
#' rgl::rgl.light(60, 30)
#'
#' @export rgl.ball

rgl.ball = function(x=0, y=0, z=0, radius=1, depth=5, png=NULL, rotation=NULL, normals='standard', ...) {

  if (depth<1 | depth>8 | depth!=round(depth)) stop('depth must be a positive integer <=8.')

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
    grid = expand.grid(long=long,lat=lat)

    # grid of Cartesian coordinates
    xyz = rbind(cos(grid$lat)*cos(grid$long),cos(grid$lat)*sin(grid$long),sin(grid$lat))
    if (!is.null(rotation)) {
      if (!is.matrix(rotation)) stop('if given, rotation must be a 3-by-3 or 4-by-4 matrix')
      if (dim(rotation)[1]!=dim(rotation)[2]) stop('if given, rotation must be a 3-by-3 or 4-by-4 rotation matrix')
      xyz = rotation[1:3,1:3]%*%xyz
    }
    px = array(xyz[1,],c(nlong,nlat))
    py = array(xyz[2,],c(nlong,nlat))
    pz = array(xyz[3,],c(nlong,nlat))

    # render sphere
    if (normals == 'none') {
      rgl::surface3d(x+radius*px, y+radius*py, z+radius*pz, col = "white", texture = png, axes = FALSE,
                     box = FALSE, xlab = "", ylab = "", zlab = "", textype = "rgb", ...)
    } else {
      if (normals == 'standard') {
        parity = 1
      } else if (normals == 'improved') {
        parity = 1-2*(1+(px[2]<px[1])+(py[2]<py[1]))%%2 # very tricky - see rgl.surface source code
      } else {
        stop('value of normals unknown.')
      }
      rgl::surface3d(x+radius*px, y+radius*py, z+radius*pz, col = "white", texture = png, axes = FALSE,
                     box = FALSE, xlab = "", ylab = "", zlab = "",
                     normal_x=parity*px, normal_y=parity*py, normal_z=parity*pz, textype = "rgb", ...)
    }
  }
}
