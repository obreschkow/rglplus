#' Produce a movie from and 3d rgl scene
#'
#' @importFrom stats splinefun approxfun
#' @importFrom rgl rgl.snapshot par3d lines3d clear3d rgl.light
#'
#' @description Generates an MP4-movie of a 3d rgl scene with time-dependent objects and/or a camera path. The routine has been developed and tested for MacOS and it requires on a working installation of ffmpeg.
#'
#' @param frame optional function that plots or updates the 3D scene at a given time. This function must have exactly one argument, which specifies the time of the frame.
#' @param path optional list that specifies the motion of the camera at some discrete times. The list contains the following elements (for more details see \code{\link{rgl.camera}}):\cr\cr
#' \code{time} = optional n-vector of strictly monotonically increasing discrete times; if not given, the other arguments (potion, direction, up, fov) are assumed equally spaced in time.\cr
#' \code{position} = n-by-3 matrix of n discrete camera positions at the specified times; can also be a 3-vector specifying a constant position.\cr
#' \code{direction} = n-by-3 matrix of n discrete camera directions at the specified times; can also be a 3-vector specifying a constant direction. If not given, the tangent vector of the path is used as direction.\cr
#' \code{up} = n-by-3 matrix of n discrete up-directions of the camera; can also be a 3-vector specifying a constant up-direction. If not given, the (outward facing) normal vector of the path is used as up-direction.\cr
#' \code{fov} = n-vector of field of view (in degrees) at the specified times; an also be a single number specifying a constant field of view.\cr\cr
#' @param tmin time of first frame in the movie. Should normally not be smaller than the first time in path$time. If not specified, the first value in path$time is adopted.
#' @param tmax time of last frame in the movie. Should normally not be larger than the last time in path$time. If not specified, the last value in path$time is adopted.
#' @param nframes number of frames in the movie. The time variable is sampled evenly between \code{tmin} and \code{tmax}.
#' @param fps number of frames per second
#' @param output.path character specifying the directory, where the movie and temporary frames are saved
#' @param output.filename movie filename without path. This filename should end on the extension '.mp4'.
#' @param keep.frames logical flag specifying whether the temporary directory with the individual frame files should be kept
#' @param quiet logical flag; if true, all console outputs produced by 'ffmpeg' are suppressed
#' @param separator filename separate of the system ('/' for Mac, Linux, Unix; '\' for Windows)
#' @param ffmpeg.cmd command used to call ffmpeg form a terminal. Normally, this is just 'ffmpeg'.
#' @param ffmpeg.opt optional arguments used with ffmpeg, such as compression and formatting options (see \url{https://www.ffmpeg.org/ffmpeg.html}).
#'
#' @details
#' Note that the frame width and height should be divisible by 2 for mp4 video compression to work.\cr
#' To accelerate the movie generation, it is possible to suppress the screen update by calling \code{\link{rgl.hold}} before calling \code{rgl.makemovie}.
#'
#' @author Danail Obreschkow
#'
#' @examples
#'
#' ## Example: Movie of spaceship flying around the Earth chasing a UFO
#'
#' # Produce basic scene
#' rgl.new(width=720, aspect=4/3, col='black', xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1))
#' rgl::clear3d(type = "lights")
#' rgl::rgl.light(80,30,viewpoint.rel = FALSE)
#' rgl.ball(0, 0, 0, 1, png=system.file('earth.png', package='rglplus'), depth=7, emission='#444466')
#'
#' # Make function to update frames
#' ptlast = NULL
#' frame = function(t) {
#'   rgl.hold()
#'   pt = c(1.01*cos(t+0.2),1.01*sin(t+0.2),sin(5*t)*0.02)
#'   if (!is.null(ptlast)) {
#'     rgl::pop3d()
#'     rgl::lines3d(rbind(ptlast,pt), col='red', lwd=3)
#'   }
#'   rgl.ball(pt[1], pt[2], pt[3], 0.003, col='orange', emission='#885500')
#'   ptlast <<- pt
#'   rgl.draw()
#' }
#'
#' # Make path
#' alpha = seq(0,2*pi,length=37)
#' path = list(position=cbind(1.1*cos(alpha),1.1*sin(alpha),0),
#'             direction=cbind(-1.5*sin(alpha)-cos(alpha),1.5*cos(alpha)-sin(alpha),0),
#'             up=cbind(cos(alpha),sin(alpha),sin(alpha)/2),
#'             fov=30)
#'
#' # Produce movie
#' \dontrun{
#' rgl.makemovie(frame=frame, path=path, tmin=0, tmax=2*pi, output.path='~/testmovie',
#'               output.filename = 'movie.mp4', ffmpeg.cmd = 'ffmpeg', nframes=600)
#' }
#'
#' @export

rgl.makemovie = function(frame=NULL, path=NULL,
                         tmin=NULL, tmax=NULL,
                         nframes=60, fps=60,
                         output.path,output.filename,
                         keep.frames=F,quiet=T,
                         separator=.Platform$file.sep,
                         ffmpeg.cmd='ffmpeg',
                         ffmpeg.opt='-vcodec libx264 -crf 18 -pix_fmt yuv420p') {

  if (is.null(frame) & is.null(path)) stop('at least one of the arguments frame or path should be provided to make successive frames different.')

  # construct path
  if (!is.null(path)) {

    s = list() # list of splines

    # handle times
    if (is.null(path$time)) {
      if (is.null(path$position)) stop('path$position must be provided')
      path$position = as.matrix(path$position)
      if (length(dim(path$position))!=2) stop('path$position must be an n-by-3 matrix')
      if (dim(path$position)[2]!=3) stop('path$position must be an n-by-3 matrix')
      n = dim(path$position)[1]
      path$time = seq(ifelse(is.null(tmin),0,tmin),ifelse(is.null(tmax),1,tmax),length=n)
    }
    if (!is.vector(path$time)) stop('path$time must be a vector')
    n = length(path$time)
    if (n==1) {
      path$time = rep(path$time,2)
      n = 2
    }
    if (is.null(tmin)) tmin = min(path$time)
    if (is.null(tmax)) tmax = max(path$time)

    # check camera position array
    if (is.null(path$position)) stop('path$position must be provided')
    path$position = as.matrix(path$position)
    if (length(path$position)==3) path$position = t(array(path$position,c(3,n)))
    if (length(dim(path$position))!=2) stop('path$position must be an n-by-3 matrix')
    if (dim(path$position)[1]!=n) stop('path$position must be an n-by-3 matrix')
    if (dim(path$position)[2]!=3) stop('path$position must be an n-by-3 matrix')

    # interpolate camera position
    s$x = stats::splinefun(path$time, path$position[,1])
    s$y = stats::splinefun(path$time, path$position[,2])
    s$z = stats::splinefun(path$time, path$position[,3])

    # construct camera direction array
    if (is.null(path$direction)) {
      dt = 0.1*min(diff(path$time))
      path$direction = array(NA,c(n,3))
      path$direction[,1] = (s$x(path$time+dt/2)-s$x(path$time-dt/2))/dt
      path$direction[,2] = (s$y(path$time+dt/2)-s$y(path$time-dt/2))/dt
      path$direction[,3] = (s$z(path$time+dt/2)-s$z(path$time-dt/2))/dt
    }

    # check camera direction array
    path$direction = as.matrix(path$direction)
    if (length(path$direction)==3) path$direction = t(array(path$direction,c(3,n)))
    if (length(dim(path$direction))!=2) stop('path$direction must be an n-by-3 matrix')
    if (dim(path$direction)[1]!=n) stop('path$direction must be an n-by-3 matrix')
    if (dim(path$direction)[2]!=3) stop('path$direction must be an n-by-3 matrix')

    # interpolate camera direction
    s$dx = stats::splinefun(path$time, path$direction[,1])
    s$dy = stats::splinefun(path$time, path$direction[,2])
    s$dz = stats::splinefun(path$time, path$direction[,3])

    # construct camera up-direction array
    if (is.null(path$up)) {
      dt = 0.1*min(diff(path$time))
      path$up = array(NA,c(n,3))
      path$up[,1] = -(s$dx(path$time+dt/2)-s$dx(path$time-dt/2))/dt
      path$up[,2] = -(s$dy(path$time+dt/2)-s$dy(path$time-dt/2))/dt
      path$up[,3] = -(s$dz(path$time+dt/2)-s$dz(path$time-dt/2))/dt
    }

    # check camera up-direction array
    path$up = as.matrix(path$up)
    if (length(path$up)==3) path$up = t(array(path$up,c(3,n)))
    if (length(dim(path$up))!=2) stop('path$up must be an n-by-3 matrix')
    if (dim(path$up)[1]!=n) stop('path$up must be an n-by-3 matrix')
    if (dim(path$up)[2]!=3) stop('path$up must be an n-by-3 matrix')

    # interpolate camera up-direction
    s$ux = stats::splinefun(path$time, path$up[,1])
    s$uy = stats::splinefun(path$time, path$up[,2])
    s$uz = stats::splinefun(path$time, path$up[,3])

    # construct field of view
    if (is.null(path$fov)) path$fov = 60

    # check field of view
    if (length(path$fov)==1) path$fov=rep(path$fov,n)
    if (length(path$fov)!=n) stop('path$fov must be a single number of n-vector')
    if (min(path$fov)<0) stop('path$fov cannot be below 0')
    if (max(path$fov)>179) stop('path$fov cannot be larger than 179')

    # interpolate field of view
    s$fov = stats::approxfun(path$time, path$fov, rule=2) # use approx rather than spline to avoid leaving range

  }

  # make output path, if needed
  if (substr(output.path,nchar(output.path),nchar(output.path))!=separator) {
    output.path=paste0(output.path,separator)
  }
  call = sprintf('mkdir -p %s',output.path)
  system(call)

  # make new path for frames
  frame.path = sprintf('%s%.0f%s',output.path,
                       as.numeric(Sys.time())*1e3,separator)
  call = sprintf('mkdir -p %s',frame.path)
  system(call)

  # write frames
  if (is.null(tmin)) stop('tmin must be specified')
  if (is.null(tmax)) stop('tmax must be specified')
  times = seq(tmin,tmax,length=nframes)
  for (i in seq(nframes)) {

    # make time step
    t = times[i]
    cat(sprintf('Write frame %0.6d at time %s.\n',i,signif(t,6)))

    # adjust frame
    if (!is.null(frame)) {
      frame(t)
    }

    # adjust camera
    if (!is.null(path)) {
      position = c(s$x(t), s$y(t), s$z(t))
      direction = c(s$dx(t), s$dy(t), s$dz(t))
      up = c(s$ux(t), s$uy(t), s$uz(t))
      fov = s$fov(t)
      rgl.camera(position, direction, up, fov)
    }

    # save frame
    fn = file.path(frame.path,sprintf('frame_%0.8d.png',i))
    rgl::rgl.snapshot(filename=fn,fmt="png",top=TRUE)
  }

  # convert into movie
  cat(sprintf('Write movie file %s\n',output.filename))
  linuxspaces = function (txt) gsub(" ", "\\\\ ", txt)
  call = sprintf('%s -y -r %d -f image2 -i %sframe_%%08d.png %s %s%s %s',
                 ffmpeg.cmd,fps,linuxspaces(frame.path),ffmpeg.opt,
                 linuxspaces(output.path),linuxspaces(output.filename),
                 ifelse(quiet,'-loglevel quiet',''))
  system(call)

  # remove frames
  if (!keep.frames) {
    cat('Delete individual frames.\n')
    call = sprintf('rm -rf %s',frame.path)
    system(call)
  }

}
