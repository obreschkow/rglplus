#' Produce a movie from and 3d rgl scene
#'
#' @importFrom stats splinefun approxfun
#' @importFrom rgl rgl.snapshot par3d lines3d clear3d rgl.light
#'
#' @description Generates an MP4-movie of a 3d rgl scene with time-dependent objects and/or a camera path. The routine has been developed and tested for MacOS and it requires on a working installation of ffmpeg.
#'
#' @param frame optional function that plots or updates the 3D scene at a given time. This function must have exactly one argument, which specifies the time of the frame.
#' @param path optional list that specifies the motion of the camera at some discrete times. The list contains the following elements (for more details see \code{\link{rgl.camera}}):\cr\cr
#' \code{time} = optional n-vector of strictly monotonically increasing discrete times, required if and only if one of the following four arguments (position, direction, up, fov) are provided as matrices/vectors. If not given, equally spaced in times between tmin and tmax are assumed.\cr
#' \code{position} = optional argument specifying the camera position along the path. This argument must be one of three types: (1) A 3-element vector specifies a fixed camera position for the whole movie. (2) A n-by-3 matrix specifies n discrete camera positions at the exact times given in the \code{time} vector (see above). The code automatically generates a smooth function going through these n points. (3) A function f(t) of a single time variable t, which returns a 3-element vector, specifies the exact position at that time.\cr
#' \code{direction} = optional argument specifying the direction of the camera's optical axis. This argument can be of the same three types as the \code{position} argument.\cr
#' \code{up} = optional argument specifying the camera's up-direction. This argument can be of the same three types as the \code{position} argument.\cr
#' \code{fov} = optional argument specifying the field-of-view (FoV) in degrees. Similarly to the above arguments, this can be either a single number (fixed FoV), a n-element vector (specifying the Fov at the n discrete times), or a scalar function (specifying the FoV at any time t).\cr\cr
#' @param tmin physical time of first frame in the movie.
#' @param tmax physical time of last frame in the movie.
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
#' path = list(position = function(t) c(1.1*cos(t),1.1*sin(t),0),
#' direction = function(t) c(-1.5*sin(t)-cos(t),1.5*cos(t)-sin(t),0),
#' up = function(t) c(cos(t),sin(t),sin(t)/2),
#' fov = 30)
#'
#' # Produce movie
#' \dontrun{
#' rgl.makemovie(frame=frame, path=path, tmin=0, tmax=2*pi, output.path='~/testmovie',
#'               output.filename = 'movie.mp4', ffmpeg.cmd = 'ffmpeg', nframes=600)
#' }
#'
#' @export

rgl.makemovie = function(frame=NULL, path=NULL,
                         tmin=0, tmax=1,
                         nframes=60, fps=60,
                         output.path,output.filename,
                         keep.frames=F,quiet=T,
                         separator=.Platform$file.sep,
                         ffmpeg.cmd='ffmpeg',
                         ffmpeg.opt='-vcodec libx264 -crf 18 -pix_fmt yuv420p') {

  if (is.null(frame) & is.null(path)) stop('at least one of the arguments frame or path should be provided to make successive frames different.')

  # construct path
  if (!is.null(path)) {

    smoothpath = {}

    for (iarg in seq(4)) {

      name = c('position','direction','up','fov')[iarg]
      arg = path[[name]]
      argerror = sprintf('argument path$%s is of unaccepted type.',name)
      ntimes = 0

      if (!is.null(arg)) {

        # make continuous function, if not already provided as such
        if (!is.function(arg)) {

          if ((iarg<=3 & length(arg)==3) | (iarg==4 & length(arg)==1)) {

            # make constant function
            f = function(t) arg

          } else if ((iarg<=3 & length(dim(arg))==2) | (iarg==4 & length(arg)>1)) {

            # get number of extrapolation points
            if (iarg<=3) {
              n = dim(arg)[1]
            } else {
              n = length(arg)
            }

            # make time vector if needed
            if (ntimes==0) {
              if (is.null(path$time)) {
                ntimes = n
                path$time = seq(tmin,tmax,length=ntimes)
              } else {
                if (!is.vector(path$time)) stop('path$time must be a vector')
                ntimes = length(path$time)
              }
            }

            # check if time vector has correct length
            if (ntimes!=n) stop(sprintf('the length of path$time is incompatible with path$%s',name))

            # make interpolation function
            if (iarg==1) {
              f = function(t) c(stats::splinefun(path$time, path$position[,1])(t),
                                stats::splinefun(path$time, path$position[,2])(t),
                                stats::splinefun(path$time, path$position[,3])(t))
            } else if (iarg==2) {
              f = function(t) c(stats::splinefun(path$time, path$direction[,1])(t),
                                stats::splinefun(path$time, path$direction[,2])(t),
                                stats::splinefun(path$time, path$direction[,3])(t))
            } else if (iarg==3) {
              f = function(t) c(stats::splinefun(path$time, path$up[,1])(t),
                                stats::splinefun(path$time, path$up[,2])(t),
                                stats::splinefun(path$time, path$up[,3])(t))
            } else if (iarg==4) {
              f = stats::approxfun(path$time, path$fov, rule=2) # use approx rather than spline to avoid leaving range
            }

          } else {

            stop(argerror)

          }

          smoothpath[[name]] = f

        } else {

          if (iarg==1) {
            smoothpath[[name]] = path$position
          } else if (iarg==2) {
            smoothpath[[name]] = path$direction
          } else if (iarg==3) {
            smoothpath[[name]] = path$up
          } else if (iarg==4) {
            smoothpath[[name]] = path$fov
          }

        }

        # check if function returns correct argument
        if (iarg<=3) {
          if (length(smoothpath[[iarg]](tmin))!=3) stop(argerror)
        } else {
          if (length(smoothpath[[iarg]](tmin))!=1) stop(argerror)
        }

      } else {

        smoothpath[[name]] = function(t) NULL

      }

    }
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
      rgl.camera(position = smoothpath$position(t),
                 direction = smoothpath$direction(t),
                 up = smoothpath$up(t),
                 smoothpath$fov(t))
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
