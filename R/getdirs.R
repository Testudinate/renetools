getdirs <-
function(pfadr){
  dirs <- dir(pfadr)
  files <- dir(pfadr, pattern="\\.")
  idx <- match(files,dirs)
  dirs <- dirs[-idx]
  dirs
}
