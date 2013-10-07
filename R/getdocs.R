getdocs <-
function(pfadr, pattern="\\.TXT"){
  files <- dir(pfadr, pattern=pattern)
  files
}
