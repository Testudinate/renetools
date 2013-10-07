fddtxt2tab <-
function(pfadr){
  files <- getdocs(pfadr) # list all *.TXT files in pfadr
  lf <- length(files)
  out <- NULL
  for (i in 1: lf){
    print(paste(i, files [i]))
    dat <- readLines(myfile(pfadr,files[i])) # read content of txt-file
    ld  <- length(dat)
    start <- grep("#DATA", dat) + 1          # look for start of data
    tdat  <- dat[start:ld]                   # delete irrelevant data
    s1    <- strsplit(tdat, "\t")            # split lines at seperator
    s1    <- lapply(s1, as.numeric)          # convert strings to numeric
    s1    <- s1[-length(s1)]
    sout <- NULL
    ## replace somewhen with fancy: as.data.frame(t(sapply(x, "[")))
    for ( l in 1:length(s1)){                # loop over all lines
      s2     <- data.frame(sample=sub(".TXT","",files[i]), lambda=s1[[l]][1], F=s1[[l]][2])
      sout   <- rbind(sout, s2)              # buildt table for single sample
    }
    out <- rbind(out, sout)                  # buildt table for all samples
  }
  out
}
