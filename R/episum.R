episum <-
  function(times, depth, temp, dat){
    check <- (ncol(temp) == ncol(dat)) & (nrow(temp) == nrow(dat)) & (nrow(temp) == length(depth)) & (nrow(dat) == length(depth)) & (length(times) == ncol(dat))
    if(check == FALSE){stop("Dimensions of temp, dat and depths don't fit together!")}
    zmix <- NULL
    for(i in 1:ncol(temp)){
      zmix[i] <- getzmix(rev(-1 * depth), rev(temp[,i]), approx=TRUE)  #need positive depths, ordered according to depth -> rev needed in this example to reverse the order
    }
    ## multiply again with -1 to make compatible again with example model output
    zmix <- zmix * -1
    meanepi <- NULL
    for(i in 1:ncol(dat)){
      id  <- which(depth > zmix[i]) #which dephts are above mixing depth = epilimnion
      meanepi[i] <- sum(dat[id, i])  
    }
    mean10  <- colSums(dat[nrow(dat):(nrow(dat)-9),])
    meanall <- colSums(dat)
    data.frame(times=times, zmix=zmix, sumepi=meanepi, sum10=mean10, sumcolumn=meanall)
  }
