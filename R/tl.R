tl <-
function(jahr, lang="english"){
  langavail <- c("german","english","germanfull","englishfull")
  if(length(lang) == 1){
    langcheck <- lang %in% langavail
    if(langcheck == FALSE){
      warning(paste(lang,"as chosen language not available! Fall back to English abbreviations."))
      lang <- "english"
    }
    monate <- switch(
      lang,
      german      = c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Dez"),
      english     = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
      germanfull  = c("Januar","Februar","Maerz","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember"),
      englishfull = c("January","February","March","April","May","June","July","August","September","October","November","December"),
    )
  } else {
    if (length(lang) == 12) {
      monate <- lang
    } else {
      stop("Argument lang of inproper length! Vector of names needs 12 elements.")
    }
  }
  tage   <- NULL
  woche  <- NULL
  for(i in 1:12){
    tage <- c(tage,format(as.POSIXct(paste(jahr,"/",i,"/","01",sep="")), "%j"))
    woche<- c(woche,format(as.POSIXct(paste(jahr,"/",i,"/","01",sep="")), "%W"))
  }
  data.frame(d = as.numeric(tage), m = as.character(monate), w=as.numeric(woche))
}
