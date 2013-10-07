vertmeanown <-
function(depth, vari, top, bot) {
 #if(min(depth) < top){print("min smaller than top!")}
 #if(max(depth) > bot){print("max bigger than bot!")}
 #print(depth)
 depthori <- depth
 variori  <- vari
 if(bot < max(depth)){
   tdepth <- depth[depth <= bot]
   tvari  <- vari[depthori <=  bot]
   if(length(tdepth) < 1){
      tdepth <- bot
      tvari  <- vari[which(vari == min(vari))[1]]
   }
   depth <- tdepth
   vari  <- tvari
 }
 depthori2 <- depth
 if(top > min(depth)){
   depth <- depth[depth >= top]
   vari  <- vari[depthori2 >=  top]
 }
 mdepth <- c(top,(depth[-1] + depth[-length(depth)])/2,bot)
 thediff <- diff(mdepth)
 sum(thediff * vari) / sum(thediff)
}
