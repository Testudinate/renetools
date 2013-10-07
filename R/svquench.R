svquench <-
function(Fluo, Kvs){
  if(is.character(Kvs)){
    Kvs <- switch(
      Kvs,
      Ast_stag  = 0.002175253,#0.002025715, # determined with loess procedure  [1/Fluoreszenzeinheit]
      Aul_stag  = 0.003180959,#0.002371022, # determined with loess procedure  [1/Fluoreszenzeinheit]
      Fra_stag  = 0.002693194,#0.002376387, # determined with loess procedure  [1/Fluoreszenzeinheit]
      Ast_exp   = 4,
      Aul_exp   = 5,
      Fra_exp   = 6
    )
  }
  F0 <- Fluo/(1 - Kvs * Fluo)
  F0
}
