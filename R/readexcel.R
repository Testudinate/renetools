readexcel <-
function(file, table){
  require(RODBC)
  db <- odbcConnectExcel(file)
  dat <- sqlQuery(db, paste("select * from", table))
  close(db)
  dat
}
