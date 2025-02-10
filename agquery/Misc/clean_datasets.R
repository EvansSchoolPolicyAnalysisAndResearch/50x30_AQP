#Small script to drop columns with all 0's or missings from the raw data files. 
#Run from within AgQuery wd

csvs <- list.files("Data", pattern=".csv", full.names=T)

for(csv in csvs){
  df <- read.csv(csv) |> data.table::as.data.table()
  df <- df[,which(unlist(lapply(df, function(x){!all(is.na(x) | x==0)}))),with=F]
  write.csv(df, csv, row.names=F)
}