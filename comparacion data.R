data_total <- read.csv(file = "data/data_total_caballa.csv", stringsAsFactors = FALSE, sep = ";")

data_total <- data_total[data_total$port %in% c(13,14) & data_total$month != "",]
data_total$date <- paste0(data_total$day, "/", data_total$month, "/", data_total$year)
#data_total$fecha <- as.POSIXct(paste0(data_total$year, "/", data_total$month, "/", data_total$day), formta = "%Y/%m/%d")

data_SR <- read.csv(file = "data/Data de Caballa de Santa Rosa.csv", stringsAsFactors = FALSE, sep = ";")
# data_pisco$fecha <- paste0(data_pisco$year, "/", data_pisco$month, "/", data_pisco$day)
#data_pisco$fecha <- as.POSIXct(paste0(data_pisco$year, "/", data_pisco$month, "/", data_pisco$day), formta = "%Y/%m/%d")

sort(unique(data_SR$date))

fechas_match <- sort(unique(match(data_SR$date, data_total$date)))

fechas_match <- data_total$date[fechas_match]

data_faltante <- data_SR[-(data_SR$date %in% fechas_match),]


data_total <- read.csv(file = "data/data_total_caballa.csv", stringsAsFactors = FALSE, sep = ";")

data_total_SR <- rbind(data_total, data_faltante)

write.csv(data_total_SR, file = "output/data_total_caballa.csv")


faltante <- NULL
for (i in 1:nrow(data_total)) {
  data_comparacion <- data_total[-1,-4]
  vector_comparacion <- data_total[1,-4]
  
  
  
  matchnames <- match(data_total[i,-1], data_faltante[i,-1])
  if (is.na(sum(matchnames[1:10]))) {
    linea <- NA
  }else{
    linea <- i
  }
  
  faltante <- c(faltante, linea)
}





##para ver cual es el problema con tu match de columnas 
##match(colnames(data_total), colnames(data_faltante))


