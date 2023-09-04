#como unir year, month, day 

data <- read.csv(file = "data/Santa Rosa_Bonito.csv", sep = ";")

data$date <- paste0(data$day,"/",data$month,"/",data$year)

#unir varias sheets en un solo documento

data_SR_1 <- read.csv(file = "data/mb de caballa 90-03.csv", sep = ";")
data_SR_2 <- read.csv(file = "data/mb de caballa 2004.csv", sep = ";")
data_SR_3 <- read.csv(file = "data/mb de caballa 05-15.csv", sep = ";")
data_SR_4 <- read.csv(file = "data/mb de caballa 16-21.csv", sep = ";")

data_total <- rbind(data_SR_1,data_SR_2,data_SR_3,data_SR_4)

write.csv(data_total, file = "output/data_total_caballa_AV.csv")

#agregar columnas a un archivo

data_total <- read.csv(file = "output/data_total_caballa_AV.csv", sep = ",")

igs <- (data_total$wgon*data_total$wevisc)/100

fc <- ((data_total$wtot-data_total$wgon)/data_total$long^3)*100

wtot_wevisc <- (data_total$wtot - data_total$wevisc)
wsingonad <- (data_total$wtot - data_total$wgon)
wtotal_solo <- (data_total$wtot - (data_total$wevisc + data_total$wgon))

data_total_igs_fc <- cbind(data_total,wtot_wevisc,wsingonad,wtotal_solo,igs,fc)

head(data_total)

write.csv(data_total_igs_fc,file = "output/data_total_caballa_AV.csv")

#
#