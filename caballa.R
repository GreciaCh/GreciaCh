data <- read.csv(file = "data/data_final_caballa.csv", stringsAsFactors = FALSE, sep = ";")
data <- data[data$sex == 0 & data$year == 2023 & data$igs < 20 & data$mat != 0 & data$hidratada != "H",]

patterns <- read.csv(file = "data/Patrones C,J y B.csv", stringsAsFactors = FALSE, sep = ";")
meses <- c("E22", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", 
           "E23", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
library(Hmisc)
data <- data[data$sex == 0 & data$long > 25 & data$igs < 20 & data$mat != 0,]

years <- sort(unique(data$year))

igs_year_month_caballa <- NULL
for (i in years) {
  data_year <- data[data$year == i,]
  data_igs_month <- colMeans(tapply(data_year$igs, list(data_year$puerto, data_year$month), mean, na.rm = TRUE), na.rm = TRUE)
  index <- match(sort(unique(data_year$month)), 1:12)
  data_igs_month_all <- rep(NA, 12)
  data_igs_month_all[index] <- data_igs_month
  
  igs_year_month_caballa <- rbind(igs_year_month_caballa, data_igs_month_all)
}

dimnames(igs_year_month_caballa) <- list(years, 1:12)
#data_igs_month_2 <- tapply(data_year$igs, list(data_year$month), mean, na.rm = TRUE)

igs_use <- igs_year_month_caballa[rownames(igs_year_month_caballa) %in% c(2022:2023),]
igs_use <- c(igs_use[1,], igs_use[2,])

mes_actual <- 12 + 5

ylim = c(0,15)
par(mar = c(4,4,1,1))
plot(igs_use[1:mes_actual], type = "o", pch = 19, col = "blue", ylim = ylim, axes = FALSE, xlab = NA, ylab = NA)
par(new = TRUE)
plot(rep(patterns$Caballa,2)[1:mes_actual], type = "o", pch = 19, col = "red", ylim = ylim, 
     axes = FALSE, xlab = NA, ylab = NA)
par(new = TRUE)
sdposi <- rep(patterns$Caballa,2) + patterns$desvest.2
sdneg <- rep(patterns$Caballa,2) - patterns$desvest.2
errbar(x = 1:16, y = rep(patterns$Caballa,2)[1:mes_actual], yplus = sdposi[1:mes_actual], 
       yminus = sdneg[mes_actual], ylim = ylim,
       axes = FALSE, xlab = NA, ylab = NA, errbar.col = "gray50", pch = NA, lwd = 1)
axis(side = 1, at = 1:17, labels = meses[1:mes_actual])
axis(side = 2, at = ylim[1]:ylim[2], las = 2)
mtext(text = "Meses", side = 1, line = 3)
mtext(text = "IGS (%)", side = 2, line = 3)
legend("topleft", legend = c("IGS", "Patrón"), lwd = 1, col = c("blue", "red"), bty = "n")
box()

dev.copy(png, filename = "figures/IGS caballa may 23.png", width = 2500, height = 1200,
         res = 300)
dev.off()

# IGS caballa JMP -----------------------------------------------------------
data_igs <- read.csv(file = "data/IGS caballa JMP.csv", stringsAsFactors = FALSE, sep = ";")
data_igs <- data_igs[data_igs$Year %in% c(2022:2023),]

meses <- c("E22", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", 
           "E23", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
library(Hmisc)
mes_actual <- 12 + 8


ylim = c(0,10)
par(mar = c(4,4,1,1))
plot(data_igs$IGS, type = "o", pch = 19, col = "blue", ylim = ylim, axes = FALSE, xlab = NA, ylab = NA,
     lty = "dashed")
par(new = TRUE)
plot(data_igs$Pattern, type = "o", pch = 19, col = "red", ylim = ylim, 
     axes = FALSE, xlab = NA, ylab = NA)
par(new = TRUE)
sdposi <- data_igs$Pattern + data_igs$desvest
sdneg <- data_igs$Pattern - data_igs$desvest
errbar(x = 1:length(data_igs$IGS), y = data_igs$Pattern, yplus = sdposi, 
       yminus = sdneg, ylim = ylim,
       axes = FALSE, xlab = NA, ylab = NA, errbar.col = "gray50", pch = NA, lwd = 1)
axis(side = 1, at = 1:mes_actual, labels = meses[1:mes_actual])

axis(side = 2, at = ylim[1]:ylim[2], las = 2)
mtext(text = "Meses", side = 1, line = 3)
mtext(text = "IGS (%)", side = 2, line = 3)
legend("topleft", y.intersp = 0.9, x.intersp = 0.5, legend = c("Patrón IGS (1990 - 2018)", "IGS"), lwd = 2, col = c("red", "blue"), bty = "n", lty = c(1,2))
box()

dev.copy(png, filename = "figures/IGS caballa JMP AGO 23.png", width = 2700, height = 1800,
         res = 300)
dev.off()

## CUANDO PIDEN QUE EL PUNTO ESTE EN EL MEDIO, SE AÑADE TICK = 0 A LA LINEA DE ARRIBA Y SE CREA ESTA:
####axis(side = 1, at = 0.5:17.5, labels = NA) EL 0.5 LO COLOCA AL MEDIO


  # IGS caballa Niño en abril por puertos--------------------------------------------------
library(abind)
data_abril <- data[data$month == 4 & data$year %in% c(2015,2017,2023),]

igs_abril_nino <- tapply(data_abril$igs, list(data_abril$puerto, data_abril$year), mean, na.rm = TRUE)

year_abril <- sort(unique(data_abril$year))
lenght_abril <- sort(unique(data_abril$long))
puertos_abril <- sort(unique(data_abril$puerto))

igs_talla_abril <- NULL

for (i in year_abril) {
  data_abril_year <- data_abril[data_abril$year == i,]
  igs_abril_nino_year <- tapply(data_abril_year$igs, list(data_abril_year$long, data_abril_year$puerto), 
                                mean, na.rm = TRUE)
  
  indexcol <- match(colnames(igs_abril_nino_year), puertos_abril)
  indexrow <- match(rownames(igs_abril_nino_year), lenght_abril)
  matrix_igs_abril_nino <- matrix(data = NA, nrow = length(lenght_abril), ncol = length(puertos_abril))
  matrix_igs_abril_nino[indexrow,indexcol] <- igs_abril_nino_year
  
  igs_talla_abril <- abind(igs_talla_abril, matrix_igs_abril_nino, along = 3)
  
}
dimnames(igs_talla_abril) <- list(lenght_abril, puertos_abril,year_abril )

# IGS caballa en abril por puertos--------------------------------------------------
data_abril <- data[data$month == 4,]

year_abril <- sort(unique(data_abril$year))
lenght_abril <- sort(unique(data_abril$long))
puertos_abril <- sort(unique(data_abril$puerto))

igs_talla_abril <- NULL

for (i in year_abril) {
  data_abril_year <- data_abril[data_abril$year == i,]
  igs_abril_nino_year <- tapply(data_abril_year$igs, list(data_abril_year$long, data_abril_year$puerto), 
                                mean, na.rm = TRUE)
  
  indexcol <- match(colnames(igs_abril_nino_year), puertos_abril)
  indexrow <- match(rownames(igs_abril_nino_year), lenght_abril)
  matrix_igs_abril_nino <- matrix(data = NA, nrow = length(lenght_abril), ncol = length(puertos_abril))
  matrix_igs_abril_nino[indexrow,indexcol] <- igs_abril_nino_year
  
  igs_talla_abril <- abind(igs_talla_abril, matrix_igs_abril_nino, along = 3)
  
}
dimnames(igs_talla_abril) <- list(lenght_abril, puertos_abril,year_abril )



# IGS caballa Pisco -------------------------------------------------------

data_pisco <- data[data$puerto == 60,]

year_pisco <- sort(unique(data_pisco$year))

tapply(data_pisco$igs, list(data_pisco$month, data_pisco$year), mean, na.rm = TRUE)

