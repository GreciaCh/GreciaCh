data_cg_caballa <- read.csv(file = "data/CG caballa soxhlet.csv", stringsAsFactors = FALSE, sep = ";")
secuencia <- seq(1,length(data_cg_caballa$Rango.Letra),3)
rango_letra_full <- NULL

for (i in seq_along(secuencia)) {
  rango_letra <- data_cg_caballa$Rango.Letra[secuencia[i]:(secuencia[i]+2)]
  rango_letra_rep <- rep(rango_letra[1],3)
  
  rango_letra_full <- c(rango_letra_full, rango_letra_rep)
  
}

data_cg_caballa$P4 <- data_cg_caballa$P3-data_cg_caballa$P2
data_cg_caballa$P5 <- data_cg_caballa$P4/data_cg_caballa$P1*100
data_cg_caballa$Rango.Letra.Full <- rango_letra_full

# Calcular promedio por rango ---------------------------------------------

cg_caballa_mean <- NULL

for (i in seq_along(secuencia)) {
  cg_rep <- round(data_cg_caballa$P5[secuencia[i]:(secuencia[i]+2)], 2)
  cg_mean <- round(mean(data_cg_caballa$P5[secuencia[i]:(secuencia[i]+2)], na.rm = TRUE),2)
  data_cg <- data_cg_caballa[secuencia[i]:(secuencia[i]+2),][1,c(1:4,6)]
  
  cg_caballa_mean <- rbind(cg_caballa_mean, c(data_cg, cg_rep, cg_mean))
}

colnames(cg_caballa_mean) <- c("Day", "Month", "Year", "Lugar", "Rango", "W1", "W2", "W3", "Wtotal")
write.csv(x = cg_caballa_mean, file = "output/cg caballa promedios.csv")
cg_caballa_mean <- read.csv(file = "output/cg caballa promedios.csv", stringsAsFactors = FALSE, row.names = 1)

cg_rango <- tapply(cg_caballa_mean$Wtotal, list(cg_caballa_mean$Rango, cg_caballa_mean$Month, cg_caballa_mean$Year),
                   mean, na.rm = TRUE)
cg_rango <- cg_rango[-1,,]

# Gráfico de lineas -------------------------------------------------------

labels_meses <- read.csv(file = "data/labels fecha.csv", stringsAsFactors = FALSE, sep = ";")
labels_meses <- labels_meses[-(1:24),]
cg_rango_CDE_2223_mean<- as.numeric(colMeans(cg_rango[3:5,,6:7], na.rm = TRUE))

par(mar = c(4,4,1,1))
plot(cg_rango_CDE_2223_mean[1:20], type = "o", pch = 19, col = "red3", axes = FALSE, xlab = NA, ylab = NA,
     ylim = c(0,15))
abline(v = 12.5, lty = "dotted", col = "gray70")
axis(side = 1, at = 1:length(labels_meses), labels = labels_meses)
axis(side = 2, at = 0:15, las = 2)
mtext(text = "Meses", side = 1, line = 3)
mtext(text = "Contenido graso (%)", side = 2, line = 3)
box()

dev.copy(png, filename = "figures/cg caballa lineas ago 23.png", width = 2500, height = 1200,
         res = 300)
dev.off()

# Gráfico barras rangos C, D Y E ------------------------------------------

cg_rango_CDE_2223<- cg_rango[3:5,,6:7]
texto_cg <- c("C (26-30 cm)", "D (31-33 cm)", "E (>33 cm)")

par(mar = c(0,0,0,0), oma = c(4,4,1,1), mfrow = c(3,1))
for (i in 1:3) {
  if (i == 3) {
    barplot(as.numeric(cg_rango_CDE_2223[i,,])[1:length(labels_meses)], ylim = c(0,17), 
            axes = FALSE, names.arg = labels_meses)
    axis(side = 2, at = seq(0,14,2), las = 2)
    legend("topright", legend = texto_cg[i], bty = "n")
    box()
    
  }else {
    barplot(as.numeric(cg_rango_CDE_2223[i,,])[1:length(labels_meses)], ylim = c(0,17), 
            axes = FALSE, names.arg = NA)
    axis(side = 2, at = seq(0,14,2), las = 2)
    legend("topright", legend = texto_cg[i], bty = "n")
    box()
  }
}

mtext(text = "Meses", side = 1, line = 3, outer = TRUE)
mtext(text = "Contenido graso (%)", side = 2, line = 2.5, outer = TRUE)

dev.copy(png, filename = "figures/cg caballa barras ago 23.png", width = 2200, height = 2600,
         res = 300)
dev.off()
