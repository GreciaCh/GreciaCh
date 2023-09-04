data <- read.csv2(file = "data/datos para AR.csv", stringsAsFactors = FALSE, sep = ";")

data <- data[data$sex == 0 & data$mat != 0, ]

FS_fases <- table(data$mat, data$month, data$year)

AR_year <- NULL

for (z in 1:dim(FS_fases)[3]) {
  AR <- NULL
  
  FS_year <- FS_fases[,,z]
  
  for (i in 1:ncol(FS_year)) {
    FS_mes <- FS_year[,i]
    FR_mes <- prop.table(FS_mes)*100
    AR <- c(AR, c(FR_mes[3] + FR_mes[4]))
  }
  AR_year <- rbind(AR_year,AR)
}
dimnames(AR_year) <- list(sort(unique(data$year)),1:12)

ar_caballa_mean <- colMeans(AR_year, na.rm = TRUE)
ar_caballa <- c(ar_caballa_mean[5:12],ar_caballa_mean[1:4])

par(mar = c(4,4,1,1)) 
plot(ar_caballa, type = "l", xlab = "Meses", ylab = "AR (%)", axes = FALSE,
     ylim = c(0,100), col = "blue")
axis(side = 1, at = 1:12,labels = c("My","Jn","Jl","Ag","S","O","N","D","E","F","Mz","Ab" ))
                                   
axis(side = 2, at = seq(10,100,10))
box()

dev.copy(png, filename = "figures/AR de jurel", width = 2500, height = 2500,
              res = 300)
dev.off()
