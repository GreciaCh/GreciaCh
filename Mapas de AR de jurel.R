# GRAFICAR EN MAPA --------------------------------------------------------

library(fenix)

addIsopara_1 = function (dataIsopara = lonlat_areaIso, Cols = "lightgray", ylim = ylim, col_area) 
{
  dataIsopara = subset(dataIsopara, subset = dataIsopara$lat <= 
                         ylim[2] & dataIsopara$lat >= ylim[1])
  idx_areas = unique(dataIsopara$area)
  for (r in seq_along(idx_areas)) {
    temp = subset(x = dataIsopara, subset = dataIsopara$area == 
                    idx_areas[r])
    polygon(x = c(temp$lon[1], temp$lon[2:nrow(temp)], temp$lon[1]), 
            y = c(temp$lat[1], temp$lat[2:nrow(temp)], temp$lat[1]), 
            border = Cols, col = col_area)
  }
}

# definiendo parametros ---------------------------------------------------
  ##IGS DE JUREL PARA EL 2019

vectore_areas_1 = c(10140,11140,1160,1170,2173,3160,4180)
vectore_areas_2 = c(1120,3170,4133,4140,4160,4163,8140,8143)

  ##IGS DE JUREL PARA EL 2020

vectore_areas_3 = c(2140)
vectore_areas_4 = c(3140,4103,4170,5133,5180)

  ##IGS DE JUREL PARA EL 2021

vectore_areas_5 = c(1050,1133,2140)
vectore_areas_6 = c(1163,3170,4173,5150,7113,7140,8123)

  ##IGS DE JUREL PARA EL 2022

vectore_areas_7 = c(3150,4150)
vectore_areas_8 = c(1093,1170,2150,2153,4130,4153,4163,5163)

categorias = c("< 1,5", "> 1,5")
colores = c("lightblue","orange")
titulo_leyenda = "Indice gonadosomático"

lista_vectores <- list(c(vectore_areas_1, vectore_areas_2))

# filtrando areas ---------------------------------------------------------

areas_1 = lonlat_areaIso_x <- lonlat_areaIso[lonlat_areaIso$area %in% vectore_areas_1, ]
areas_2 = lonlat_areaIso_x <- lonlat_areaIso[lonlat_areaIso$area %in% vectore_areas_2, ]

# ploteando mapa y areas --------------------------------------------------
## FALSE si no desean que salgan todas las área
par(mar = c(2,2,1,1))
mapa_peru2(area_iso = TRUE, col_harbor = NA,ylim = c(-21, -3),cex_harbor = 1.6,
          name_area_iso = lista_vectores, n_perfil = 4) 

addIsopara_1(dataIsopara = areas_1, ylim = c(-21, -3), Cols = "gray30", col_area = colores[1])
addIsopara_1(dataIsopara = areas_2, ylim = c(-21, -3), Cols = "gray30", col_area = colores[2])


# leyenda -----------------------------------------------------------------

legend(x = -77,y = -5,legend = categorias, fill = colores, bg = NULL, density = NA, box.lwd = "o",
       cex =1.8, title.cex = 1.8,title = titulo_leyenda)


# guardar imagen ----------------------------------------------------------

dev.copy(jpeg,filename = "figures/mapa IGS del 2022-A.jpeg", units = "cm",res = 300,
         width = 30, height = 30)

dev.off()



# MAPA DE AR --------------------------------------------------------------

# CALCULAR AR -------------------------------------------------------------
#tabla_fases <- table(data$mat, data$month, data$year)
#tabla_fases_2019 <- tabla_fases[,,5]
#tabla_hembras_totales <- tabla_fases[1:5,9:12,5]
#tabla_fases_2020 <- tabla_fases[3:4,9:12,6]
#tabla_fases_2021 <- tabla_fases[3:4,9:12,7]
#tabla_fases_2022 <- tabla_fases[3:4,9:12,8]

# AR 2019  -----------------------------------------------------------------
#for (i in 1:12) {
#tabla_fases_2019_AR <- tabla_fases[i,,]/sum(tabla_fases[i,,])*100
#AR <- tabla_fases_2019_AR[3] + tabla_fases_2019_AR[4]
#FD <- tabla_fases_2019_AR[4]

#FR_monthly <- rbind(FR_monthly, c(AR, FD))

}

# definiendo parametros ---------------------------------------------------

#AR DE JUREL PARA EL 2019

vectore_areas_1 = c(10140,11140,1160,2173,3160,4180,8140)
vectore_areas_2 = c(1120,3170,4133,4140,4160,4163,8143)

#AR DE JUREL PARA EL 202O

vectore_areas_1 = c(2140)
vectore_areas_2 = c(3140,4103,4170,5133,5180)

#AR DE JUREL PARA EL 2021

vectore_areas_1 = c(1050,1133,1163,2140)
vectore_areas_2 = c(3170,4173,5150,7113,7140,8123)

#AR DE JUREL PARA EL 2022

vectore_areas_1 = c(2153,3150,4153)
vectore_areas_2 = c(1093,1170,2150,4130,4163,5163)

categorias = c("AR < 50% ", "AR > 50% ")

colores = c("lightgreen","darkorange")

titulo_leyenda = "AR 2020"

# filtrando areas ---------------------------------------------------------

areas_1 = lonlat_areaIso_x <- lonlat_areaIso[lonlat_areaIso$area %in% vectore_areas_1, ]
areas_2 = lonlat_areaIso_x <- lonlat_areaIso[lonlat_areaIso$area %in% vectore_areas_2, ]

# ploteando mapa y areas --------------------------------------------------

## FALSE si no desean que salgan todas las áreas
par(mar = c(4,4,1,1))
mapa_peru(area_iso = FALSE, col_harbor = NA,ylim = c(-21, -3), n_perfil = 1,cex_harbor = 1.0)              
addIsopara_1(dataIsopara = areas_1, ylim = c(-21, -3), Cols = "gray30", col_area = colores[1])
addIsopara_1(dataIsopara = areas_2, ylim = c(-21, -3), Cols = "gray30", col_area = colores[2])


# leyenda -----------------------------------------------------------------

legend(x = -76,y = -5,legend = categorias, fill = colores, bg = NULL, 
       density = NA, box.lwd = "o",cex = 1.0, title.cex = 1.2,
       title = titulo_leyenda)

# guardar imagen ----------------------------------------------------------

dev.copy(png,filename = "figures/mapa AR del 2020.png", units = "cm",res = 300,
         width = 30, height = 30)

dev.off()


# PLOTEAR AREAS  ----------------------------------------------------------

data <- read.csv(file = "output/IGS_code.csv", stringsAsFactors = FALSE, sep = ";")
data$color = NA
data$color[data$value > 1.5] <- "orange"
data$color[data$value < 1.5] <- "lightblue"
data = data[!is.na(data$value),]

# dibujar areas iso coloreadas + perfiles

#points_fishing$dc   = estima_dc(x = points_fishing$lon,y = points_fishing$lat)
#area                = area_isoparalitoral(dist_costa = points_fishing$dc ,latitude = points_fishing$lat)
#points_fishing$area = area$area
#points_fishing      = points_fishing[!is.na(points_fishing$area),]
#scale = 0:length(sort(unique(points_fishing$week)))*-3

maxAll = NULL
for(j in sort(unique(data$year))){

  dataj        = data[data$year == j,]
  tmp_isoareas = isopara$area %in% dataj$area
  tmp_isopara  = isopara[tmp_isoareas, ]
  idx_areas    = unique(tmp_isopara$area)
  
  maxVar = NULL
  for(i in seq_along(idx_areas)){

    temp = subset(x = tmp_isopara, subset = tmp_isopara$area == idx_areas[i])
    temp_data = subset(x = dataj, subset = dataj$area == idx_areas[i])
    colorVariable = temp_data$color#tapply(temp_data$lon, temp_data$area, length) ## aqui modificar las variable y la funcion para colorear las areas
    maxVar = c(colorVariable, maxVar)
  }
  maxAll = c(max(maxVar), maxAll)
}

# max_valor   = max(maxAll)
# scale.color = designer.colors(max_valor, c('#00007F','blue',
#                                            '#007FFF','cyan',
#                                            '#7FFF7F','yellow',
#                                            '#FF7F00','red','#7F0000'))

n_perfil = length(sort(unique(data$year)))
par(mar = c(3,3,3,3))
mapa_peru(n_perfil = n_perfil, area_iso = F, hadj = 0.8)
scale = rev(c(0,-3,-6,-9))
#scale = c(0,-3,-6,-9)

for(j in seq_along(unique(data$year))){
  
  dataj        = data[data$year == unique(data$year)[j],]
  tmp_isoareas = isopara$area %in% dataj$area
  tmp_isopara  = isopara[tmp_isoareas, ]
  idx_areas    = unique(tmp_isopara$area)
  
  maxVariable = NULL
  for(i in seq_along(idx_areas)){
    
    temp = subset(x = tmp_isopara, subset = tmp_isopara$area == idx_areas[i])
    temp_data = subset(x = dataj$color, subset = dataj$area == idx_areas[i])
    
    colorVariable = temp_data
    
    polygon(x = c(temp$lon[1]+scale[j], temp$lon[2:nrow(temp)]+scale[j], temp$lon[1]+scale[j]), 
            y = c(temp$lat[1], temp$lat[2:nrow(temp)], temp$lat[1]), 
            border = "black", col = colorVariable)
    
    
    #maxVariable = c(max(colorVariable), maxVariable)
  }
 
  text(x = -71.75+scale[j], y = -20, labels = unique(data$year)[j])
}

categorias = c("< 1,5", "> 1,5")
colores = c("lightblue","orange")
titulo_leyenda = "Gonadosomatic\nindex"


legend("bottomleft", x.intersp = 0.5, y.intersp = 0.7, legend = categorias, fill = colores,
       bg = NULL, density = NA,box.lwd = "o",cex = 1.4, title.cex = 1.6, title = titulo_leyenda)

dev.copy(png,filename = "figures/mapa.png", units = "cm",res = 300,
         width = 30, height = 30)
dev.off()



