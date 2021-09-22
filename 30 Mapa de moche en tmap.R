#------------------------------------------------------------------------
require(pacman)
pacman::p_load(RColorBrewer, ggspatial, raster,colorspace, ggpubr, sf,openxlsx)
#------------------------------------------------------------------------
library(extrafont)
Peru               <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Cuencas_peru       <- st_read ("SHP/Cuencas_peru.shp")  
Rio_libe           <- st_read ("SHP/RIOS_LA_LIBERTAD_geogpsperu_SuyoPomalia_931381206.shp")  
Cuencas_peru       <- st_transform(Cuencas_peru ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rio_libe           <- st_transform(Rio_libe ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Cuenca_moche       <- subset(Cuencas_peru , NOMB_UH_N6  == "Moche")
Cuencas_rios       <- st_intersection(Rio_libe, Cuenca_moche)

dem  = raster("raster/ASTGTM_S08W079_dem.tif")
dem2 = raster("raster/ASTGTM_S09W079_dem.tif")
dem3 = raster("raster/ASTGTM_S09W080_dem.tif")
DEM_total <- raster::merge(dem, dem2,dem3)

Cuenca_moche_alt     <- crop(DEM_total, Cuenca_moche)
Cuenca_moche_alt     <- Cuenca_moche_alt  <- mask(Cuenca_moche_alt , Cuenca_moche)
plot(Cuenca_moche_alt)

slope = terrain(Cuenca_moche_alt, opt = "slope") 
aspect = terrain(Cuenca_moche_alt, opt = "aspect")
hill = hillShade(slope, aspect, angle = 40, direction = 270)
#------------------------------------------------------------------------
cols <-c("#5F8141","#779F44","#93C649","#A9DD55","#CBD689","#ECE5B2","#E1C678","#978055","#45280E")

map =tm_shape(hill) +
  tm_raster(palette = gray(0:100 / 100), n = 100, legend.show = FALSE) +
  tm_shape(Cuenca_moche_alt) +
  tm_raster(alpha = 0.5, palette = cols,n=12,
            legend.show = T, title="Elevacion(m.s.n.m)")+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "black", color.dark = "lightsteelblue4", 
               position = c(.3, 0.005), lwd = 1, color.light= "white")+
  tm_compass(type="radar", position=c("right", "top"), text.color = "black")+
  tm_layout(title = "Mapa de Elevacion de la \nCuenca de MOCHE", legend.title.size=.9,legend.text.size = 0.7,
            legend.position = c(.90, .05) ,
            legend.hist.width = 0.2,
            legend.hist.height = 0.2, 
            title.color  = "black",
            title.size = .9,
            legend.title.color = "black",
            legend.text.color = "black",
            fontface="bold",
            legend.stack = 'horizontal',
            bg.color="#F9F9F9",
            legend.bg.color = "#F9F9F9",
            panel.labels = c("R y RStudio con paquete Tmap"),
            panel.label.color = "darkslateblue",main.title.position = "center",
            main.title = "Terrain analysis based on DEM de la cuenca Moche. Mapa:R", main.title.size = 0.8)+
  tm_credits("MAPA de RELIEVE \nde", position = c(0.4, .17), col = "black", fontface="bold")+
  tm_credits("CUENCA HIDROGRAFICA de \nMOCHE", position = c(0.4, .14), col = "black", fontface="bold", size=1)+
  tm_credits("Data: DEM SRTM \n#Aprende R desde Cero Para SIG \nGorky Florez Castillo", position = c(0.005, .6), col = "black", fontface="bold")+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 3, position = c(0.74, 0.20))+
  tm_grid(col = "grey90",ticks = T, labels.col = "black")

tmap_save(map , "Mpas/Mapa del moche.png", dpi = 1200, height = 10)





