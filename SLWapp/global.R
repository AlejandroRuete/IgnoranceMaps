# The bundled grids use SWEREF99 TM, but their GeoTIFFs omit the CRS.
readGrid <- function(path) {
  x <- terra::rast(path)
  if (!nzchar(terra::crs(x))) terra::crs(x) <- terra::crs(Swe)
  x
}

Swe <- terra::vect("data/Sweden Simple Sweref.shp")
GreyColors<-colorRampPalette(c("white", "black"),interpolate="spline", space="Lab")( 16 )
RedBlue<-colorRampPalette(c("blue","white", "red"),interpolate="spline", space="Lab")( 11 )
Topo<-terrain.colors(16)
Topo[16]<-"#FFFFFFFF"

Amp <- readGrid("data/Amp.tif")
AmpR <- readGrid("data/Amp richness.tif")
Buf<-readGrid("data/Buf.tif")
Pel<-readGrid("data/Pel.tif")

Bir <- readGrid("data/Bir.tif")
BirR <- readGrid("data/Bir richness.tif")
Par<-readGrid("data/Par.tif")
Poe<-readGrid("data/Poe.tif")

Pae <- readGrid("data/Pae.tif")
PaeR <- readGrid("data/Pae richness.tif")
Pap<-readGrid("data/Pap.tif")
Col<-readGrid("data/Col.tif")

Mam <- readGrid("data/MamLnB.tif")
MamR <- readGrid("data/MamLnB richness.tif")
Alc<-readGrid("data/Alc.tif")
Eri<-readGrid("data/Eri.tif")

Opi <- readGrid("data/Opi.tif")
OpiR <- readGrid("data/Opi richness.tif")
Opca<-readGrid("data/Opc.tif")
Lac<-readGrid("data/Lac.tif")

Odo <- readGrid("data/Odo.tif")
OdoR <- readGrid("data/Odo richness.tif")
Lib<-readGrid("data/Lib.tif")
Neh<-readGrid("data/Neh.tif")

Vas <- readGrid("data/Vas.tif")
VasR <- readGrid("data/Vas richness.tif")
Pan<-readGrid("data/Pan.tif")
Eup<-readGrid("data/Eup.tif")

cellwdata <- which(!is.na(terra::values(Amp, mat = FALSE)))

# Use computed cell values rather than cached statistics for derived layers.
rasterMax <- function(x) terra::global(x, "max", na.rm = TRUE)[1, 1]
normalizeRaster <- function(x) {
  maximum <- rasterMax(x)
  if (is.finite(maximum) && maximum == 0) return(x)
  x / maximum
}

# Keep the scale bar at the original bottom-right cell centre.
drawScaleBar <- function(x) {
  right <- terra::xFromCol(x, ncol(x))
  bottom <- terra::yFromRow(x, nrow(x))
  scale.lng <- 100000
  segments(right, bottom, right - scale.lng, bottom, lwd = 2)
  text(right - scale.lng / 2, bottom + 50000,
       labels = paste(scale.lng / 1000, "km"), cex = 1.5, xpd = NA)
}