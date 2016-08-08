require(raster)
require(rgdal)
library(maptools)


Swe<-readShapePoly("data/Sweden Simple Sweref.shp", proj4string=CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
GreyColors<-colorRampPalette(c("white", "black"),interpolate="spline", space="Lab")( 16 )
RedBlue<-colorRampPalette(c("blue","white", "red"),interpolate="spline", space="Lab")( 11 )
Topo<-terrain.colors(16)
Topo[16]<-"#FFFFFFFF"

Amp <- raster("data/Amp.tif")
AmpR <- raster("data/Amp richness.tif")
Buf<-raster("data/Buf.tif")
Pel<-raster("data/Pel.tif")

Bir <- raster("data/Bir.tif")
BirR <- raster("data/Bir richness.tif")
Par<-raster("data/Par.tif")
Poe<-raster("data/Poe.tif")

Pae <- raster("data/Pae.tif")
PaeR <- raster("data/Pae richness.tif")
Pap<-raster("data/Pap.tif")
Col<-raster("data/Col.tif")

Mam <- raster("data/MamLnB.tif")
MamR <- raster("data/MamLnB richness.tif")
Alc<-raster("data/Alc.tif")
Eri<-raster("data/Eri.tif")

Opi <- raster("data/Opi.tif")
OpiR <- raster("data/Opi richness.tif")
Opca<-raster("data/Opc.tif")
Lac<-raster("data/Lac.tif")

Odo <- raster("data/Odo.tif")
OdoR <- raster("data/Odo richness.tif")
Lib<-raster("data/Lib.tif")
Neh<-raster("data/Neh.tif")

Vas <- raster("data/Vas.tif")
VasR <- raster("data/Vas richness.tif")
Pan<-raster("data/Pan.tif")
Eup<-raster("data/Eup.tif")

cellwdata<-which(!is.na(Amp[]))

##################
# Shiny server function
shinyServer(function(input, output) {

# Return the requested dataset
datasetInput <- reactive({
     switch(input$dataset,
           "Amphibia" = Amp,
           "Aves" = Bir,
           "Papilionoidea" = Pae,
           "Mammals" = Mam,
           "Odonata" = Odo,
           "Opilions" = Opi,
           "Tracheophyta" = Vas)
     })
richnessInput <- reactive({
     switch(input$dataset,
           "Amphibia" = AmpR,
           "Aves" = BirR,
           "Papilionoidea" = PaeR,
           "Mammals" = MamR,
           "Odonata" = OdoR,
           "Opilions" = OpiR,
           "Tracheophyta" = VasR)
     })

ignorInput <- reactive({
     dataset <- datasetInput()
     rich <- richnessInput()
     if(input$index==TRUE){
                           o<-dataset
                           o<-dataset/rich
                           o[which(dataset[]==0)]<-0
                           dataset<-o
                           }
     if(input$trans==1){
                          dataset.norm<-calc(dataset, fun=function(x){return(x/dataset@data@max)})
                          CI<-1-dataset.norm
       }
     if(input$trans==2){
                          dataset.log<- calc(dataset, fun=function(x){return(log(x+1))})
                          dataset.norm<- dataset.log/dataset.log@data@max
                          CI<-1-dataset.norm
     }

     if(input$trans==3){
       obs50<-input$obs50
                          CI<-calc(dataset, fun=function(x){return(obs50/(x+obs50))})
      }
     return(CI)
  }) # end ignorInput

  spptargetInput<-reactive({
       #############################
       if(input$dataset=="Amphibia"){
         if(input$target=="Common"){
           sppname<-"Bufo bufo"
           spp<-Buf
         } #en Common

         if(input$target=="Rare"){
           sppname<-"Pelophylax lessonae"
           spp<-Pel
         }  #end Rare
       } #end Amphibians
       #############################
       if(input$dataset=="Aves"){
         if(input$target=="Common"){
           sppname<-"Parus major"
           spp<-Par
         } #end Common

         if(input$target=="Rare"){
           sppname<-"Poecile cinctus"
           spp<-Poe
         } # end Rare
       } #end Birds
       ##############################
       if(input$dataset=="Papilionoidea"){
         if(input$target=="Common"){
           sppname<-"Papilio machaon"
           spp<-Pap
         } #end Common
         if(input$target=="Rare"){
           sppname<-"Colias hecla"
           spp<-Col
         } #end rare
       } #end Mammals
       ##############################
       if(input$dataset=="Mammals"){
         if(input$target=="Common"){
           sppname<-"Alces alces"
           spp<-Alc
         } #end Common
         if(input$target=="Rare"){
           sppname<-"Erinaceus europaeus"
           spp<-Eri
         } #end rare
       } #end Mammals
       #############################
       if(input$dataset=="Opilions"){
         if(input$target=="Common"){
           sppname<-"Opilio canestrinii"
           spp<-Opca
         } #en Common

         if(input$target=="Rare"){
           sppname<-"Lacinius horridus"
           spp<-Lac
         } #end Rare
       } #end Opilions
       #############################
       if(input$dataset=="Odonata"){
         if(input$target=="Common"){
           sppname<-"Libellula quadrimaculata"
           spp<-Lib
         } #en Common

         if(input$target=="Rare"){
           sppname<-"Nehalennia speciosa"
           spp<-Neh
         } #end Rare
       } #end Opilions

       #############################
       if(input$dataset=="Tracheophyta"){
         if(input$target=="Common"){
           sppname<-"Parnassia palustris"
           spp<-Pan
         } #en Common

         if(input$target=="Rare"){
           sppname<-"Euphrasia officinalis officinalis"
           spp<-Eup
         }  #end Rare
       } #end Vascular Plants
       return(list(sppname,spp))
  }) # end sppTarget

  sppPAInput<-reactive({
      spp<-spptargetInput()[[2]]
      sppname<-spptargetInput()[[1]]
      obs50<-input$obs502

      if(input$trans2==1){
                          spp.norm<- calc(spp, fun=function(x){return(x/spp@data@max)})
                          spp.psabs<- 1- spp.norm
                          }
      if(input$trans2==2){
                          spp.log<- calc(spp, fun=function(x){return(log(x+1))})
                          spp.norm<- spp.log/spp.log@data@max
                          spp.psabs<- 1-spp.norm
                          }
      if(input$trans2==3){
                          spp.norm<- calc(spp, fun=function(x){return(x/spp@data@max)})
                          spp.psabs<- calc(spp, fun=function(x){return(obs50/(x+obs50))})
                          }
      if(input$trans2==4){
                          spp.norm<- calc(spp, fun=function(x){return(x/spp@data@max)})
                          spp.psabs<- calc(spp, fun=function(x){
                                                return(ifelse(x<obs50, 1, obs50/(x+obs50)))
                                                })
                          }

      return(list(spp.psabs,spp.norm))
  }) # end reactive sppPA

sppOddsInput<-reactive({
    spp<-spptargetInput()[[2]]
    obs <- datasetInput()
    rich <- richnessInput()
    spp.odd<- overlay(spp, obs, rich, fun=function(x,y,z){return(x/(y/z))})
    return(spp.odd)
}) # end reactive sppPA
  
  
  output$ObsPlot <- renderPlot(height = 800, expr = {
              par(mfrow=c(1,4), oma=c(0,0,1,1))
              dataset <- datasetInput()
              rich <- richnessInput()
               if(input$index==TRUE){
                           o<-dataset
                           o<-dataset/rich
                           o[which(dataset[]==0)]<-0
                           dataset<-o
                       }

              if(input$trans==2) {
                                 #dataset<- calc(datasetInput(), fun=function(x){return(log(x+1))})
                                 dataset<- calc(dataset, fun=function(x){return(log(x+1))})
                                 }
              CI<-ignorInput()
              ########
              par(mar=c(0,0,0,3),cex=1,las=0, tck=.5, bty="n")
              plot(dataset, zlim=c(0,dataset@data@max), bty="n", legend=FALSE, axes=FALSE, col=rev(Topo))
              r.range <- c(dataset@data@min, dataset@data@max)
              r.rangeseq<-seq(r.range[1], r.range[2],
                              by=round((r.range[2]-r.range[1])/10,ifelse(r.range[2]>1000,-2,ifelse(r.range[2]>100,-1,0))))
              # par(mar=c(0,0,8,3))
              plot(dataset, legend.only=TRUE, zlim=c(0,dataset@data@max), col=rev(Topo),
                   legend.width=3, legend.shrink=0.5,
                   axis.args=list(at=r.rangeseq,
                                  labels=r.rangeseq,
                                  cex.axis=1.5),
                   legend.args=list(text=ifelse(input$index==TRUE,paste(ifelse(input$trans!=2,"Obs Index","Log(Obs Index)")," for", as.character(input$dataset)),paste(ifelse(input$trans!=2,"No.","Log(No.)"),"of Obs for", as.character(input$dataset))),
                                   side=2, font=2, line=1.5, cex=1))
              # par(mar=c(0,0,0,3))
              plot(Swe, lwd=1.5, border="grey50", add=TRUE)
              scale.lng<-100000 #(m)
              segments(max(coordinates(dataset)[,1]),min(coordinates(dataset)[,2]),max(coordinates(dataset)[,1])-scale.lng,min(coordinates(dataset)[,2]),lwd=2)
              text(max(coordinates(dataset)[,1])-scale.lng/2,min(coordinates(dataset)[,2])+50000, labels=paste(scale.lng/1000, "km"),cex=1.5)

              #######
              par(mar=c(0,0,0,3),cex=1,las=0, tck=.05, bty="n")
              plot(CI, zlim=c(0,1), bty="n", legend=FALSE, axes=FALSE, col=RedBlue)
              plot(CI, legend.only=TRUE, zlim=c(0,1),col=RedBlue,
                   legend.width=3, legend.shrink=0.5,
                   axis.args=list(at=seq(0, 1, .2),
                                  labels=seq(0, 1, .2),
                                  cex.axis=1.5),
                   legend.args=list(text=paste("Ignorance for", as.character(input$dataset)),
                                   side=2, font=2, line=1.5, cex=1))
              plot(Swe, lwd=1.5, add=TRUE)
              scale.lng<-100000 #(m)
              segments(max(coordinates(dataset)[,1]),min(coordinates(dataset)[,2]),max(coordinates(dataset)[,1])-scale.lng,min(coordinates(dataset)[,2]),lwd=2)
              text(max(coordinates(dataset)[,1])-scale.lng/2,min(coordinates(dataset)[,2])+50000, labels=paste(scale.lng/1000, "km"),cex=1.5)

              ########
             spp.psabs<-sppPAInput()[[1]]
             spp.norm<-sppPAInput()[[2]]
              par(mar=c(0,0,0,3),cex=1,las=0, tck=.05, bty="n")
              plot(spp.psabs, zlim=c(0,1), bty="n", legend=FALSE, axes=FALSE,col=RedBlue)
              plot(spp.psabs, legend.only=TRUE, zlim=c(0,1),col=RedBlue,
                   legend.width=3, legend.shrink=0.5,
                   axis.args=list(at=seq(0, 1, .2),
                                  labels=seq(0, 1, .2),
                                  cex.axis=1.5),
                   legend.args=list(text=paste("Ps. absence of",spptargetInput()[[1]]),
                                   side=2, font=2, line=1.5, cex=1))
              plot(Swe, lwd=1.5, add=TRUE)
              scale.lng<-100000 #(m)
              segments(max(coordinates(dataset)[,1]),min(coordinates(dataset)[,2]),max(coordinates(dataset)[,1])-scale.lng,min(coordinates(dataset)[,2]),lwd=2)
              text(max(coordinates(dataset)[,1])-scale.lng/2,min(coordinates(dataset)[,2])+50000, labels=paste(scale.lng/1000, "km"),cex=1.5)

              #######
              fun="prod" #alt "geomean"
              sppOdds<-sppOddsInput()
              maxOdds<-ceiling(max(sppOdds[], na.rm = TRUE))
              oddstep<-ifelse(maxOdds/5 < 1, round(maxOdds/5, 1), round(maxOdds/5))
              par(mar=c(0,0,0,3),cex=1,las=0, tck=.05, bty="n")
              plot(sppOdds, zlim=c(0,maxOdds), bty="n", legend=FALSE, axes=FALSE,col=GreyColors)
              plot(sppOdds, legend.only=TRUE, zlim=c(0,maxOdds), col=GreyColors,
                   legend.width=3, legend.shrink=0.5,
                   axis.args=list(at=seq(0, maxOdds, oddstep),
                                  labels=seq(0, maxOdds, oddstep),
                                  cex.axis=1.5),
                   legend.args=list(text=paste("Population Size Index of",spptargetInput()[[1]]),
                                    side=2, font=2, line=1.5, cex=1))
              plot(overlay(spp.psabs,1-CI,fun=fun),
                          zlim=c(input$minAbs,1),col="#FF0000",alpha=input$alpha, legend=FALSE, add=T)
              plot(overlay(1-spp.psabs,1-CI,fun=fun), #1-spp.psabs,
                          zlim=c(input$minPres,1),col="#00FF00",alpha=input$alpha,legend=FALSE, add=T)
              plot(Swe, lwd=1.5, border="grey50", add=TRUE)
              scale.lng<-100000 #(m)
              segments(max(coordinates(dataset)[,1]),min(coordinates(dataset)[,2]),max(coordinates(dataset)[,1])-scale.lng,min(coordinates(dataset)[,2]),lwd=2)
              text(max(coordinates(dataset)[,1])-scale.lng/2,min(coordinates(dataset)[,2])+50000, labels=paste(scale.lng/1000, "km"),cex=1.5)
              legend("topleft", c(paste0("Certain ps.absence (", input$minAbs," - 1)"), paste0("Certain presence (", input$minPres," - 1)")),
                                 col=c(paste0(c("#FF0000","#00FF00"),input$alpha * 100)),
                                 bty="n", pch= 15, cex=1.5)
  }) #end outputPlot

output$TransPlot <- renderPlot({
              par(mfrow=c(1,3), oma=c(1,0,1,0))
              richV <- as.numeric(richnessInput()[cellwdata])
              datasetV<-as.numeric(datasetInput()[cellwdata])

              if(input$index==TRUE){datasetI<-ifelse(datasetV==0, 0, datasetV/richV) }
              if(input$index==FALSE){datasetI<-datasetV}

              if(input$trans!=2) {dataset.D<-datasetI}
              if(input$trans==2) {
                                 dataset.log<- log(datasetI+1)
                                 dataset.D<- dataset.log
              }
              ## Density plot
              par(mar=c(4,4,3,2),cex=1)
              #plot(density(dataset.D, from=0), #na.rm=T,
              hist(dataset.D, from=0, col="lightblue", #na.rm=T,
                                      xlab=ifelse(input$index==TRUE,paste(ifelse(input$trans!=2,"Obs Index","Log(Obs Index)")," for", as.character(input$dataset)),paste(ifelse(input$trans!=2,"No.","Log(No.)"),"of Obs for", as.character(input$dataset))),
                                      #paste(ifelse(input$trans!=2,"No.","Log(No.)"),"of Observations for", as.character(input$dataset)),
                                      ylab="No. cells",
                                      main=paste("No. records for", as.character(input$dataset)))


              ## Species Discovery plot
              plot(dataset.D, richV,
                                      pch=19,
                                      xlab=ifelse(input$index==TRUE,paste(ifelse(input$trans!=2,"Obs Index","Log(Obs Index)")," for", as.character(input$dataset)),paste(ifelse(input$trans!=2,"No.","Log(No.)"),"of Obs for", as.character(input$dataset))),
                                      #paste(ifelse(input$trans!=2,"No.","Log(No.)"),"of Observations for", as.character(input$dataset)),
                                      ylab="Richness",
                                      main=paste("Richnes vs. Observations for", as.character(input$dataset)))
              #abline(a=0,b=1)

              ## Algorithms plot
              maxX<-max(datasetI)
              transnorm<-function(x, maxX){
                                    norm<-x/maxX
                                    norm<- 1- norm
                                    return(norm)
              }
              par(mar=c(4,4,3,2),cex=1)
              curve(transnorm(x,maxX), from=0,to=maxX, n = 1001, ylim=c(0,1), lwd=2,
                                    xlab=ifelse(input$index==TRUE,paste("Obs Index for", as.character(input$dataset)),paste("No. of Obs for", as.character(input$dataset))),
                                    #paste("No. of Observations for", as.character(input$dataset)), #paste(ifelse(input$trans!=2,"No.","Log(No.)"),"of Observations for", as.character(input$dataset)),
                                    ylab="Ignorance score",
                                    main="Ignorance scores")

              translog<-function(x,dec){
                      logx<-log(x+dec)#+abs(min(log(x+dec))) ## second term not needed if dec = 1
                      logx.norm<-logx/max(logx)
                      logCI<-1 -(logx.norm)
                      return(logCI)
                      }
              curve(translog(x,1), col=4, lwd=2,add=T)

              obs50<-input$obs50
              par(mar=c(4,4,3,2),cex=1)
              curve(obs50/(x+obs50), lwd=2, add=T, col=2)
              abline(v=1, lty=3)
              abline(v=obs50, lty=3, col=2)
              abline(h=0.5, lty=3, col=2)
#              exp1<-expression(Normalized = 1 - x/ max(x),
#                               LogNormalized = 1 - log(x+1)/max( log(x+1) ),
#                               Inversed = O[0.5]/(x+O[0.5]))
              legend("topright", legend=c("Normalized","Log-Normalized","Half-ignorance"),
                                          lty=1, lwd=2, col=c("black","blue","red"),bty="n")
  }) #end outputPlot

}) #end server
