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
                           o <- terra::ifel(dataset == 0, 0, o)
                           dataset<-o
                           }
     if(input$trans==1){
                          dataset.norm<-normalizeRaster(dataset)
                          CI<-1-dataset.norm
       }
     if(input$trans==2){
                          dataset.log<- log(dataset + 1)
                          dataset.norm<- normalizeRaster(dataset.log)
                          CI<-1-dataset.norm
     }

     if(input$trans==3){
       obs50<-input$obs50
                          CI<-obs50 / (dataset + obs50)
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
                          spp.norm<- normalizeRaster(spp)
                          spp.psabs<- 1- spp.norm
                          }
      if(input$trans2==2){
                          spp.log<- log(spp + 1)
                          spp.norm<- normalizeRaster(spp.log)
                          spp.psabs<- 1-spp.norm
                          }
      if(input$trans2==3){
                          spp.norm<- normalizeRaster(spp)
                          spp.psabs<- obs50 / (spp + obs50)
                          }
      if(input$trans2==4){
                          spp.norm<- normalizeRaster(spp)
                          spp.psabs<- terra::ifel(spp < obs50, 1, obs50 / (spp + obs50))
                          }

      return(list(spp.psabs,spp.norm))
  }) # end reactive sppPA

sppOddsInput<-reactive({
    spp<-spptargetInput()[[2]]
    obs <- datasetInput()
    rich <- richnessInput()
    spp.odd<- spp / (obs / rich)
    return(spp.odd)
}) # end reactive sppPA
  
  
  output$ObsPlot <- renderPlot(height = 800, expr = {
              par(mfrow=c(1,4), oma=c(0,0,1,1))
              dataset <- datasetInput()
              rich <- richnessInput()
               if(input$index==TRUE){
                           o<-dataset
                           o<-dataset/rich
                           o <- terra::ifel(dataset == 0, 0, o)
                           dataset<-o
                       }

              if(input$trans==2) {
                                 dataset<- log(dataset + 1)
                                 }
              CI<-ignorInput()
              ########
              dataset.max <- rasterMax(dataset)
              terra::plot(dataset, range=c(0,dataset.max), axes=FALSE, box=FALSE,
                   mar=c(0,0,0,3), col=rev(Topo),
                   plg=list(size=c(0.5,1), cex=1,
                            title=ifelse(input$index==TRUE,
                              paste(ifelse(input$trans!=2,"Obs Index","Log(Obs Index)"),"for",input$dataset),
                              paste(ifelse(input$trans!=2,"No.","Log(No.)"),"of Obs for",input$dataset)),
                            title.srt=90, title.cex=1))
              terra::lines(Swe, lwd=1.5, col="grey50")
              drawScaleBar(dataset)

              #######
              terra::plot(CI, range=c(0,1), axes=FALSE, box=FALSE,
                   mar=c(0,0,0,3), col=RedBlue,
                   plg=list(size=c(0.5,1), at=seq(0,1,.2), cex=1,
                            title=paste("Ignorance for",input$dataset),
                            title.srt=90, title.cex=1))
              terra::lines(Swe, lwd=1.5)
              drawScaleBar(dataset)

              ########
              spp.psabs <- sppPAInput()[[1]]
              terra::plot(spp.psabs, range=c(0,1), axes=FALSE, box=FALSE,
                   mar=c(0,0,0,3), col=RedBlue,
                   plg=list(size=c(0.5,1), at=seq(0,1,.2), cex=1,
                            title=paste("Ps. absence of",spptargetInput()[[1]]),
                            title.srt=90, title.cex=1))
              terra::lines(Swe, lwd=1.5)
              drawScaleBar(dataset)

              #######
              sppOdds <- sppOddsInput()
              maxOdds <- ceiling(rasterMax(sppOdds))
              terra::plot(sppOdds, range=c(0,maxOdds), axes=FALSE, box=FALSE,
                   mar=c(0,0,0,3), col=GreyColors,
                   plg=list(size=c(0.5,1), cex=1,
                            title=paste("Population Size Index of",spptargetInput()[[1]]),
                            title.srt=90, title.cex=1))
              # Explicit masks keep cells below the certainty sliders transparent,
              # including the endpoint where the selected threshold equals one.
              absence <- spp.psabs * (1-CI)
              presence <- (1-spp.psabs) * (1-CI)
              absence <- terra::ifel(absence >= input$minAbs, 1, NA)
              presence <- terra::ifel(presence >= input$minPres, 1, NA)
              terra::plot(absence, range=c(0,1), col="#FF0000", alpha=input$alpha,
                   legend=FALSE, add=TRUE)
              terra::plot(presence, range=c(0,1), col="#00FF00", alpha=input$alpha,
                   legend=FALSE, add=TRUE)
              terra::lines(Swe, lwd=1.5, col="grey50")
              drawScaleBar(dataset)
              legend("topleft", c(paste0("Certain ps.absence (",input$minAbs," - 1)"),
                                  paste0("Certain presence (",input$minPres," - 1)")),
                     col=adjustcolor(c("#FF0000","#00FF00"), alpha.f=input$alpha),
                     bty="n", pch=15, cex=1.5)
  }) #end outputPlot

output$TransPlot <- renderPlot({
              par(mfrow=c(1,3), oma=c(1,0,1,0))
              richV <- terra::values(richnessInput(), mat=FALSE)[cellwdata]
              datasetV<-terra::values(datasetInput(), mat=FALSE)[cellwdata]

              if(input$index==TRUE){datasetI<-ifelse(datasetV==0, 0, datasetV/richV) }
              if(input$index==FALSE){datasetI<-datasetV}

              if(input$trans!=2) {dataset.D<-datasetI}
              if(input$trans==2) {
                                 dataset.log<- log(datasetI+1)
                                 dataset.D<- dataset.log
              }
              ## Density plot
              par(mar=c(4,4,3,2),cex=1)
              hist(dataset.D, col="lightblue", #na.rm=T, from=0, 
                              xlab=ifelse(input$index==TRUE,
                                          paste(ifelse(input$trans!=2,"Obs Index","Log(Obs Index)")," for", as.character(input$dataset)),
                                          paste(ifelse(input$trans!=2,"No.","Log(No.)"),"of Obs for", as.character(input$dataset))),
                              ylab="No. cells",
                              main=paste("No. records for", as.character(input$dataset)))


              ## Species Discovery plot
              plot(dataset.D, richV,
                            pch=19,
                            xlab=ifelse(input$index==TRUE,
                                        paste(ifelse(input$trans!=2,"Obs Index","Log(Obs Index)")," for", as.character(input$dataset)),
                                        paste(ifelse(input$trans!=2,"No.","Log(No.)"),"of Obs for", as.character(input$dataset))),
                            ylab="Richness",
                            main=paste("Richnes vs. Observations for", as.character(input$dataset)))

              ## Algorithms plot
              maxX<-max(datasetI)
              transnorm<-function(x, maxX){
                          norm<-x/maxX
                          norm<- 1- norm
                          return(norm)
              }
              par(mar=c(4,4,3,2),cex=1)
              curve(transnorm(x,maxX), from=0,to=maxX, n = 1001, ylim=c(0,1), lwd=2,
                            xlab=ifelse(input$index==TRUE,
                                        paste("Obs Index for", as.character(input$dataset)),
                                        paste("No. of Obs for", as.character(input$dataset))),
                            ylab="Ignorance score",
                            main="Ignorance scores")

              translog<-function(x,dec){
                        logx<-log(x+dec)
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
              legend("topright", legend=c("Normalized","Log-Normalized","Half-ignorance"),
                                          lty=1, lwd=2, col=c("black","blue","red"),bty="n")
  }) #end outputPlot

}) #end server
