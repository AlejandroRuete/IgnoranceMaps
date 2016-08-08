palRWB <- colorNumeric(c("blue","white", "red"), c(0,1), na.color = "transparent")
palGWR <- colorNumeric(c("red","lightpink", "green4"), c(0,1), na.color = "transparent")
colCount<-c("black", "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666") # Accent

shinyServer(function(input, output, session) {
  # Return the requested dataset
  obsInput <- reactive({
    switch(input$res,
           "100" = AmpEur100,
           "50" = AmpEur50,
           "25" = AmpEur25)
  })
  richInput <- reactive({
    switch(input$res,
           "100" = AmpEurR100,
           "50" = AmpEurR50,
           "25" = AmpEurR25)
  })
  
  ranaInput <- reactive({
    switch(input$res,
           "100" = Rana100,
           "50" = Rana50,
           "25" = Rana25)
  })

  O05real <- reactive({
    obs50<-input$obs50
    res<-as.numeric(input$res)
    obs50 *(res/25)^2 # assuming 
  })

ignorInput <- reactive({
  withProgress(message = 'Calculating Ignorance scores',
                value = 0, {
    obs <- obsInput()
    rich <- richInput()
    if(input$index==TRUE){
      o<-obs
      o<-obs/rich
      o[which(obs[]==0)]<-0
      obs<-o
    }
    res<-as.numeric(input$res)
    obs50<-input$obs50 * (res/25)^2
    setProgress(0.1)
    ign<-calc(obs, fun=function(x){return(obs50/(x+obs50))})
    setProgress(0.25)
    return(ign)
  })
}) # end ignorInput

sppPAInput<-reactive({
  withProgress(message = 'Calculating Pseudo Absences', 
               value = 0.25, {
    spp<-ranaInput()
    res<-as.numeric(input$res)
    obs50<-input$obs50spp * (res/25)^2
    setProgress(0.26)
    spp.psabs<- calc(spp, fun=function(x){return(obs50/(x+obs50))})
    return(spp.psabs)
    setProgress(0.5)
  })
}) # end reactive sppPA

sppOddsInput<-reactive({ #Populaiton size index or Odds of sampling a species
  withProgress(message = 'Calculating PSI', 
               value = 0.5, {
                 spp <-ranaInput()
                 obs <- obsInput()
                 rich <- richInput()
                 setProgress(0.51)
                 spp.odd<- overlay(spp, obs, rich, fun=function(x,y,z){return(x/(y/z))})
                 return(spp.odd)
                 setProgress(0.75)
               })
}) # end spp odds

sppPAcertInput<-reactive({
  withProgress(message = 'Calculating Species Presence', 
               value = 0.75, {
    setProgress(0.76)
    spp.abs<-overlay(sppPAInput(), 1-ignorInput(), fun="prod")
    spp.abs<-calc(spp.abs, fun=function(x) ifelse(x<=0.5, -99999, 1-x)) ## How sure  that it is not there 
    spp.pres<-calc(1-sppPAInput(), fun=function(x) ifelse(x<=(1-input$prestol), -99999, 1)) ##Has it been observed more than O0.5? #
    
    zero<-calc(spp.abs, fun=function(x) x<- -99999)  
    
    setProgress(0.85)
    
    s<-stack(spp.pres, spp.abs, zero) #zero to avoid warnings()
    spp.pa<-calc(s, fun=max, na.rm=TRUE) 
    spp.pa<-calc(spp.pa, fun=function(x) ifelse(x== -99999, NA, x))
    return(spp.pa)
    setProgress(1)
  })
})

###For Temporal Tab
obsDInput <- AmpEur100
obsTempInput <- AmpEur100Stack
richTempInput <- AmpEurR100Stack

ignorATInput <- reactive({
  withProgress(message = 'Calculating Ignorance Scores', value = 0, {
    if(input$NAplot==FALSE) wY<-c(which(Years==input$time[1]):which(Years==input$time[2]))
    if(input$NAplot==TRUE) wY<-c(1, which(Years==input$time[1]):which(Years==input$time[2]))
    RasRef<-obsDInput #Raster Reference
    
    obs <- obsTempInput[,,wY]
    obs <- apply(obs,1:2,sum, na.rm=T)
    obs <- ifelse(is.na(RasRef[]),NA,obs)
    
    rich <- richTempInput[,,wY]
    rich <- apply(rich,1:2,sum, na.rm=T)
    rich <- ifelse(is.na(RasRef[]),NA,rich)
    setProgress(0.25)
    if(input$indexD==TRUE){
      o<-obs
      o<-obs/rich
      o[which(obs==0)]<-0
      obs<-o
    }
    res<-as.numeric(input$res)
    obs50<-input$obs50D * (res/25)^2
    CI<-obs50/(obs+obs50)
    return(CI)
    setProgress(0.5)
  })
}) # end ignorInput

ignorTempInput <- reactive({
  withProgress(message = 'Calculating Ignorance Scores', value = 0.5, {
    obs <- obsTempInput
    rich <- richTempInput
    
    if(input$indexD==TRUE){
      o<-obs
      o<-obs/rich
      o[which(obs[]==0)]<-0
      obs<-o
    }
    res<-as.numeric(input$res)
    obs50<-input$obs50D * (res/25)^2
    CItemp<-obs50/(obs+obs50)
    return(CItemp)
    setProgress(0.1)
  })
}) # end ignorInput


  ## Interactive Map ###########################################

  # Create the map
  # project=FALSE when adding the rasters becuase they are created as epsg:3857 as expected by leafleat
  output$map <- renderLeaflet({
    input$goButton 
    
    Ign<-isolate(ignorInput())
    spp.psabs<-isolate(sppPAInput()) #pseudo absences
    spp.odds<-isolate(sppOddsInput()) #odds
    spp.pa<-isolate(sppPAcertInput()) #certain PA absences
    popup <- "<strong><i>Rana temporaria</i> distribution</strong> (IUCN)"
    
    leaflet() %>%
      addTiles() %>% #options = tileOptions()
      setView(lng = 15, lat = 50, zoom = 3) %>% 
      addRasterImage(Ign, colors = palRWB, opacity = input$alpha, project=FALSE, layerId = "L") %>%
      addPolygons(data=RanaPoly, weight = 2, col = "black", fillOpacity = 0, popup = popup) %>%
      addLegend(position = "bottomright", colors = palRWB(c(0,0.2,0.4,0.6,0.8,1)), labels = c(0,0.2,0.4,0.6,0.8,1), title = "Ignorance", opacity = input$alpha)
}) ## end render map

#### Add layer
observe({
  layer <- input$layer
  proxy <- leafletProxy("map")
 
  Ign<-isolate(ignorInput())
  spp.psabs<-isolate(sppPAInput()) #pseudo absences
  spp.odds<-isolate(sppOddsInput()) #odds
  spp.pa<-isolate(sppPAcertInput()) #certain PA absences
  maxOdd<-ceiling(max(spp.odds[], na.rm=TRUE))
  palYOR <- colorNumeric(c("white","yellow","orange", "red"), c(0,maxOdd), na.color = "transparent")
  
  popup <- "<strong><i>Rana temporaria</i> distribution</strong> (IUCN)"
  
  # Remove any existing legend, and only if the legend is
  # enabled, create a new one.
  proxy %>% clearShapes()
  proxy %>% removeImage("L")
  if(layer == "RTGIgn") proxy %>% addRasterImage(Ign, colors = palRWB, opacity = input$alpha, project=FALSE, layerId = "L") %>%
    addPolygons(data=RanaPoly, weight = 2, col = "black", fillOpacity = 0, popup = popup)
  if(layer == "SppIgn") proxy %>% addRasterImage(spp.psabs, colors = palRWB, opacity = input$alpha, project=FALSE, layerId = "L") %>% 
    addPolygons(data=RanaPoly, weight = 2, col = "black", fillOpacity = 0, popup = popup)
  if(layer == "PSI" ) proxy %>% addRasterImage(spp.odds, colors = palYOR, opacity = input$alpha, project=FALSE, layerId = "L") %>% 
    addPolygons(data=RanaPoly, weight = 2, col = "black", fillOpacity = 0, popup = popup)
  if(layer == "SppPres") proxy %>% addRasterImage(spp.pa, colors = palGWR, opacity = input$alpha, project=FALSE, layerId = "L") %>%
    addPolygons(data=RanaPoly, weight = 2, col = "black", fillOpacity = 0, popup = popup)
    
})
#### Change legend
observe({
  proxy <- leafletProxy("map")
  spp.odds<-isolate(sppOddsInput()) #odds
  maxOdd<- ceiling(max(spp.odds[], na.rm=TRUE))
  palYOR <- colorNumeric(c("white","yellow","orange", "red"), c(0,maxOdd), na.color = "transparent")
  
  # Remove any existing legend, and only if the legend is
  # enabled, create a new one.
  proxy %>% clearControls()
  layer <- input$layer
  if(layer == "RTGIgn") proxy %>% addLegend(position = "bottomright", colors = palRWB(c(0,0.2,0.4,0.6,0.8,1)), labels = c(0,0.2,0.4,0.6,0.8,1), title = "Ignorance", opacity = input$alpha)
  if(layer == "SppIgn") proxy %>% addLegend(position = "bottomright", colors = palRWB(c(0,0.2,0.4,0.6,0.8,1)), labels = c(0,0.2,0.4,0.6,0.8,1), title = "Ignorance", opacity = input$alpha)
  if(layer == "PSI" ) proxy %>% addLegend(position = "bottomright", colors = palYOR(seq(0,maxOdd, by=1)), labels = seq(0,maxOdd,by=1), title = "PSI", opacity = input$alpha)
  if(layer == "SppPres") proxy %>% addLegend(position = "bottomright", colors = palGWR(c(0,0.1,0.2,0.3,0.4,0.5,1)), labels = c(0,0.1,0.2,0.3,0.4,0.5,1), title = "Presence", opacity = input$alpha)
})

  ## Temporal Bias ###########################################
output$TempIgn <- renderPlot({
  ignTmp<-ignorTempInput()
  ignTmpV<-apply(ignTmp, 3, mean, na.rm = TRUE)
  ign<-ignorATInput()
  ignM<-mean(ign, na.rm = TRUE)
  
  par(mar=c(4,4,1,1), las=1, cex=1.5)
  plot(Years[-1],ignTmpV[-1], type = "l", ylim=c(0,1), xlim=c(input$time[1],input$time[2]), lwd=3,
       xlab="Years", ylab="Mean Ignorance Score")
  if(input$NAplot==TRUE) points(input$time[2]-5, ignTmpV[1])
  points(input$time[2], ignM, pch=19)
  
  RasRef<-obsDInput #Raster Reference
  whichCellCount <- cellFromPolygon(RasRef, CountEurope)
  
  chosenCount<-input$countries
  if(length(chosenCount)>0){
    whichCount<-numeric()
    withProgress(message = 'Adding country', value = 0, {
      for(c in 1:length(chosenCount)){
        whichCount[c]<-which(Countries%in%chosenCount[c])
        wC<-whichCellCount[[whichCount[c]]]
        ignMc<-mean(ign[wC], na.rm = TRUE)
        
        igntmp<-numeric(length(Years))
        if(length(wC)<1)igntmp<-rep(0,length(Years))
        if(length(wC)==1)for(y in 1:length(Years)) igntmp[y]<-ignTmp[,,y][wC]
        if(length(wC)>1) for(y in 1:length(Years)) igntmp[y]<-mean(ignTmp[,,y][wC], na.rm = TRUE)
        lines(YearsPlot[-1],igntmp[-1], col=colCount[c+1], lwd=2)
        if(input$NAplot==TRUE) points(input$time[2]-5, igntmp[1], col=colCount[c+1])
        points(input$time[2], ignMc, pch=19, col=colCount[c+1])
        incProgress(amount=1/c)
      }
      legend("bottomleft", legend=c("Europe", CountriesAb[whichCount]), col=colCount[1:(length(chosenCount)+1)], 
             lwd=c(3,rep(2,length(chosenCount))),  bty = "n", ncol = 3, cex=0.8)
      legend(input$time[1], 0.5, legend=c("Obs w/o year", "Time range"), title="Mean of:", col=colCount[1], pch=c(1,19),  bty="n", yjust=1.2, cex=0.8)
    })
  }# end if
})

## Country Bias ###########################################
output$DensIgn <- renderPlot({
  RasRef<-obsDInput #Raster Reference
  ign<-ignorATInput()
  
  par(mar=c(4,4,1,1), las=1, cex=1.5)
  dens<-density(ign, from=0, to=1, na.rm=TRUE)
  plot(dens$x, dens$y/max(dens$y), xlim=c(0,1), ylim=c(0,1),lwd=3, type="l",
       xlab="Ignorance Score", ylab="Relative Density", main="")
  
  whichCellCount <- cellFromPolygon(RasRef, CountEurope)
  chosenCount<-input$countries
  if(length(chosenCount)>0){
    for(c in 1:length(chosenCount)){
      whichCount<-which(Countries%in%chosenCount[c])
      wC<-whichCellCount[[whichCount]]
      dens<-density(ign[wC], from=0, to=1, na.rm=TRUE)
      lines(dens$x, dens$y/max(dens$y), col=colCount[c+1], lwd=2)
    }
  
  }
})


}) # end server function
