library(shiny)
library(tmap)
library(RColorBrewer)
library(sp)

shinyServer(function(input, output, session){
  passData4 <- reactive({        
    if (is.null(input$file))return( {
      data <- read.table("./www/data.csv", sep = ",", check.names=FALSE,dec = ".", header= TRUE)    
      data <- data[data$Year %in% input$year4,]
      data <- data[grep ("CNT", data$NUTS2_Name),]
      nuts0<- read_shape('./shapes2/nuts0.shp')
      colnames(data)[1] <- "ICC"
      oo <- merge(nuts0,data, by="ICC")
      nuts0 <- oo[,c(-1,-2,-3,-14)]
    })
    else{      
      df <- input$file
      path <- df$datapath  
      data <- read.table(path, sep = ",", check.names=FALSE,dec = ".", header= TRUE)  
      data <- data[data$Year %in% input$year4,]
      data <- data[grep ("CNT", data$NUTS2_Name),]
      nuts0<- read_shape('./shapes2/nuts0.shp')
      colnames(data)[1] <- "ICC"
      oo <- merge(nuts0,data, by="ICC")
      nuts0 <- oo[,c(-1,-2,-3,-14)]
    } 
  })
  
  passData5 <- reactive({   
    if (is.null(input$file))return( {
      data <- read.table("./www/data.csv", sep = ",", check.names=FALSE,dec = ".", header= TRUE)    
      data <- data[data$Year %in% input$year4,]
      data <- data[grep ("CNT", data$NUTS2_Name, invert = TRUE),]
      data <- data [grep("^[A-Z].{3}", data$NUTS2, invert = FALSE),]
      nuts2<- read_shape('./shapes2/nuts2.shp')
      colnames(data)[1] <- "NUTS_CODE"
      oo2 <- merge(nuts2, data,  by="NUTS_CODE")
     nuts2 <- oo2[,c(-1:-12,-23)]
    })
    else{      
      df <- input$file
      path <- df$datapath  
      data <- read.table(path, sep = ",", check.names=FALSE,dec = ".", header= TRUE) 
      data <- data[data$Year %in% input$year4,]
      data <- data[grep ("CNT", data$NUTS2_Name, invert = TRUE),]
      data <- data [grep("^[A-Z].{3}", data$NUTS2, invert = FALSE),]
      nuts2<- read_shape('./shapes2/nuts2.shp')
      colnames(data)[1] <- "NUTS_CODE"
      oo2 <- merge(nuts2, data,  by="NUTS_CODE")
      nuts2 <- oo2[,c(-1:-12,-23)]
    } 
  })
  
  observe({
    if (input$dat == 'nuts0') {
      updateSelectInput(session,"fil", choices= c(names(passData4())), selected="IND1")
    } else {
      updateSelectInput(session, "fil", choices= c(names(passData5())), selected="IND1")  
    } 
  })

  
  output$breakCont<-reactive({ 
    if(input$style == "fixed")   { 
      output$breakCont <-renderUI(({ textInput("nbreaks", label= 'Enter manual intervals separated by commas', '1,10,25,50')})) 
    } 
    outputOptions(output, 'breakCont', suspendWhenHidden=FALSE)
  })
  
  
  output$map <- renderPlot({  
    validate(
        need(input$fil != "", "Please select the list of parameters with the filter on the left to generate your map.")) 
    
    if ((input$dat== "nuts0") & (input$style== "fixed")){ 
      
      withProgress(message = 'Generating your map...',
                   detail = '', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                   })
      
    tm_shape(Europe) +tm_fill( style="kmeans", textNA="Non-European countries") +
    qtm(passData4(),  
        title= (text=paste0(input$fil, "-YEAR: ", input$year4)),
        fill.breaks=  scan(text = input$nbreaks, sep = ',', encoding = "UTF-8"),
        title.snap.to.legend = TRUE,
        legend.title.size = 0.00001, 
        fill.palette= input$pal,
        fill.style = input$style,
      # fill.n = input$ncuts,
        fill = input$fil,          
        scale = 1.2,
        draw.frame = TRUE,
        borders = "grey", 
        fill.textNA="Non-European countries",
        layout.bg.color = '#E6FFFF',
        legend.bg.color = "white", 
        legend.frame="gray50",
        #  asp = 0,
        legend.position = c("left","top"),
        legend.is.portrait = FALSE, 
        ## bottom left, bottom right, up, bottom
        inner.margins = c(0.01, -0.3, -0.01, -0.5),
        legend.width = 0.40,        
        )+ tm_scale_bar(size = 0.7, position = c(0.6, 0.001, 0.3)) + tm_credits("(c) Randbee Consultants", position=c("left", "bottom"))   
    }
    else if ( (input$dat== "nuts0") & (input$style != "fixed") ){ 
      
      withProgress(message = 'Generating your map...',
                   detail = '', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                   })
      
      tm_shape(Europe) +tm_fill( style="kmeans", textNA="Non-European countries") +
        qtm(passData4(),  
            title= (text=paste0(input$fil, "-YEAR: ", input$year4)),
           # fill.breaks=  scan(text = input$nbreaks, sep = ',', encoding = "UTF-8"),
            title.snap.to.legend = TRUE,
            legend.title.size = 0.00001, 
            fill.palette= input$pal,
            fill.style = input$style,
            fill.n = input$ncuts,
            fill = input$fil,          
            scale = 1.2,
            draw.frame = TRUE,
            borders = "grey", 
            fill.textNA="Non-European countries",
            layout.bg.color = '#E6FFFF',
            legend.bg.color = "white", 
            legend.frame="gray50",
            #  asp = 0,
            legend.position = c("left","top"),
            legend.is.portrait = FALSE, 
            ## bottom left, bottom right, up, bottom
            inner.margins = c(0.01, -0.3, -0.01, -0.5),
            legend.width = 0.40,        
        )+ tm_scale_bar(size = 0.7, position = c(0.6, 0.001, 0.3)) + tm_credits("(c) Randbee Consultants", position=c("left", "bottom"))   
    }
      
    else if ( (input$dat== "nuts2") & (input$style == "fixed") ) {
      withProgress(message = 'Generating your map...',
                   detail = '', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                   })
      tm_shape(Europe) +tm_fill( style="kmeans", textNA="Non-European countries") +
      qtm(passData5(), 
          title= (text=paste0(input$fil, "-YEAR: ", input$year4)),
          fill.breaks=  scan(text = input$nbreaks, sep = ','),
          title.snap.to.legend = TRUE, 
          legend.title.size = 0.00001,
          fill.palette= input$pal,
          fill.style = input$style,
       #  fill.n = input$ncuts,
          fill = input$fil,          
          scale = 1.2,
          draw.frame = TRUE,
          borders = "grey", 
       #   fill.textNA="Non-European countries",
          layout.bg.color = '#E6FFFF',
          legend.bg.color = "white", 
          legend.frame="gray50",
         # asp = 0,
          legend.position = c("left","top"),
          legend.is.portrait = TRUE, 
          ## bottom left, bottom right, up, bottom
          inner.margins = c(0.01, -0.3, -0.01, -0.5),
          legend.width = 0.25,      
      )  + tm_scale_bar(size = 0.7, position = c(0.6, 0.001, 0.3)) + tm_credits("(c) Randbee Consultants", position=c("left", "bottom")) + tm_shape(nuts0) + tm_borders("grey35")
    }
    else if ( (input$dat== "nuts2") & (input$style != "fixed") ) {
      withProgress(message = 'Generating your map...',
                   detail = '', value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                   })
      tm_shape(Europe) +tm_fill( style="kmeans", textNA="Non-European countries") +
        qtm(passData5(), 
            title= (text=paste0(input$fil, "-YEAR: ", input$year4)),
           # fill.breaks=  scan(text = input$nbreaks, sep = ','),
            title.snap.to.legend = TRUE, 
            legend.title.size = 0.00001,
            fill.palette= input$pal,
            fill.style = input$style,
            fill.n = input$ncuts,
            fill = input$fil,          
            scale = 1.2,
            draw.frame = TRUE,
            borders = "grey", 
            #   fill.textNA="Non-European countries",
            layout.bg.color = '#E6FFFF',
            legend.bg.color = "white", 
            legend.frame="gray50",
            # asp = 0,
            legend.position = c("left","top"),
            legend.is.portrait = TRUE, 
            ## bottom left, bottom right, up, bottom
            inner.margins = c(0.01, -0.3, -0.01, -0.5),
            legend.width = 0.25,      
        )  + tm_scale_bar(size = 0.7, position = c(0.6, 0.001, 0.3)) + tm_credits("(c) Randbee Consultants", position=c("left", "bottom")) + tm_shape(nuts0) + tm_borders("grey35")
    }
  })    
  ######### This function is used for exporting them as PDF and png formats
  plotOutput = reactive({
    if (input$dat== "nuts0"){
      tm_shape(Europe) +tm_fill( style="kmeans", textNA="Non-European countries") +
        qtm(passData4(),  
            title= (text=paste0(input$fil, "-YEAR: ", input$year4)),
            title.snap.to.legend = TRUE,
            legend.title.size = 0.00001, 
            fill.palette= input$pal,
            fill.style = input$style,
            fill.n = input$ncuts,
            fill = input$fil,          
            scale = 1.2,
            draw.frame = TRUE,
            borders = "grey", 
            fill.textNA="Non-European countries",
            layout.bg.color = '#E6FFFF',
            legend.bg.color = "white", 
            legend.frame="gray50",
            #  asp = 0,
            legend.position = c("left","top"),
            legend.is.portrait = FALSE, 
            ## bottom left, bottom right, up, bottom
            inner.margins = c(0.01, -0.3, -0.01, -0.5),
            legend.width = 0.40,        
        )+ tm_scale_bar(size = 0.7, position = c(0.6, 0.001, 0.3)) + tm_credits("(c) Randbee Consultants", position=c("left", "bottom"))   
  }
    else {
      tm_shape(Europe) +tm_fill( style="kmeans", textNA="Non-European countries") +
        qtm(passData5(), 
            title= (text=paste0(input$fil, "-YEAR: ", input$year4)),
            title.snap.to.legend = TRUE, 
            legend.title.size = 0.00001,
            fill.palette= input$pal,
            fill.style = input$style,
            fill.n = input$ncuts,
            fill = input$fil,          
            scale = 1.2,
            draw.frame = TRUE,
            borders = "grey", 
            #   fill.textNA="Non-European countries",
            layout.bg.color = '#E6FFFF',
            legend.bg.color = "white", 
            legend.frame="gray50",
            # asp = 0,
            legend.position = c("left","top"),
            legend.is.portrait = TRUE, 
            ## bottom left, bottom right, up, bottom
            inner.margins = c(0.01, -0.3, -0.01, -0.5),
            legend.width = 0.25,      
        )  + tm_scale_bar(size = 0.7, position = c(0.6, 0.001, 0.3)) + tm_credits("(c) Randbee Consultants", position=c("left", "bottom")) + tm_shape(nuts0) + tm_borders("grey35")
       }
  })
  output$savemap <- downloadHandler(
    filename = "map.pdf", content = function(file) {
      pdf(file, height=8, width=12)
      print(plotOutput())
      dev.off()
    })  
  output$savemap2 <- downloadHandler(
    filename = "map.png",
    content = function(file) {
      png(file)
      print(plotOutput())
      dev.off()
    })
  output$downloadData2 <- downloadHandler(
    filename = 'data.csv',
    content = function(file) {
      write.csv(data, file, row.names = FALSE, sep = "," , dec = ".")
    }
  )
 
  }
)