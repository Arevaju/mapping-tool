library(shiny)
library(tmap)
  ui = pageWithSidebar(
    headerPanel("Mapping Tool"), 
    sidebarPanel(
      radioButtons("dat","Select Spatial Dimension", choices = list("Country"= "nuts0", "Regions" = "nuts2")),
       selectInput("fil", label="Select an Indicator", ''),
      #selectInput("pal", 'Select Palette', c('Blues', 'PuRd', 'Greens', 'YlOrRd', 'PuBu'),selected = "Blues"), 
      selectInput("pal", 'Select Color Palette', c('Blues', 'Purples', 'Greens', 'YlOrRd', 'PuBu', 'Oranges', 'YlGn'),selected="YlOrRd"),
      selectInput("style", 'Select Type of Interval', c( 'equal', 'quantile','pretty', 'kmeans')),
      numericInput("ncuts", 'Select Number of Classes', value = 5, min = 3, max = 9),
      radioButtons(inputId ="year4",
                   label = (HTML("<b>Select the year of interest:</b>")),
                   choices = list ("2010"= "2010",
                                   "2020" = "2020",
                                   "2040" = "2040")
      ),
	  
	  helpText(HTML("<b>Do you want to upload your own dataset?</b>")),
      helpText(HTML("<div style=\"\">1.- Download and modify the data</div>")), 
      downloadButton('downloadData2', label = "Download"),	
      fileInput(inputId = "file", label="2.- Upload your own data", accept=c("txt/csv", "text/comma-separated-values,text/plain", ".csv")), 
      submitButton("Generate Map")
    ),
mainPanel(
  plotOutput("map", height="800px"),
  downloadButton("savemap", "Download PDF map"),
  downloadButton("savemap2", "Download PNG map")
)
  )
