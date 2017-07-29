#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Visualisering av bibliografi"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        p("CSV-fil MED FORMAT X;Y;KLASSE;YEAR;AUTHOR;JOURNAL;TITLE;etc. Ingen tegn i header"),
        tableOutput("example"),
        fileInput("file1", "CSV FIL",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput("header", "Header", TRUE),
        textInput("cluster1", "cluster1", "metafysikk"),
        textInput("cluster2", "cluster2", "informasjonsteori"),
        textInput("cluster3", "cluster3", "vitenskapsteori")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("plot"),
        tableOutput("contents")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  example.df <- structure(list(AUTHOR = structure(c(3L, 4L, 31L, 30L, 28L), .Label = c("A.F.CHALMERS", 
                                                                                       "André De Tienne", "Baker, L. R.", "BORGHINI, ANDREA", "CASSIRER, E", 
                                                                                       "Chen, M. & Floridi, L. ", "CLAUS EMMECHE", "Claus Emmeche*, Simo Køppe** and Frederik Stjernfelt***", 
                                                                                       "Franseen, Maarten; Kroes, Peter; Reydon, Thomas A.C.; Vermaas, Pieter E. (editors)", 
                                                                                       "Gary Fuhrman", "João Queiroza,b, Claus Emmechec and Charbel Niño El-Hania", 
                                                                                       "Jørgen", "Koslicki, Kathrin", "LAKATOS, I.", "Martin J Eppler", 
                                                                                       "Pattee, H.H.", "Peirce, C.S.", "POPPER, K.", "QUEIROZ, J., EMMECHE, C.", 
                                                                                       "QUINE, W.V.", "QUINE, W.V., ULLIAN, J.S.", "Sami Pihlstrom", 
                                                                                       "SHANNON, C. E., WEAVER, W", "Simons, Peter", "Søren Brier ", 
                                                                                       "Stanley N. Salthe", "Thagard, Paul,", "Thomasson, Annie L", 
                                                                                       "TUFTE, E. R.", "Vetter, Barbara", "Whitehead, Alfred North ", 
                                                                                       "William H. B. Mcauliffe", "Winfried Nöth"), class = "factor"), 
                               YEAR = c(2004L, 2016L, 1979L, 2015L, 2007L), X = c(0.7, 0.8, 
                                                                                  0.95, 0.9, 0.7), Y = c(0.8, 0.8, 0.95, 0.85, 0.8), klasse = structure(c(2L, 
                                                                                                                                                          2L, 2L, 2L, 2L), .Label = c("Egen", "Referanse"), class = "factor")), .Names = c("AUTHOR", 
                                                                                                                                                                                                                                           "YEAR", "X", "Y", "klasse"), row.names = c(NA, 5L), class = "data.frame")
  
  output$example <- renderTable(example.df)
  
  reactive_clusterNavn <- reactive({
    clusterNavn <- c(input$cluster1,input$cluster2, input$cluster3)
  })
  
  output$plot <- renderPlotly({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    ref.comp <- read.csv(inFile$datapath, header = input$header, sep = ";")
    
    ref.comp$X <- as.numeric(as.character(ref.comp$X))
    ref.comp$Y <- as.numeric(as.character(ref.comp$Y))
    
    clusters <- kmeans(ref.comp[,c("X","Y")], centers = 3)
    ref.comp$rank.x <- match(sort(ref.comp$X), ref.comp$X)
    ref.comp$rank.y <- match(sort(ref.comp$Y), ref.comp$Y)
    
    clusterNavn <- reactive_clusterNavn()
    
    set.seed(1)
    clusters$clusterNavn <- NA
    clusters$clusterNavn[clusters$cluster == 1] <- clusterNavn[1]
    clusters$clusterNavn[clusters$cluster == 2] <- clusterNavn[2]
    clusters$clusterNavn[clusters$cluster == 3] <- clusterNavn[3]
    
    centers <- data.frame(X = clusters$centers[,"X"], 
                          Y = clusters$centers[,"Y"], 
                          cluster = c(clusterNavn[1],clusterNavn[2],clusterNavn[3]),
                          clusterNavnMeta = c(clusterNavn[1],clusterNavn[2],clusterNavn[3]))
    
    ref.comp$cluster <- clusters$clusterNavn
    
    ref.comp <- rbind.fill(ref.comp, centers)
    
    ref.comp$klasse[is.na(ref.comp$klasse)] <- "Referanse"
    
    t <- list(
      family = "sans serif",
      size = 14,
      color = toRGB("grey50"))
    
    plot_ly(data = ref.comp, x = ~X, y = ~Y, 
            color = ~cluster,
            symbol = ~klasse, symbols = c('x','circle'),
            text = ~clusterNavnMeta) %>%
      add_markers() %>%
      add_text(textfont = t, textposition = "top right") %>%
      layout(title = "Relativ distanse til referanser")
    
  })
   
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header, sep = ";")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

