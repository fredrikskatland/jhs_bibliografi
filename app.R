library(shiny)
library(shinydashboard)
#library(highcharter)
require(ggplot2)
require(Cairo)
require(rgl)
require(sphereplot)
#require(shinyRGL)
require(rglwidget)

info <- read.csv("/home/fredrik/Informasjons modell/10_03_2017_database_sirkus.csv", sep = ";", dec = ",", stringsAsFactors = F)
colnames(info) <- c("k.datapunkt","s.topic.sektor", "r.modality", "theta.specificity", "theta.polar", "success")
info$success[is.na(info$success)] <- 0

info$sektor <-info$s.topic.sektor * 4*pi

ownerDummy <- sample(c("Arkitekt","Prosjektleder"), nrow(info), replace = T)
info$tekst <- "Lorem ipsum dolor sit amet"
info$owner <- ownerDummy

radius <- function(x,y,z){
  sqrt(x^2+y^2+z^2)
}

theta <- function(x,y,z) {
  acos(z/radius(x,y,z))
}

azimuth <- function(x,y,z){
  atan(y/x)
}

info$k.datapunkt <- as.numeric(info$k.datapunkt)
info$s.topic.sektor <- as.numeric(info$s.topic.sektor)
info$r.modality <- as.numeric(info$r.modality)
info$theta.specificity <- as.numeric(info$theta.specificity)
info$theta.polar <- as.numeric(info$theta.polar)

info$sektor.longitude <- 360/jitter(info$s.topic.sektor,5)
info$specificity.latitude <- info$theta.specificity *  100


# RADIUS = MODALITY
# AZIMUTHAL ANGLE  = SEKTOR
# PHI = specificity

info$radius <- radius(x = info$s.topic.sektor, y = info$r.modality, z = info$theta.specificity)



ui <- shinyUI(fluidPage(
  sidebarPanel(
    sliderInput("modality", "Modality:", 0.5, min = 0, max = 1),
    sliderInput("spesificity", "Spesificity:", 0, min = -1, max = 1, step = 0.1),
    selectInput("emne", "Emne",  paste("emne",seq(0,28))),
    selectInput("owner", "Sagt av", c("Jørgen","Fredrik","Mordi","Fardin")),
    textAreaInput("tekst", "Tekst", NULL),
    submitButton(text = "Legg til", icon = icon("check-square"))
  ),
  mainPanel(
        rglwidgetOutput('thewidget1'),
      
      fluidRow(
        column(width = 3,
               verbatimTextOutput("click_info")
        ),
        column(width = 3,
               verbatimTextOutput("dblclick_info")
        )
      ),
      fluidRow(
        column(width = 3,
               verbatimTextOutput("hover_info")
        ),
        column(width = 3,
               verbatimTextOutput("brush_info")
        )
      )
    )
))


                   
                   
                   
    #plotlyOutput("plot1"),
    # plotOutput("plot2", width=600, 
    #            click = "plot_click",
    #            dblclick = dblclickOpts(
    #              id = "plot_dblclick"
    #            ),
    #            hover = hoverOpts(
    #              id = "plot_hover"
    #            ),
    #            brush = brushOpts(
    #              id = "plot_brush")
    #            ),

options(rgl.useNULL = TRUE)

server <- function(input, output) {

  data.reactive <- reactive({
    
    info <- read.csv("/home/fredrik/Informasjons modell/10_03_2017_database_sirkus.csv", sep = ";", dec = ",", stringsAsFactors = F)
    colnames(info) <- c("k.datapunkt","s.topic.sektor", "r.modality", "theta.specificity", "theta.polar", "success")
    info$success[is.na(info$success)] <- 0
    
    info$sektor <-info$s.topic.sektor * 4*pi
    
    ownerDummy <- sample(c("Arkitekt","Prosjektleder"), nrow(info), replace = T)
    info$tekst <- "Lorem ipsum dolor sit amet"
    info$owner <- ownerDummy
    
    info$k.datapunkt <- as.numeric(info$k.datapunkt)
    info$s.topic.sektor <- as.numeric(info$s.topic.sektor)
    info$r.modality <- as.numeric(info$r.modality)
    info$theta.specificity <- as.numeric(info$theta.specificity)
    info$theta.polar <- as.numeric(info$theta.polar)
    
    info$sektor.longitude <- 360/jitter(info$s.topic.sektor,5)
    info$specificity.latitude <- info$theta.specificity *  100
    
  })
  
  x <- rnorm(100)
  y <- 2*rnorm(100)
  z <- 10*rnorm(100)
  open3d()
  plot3d(x, y, z, col = "red")
  scene1 <- scene3d()
  plot3d(z, y, x, col = "green")
  scene2 <- scene3d()
  rgl.close()
  
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  
  output$thewidget1 <- renderRglwidget(
    rglwidget(scene1)
  )
  
  output$thewidget2 <- renderRglwidget(
    rglwidget(scene2)
  )
  
  # output$plot2 <- renderPlot({
  #   p <- ggplot(info, aes(x = jitter(sektor,5), y = r.modality, fill = owner))+
  #     coord_polar() + geom_point(shape = 21) +scale_size_area(max_size = 1)+
  #     scale_x_continuous(limits = c(0,360), breaks = seq(0,360, by = 360/28), labels = paste("emne",seq(0,28)))+
  #     scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by = 0.05))+
  #     theme_bw()
  #   return(p)
  # })
  # 
  # output$testing <- renderRglwidget({
  #   data <- data.reactive()
  #   rgl.sphgrid()
  #   rgl.sphpoints( long = data$sektor.longitude, 
  #                  lat = data$specificity.latitude, 
  #                  radius = data$r.modality, deg = F )
  #   x <- 45
  #   rgl.sphpoints(long = x, lat = 45, radius = 0.5)
  # })
  
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  output$dblclick_info <- renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })
  output$brush_info <- renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })
  
}

shinyApp(ui, server)