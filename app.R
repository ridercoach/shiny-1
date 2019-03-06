
library(shiny)
library(ggplot2)

#--------------- global vars and helper functions ---------------------

N_POINTS = 2000

# find one limit that encompasses the data +/- on both axes
get_axes_limit <- function(x_coords, y_coords) {
  xlim <- max(abs(floor(min(x_coords))), abs(ceiling(max(x_coords))))
  ylim <- max(abs(floor(min(y_coords))), abs(ceiling(max(y_coords))))
  lim <- max(xlim, ylim)
}

# return a df with three columns: PC, x, y;
get_pca_df <- function(df) {
  pca <- prcomp(df)
  m <- t(pca$rotation)
  m_wts <- matrix(rep(summary(pca)$importance[1,], 2), nrow=2)
  df_pca <- data.frame("PC" = rownames(m), as.data.frame(m * m_wts * 2))
  df_pca$len <- sqrt(df_pca$x ^ 2 + df_pca$y ^ 2)
  df_pca$SD <- summary(pca)$importance[1,]
  df_pca$PropVar <- summary(pca)$importance[2,]
  df_pca
}

angle_between_vecs <- function(u, v) {
  mag_u <- sqrt(sum(u*u))
  mag_v <- sqrt(sum(v*v))
  dot_uv <- sum(u*v)
  cosine_uv <- dot_uv / (mag_u * mag_v)
  acos(cosine_uv) * (180/pi)
}

dist_parms <- function(dist, sd, rate) {
  r <- list(n = N_POINTS)
  if (dist == "rnorm") {
    r$sd <- sd
  } else if (dist == "rexp") {
    r$rate <- rate
  }
  r
}

#----------------------- coords module --------------------------------

# TODO: I would like to get the UI and server sides of the coords distributions 
# somehow driven from the same data structure;

# TODO: it might be more intuitive and make more room for controls is
# instead of making the X and Y wellpanels vertically stacked, they were 
# a pair of higher level tabs;

coordsUI <- function(id) {
  ns <- NS(id)
  
  wellPanel(tabsetPanel( id = ns("tabset"),
    tabPanel("rnorm", sliderInput(ns("sd"), "SD:", min = 0.1, max = 5, value = 1)),
    tabPanel("runif", sliderInput(ns("scale"), "Scale:", min = 0.1, max = 10, value = 1)),
    tabPanel("rexp", sliderInput(ns("rate"), "Rate:", min = 0.1, max = 5, value = 1))
  ),
  plotOutput(ns("dist_plot"), height = "100px"))
}

coords <- function(input, output, session) {
  
  values <- reactive({
    scale(do.call(input$tabset, dist_parms(input$tabset, input$sd, input$rate)), 
          center = TRUE, 
          scale = ifelse(input$tabset == "runif", input$scale, 1))
  })

  output$dist_plot <- renderPlot({
    ggplot(data.frame(n = values()), aes(n)) + geom_histogram(binwidth = 0.1) + 
      labs(x = NULL, y = NULL) + xlim(-10, 10)
  })
  
  values 
}

#----------------------- main program ---------------------------------

# TODO: we might not need so many levels of column/row nesting here if 
# we use the idea of making the X/Y wellpanels tabs;

ui <- fluidPage(
  fluidRow(
    column(12,
      titlePanel(HTML('<span style="text-align: center; font-weight: bold">2-D PCA Playground</span>')),
      fluidRow(
        
        # left-hand column
        column(6,
          #fluidRow(column(11, offset = 1,
              tabsetPanel( id = "ABtabset",
                tabPanel("Property A", coordsUI("X")),
                tabPanel("Property B", coordsUI("Y"))
              )
          #))
        ),
        
        # right-hand column
        column(6,
          fluidRow(tableOutput("distTable")),
          fluidRow(textOutput("distAngle")),
          fluidRow(plotOutput("distPlot"))
        )
        
      ) # end second-level fluidPage
    ) # end full-width column
  ) # end outermost fluidRow
) # end fluidPage


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  x_coords <- callModule(coords, "X")
  y_coords <- callModule(coords, "Y")
  
  # same for dataframes holding coords and PCA info
  df_coords <- reactive({
    data.frame("x" = x_coords(), "y" = y_coords())
  })
  pca_info <- reactive({
    get_pca_df(df_coords())
  })
  
  output$distPlot <- renderPlot({
     
      # make a square plot with limits that match the data
      lim <- get_axes_limit(x_coords(), y_coords())

      ggplot(df_coords()) + 
        geom_point(aes(x, y), color = "blue", alpha = 0.3) + 
        geom_segment(data = pca_info(), 
                     aes(x = 0, y = 0, xend = x, yend = y, color = PC), 
                     size = 2) + 
        xlim(-lim, lim) + ylim(-lim, lim) + 
        labs(x = "Property A", y = "Property B")
   })
  
   output$distTable <- renderTable({
     pca_info()
   })
   
   output$distAngle <- renderText({
     df <- pca_info()
     a <- angle_between_vecs(df[1, 2:3], df[2, 2:3])
     sprintf("Angle between PCs is %6.2f deg", a)
   })

   #output$test_output <- renderText({
   #  sprintf("x tab (%s), y tab (%s)", input$x_tabset, input$y_tabset)
   #})
}

# Run the application
shinyApp(ui = ui, server = server)

