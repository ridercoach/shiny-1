
library(shiny)
library(ggplot2)

#--------------- global vars and helper functions ---------------------

N_POINTS = 2000

#tabPanel("rnorm", sliderInput(ns("sd"), "SD:", min = 0.1, max = 5, value = 1)),
DIST <- list(
  rnorm = list(param = "sd", lbl = "SD:", min = 0.1, max = 5, value = 1), 
  runif = list(param = "max", lbl = "Max:", min = 0.1, max = 10, value = 1), 
  rexp = list(param = "rate", lbl = "Rate:", min = 0.1, max = 5, value = 1), 
  rchisq = list(param = "df", lbl = "Deg of Freedom:", min = 0.1, max = 10, value = 1), 
  rlnorm = list(param = "sdlog", lbl = "SDLog:", min = 0.1, max = 5, value = 1)
)

s_expl_1 = "Specify distributions of properties A and B for the data, "
s_expl_2 = "using the tabs below. The resulting principal "
s_expl_3 = "components are shown in the table and plot."

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
  cosine_uv <- sum(u*v) / (sqrt(sum(u*u)) * sqrt(sum(v*v)))
  acos(cosine_uv) * (180/pi)
}

#----------------------- coords module --------------------------------

make_tabpanel <- function(dist_name, ns) {
  di <- DIST[[dist_name]]
  tabPanel(dist_name, 
    sliderInput(ns(di$param), di$lbl, min = di$min, max = di$max, value = di$value))
}

get_dist <- function(input) {
  param <- DIST[[input$tabset]][["param"]]
  p <- list(n = N_POINTS)
  p[[param]] <- input[[param]]
  dist <- do.call(input$tabset, p)
  if (input$flip == TRUE) dist = -dist
  scale(dist, center = TRUE, scale = FALSE)
}

coordsUI <- function(id) {
  ns <- NS(id)
  wellPanel(
    do.call(tabsetPanel, c(list(id = ns("tabset")), 
                           lapply(names(DIST), function(x) make_tabpanel(x, ns)))),
    plotOutput(ns("dist_plot"), height = "100px"),
    checkboxInput(ns("flip"), label = "Rotate distribution about 0", value = FALSE)
  )
}

coords <- function(input, output, session) {
  
  values <- reactive({get_dist(input)})

  output$dist_plot <- renderPlot({
    ggplot(data.frame(n = values()), aes(n)) + geom_histogram(binwidth = 0.1) + 
      labs(x = NULL, y = NULL) + xlim(-10, 10)
  })
  
  values 
}

#----------------------- main program ---------------------------------

ui <- fluidPage(
  
  # header row
  fluidRow(
    column(10, offset = 1, 
      HTML('<h3 style="text-align: center; font-weight: bold;">2-D PCA Playground</h3>'),
      HTML(sprintf('<p style="text-align: center;">%s%s%s</p>', s_expl_1, s_expl_2, s_expl_3)),
      hr()
    )
  ),

  # main row
  fluidRow(
        
    # left-hand column
    column(6,
      tabsetPanel( id = "ABtabset",
        tabPanel("Property A", coordsUI("X")),
        tabPanel("Property B", coordsUI("Y"))
      )
    ),
        
    # right-hand column
    column(6,
      fluidRow(tableOutput("distTable")),
      fluidRow(textOutput("distAngle")),
      fluidRow(plotOutput("distPlot"))
    )
        
  )
)

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
  
  # TODO: it would be nice if the plot was always visually square regardless of 
  # how the app is resized;
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
     sprintf("Computed angle between PCs: %6.2f deg", a)
   })
}

# Run the application
shinyApp(ui = ui, server = server)

