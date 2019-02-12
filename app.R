
library(shiny)
library(ggplot2)

# generate a set of numbers according to the specified 
# distribution and related params
get_coords <- function(sd = 1) {
  rnorm(10000, mean = 0, sd = sd)
}

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
  df_pca <- data.frame("PC" = rownames(m), as.data.frame(m * m_wts))
}

ui <- fluidPage(
   
   titlePanel("2-D PCA Playground"),
   
   sidebarLayout(
     
      sidebarPanel(
         sliderInput("sdx", "SD along x-axis:", min = 0.1, max = 5, value = 1),
         sliderInput("sdy", "SD along y-axis:", min = 0.1, max = 5, value = 1)
      ),
      
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({

      x_coords <- get_coords(input$sdx)     
      y_coords <- get_coords(input$sdy)
      lim <- get_axes_limit(x_coords, y_coords)
      
      df <- data.frame("x" = x_coords, "y" = y_coords)
      df_pca <- get_pca_df(df)
    
      ggplot(df) + 
        geom_point(aes(x, y), color = "blue", alpha = 0.3) + 
        geom_segment(data = df_pca, 
                     aes(x = 0, y = 0, xend = x, yend = y, color = PC), 
                     size = 2) + 
        xlim(-lim, lim) + ylim(-lim, lim)
   }, height = 800, width = 800)
}

# Run the application
shinyApp(ui = ui, server = server)

