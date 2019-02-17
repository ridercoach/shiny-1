
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
  df_pca <- data.frame("PC" = rownames(m), as.data.frame(m * m_wts * 2))
}

angle_between_vecs <- function(u, v) {
  mag_u <- sqrt(sum(u*u))
  mag_v <- sqrt(sum(v*v))
  dot_uv <- sum(u*v)
  cosine_uv <- dot_uv / (mag_u * mag_v)
  acos(cosine_uv) * (180/pi)
}


ui <- fluidPage(
  fluidRow(
    column(12,
      titlePanel("2-D PCA Playground"),
      fluidRow(
        column(6,

          fluidRow(column(11, offset = 1,
                          
              wellPanel(tabsetPanel( id = "x_tabset",
                tabPanel("Norm",
                  sliderInput("sdx", "SD:", min = 0.1, max = 5, value = 1)
                ),
                tabPanel("Unif",
                  sliderInput("sclx", "Scale:", min = 0.1, max = 10, value = 1)
                ),
                tabPanel("Exp",
                  sliderInput("ertx", "Rate:", min = 0.1, max = 5, value = 1)
                )
              ),
              plotOutput("x_dist_plot", height = "100px")
              ), # end wellPanel for X

              wellPanel(tabsetPanel( id = "y_tabset",
                tabPanel("Norm",
                  sliderInput("sdy", "SD:", min = 0.1, max = 5, value = 1)
                ),
                tabPanel("Unif",
                  sliderInput("scly", "Scale:", min = 0.1, max = 10, value = 1)
                ),
                tabPanel("Exp",
                  sliderInput("erty", "Rate:", min = 0.1, max = 5, value = 1)
                )
              ),
              plotOutput("y_dist_plot", height = "100px")
              ) # end wellPanel for Y
              
            ))
          
        ), # end left-hand column
        column(6,
               
          fluidRow(
            column(6,
              tableOutput("distTable")
            ),
            column(6,
              textOutput("distText")
            )
          ),
          fluidRow(
            plotOutput("distPlot")
          ), 
          fluidRow(
            textOutput("test_output")
          )
               
        ) # end right-hand column
      ) # end second-level fluidPage
    ) # end full-width column
  ) # end outermost fluidRow
) # end fluidPage


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # X and Y coordinates are used multiple places, so use reactive expressions
  x_coords <- reactive({
    #get_coords(input$sdx)
    if (input$x_tabset == "Norm") {
      rnorm(10000, mean = 0, sd = input$sdx)
    } else if (input$x_tabset == "Unif") {
      runif(10000) * input$sclx - input$sclx / 2
    } else if (input$x_tabset == "Exp") {
      scale(rexp(10000, rate = input$ertx), center = TRUE, scale = FALSE)
    }
  })
  y_coords <- reactive({
    #get_coords(input$sdy)
    if (input$y_tabset == "Norm") {
      rnorm(10000, mean = 0, sd = input$sdy)
    } else if (input$y_tabset == "Unif") {
      runif(10000) * input$scly - input$scly / 2
    } else if (input$y_tabset == "Exp") {
      scale(rexp(10000, rate = input$erty), center = TRUE, scale = FALSE)
    }
  })
  
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
        xlim(-lim, lim) + ylim(-lim, lim)
   })
  
   output$distTable <- renderTable({
     pca_info()
   })
   
   output$distText <- renderText({
     df <- pca_info()
     a <- angle_between_vecs(df[1, 2:3], df[2, 2:3])
     sprintf("Angle between PCs is %6.2f deg", a)
   })
   
   output$x_dist_plot <- renderPlot({
     ggplot(df_coords(), aes(x)) + geom_histogram(binwidth = 0.1) + 
       labs(x = NULL, y = NULL) + xlim(-10, 10)
   })
   
   output$y_dist_plot <- renderPlot({
     ggplot(df_coords(), aes(y)) + geom_histogram(binwidth = 0.1) + 
       labs(x = NULL, y = NULL) + xlim(-10, 10)
   })
   
   output$test_output <- renderText({
     sprintf("x tab (%s), y tab (%s)", input$x_tabset, input$y_tabset)
   })
}

# Run the application
shinyApp(ui = ui, server = server)

