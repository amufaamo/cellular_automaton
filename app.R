library(shiny)

# UI部分
ui <- fluidPage(
  titlePanel("Cellular Automaton"),
  sidebarLayout(
    sidebarPanel(
      actionButton("start", "Start"),
      actionButton("stop", "Stop"),
      sliderInput("speed", "Speed", min = 1, max = 1000, value = 500)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# サーバーロジック部分
server <- function(input, output, session) {
  grid_size <- 50
  grid <- matrix(sample(0:1, grid_size^2, replace = TRUE), grid_size, grid_size)
  
  evolve <- function(grid) {
    new_grid <- grid
    for (i in 1:grid_size) {
      for (j in 1:grid_size) {
        neighbors <- sum(grid[max(1, i-1):min(grid_size, i+1), max(1, j-1):min(grid_size, j+1)]) - grid[i, j]
        if (grid[i, j] == 1) {
          if (neighbors < 2 || neighbors > 3) {
            new_grid[i, j] <- 0
          }
        } else {
          if (neighbors == 3) {
            new_grid[i, j] <- 1
          }
        }
      }
    }
    new_grid
  }
  
  auto_run <- reactiveVal(FALSE)
  
  observeEvent(input$start, {
    auto_run(TRUE)
  })
  
  observeEvent(input$stop, {
    auto_run(FALSE)
  })
  
  output$plot <- renderPlot({
    invalidateLater(input$speed)
    if (auto_run()) {
      grid <<- evolve(grid)
    }
    image(t(grid)[, ncol(grid):1], col = c("white", "black"), axes = FALSE)
  })
}


# Shinyアプリの起動
shinyApp(ui = ui, server = server)
