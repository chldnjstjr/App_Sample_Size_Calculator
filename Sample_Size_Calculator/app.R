# 필요한 라이브러리를 불러옵니다.
library(shiny)

# UI 부분을 정의합니다.
ui <- fluidPage(
  titlePanel("표본 크기 계산기"),
  
  # 사용자 입력값을 받을 수 있는 입력 요소들을 정의합니다.
  sidebarLayout(
    sidebarPanel(
      numericInput("population_size", "모집단 크기:", value = 100, min = 1, max = 10000000000),
      selectInput("confidence_level", "신뢰수준:", choices = c("90%", "95%", "99%")),
      textInput("margin_of_error", "오차한계(%):", value = "5")
    ),
    
    # 표본 크기를 출력하는 출력 요소를 정의합니다.
    mainPanel(
      verbatimTextOutput("sample_size_output")
    )
  )
)

# 유한 모집단 보정 함수를 정의합니다.
finite_correction <- function(sample_size, population_size) {
  correction_factor <- sqrt((population_size - sample_size) / (population_size - 1))
  return(sample_size * correction_factor)
}

server <- function(input, output) {
  calculate_sample_size <- function(population_size, confidence_level, margin_of_error) {
    z_scores <- c(1.645, 1.96, 2.576)  # Z-scores for 90%, 95%, 99% confidence levels
    z_score <- z_scores[confidence_level == c("90%", "95%", "99%")]
    margin_of_error <- as.numeric(margin_of_error) / 100  # Convert margin of error to a proportion
    
    # Calculate sample size
    n <- (z_score^2 * 0.5 * (1 - 0.5)) / (margin_of_error^2)
    
    # Correct for finite population
    n_corrected <- (n / (1 + ((n - 1) / population_size)))
    
    return(ceiling(n_corrected))  # Round up to nearest whole number
  }
  
  output$sample_size_output <- renderPrint({
    population_size <- input$population_size
    confidence_level <- input$confidence_level
    margin_of_error <- input$margin_of_error
    
    sample_size <- calculate_sample_size(population_size, confidence_level, margin_of_error)
    paste("The required sample size is", sample_size, "people.")
  })
}


# Shiny 앱을 실행합니다.
shinyApp(ui, server)