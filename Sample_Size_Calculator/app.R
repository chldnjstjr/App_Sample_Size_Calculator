# 필요한 라이브러리를 불러옵니다.
library(shiny)
library(shinythemes)

# UI 부분을 정의합니다.
ui <- navbarPage("표본 크기 및 오차 한계 계산기",
                 theme = shinytheme("journal"),  # 초기 테마 설정
                 tabPanel("① 표본 크기 계산",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("population_size_sample", "모집단 크기:", value = 100, min = 1, max = 10000000000),
                              selectInput("confidence_level_sample", "신뢰수준:", choices = c("90%", "95%", "99%")),
                              numericInput("margin_of_error_sample", "오차한계(%):", value = 5, min = 1, max = 100)
                            ),
                            mainPanel(
                              uiOutput("sample_size_output")
                            )
                          )
                 ),
                 tabPanel("② 오차 한계 계산",
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("population_size_margin", "모집단 크기:", value = 10000, min = 1, max = 10000000000),
                              selectInput("confidence_level_margin", "신뢰수준:", choices = c("90%", "95%", "99%")),
                              numericInput("sample_size_margin", "표본 크기:", value = 200, min = 1, max = 10000000000)
                            ),
                            mainPanel(
                              uiOutput("margin_of_error_output")
                            )
                          )
                 ),
                 tabPanel("③ 표본 크기 계산 방법",
                          mainPanel(
                            h3("표본 크기 계산 방법"),
                            p("표본 크기는 다음 공식을 사용하여 계산됩니다:"),
                            p("n = (Z^2 * p * (1 - p)) / E^2"),
                            p("여기서,"),
                            p("n은 필요한 표본 크기,"),
                            p("Z는 신뢰수준에 해당하는 Z 점수 (예: 90% 신뢰수준의 Z 점수는 1.645, 95%는 1.96, 99%는 2.576),"),
                            p("p는 모집단 비율 (일반적으로 0.5를 사용),"),
                            p("E는 허용 오차 (오차 한계) 입니다."),
                            p("예: 모집단 크기가 10,000이고, 신뢰 수준이 99%, 오차 한계가 5%일 때 필요한 표본 크기는 다음과 같이 계산됩니다:"),
                            p("n = (2.576^2 * 0.5 * 0.5) / 0.05^2 = 666.82"),
                            p("따라서 필요한 표본 크기는 667명입니다."),
                            h3("유한 모집단 보정"),
                            p("표본 크기를 계산할 때 모집단이 유한하다는 것을 고려해 보정을 할 필요가 있습니다. 모집단이 작을 경우, 계산된 표본 크기가 모집단 크기를 초과할 수 있기 때문입니다."),
                            p("보정 공식은 다음과 같습니다:"),
                            p("n_corrected = n / (1 + ((n - 1) / N))"),
                            p("여기서,"),
                            p("n_corrected는 보정된 표본 크기,"),
                            p("n은 초기 계산된 표본 크기,"),
                            p("N은 모집단 크기 입니다."),
                            p("이 공식을 사용하면, 모집단 크기 내에서 적절한 표본 크기를 계산할 수 있습니다.")
                          )
                 ), 
                 tabPanel("④ 오차 한계 계산 방법",
                          mainPanel(
                            h3("오차 한계 계산 방법"),
                            p("오차 한계는 다음 공식을 사용하여 계산됩니다:"),
                            p("E = Z * sqrt((p * (1 - p)) / n)"),
                            p("여기서,"),
                            p("E는 계산된 오차 한계,"),
                            p("Z는 신뢰수준에 해당하는 Z 점수 (예: 90% 신뢰수준의 Z 점수는 1.645, 95%는 1.96, 99%는 2.576),"),
                            p("p는 모집단 비율 (일반적으로 0.5를 사용),"),
                            p("n은 표본 크기입니다."),
                            p("예: 모집단 크기가 10,000이고, 표본 크기가 1000명, 신뢰 수준이 99%일 때 오차 한계는 다음과 같이 계산됩니다:"),
                            p("E = 2.576 * sqrt((0.5 * 0.5) / 1000) = 0.0816"),
                            p("따라서 계산된 오차 한계는 ±8.16%입니다.")
                          )
                 ), 
                 tabPanel("테마 설정", 
                          mainPanel(
                            themeSelector()
                          ))
)

# 유한 모집단 보정 함수를 정의합니다.
finite_correction <- function(sample_size, population_size) {
  correction_factor <- sqrt((population_size - sample_size) / (population_size - 1))
  return(sample_size * correction_factor)
}

server <- function(input, output) {
  calculate_sample_size <- function(population_size, confidence_level, margin_of_error) {
    z_scores <- c(1.65, 1.96, 2.58)  # Z-scores for 90%, 95%, 99% confidence levels
    z_score <- z_scores[confidence_level == c("90%", "95%", "99%")]
    margin_of_error <- margin_of_error / 100  # Convert margin of error to a proportion
    
    # Calculate sample size
    n <- (z_score^2 * 0.5 * (1 - 0.5)) / (margin_of_error^2)
    
    # Correct for finite population
    n_corrected <- (n / (1 + ((n - 1) / population_size)))
    
    return(round(n_corrected))  # Round up to nearest whole number
  }
  
  calculate_margin_of_error <- function(population_size, confidence_level, sample_size) {
    z_scores <- c(1.65, 1.96, 2.58)  # Z-scores for 90%, 95%, 99% confidence levels
    z_score <- z_scores[confidence_level == c("90%", "95%", "99%")]
    
    # Calculate margin of error
    E <- z_score * sqrt((0.5 * (1 - 0.5)) / sample_size)
    
    return(round(E * 100, 2))  # Return as a percentage, rounded to 2 decimal places
  }
  
  output$sample_size_output <- renderUI({
    population_size <- input$population_size_sample
    confidence_level <- input$confidence_level_sample
    margin_of_error <- input$margin_of_error_sample
    
    sample_size <- calculate_sample_size(population_size, confidence_level, margin_of_error)
    HTML(paste("<div style='text-align: center;'>필요한 최소 표본 크기는 <span style='font-size:150%; color:darkblue; font-weight:bold;'>", sample_size, "</span>명입니다.</div>"))
  })
  
  output$margin_of_error_output <- renderUI({
    population_size <- input$population_size_margin
    confidence_level <- input$confidence_level_margin
    sample_size <- input$sample_size_margin
    
    margin_of_error <- calculate_margin_of_error(population_size, confidence_level, sample_size)
    HTML(paste("<div style='text-align: center;'>계산된 오차 한계는 <span style='font-size:150%; color:darkblue; font-weight:bold;'>±", margin_of_error, "%</span>입니다.</div>"))
  })
}

# Shiny 앱을 실행합니다.
shinyApp(ui, server)
