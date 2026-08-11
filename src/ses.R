library(shiny)
library(forecast)
library(ggplot2)

# Interfaz de Usuario (UI)
ui <- fluidPage(
  titlePanel("Pronóstico: Suavización Exponencial Simple (SES)"),
  
  sidebarLayout(
    sidebarPanel(
      # Carga del archivo CSV
      fileInput("file", "Cargar archivo CSV desde 'data/'",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      helpText("El CSV debe tener 2 columnas: 1ª Fecha/Tiempo y 2ª Observaciones."),
      
      hr(),
      
      # Parámetro Alpha
      sliderInput("alpha", "Parámetro de Suavizado (α):",
                  min = 0.01, max = 0.99, value = 0.2, step = 0.01),
      
      checkboxInput("opt_alpha", "Optimizar α automáticamente", value = FALSE)
    ),
    
    mainPanel(
      # Banner con el Pronóstico Destacado
      uiOutput("forecastBanner"),
      
      # Gráfica
      plotOutput("tsPlot"),
      
      hr(),
      h4("Resumen detallado del modelo:"),
      verbatimTextOutput("forecastOutput")
    )
  )
)

# Lógica del Servidor (Server)
server <- function(input, output, session) {
  
  # Procesamiento reactivo del CSV
  data_ts <- reactive({
    req(input$file)
    
    df <- read.csv(input$file$datapath)
    
    if (ncol(df) < 2) {
      stop("El archivo debe contener al menos dos columnas (Fecha y Observaciones).")
    }
    
    time_col <- df[[1]]   # Column 1: Tiempo
    values_col <- df[[2]] # Column 2: Valores
    
    start_year <- floor(min(time_col, na.rm = TRUE))
    
    # Calcular frecuencia
    diffs <- round(diff(sort(unique(time_col))), 4)
    min_diff <- suppressWarnings(min(diffs[diffs > 0]))
    
    freq <- if (!is.na(min_diff) && min_diff > 0) {
      round(1 / min_diff)
    } else {
      1
    }
    
    ts(values_col, start = start_year, frequency = freq)
  })
  
  # Ajuste del modelo SES
  fit_ses <- reactive({
    y <- data_ts()
    if (input$opt_alpha) {
      ses(y, h = 1)
    } else {
      ses(y, h = 1, alpha = input$alpha)
    }
  })
  
  # Banner interactivo para destacar el pronóstico
  output$forecastBanner <- renderUI({
    fc <- fit_ses()
    
    # Extraer el momento temporal, valor puntual y rango de confianza
    next_time <- time(fc$mean)[1]
    val_forecast <- round(as.numeric(fc$mean[1]), 4)
    low95 <- round(as.numeric(fc$lower[1, "95%"]), 4)
    high95 <- round(as.numeric(fc$upper[1, "95%"]), 4)
    
    # Renderizado en HTML/CSS
    div(
      style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
               border-left: 6px solid #1e88e5; 
               padding: 20px; 
               border-radius: 8px; 
               margin-bottom: 20px; 
               box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
      
      div(style = "display: flex; justify-content: space-between; align-items: center;",
        div(
          h4(style = "margin: 0; color: #0d47a1; font-weight: bold;", 
             paste("📌 Pronóstico Próximo Periodo (t =", next_time, ")")),
          h1(style = "margin: 8px 0 0 0; color: #1565c0; font-size: 42px; font-weight: bold;", 
             val_forecast)
        ),
        div(style = "text-align: right; color: #37474f; background: rgba(255,255,255,0.7); padding: 10px 15px; border-radius: 6px;",
          p(style = "margin: 0; font-size: 13px; font-weight: bold;", "INTERVALO DE CONFIANZA (95%):"),
          p(style = "margin: 3px 0 0 0; font-size: 17px; font-weight: bold; color: #2e7d32;", 
            paste0("[ ", low95, "   —   ", high95, " ]"))
        )
      )
    )
  })
  
  # Gráfica
  output$tsPlot <- renderPlot({
    fc <- fit_ses()
    
    autoplot(fc) +
      autolayer(fitted(fc), series = "Ajustado (SES)") +
      labs(title = "Serie de Tiempo con Suavización Exponencial Simple",
           x = "Tiempo (Años / Periodos)", 
           y = "Observaciones") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Resumen numérico detallado
  output$forecastOutput <- renderPrint({
    fc <- fit_ses()
    summary(fc)
  })
}

# Ejecución
shinyApp(ui = ui, server = server)

# ======================================================== #
#                PARA EJECUTAR LA APLIACIÓN                #
# ======================================================== #
# 1. Abrir la terminal                                     #
# 2. Escribir "R" para abrirlo en la terminal              #
# 3. Ejecutar en la consola: shiny::runApp("src/ses.R")    #
# ======================================================== #