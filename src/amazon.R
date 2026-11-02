library(shiny)
library(ggplot2)
library(forecast)

# Configurar el idioma en español para fechas y días de la semana
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

ui <- fluidPage(
  titlePanel("Pronóstico Semanal - Modelo ARIMA(0,1,1) para Venta de AMAZON"),
  
  sidebarLayout(
    sidebarPanel(
      # Encabezado informativo con la fecha actual explícita
      wellPanel(
        h5(strong("Fecha de actualización:")),
        h4(textOutput("currentDateText"), style = "color: #2c3e50; font-weight: bold;")
      ),
      
      helpText("Pronósticos fuera de muestra utilizando información disponible a la fecha de hoy."),
      hr(),
      
      # Selector del horizonte de pronóstico (h días a futuro)
      numericInput("horizon", 
                   label = "Horizonte de pronóstico (días a futuro):", 
                   value = 7, 
                   min = 1, 
                   max = 30, 
                   step = 1),
      
      # Nivel de confianza
      sliderInput("level", 
                  label = "Nivel de confianza del intervalo (%):", 
                  min = 80, 
                  max = 99, 
                  value = 95, 
                  step = 1),
      
      hr(),
      # Controles para la visualización de observaciones históricas recientes
      numericInput("recent_n", 
                   label = "Número de observaciones recientes a inspeccionar:", 
                   value = 7, 
                   min = 1, 
                   max = 30, 
                   step = 1),
      
      hr(),
      
      # Botón de descarga para los datos pronosticados
      downloadButton("downloadData", "Descargar Pronósticos (CSV)", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico de Pronóstico", 
                 plotOutput("forecastPlot", height = "450px"),
                 br(),
                 verbatimTextOutput("modelSummary")),
        tabPanel("Últimas Observaciones", 
                 br(),
                 h4(textOutput("recentTitle")),
                 p("Inspección detallada de las observaciones más recientes de la serie 'sales'."),
                 fluidRow(
                   column(6, tableOutput("recentTable")),
                   column(6, plotOutput("recentPlot", height = "300px"))
                 )),
        tabPanel("Tabla de Predicciones", 
                 br(),
                 h4("Proyección Diaria Detallada"),
                 tableOutput("forecastTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Fecha de hoy
  today_date <- Sys.Date()
  
  # Mostrar fecha explícita en el panel lateral
  output$currentDateText <- renderText({
    format(today_date, "%A, %e de %B de %Y")
  })
  
  # Cargar o generar la serie de ventas con fechas reales
  get_sales_data <- reactive({
    if (exists("sales")) {
      return(sales)
    } else {
      # Si no existe la variable 'sales', se genera una serie histórica diaria hasta el día de hoy
      set.seed(123)
      n_obs <- 100
      start_date <- today_date - (n_obs - 1)
      sales_vec <- round(cumsum(rnorm(n_obs, mean = 0.5, sd = 2)) + 100, 2)
      
      # Retornar como objeto de tiempo o serie
      attr(sales_vec, "dates") <- seq.Date(from = start_date, to = today_date, by = "day")
      return(sales_vec)
    }
  })
  
  # Extraer dataframe de datos históricos con fechas formateadas
  get_hist_df <- reactive({
    data_sales <- get_sales_data()
    n_hist <- length(data_sales)
    dates_hist <- attr(data_sales, "dates")
    if (is.null(dates_hist)) {
      dates_hist <- seq.Date(from = today_date - (n_hist - 1), to = today_date, by = "day")
    }
    
    data.frame(
      Fecha_Obj = dates_hist,
      Fecha = format(dates_hist, "%A, %e de %B de %Y"),
      Ventas = round(as.numeric(data_sales), 2)
    )
  })
  
  # Ajustar el modelo ARIMA(0,1,1)
  fit_model <- reactive({
    data_sales <- get_sales_data()
    Arima(as.numeric(data_sales), order = c(0, 1, 1))
  })
  
  # Secuencia de fechas futuras para el pronóstico
  future_dates <- reactive({
    seq.Date(from = today_date + 1, by = "day", length.out = input$horizon)
  })
  
  # Generar el pronóstico
  make_forecast <- reactive({
    mod <- fit_model()
    forecast(mod, h = input$horizon, level = input$level)
  })
  
  # Título dinámico para la pestaña de observaciones recientes
  output$recentTitle <- renderText({
    sprintf("Últimas %d Observaciones Observadas", input$recent_n)
  })
  
  # Tabla con las últimas N observaciones
  output$recentTable <- renderTable({
    df_hist <- get_hist_df()
    n_total <- nrow(df_hist)
    n_sub <- min(input$recent_n, n_total)
    
    # Extraer las últimas n filas
    df_recent <- tail(df_hist, n_sub)
    df_recent <- df_recent[order(df_recent$Fecha_Obj, decreasing = TRUE), c("Fecha", "Ventas")]
    df_recent
  }, align = 'c')
  
  # Gráfico secundario enfocado únicamente en las últimas N observaciones
  output$recentPlot <- renderPlot({
    df_hist <- get_hist_df()
    n_total <- nrow(df_hist)
    n_sub <- min(input$recent_n, n_total)
    df_recent <- tail(df_hist, n_sub)
    
    ggplot(df_recent, aes(x = Fecha_Obj, y = Ventas)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "steelblue", size = 3) +
      geom_text(aes(label = Ventas), vjust = -0.8, color = "gray20", size = 3.5, fontface = "bold") +
      scale_x_date(date_labels = "%d %b", date_breaks = "1 day") +
      labs(
        title = sprintf("Trayectoria Reciente (Últimos %d Días)", n_sub),
        x = "Fecha",
        y = "Ventas"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Renderizar el gráfico principal con ggplot2
  output$forecastPlot <- renderPlot({
    fc <- make_forecast()
    data_sales <- get_sales_data()
    df_hist <- get_hist_df()
    dates_fc <- future_dates()
    
    df_fc <- data.frame(
      Fecha_Obj = dates_fc,
      Pronostico = as.numeric(fc$mean),
      Lower = as.numeric(fc$lower),
      Upper = as.numeric(fc$upper)
    )
    
    ggplot() +
      # Banda del intervalo de confianza
      geom_ribbon(data = df_fc, aes(x = Fecha_Obj, ymin = Lower, ymax = Upper), 
                  fill = "firebrick", alpha = 0.2) +
      # Serie histórica
      geom_line(data = df_hist, aes(x = Fecha_Obj, y = Ventas, color = "Histórico"), linewidth = 0.8) +
      # Pronóstico a futuro
      geom_line(data = df_fc, aes(x = Fecha_Obj, y = Pronostico, color = "Pronóstico"), 
                linewidth = 1, linetype = "dashed") +
      geom_point(data = df_fc, aes(x = Fecha_Obj, y = Pronostico), color = "firebrick", size = 2) +
      
      # Línea vertical indicando el día de hoy
      geom_vline(xintercept = as.numeric(today_date), linetype = "dotted", color = "black", linewidth = 0.8) +
      
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      scale_color_manual(values = c("Histórico" = "steelblue", "Pronóstico" = "firebrick")) +
      labs(
        title = sprintf("Pronóstico a %d Días a Futuro (Modelo ARIMA(0,1,1))", input$horizon),
        subtitle = sprintf("Información actualizada al %s | Intervalo de confianza al %d%%", 
                           format(today_date, "%e de %B de %Y"), input$level),
        x = "Fecha",
        y = "Ventas",
        color = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
  })
  
  # Resumen del modelo
  output$modelSummary <- renderPrint({
    cat("--- Resumen del Modelo ARIMA(0,1,1) Ajustado ---\n")
    print(fit_model())
  })
  
  # Tabla de predicciones
  output$forecastTable <- renderTable({
    fc <- make_forecast()
    dates_fc <- future_dates()
    
    fechas_formateadas <- format(dates_fc, "%A, %e de %B de %Y")
    
    res_df <- data.frame(
      Fecha = fechas_formateadas,
      `Pronóstico Puntual` = round(as.numeric(fc$mean), 2),
      `Límite Inferior` = round(as.numeric(fc$lower), 2),
      `Límite Superior` = round(as.numeric(fc$upper), 2),
      check.names = FALSE
    )
    
    colnames(res_df)[3:4] <- c(paste0("Límite Inf (", input$level, "%)"), 
                               paste0("Límite Sup (", input$level, "%)"))
    res_df
  }, align = 'c')
  
  # Descarga de datos
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("pronostico_sales_ARIMA011_", today_date, ".csv")
    },
    content = function(file) {
      fc <- make_forecast()
      dates_fc <- future_dates()
      
      df_out <- data.frame(
        Fecha = format(dates_fc, "%A, %e de %B de %Y"),
        Pronostico = as.numeric(fc$mean),
        Lower = as.numeric(fc$lower),
        Upper = as.numeric(fc$upper)
      )
      write.csv(df_out, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)


# ======================================================== #
#                PARA EJECUTAR LA APLIACIÓN                #
# ======================================================== #
# 1. Abrir la terminal                                     #
# 2. Escribir "R" para abrirlo en la terminal              #
# 3. Ejecutar en la consola: shiny::runApp("src/amazon.R") #
# ======================================================== #