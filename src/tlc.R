# ==============================================================================
# Script: Teorema del Límite Central (TLC) en R Shiny
# Archivo: src/tlc.R
# Para ejecutar: shiny::runApp("src/tlc.R")
# ==============================================================================

# Verificación e instalación de paquetes necesarios
required_packages <- c("shiny", "ggplot2", "gridExtra")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) install.packages(new_packages)

library(shiny)
library(ggplot2)

# ------------------------------------------------------------------------------
# Definición de la Interfaz de Usuario (UI)
# ------------------------------------------------------------------------------
ui <- fluidPage(
  
  # Estilos CSS personalizados
  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      .well { background-color: #ffffff; border: 1px solid #e3e6f0; border-radius: 8px; box-shadow: 0 0.15rem 1.75rem 0 rgba(58, 59, 69, 0.15); }
      .nav-tabs > li.active > a { border-top: 3px solid #4e73df !important; font-weight: bold; }
      .stat-card { background-color: #f1f5f9; border-left: 4px solid #4e73df; padding: 12px; margin-bottom: 10px; border-radius: 4px; }
      .stat-title { font-size: 0.85rem; color: #6e7881; text-transform: uppercase; font-weight: bold; }
      .stat-value { font-size: 1.25rem; color: #2e384d; font-weight: bold; }
    "))
  ),
  
  # Encabezado principal
  titlePanel(
    div(
      style = "margin-bottom: 25px; margin-top: 10px;",
      h2("Teorema del Límite Central (TLC)", style = "color: #2c3e50; font-weight: 700; margin-bottom: 5px;"),
      p("Exploración interactiva de la convergencia de la media muestral hacia una distribución normal", style = "color: #7f8c8d; font-size: 1.1em;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      h4("Configuración de la Simulación", style = "color: #2c3e50; font-weight: 600; border-bottom: 2px solid #e2e8f0; padding-bottom: 8px;"),
      br(),
      
      selectInput(
        inputId = "dist",
        label = "1. Distribución Poblacional:",
        choices = c(
          "Uniforme [0, 1]" = "unif",
          "Exponencial (λ = 1)" = "exp",
          "Poisson (λ = 3)" = "pois",
          "Binomial (n = 20, p = 0.2)" = "binom",
          "Gamma (forma = 2, escala = 2)" = "gamma",
          "Bimodal (Mezcla de Normales)" = "bimodal"
        ),
        selected = "exp"
      ),
      
      sliderInput(
        inputId = "n",
        label = "2. Tamaño de Muestra (n):",
        min = 1,
        max = 200,
        value = 30,
        step = 1
      ),
      
      sliderInput(
        inputId = "nsim",
        label = "3. Número de Simulaciónes (K):",
        min = 500,
        max = 10000,
        value = 2000,
        step = 500
      ),
      
      hr(),
      h4("Opciones Visuales", style = "color: #2c3e50; font-weight: 600; border-bottom: 2px solid #e2e8f0; padding-bottom: 8px;"),
      
      checkboxInput("show_normal", "Superponer Curva Normal Teórica", value = TRUE),
      checkboxInput("show_density", "Superponer Densidad Empírica (Kernel)", value = TRUE),
      
      actionButton("resample", "Regenerar Simulación", class = "btn-primary btn-block", style = "margin-top: 15px; font-weight: bold;")
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        type = "tabs",
        
        # Pestaña 1: Muestreo y TLC
        tabPanel(
          "Distribución Muestral",
          br(),
          plotOutput("plot_means", height = "380px"),
          br(),
          h4("Comparación de Parámetros Teóricos vs. Empíricos", style = "color: #2c3e50; font-weight: 600;"),
          tableOutput("stats_table")
        ),
        
        # Pestaña 2: Población Original y Q-Q Plot
        tabPanel(
          "Población y Diagnóstico Normal",
          br(),
          fluidRow(
            column(6, 
                   h5("Distribución Poblacional Original", style = "text-align: center; font-weight: bold; color: #334155;"),
                   plotOutput("plot_pop", height = "320px")
            ),
            column(6, 
                   h5("Gráfico Q-Q Normal de Medias Muestrales", style = "text-align: center; font-weight: bold; color: #334155;"),
                   plotOutput("plot_qq", height = "320px")
            )
          )
        ),
        
        # Pestaña 3: Explicación Teórica
        tabPanel(
          "Explicación Teórica",
          br(),
          div(
            style = "background-color: #ffffff; padding: 20px; border-radius: 8px; border: 1px solid #e2e8f0;",
            h3("¿Qué establece el Teorema del Límite Central?", style = "color: #1e293b; margin-top: 0;"),
            p("El ", tags$strong("Teorema del Límite Central (TLC)"), " es uno de los resultados más fundamentales de la estadística e inferencia probabilística."),
            p("Establece que si se toman muestras aleatorias independientes de tamaño ", tags$em("n"), " de cualquier población con media finita ", tags$span(style="font-family: serif; font-style: italic;", "μ"), " y varianza finita ", tags$span(style="font-family: serif; font-style: italic;", "σ²"), ", la distribución de las medias muestrales ", tags$span(style="font-family: serif; font-style: italic;", "X̄"), " se aproximará a una distribución normal a medida que el tamaño de la muestra ", tags$em("n"), " aumenta:"),
            
            div(
              style = "text-align: center; background-color: #f8fafc; padding: 15px; margin: 15px 0; border-radius: 6px; font-size: 1.2em; font-family: serif;",
              "X̄ ~ N( μ , σ² / n )   ó en forma estandarizada:   Z = (X̄ - μ) / (σ / √n)  ─d─>  N(0, 1)"
            ),
            
            h4("Puntos Clave a Observar en la App:"),
            tags$ul(
              tags$li(tags$strong("Independencia del origen: "), "No importa qué tan asimétrica o bimodal sea la población original (como la Exponencial o Bimodal), la distribución de las medias siempre tomará forma acampanada."),
              tags$li(tags$strong("Efecto del tamaño de muestra (n): "), "Con ", tags$code("n = 1"), ", la distribución muestral es idéntica a la población original. Conforme aumentas ", tags$code("n"), " (por ejemplo, a 30 o 50), la variabilidad disminuye y la simetría aumenta."),
              tags$li(tags$strong("Error Estándar: "), "La dispersión de las medias es ", tags$span(style="font-family: serif;", "σ / √n"), ", lo cual explica por qué el histograma se vuelve más angosto al aumentar ", tags$em("n"), "."),
              tags$li(tags$strong("Gráfico Q-Q Normal: "), "Si los puntos siguen alineados con la recta diagonal roja, confirma la normalidad de los datos muestrales.")
            )
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Definición de la Lógica del Servidor (Server)
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactivo para generar los datos muestrales según la distribución seleccionada
  sim_data <- reactive({
    input$resample # Reacciona al botón
    
    n <- input$n
    nsim <- input$nsim
    dist <- input$dist
    
    # Matriz para almacenar las muestras: filas = nsim, columnas = n
    samples_matrix <- switch(
      dist,
      "unif"    = matrix(runif(n * nsim, min = 0, max = 1), nrow = nsim, ncol = n),
      "exp"     = matrix(rexp(n * nsim, rate = 1), nrow = nsim, ncol = n),
      "pois"    = matrix(rpois(n * nsim, lambda = 3), nrow = nsim, ncol = n),
      "binom"   = matrix(rbinom(n * nsim, size = 20, prob = 0.2), nrow = nsim, ncol = n),
      "gamma"   = matrix(rgamma(n * nsim, shape = 2, scale = 2), nrow = nsim, ncol = n),
      "bimodal" = {
        # Mezcla 50/50 de N(-2, 0.8^2) y N(2, 0.8^2)
        comp <- rbinom(n * nsim, size = 1, prob = 0.5)
        vals <- comp * rnorm(n * nsim, mean = -2, sd = 0.8) + (1 - comp) * rnorm(n * nsim, mean = 2, sd = 0.8)
        matrix(vals, nrow = nsim, ncol = n)
      }
    )
    
    # Calcular las medias por fila (una media por cada simulación)
    means <- rowMeans(samples_matrix)
    
    # Obtener parámetros teóricos de la población
    pop_params <- switch(
      dist,
      "unif"    = list(mu = 0.5, sigma = sqrt(1/12), name = "Uniforme [0, 1]"),
      "exp"     = list(mu = 1.0, sigma = 1.0, name = "Exponencial (λ = 1)"),
      "pois"    = list(mu = 3.0, sigma = sqrt(3), name = "Poisson (λ = 3)"),
      "binom"   = list(mu = 20 * 0.2, sigma = sqrt(20 * 0.2 * 0.8), name = "Binomial (20, 0.2)"),
      "gamma"   = list(mu = 2 * 2, sigma = sqrt(2 * 2 * 2^2), name = "Gamma (k=2, θ=2)"),
      "bimodal" = list(mu = 0.0, sigma = sqrt(0.5*( (-2)^2 + 0.8^2 ) + 0.5*( 2^2 + 0.8^2 )), name = "Bimodal (Mezcla Normal)")
    )
    
    list(
      means = means,
      pop_sample = as.vector(samples_matrix[1:min(nsim, 500), ]), # Submuestra para graficar la población
      mu_pop = pop_params$mu,
      sigma_pop = pop_params$sigma,
      dist_name = pop_params$name,
      se_theory = pop_params$sigma / sqrt(n)
    )
  })
  
  # Histogramas de Medias Muestrales
  output$plot_means <- renderPlot({
    res <- sim_data()
    df <- data.frame(mean = res$means)
    
    p <- ggplot(df, aes(x = mean)) +
      geom_histogram(
        aes(y = ..density..),
        bins = 35,
        fill = "#6366f1",
        color = "#ffffff",
        alpha = 0.75
      ) +
      geom_vline(
        aes(xintercept = mean(mean)),
        color = "#e11d48",
        linetype = "dashed",
        linewidth = 1.1
      ) +
      labs(
        title = paste("Distribución Muestral de Medias (n =", input$n, "| Simulaciónes K =", input$nsim, ")"),
        subtitle = paste("Población Origen:", res$dist_name, "| Línea roja: Media Muestral Empírica"),
        x = "Valor de la Media Muestral (X̄)",
        y = "Densidad"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", color = "#1e293b"),
        plot.subtitle = element_text(color = "#64748b"),
        panel.grid.minor = element_blank()
      )
    
    # Superponer densidad empírica (Kernel)
    if (input$show_density) {
      p <- p + geom_density(color = "#0284c7", linewidth = 1.2)
    }
    
    # Superponer curva normal teórica N(mu, sigma / sqrt(n))
    if (input$show_normal) {
      p <- p + stat_function(
        fun = dnorm,
        args = list(mean = res$mu_pop, sd = res$se_theory),
        color = "#15803d",
        linewidth = 1.2,
        linetype = "solid"
      )
    }
    
    p
  })
  
  # Gráfico de la Distribución Poblacional Original
  output$plot_pop <- renderPlot({
    res <- sim_data()
    df_pop <- data.frame(x = res$pop_sample)
    
    ggplot(df_pop, aes(x = x)) +
      geom_histogram(
        aes(y = ..density..),
        bins = 30,
        fill = "#64748b",
        color = "#ffffff",
        alpha = 0.7
      ) +
      geom_density(color = "#0f172a", linewidth = 1) +
      labs(
        title = "Población Base",
        subtitle = res$dist_name,
        x = "X",
        y = "Densidad"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 12, color = "#1e293b"),
        plot.subtitle = element_text(size = 10, color = "#64748b")
      )
  })
  
  # Gráfico Q-Q Normal
  output$plot_qq <- renderPlot({
    res <- sim_data()
    df <- data.frame(mean = res$means)
    
    ggplot(df, aes(sample = mean)) +
      stat_qq(color = "#4f46e5", alpha = 0.6) +
      stat_qq_line(color = "#dc2626", linewidth = 1) +
      labs(
        title = "Gráfico Q-Q Normal",
        subtitle = "Alineación a la recta indica normalidad",
        x = "Cuantiiles Teóricos N(0,1)",
        y = "Cuantiles Empíricos"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 12, color = "#1e293b"),
        plot.subtitle = element_text(size = 10, color = "#64748b")
      )
  })
  
  # Tabla de Resumen Estadístico
  output$stats_table <- renderTable({
    res <- sim_data()
    
    emp_mean <- mean(res$means)
    emp_se <- sd(res$means)
    
    df_stats <- data.frame(
      Métrica = c("Media (μ)", "Error Estándar / Desviación de X̄ (SE)"),
      `Valor Teórico` = c(
        round(res$mu_pop, 4),
        round(res$se_theory, 4)
      ),
      `Valor Empírico (Simulado)` = c(
        round(emp_mean, 4),
        round(emp_se, 4)
      ),
      Diferencia = c(
        round(abs(res$mu_pop - emp_mean), 4),
        round(abs(res$se_theory - emp_se), 4)
      ),
      check.names = FALSE
    )
    
    df_stats
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "c")
  
}

# ------------------------------------------------------------------------------
# Ejecución de la Aplicación Shiny
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)