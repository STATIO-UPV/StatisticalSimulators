library(shiny)
library(tidyverse)
library(car)
library(agricolae)

# No borrar esto
if (FALSE) {
  library(munsell)
}

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("ANOVA unifactorial"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    # Sidebar panel for inputs ----
    sidebarPanel(

      h4("Configurador de datos de entrada:"),
      h5("Utiliza los siguientes sliders para definir el número de observaciones por grupo, así como sus medias y sus desviaciones. Con estos valores se generará el dataset de manera sintética para que puedas explorar el funcionamiento del ANOVA de manera interactiva."),
      
      # Input: Slider for the number of observations to generate ----
      selectInput("optType", "Tipo de problema", choices = c("Maximización", "Minimización")),
      sliderInput("n",
                  "Número de observaciones por grupo (n):",
                  value = 30,
                  min = 1,
                  max = 100),
      
      h4("Medias:"),
      sliderInput("uA",
                  HTML("Grupo A (&mu;<sub>A</sub>):"),
                  value = 50,
                  min = 40,
                  max = 60),
      sliderInput("uB",
                  HTML("Grupo B (&mu;<sub>B</sub>):"),
                  value = 50,
                  min = 40,
                  max = 60),
      sliderInput("uC",
                  HTML("Grupo C (&mu;<sub>C</sub>):"),
                  value = 50,
                  min = 40,
                  max = 60),
      
      h4("Desviación estandar:"),
      sliderInput("sdA",
                  HTML("Grupo A (&sigma;<sub>A</sub>):"),
                  value = 5,
                  min = 1,
                  max = 20),
      
      sliderInput("sdB",
                  HTML("Grupo B (&sigma;<sub>B</sub>):"),
                  value = 5,
                  min = 1,
                  max = 20),
      
      sliderInput("sdC",
                  HTML("Grupo C (&sigma;<sub>C</sub>):"),
                  value = 5,
                  min = 1,
                  max = 20),
      h5("Aplicacion realizada para el proyecto docente de Javier Marín Morales. Concurso a PPL C03/24, código de la plaza 7266."),
      tags$div(
        tags$img(src = "DEIOAC.png", height = "150px", style = "display: block; margin-left: auto; margin-right: auto;")
      ),
      tags$div(
        tags$img(src = "UPV.png", height = "80px", style = "display: block; margin-left: auto; margin-right: auto;")
      ),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Resumen", h4("Distribución de los datos:"), plotOutput("violinPlot"),uiOutput("sampleStats"),
                           h4("ANOVA:"), h5("Tabla:"), verbatimTextOutput("aov"),
                           h5("Interpretación:"),textOutput("conclusionText"),br(),
                           h5("Intervalos LSD:"),plotOutput("lsdPlot3"),textOutput("lsdConclusionText3"),br(),
                           h5("Condición operativa óptima:"),textOutput("optimalConditionText"),br(),
                           ),
                  tabPanel("Análisis ANOVA", h4("ANOVA:"), h5("Tabla:"), verbatimTextOutput("aov2"),br(),
                           h5("Hipótesis del Test F:"),uiOutput("hypothesesText"),br(),
                           h5("Distribución a utilizar:"),textOutput("distributionText"),br(),
                           h5("Calculamos las regiones de rechazo:"),uiOutput("fCriticalText"),br(),
                           h5("Calculamos el estadístico:"),uiOutput("fRatioFormula"),br(),
                           h5("Calculamos el p-valor:"),uiOutput("pValueTextDetailed"),br(),
                           h5("Ploteamos las regiones, el estadístico y el p-valor:"),plotOutput("fDistributionPlot"),
                           h5("Conclusión:"),textOutput("fConclusionText"),br(),br()
                           ),
                  tabPanel("Condiciones",
                           h4("Independencia:"), plotOutput("independencePlot"),textOutput("independenceText"),br(),
                           h4("Normalidad:"), plotOutput("normalityPlot"),textOutput("normalityText"),br(),
                           h4("Homocedasticidad (ANOVA residuos al cuadrado):"),
                           h5("Distribución de los residuos al cuadrado:"), plotOutput("residualsViolinPlot"),
                           h5("Hipótesis del Test F asociado:"),uiOutput("hypothesesText_res"),br(),
                           h5("Tabla:"), verbatimTextOutput("aov_res"),
                           h5("Interpretación:"),textOutput("conclusionText_res"),br(),br()
                           ),
                  tabPanel("Condicion Operativa Optima",
                           h4("Condición operativa óptima:"),textOutput("optimalConditionText2"),br(),
                           h4("Desviación de la Condición Operativa Óptima:"),uiOutput("optimalConditionDeviation"),br(),
                           h4("Distribución:"),uiOutput("cooDistribution"),br(),br()
                           ),
                  tabPanel("Datos", tableOutput("dat")),
                  tabPanel("Datos residuos", tableOutput("dat_res"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression

  ##################### CALCULOS DATASET  ####################
  
  d <- reactive({
    n <- input$n
    uA <- input$uA
    uB <- input$uB
    uC <- input$uC
    sdA <- input$sdA
    sdB <- input$sdB
    sdC <- input$sdC
    
    set.seed(123)
    
    group_A <- data.frame(
      group = "A",
      value = rnorm(n, uA, sdA)
    )
    
    group_B <- data.frame(
      group = "B",
      value = rnorm(n, uB, sdB)
    )
    
    group_C <- data.frame(
      group = "C",
      value = rnorm(n, uC, sdC)
    )
    
    bind_rows(group_A, group_B, group_C)
  })
  
  ##################### VIOLIN PLOT  ##################### 
  
  output$violinPlot <- renderPlot({
    data <- d()
    ggplot(data, aes(x = group, y = value, fill = group)) +
      geom_violin(trim = FALSE) +
      geom_jitter(width = 0.1, size = 0.5) +
      labs(title = "Violin Plot de los Grupos A, B y C",
           x = "Grupo",
           y = "Valor") +
      theme_minimal()
  })
  
  
  # Calcular y mostrar las medias y desviaciones estándar muestrales
  output$sampleStats <- renderUI({
    data <- d()
    means <- data %>% group_by(group) %>% summarise(mean = mean(value))
    sds <- data %>% group_by(group) %>% summarise(sd = sd(value))
    
    withMathJax(HTML(paste0(
      "Datos muestrales: ",
      "$$Medias:\\quad\\bar{x}_A = ", round(means$mean[means$group == "A"], 2), ",\\quad ",
      "\\bar{x}_B = ", round(means$mean[means$group == "B"], 2), ",\\quad ",
      "\\bar{x}_C = ", round(means$mean[means$group == "C"], 2), "$$",
      "$$Desviación\\hspace{0.2cm} est.:\\quad s_A = ", round(sds$sd[sds$group == "A"], 2), ",\\quad ",
      "s_B = ", round(sds$sd[sds$group == "B"], 2), ",\\quad ",
      "s_C = ", round(sds$sd[sds$group == "C"], 2), "$$"
    )))
  })
  
  ##################### ANOVA  ##################### 
  
  # Crear el ANOVA reactivo
  anova_result <- reactive({
    data <- d()
    aov(value ~ group, data = data)
  })
  
  # Extraer el p-valor del ANOVA
  p_value <- reactive({
    anova_summary <- summary(anova_result())
    anova_summary[[1]][["Pr(>F)"]][1]
  })
  
  # Generar el mensaje de conclusión basado en el p-valor
  conclusion <- reactive({
    if (p_value() < 0.05) {
      "El análisis ANOVA muestra que hay una diferencia estadísticamente significativa entre los grupos (p < 0.05).
Por lo tanto, hay al menos un par de medias diferentes (H1)."
    } else {
      "El análisis ANOVA no muestra una diferencia estadísticamente significativa entre los grupos (p >= 0.05).
Por lo tanto, no se puede afirmar que las medias son diferentes (H0)."
    }
  })
  
  # Calcular el LSD y crear los intervalos de confianza
  lsd_result <- reactive({
    data <- d()
    anova_model <- anova_result()
    LSD.test(anova_model, "group", alpha = 0.05, p.adj="none")
  })
  
  # Generar el mensaje de conclusión basado en los intervalos LSD
  lsd_conclusion <- reactive({
    lsd_res <- lsd_result()
    significant_pairs <- lsd_res$groups
    paste("Los intervalos LSD muestran diferencias significativas entre los siguientes pares de grupos:", 
          paste(rownames(significant_pairs[significant_pairs$groups != significant_pairs[1,1], ]), collapse = ", "))
  })
  
  # Generate a ANOVA ----
  output$aov <- renderPrint({
    anova_result <- anova_result()
    summary(anova_result)
  })
  
  output$aov2 <- renderPrint({
    anova_result <- anova_result()
    summary(anova_result)
  })
  
  # Mostrar el p-valor
  output$pValueText <- renderText({
    paste("p-valor del ANOVA: ", format(p_value(), digits = 5))
  })
  
  # Mostrar la conclusión
  output$conclusionText <- renderText({
    conclusion()
  })
  
  ##################### Intervalos LSD  ##################### 
  
  #Este intervalo no se utiliza, es el de la librería agricolae. Luego se hacen una serie de cálculos para sacar los LSD con la formula.
  output$lsdPlot1 <- renderPlot({
    lsd_res <- lsd_result()
    plot(lsd_res, main="Intervalos LSD para cada grupo")
  })
  
  output$lsdtable <- renderPrint({
    lsd_res<-lsd_result()
    capture.output(print(lsd_res))
  })
  
  output$lsdConclusionText <- renderText({
    lsd_conclusion()
  })
  
  mse_and_df <- reactive({
    anova_model <- anova_result()
    mse <- summary(anova_model)[[1]]["Residuals", "Mean Sq"]
    df <- summary(anova_model)[[1]]["Residuals", "Df"]
    list(mse = mse, df = df)
  })
  
  lsd_intervals <- reactive({
    data <- d()
    means <- data %>%
      group_by(group) %>%
      summarise(mean = mean(value), .groups = 'drop')
    mse <- mse_and_df()$mse
    df <- mse_and_df()$df
    n <- input$n
    alpha <- 0.05
    t_value <- qt(1 - alpha / 2, df)
    lsd_factor <- (sqrt(2) / 2) * t_value * sqrt(mse / n)
    
    means <- means %>%
      mutate(
        LCL = mean - lsd_factor,
        UCL = mean + lsd_factor
      )
    means
  })
  
  output$lsdPlot3 <- renderPlot({
    lsd_data <- lsd_intervals()
    ggplot(lsd_data, aes(x = group, y = mean)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
      labs(title = "Intervalos de Confianza LSD Calculados",
           x = "Grupo",
           y = "Valor Promedio con Intervalos de Confianza") +
      theme_minimal()
  })
  
  # Generar la conclusión basada en los intervalos LSD calculados
  lsd_conclusion3 <- reactive({
    lsd_data <- lsd_intervals()
    comparisons <- combn(lsd_data$group, 2, simplify = FALSE)
    significant_differences <- lapply(comparisons, function(pair) {
      group1 <- lsd_data %>% filter(group == pair[1])
      group2 <- lsd_data %>% filter(group == pair[2])
      if (group1$UCL < group2$LCL | group1$LCL > group2$UCL) {
        paste("Hay una diferencia significativa entre", pair[1], "y", pair[2])
      } else {
        paste("No hay una diferencia significativa entre", pair[1], "y", pair[2])
      }
    })
    paste(significant_differences, collapse = "\n")
  })
  
  # Renderizar la conclusión de LSD
  output$lsdConclusionText3 <- renderText({
    lsd_conclusion3()
  })
  
  ##################### Condición operativa óptima pagina resumen ##################### 
  optimal_condition <- reactive({
    p_value <- p_value()
    if (p_value >= 0.05) {
      return("No hay diferencias significativas entre las condiciones según el ANOVA.")
    }
    
    lsd_data <- lsd_intervals()
    
    if (input$optType == "Maximización") {
      max_mean <- max(lsd_data$mean)
      best_groups <- lsd_data %>% filter(mean == max_mean)
      other_groups <- lsd_data %>% filter(mean != max_mean)
      
      no_overlap <- all(other_groups$UCL < best_groups$LCL | other_groups$LCL > best_groups$UCL)
      
      if (no_overlap) {
        paste("La mejor condición operativa es el grupo", best_groups$group, ".")
      } else {
        overlapping_groups <- lsd_data %>% filter(mean == max_mean | (mean != max_mean & UCL >= max_mean & LCL <= max_mean))
        if (nrow(overlapping_groups) > 1) {
          best_group <- overlapping_groups %>% arrange(desc(mean)) %>% slice(1)
          paste("La mejor condición operativa es cualquiera de las siguientes, ya que no hay diferencias significativas:", 
                paste(overlapping_groups$group, collapse = ", "),
                ".")
        } else {
          paste("La mejor condición operativa es el grupo", overlapping_groups$group, ".")
        }
      }
    } else {
      min_mean <- min(lsd_data$mean)
      best_groups <- lsd_data %>% filter(mean == min_mean)
      other_groups <- lsd_data %>% filter(mean != min_mean)
      
      no_overlap <- all(other_groups$UCL < best_groups$LCL | other_groups$LCL > best_groups$UCL)
      
      if (no_overlap) {
        paste("La mejor condición operativa es el grupo", best_groups$group, ".")
      } else {
        overlapping_groups <- lsd_data %>% filter(mean == min_mean | (mean != min_mean & UCL >= min_mean & LCL <= min_mean))
        if (nrow(overlapping_groups) > 1) {
          best_group <- overlapping_groups %>% arrange(mean) %>% slice(1)
          paste("La mejor condición operativa es cualquiera de las siguientes, ya que no hay diferencias significativas:", 
                paste(overlapping_groups$group, collapse = ", "),
                ".")
        } else {
          paste("La mejor condición operativa es el grupo", overlapping_groups$group, "c.")
        }
      }
    }
  })
  
  # Renderizar la condición óptima
  output$optimalConditionText <- renderText({
    optimal_condition()
  })
  
  
  ##################### ANOVA RESIDUOS ####################
  
  # Mostrar las hipótesis del test F
  output$hypothesesText_res <- renderUI({
    withMathJax(HTML(paste("$$H_0: \\sigma_1^2 = \\sigma_2^2 = \\sigma_3^2$$",
                           "$$H_1: \\text{Al menos un par de varianzas es diferente}$$")))
  })
  
  
  # Renderizar el violin plot de los residuos al cuadrado
  output$residualsViolinPlot <- renderPlot({
    data <- d()
    data$residuals_squared <- residuals(anova_result())^2
    ggplot(data, aes(x = group, y = residuals_squared, fill = group)) +
      geom_violin(trim = FALSE) +
      geom_jitter(width = 0.1, size = 0.5) +
      labs(title = "Violin Plot de los Residuos al Cuadrado de los Grupos A, B y C",
           x = "Grupo",
           y = "Residuos al Cuadrado") +
      theme_minimal()
  })
  
  
  
  # Crear el dataset de residuos reactivo
  residuals_data <- reactive({
    anova_model <- anova_result()
    residuals <- residuals(anova_model)
    data <- d()
    data.frame(group = data$group, residuals = residuals)
  })
  
  anova_res_result <- reactive({
    res_data <- residuals_data()
    aov(residuals^2 ~ group, data = res_data)
  })
  
  # Extraer el p-valor del ANOVA
  p_value_res <- reactive({
    anova_summary <- summary(anova_res_result())
    anova_summary[[1]][["Pr(>F)"]][1]
  })
  
  # Generar el mensaje de conclusión basado en el p-valor
  conclusion_res <- reactive({
    if (p_value_res() < 0.05) {
      "El análisis ANOVA muestra que hay una diferencia estadísticamente significativa entre los grupos (p < 0.05).
Por lo tanto, las distribuciones son heterocedasticas ya que hay al menos un par de varianzas diferentes (H1)."
    } else {
      "El análisis ANOVA no muestra una diferencia estadísticamente significativa entre los grupos (p >= 0.05).
Por lo tanto, las distribuciones son homocedasticas ya que no se puede afirmar que las varianzas son diferentes (H0)."
    }
  })
  
  
  
  # Generate a ANOVA res ----
  output$aov_res <- renderPrint({
    anova_res_result <- anova_res_result()
    summary(anova_res_result)
  })
  
  # Mostrar el p-valor
  output$pValueText_res <- renderText({
    paste("p-valor del ANOVA: ", format(p_value_res(), digits = 5))
  })
  
  # Mostrar la conclusión
  output$conclusionText_res <- renderText({
    conclusion_res()
  })

  ##################### MOSTRAR CÁLCULO ANOVA ####################
  
  # Mostrar las hipótesis del test F
  output$hypothesesText <- renderUI({
    withMathJax(HTML(paste("$$H_0: \\mu_1 = \\mu_2 = \\mu_3$$",
                           "$$H_1: \\text{Al menos un par de medias es diferente}$$")))
  })
  
  # Mostrar la distribución F y los grados de libertad
  output$distributionText <- renderText({
    anova_summary <- summary(anova_result())
    df_between <- anova_summary[[1]]["group", "Df"]
    df_within <- anova_summary[[1]]["Residuals", "Df"]
    paste0("Vamos a trabajar con la distribución F de Snedecor con ",
           df_between, " grados de libertad en el numerador, y ",
           df_within, " grados de libertad en el denominador.")
  })
  
  # Calcular y mostrar el F-ratio como fórmula dinámica
  output$fRatioFormula <- renderUI({
    anova_summary <- summary(anova_result())
    ms_between <- anova_summary[[1]]["group", "Mean Sq"]
    ms_within <- anova_summary[[1]]["Residuals", "Mean Sq"]
    f_value <- ms_between / ms_within
    withMathJax(
      paste0("$$\\frac{\\text{Cuadrado Medio del Factor}}{\\text{Cuadrado Medio del Residuo}} = \\frac{", 
             round(ms_between, 4), "}{", round(ms_within, 4), "} = ", round(f_value, 4), "$$")
    )
  })
  
  
  # Calcular y mostrar el F crítico
  output$fCriticalText <- renderUI({
    anova_summary <- summary(anova_result())
    df_between <- anova_summary[[1]]["group", "Df"]
    df_within <- anova_summary[[1]]["Residuals", "Df"]
    f_critical <- qf(0.95, df_between, df_within)
    withMathJax(HTML(paste0(
      "El F crítico es el valor de la distribución F con ", df_between, 
      " grados de libertad en el numerador y ", df_within, 
      " grados de libertad en el denominador, que deja por la derecha el error de tipo I (0.05).<br>",
      "$$F_{\\alpha, gl1, gl2} = qf(1 - \\alpha, gl1, gl2) = qf(0.95, ", df_between, ", ", df_within, ") = ", round(f_critical, 4), "$$"
    )))
  })
  

  
  # Calcular las regiones de aceptación y rechazo y plot de la función F
  output$fDistributionPlot <- renderPlot({
    anova_summary <- summary(anova_result())
    ms_between <- anova_summary[[1]]["group", "Mean Sq"]
    ms_within <- anova_summary[[1]]["Residuals", "Mean Sq"]
    f_value <- ms_between / ms_within
    df_between <- anova_summary[[1]]["group", "Df"]
    df_within <- anova_summary[[1]]["Residuals", "Df"]
    
    f_critical <- qf(0.95, df_between, df_within)
    
    x_max <- max(f_value * 1.2, f_critical * 1.2)  # Ajustar el eje x para incluir el valor F calculado y el F crítico
    
    curve(df(x, df_between, df_within), from = 0, to = x_max, n = 1000,
          ylab = "Densidad", xlab = "F", main = "Distribución F con Regiones de Aceptación y Rechazo")
    
    # Sombrear la región de rechazo
    x_fill_rejection <- seq(f_critical, x_max, length.out = 100)
    y_fill_rejection <- df(x_fill_rejection, df_between, df_within)
    polygon(c(f_critical, x_fill_rejection, x_max), c(0, y_fill_rejection, 0), col = rgb(1, 0, 0, 0.3))
    
    # Sombrear la región de p-valor
    x_fill_pvalue <- seq(f_value, x_max, length.out = 100)
    y_fill_pvalue <- df(x_fill_pvalue, df_between, df_within)
    polygon(c(f_value, x_fill_pvalue, x_max), c(0, y_fill_pvalue, 0), col = rgb(0, 0, 1, 0.3))
    
    abline(v = f_critical, col = "red", lwd = 2, lty = 2)
    abline(v = f_value, col = "blue", lwd = 2, lty = 1)
    legend("topright", legend = c("F crítico", "F calculado", "Región de rechazo", "p-valor"),
           col = c("red", "blue", rgb(1, 0, 0, 0.3), rgb(0, 0, 1, 0.3)), lwd = 2, lty = c(2, 1, NA, NA), 
           fill = c(NA, NA, rgb(1, 0, 0, 0.3), rgb(0, 0, 1, 0.3)), border = NA)
  })
  
  
  # Mostrar el valor del p-valor y la región sombreada
  output$pValueTextDetailed <- renderUI({
    anova_summary <- summary(anova_result())
    ms_between <- anova_summary[[1]]["group", "Mean Sq"]
    ms_within <- anova_summary[[1]]["Residuals", "Mean Sq"]
    f_value <- ms_between / ms_within
    df_between <- anova_summary[[1]]["group", "Df"]
    df_within <- anova_summary[[1]]["Residuals", "Df"]
    
    p_value <- 1 - pf(f_value, df_between, df_within)
    
    withMathJax(HTML(paste0(
      "El p-valor es el área a la derecha del estadístico F calculado, que está sombreada en rojo en la imagen.<br>",
      "$$P(F_{gl1,gl2} > F_{calc}) = 1 - pf(F_{calc}, gl1, gl2) = 1 - pf(", round(f_value, 4), ", ", df_between, ", ", df_within, ") = ", format(p_value, digits = 5), "$$"
    )))
  })

  
  # Proporcionar una conclusión basada en el test F
  output$fConclusionText <- renderText({
    anova_summary <- summary(anova_result())
    ms_between <- anova_summary[[1]]["group", "Mean Sq"]
    ms_within <- anova_summary[[1]]["Residuals", "Mean Sq"]
    f_value <- ms_between / ms_within
    df_between <- anova_summary[[1]]["group", "Df"]
    df_within <- anova_summary[[1]]["Residuals", "Df"]
    
    f_critical <- qf(0.95, df_between, df_within)
    
    if (f_value > f_critical) {
      "Dado que el estadístico cae en la región de rechazo (p-valor < 0.05), rechazamos la hipótesis nula (H0). Hay diferencias significativas entre las medias de los grupos."
    } else {
      "Dado que el estadístico cae en la región de aceptacion (p-valor > 0.05), no podemos rechazar la hipótesis nula (H0). No hay diferencias significativas entre las medias de los grupos."
    }
  })
  
  
  
  
  
  
  ##################### CONDICIONES ####################
  
  # Análisis de independencia
  output$independencePlot <- renderPlot({
    data <- d()
    ggplot(data, aes(x = 1:nrow(data), y = value)) +
      geom_line() +
      geom_point() +
      labs(title = "Plot para Análisis de Independencia",
           x = "Orden de las Observaciones",
           y = "Valor") +
      theme_minimal()
  })
  
  output$independenceText <- renderText({
    "Este plot no debería tener ninguna tendencia si se ha realizado un muestreo aleatorio simple, para que se cumpla la condición de independencia."
  })
  
  
  # Análisis de normalidad de los residuos
  output$normalityPlot <- renderPlot({
    model <- anova_result()
    qqPlot(residuals(model), main = "QQ Plot para Análisis de Normalidad")
  })
  

  output$normalityText <- renderText({

    paste0("Para analizar la normalidad utilizamos el papel probabilistico normal. Si todos los valores forman una recta, la distribución de nuestros datos seguirán una distribución normal. Para ello nos fijamos si están dentro del intervalo de confianza marcado con la zona azul. En caso de tener valores fuera, no podemos asegurar la condición de normalidad.")
  })
  
  
  ##################### COO ####################
  
  
  optimal_condition2 <- reactive({
    p_value <- p_value()
    if (p_value >= 0.05) {
      return("No hay COO, ya que no hay diferencias significativas entre los grupos según el ANOVA.")
    }
    
    lsd_data <- lsd_intervals()
    
    if (input$optType == "Maximización") {
      max_mean <- max(lsd_data$mean)
      best_groups <- lsd_data %>% filter(mean == max_mean)
      other_groups <- lsd_data %>% filter(mean != max_mean)
      
      no_overlap <- all(other_groups$UCL < best_groups$LCL | other_groups$LCL > best_groups$UCL)
      
      if (no_overlap) {
        paste("La mejor condición operativa es el grupo", best_groups$group, "con un valor promedio de", round(best_groups$mean, 2))
      } else {
        overlapping_groups <- lsd_data %>% filter(mean == max_mean | (mean != max_mean & UCL >= max_mean & LCL <= max_mean))
        if (nrow(overlapping_groups) > 1) {
          best_group <- overlapping_groups %>% arrange(desc(mean)) %>% slice(1)
          paste("La mejor condición operativa es cualquiera de las siguientes, ya que no hay diferencias significativas:", 
                paste(overlapping_groups$group, collapse = ", "),
                ". Aunque ambas son válidas, elegimos la que tiene mayor media, el grupo", best_group$group, "con un valor promedio de", round(best_group$mean, 2))
        } else {
          paste("La mejor condición operativa es el grupo", overlapping_groups$group, "con un valor promedio de", round(overlapping_groups$mean, 2))
        }
      }
    } else {
      min_mean <- min(lsd_data$mean)
      best_groups <- lsd_data %>% filter(mean == min_mean)
      other_groups <- lsd_data %>% filter(mean != min_mean)
      
      no_overlap <- all(other_groups$UCL < best_groups$LCL | other_groups$LCL > best_groups$UCL)
      
      if (no_overlap) {
        paste("La mejor condición operativa es el grupo", best_groups$group, "con un valor promedio de", round(best_groups$mean, 2))
      } else {
        overlapping_groups <- lsd_data %>% filter(mean == min_mean | (mean != min_mean & UCL >= min_mean & LCL <= min_mean))
        if (nrow(overlapping_groups) > 1) {
          best_group <- overlapping_groups %>% arrange(mean) %>% slice(1)
          paste("La mejor condición operativa es cualquiera de las siguientes, ya que no hay diferencias significativas:", 
                paste(overlapping_groups$group, collapse = ", "),
                ". Aunque ambas son válidas, elegimos la que tiene menor media, el grupo", best_group$group, "con un valor promedio de", round(best_group$mean, 2))
        } else {
          paste("La mejor condición operativa es el grupo", overlapping_groups$group, "con un valor promedio de", round(overlapping_groups$mean, 2))
        }
      }
    }
  })
  
  # Renderizar la condición óptima
  output$optimalConditionText2 <- renderText({
    optimal_condition2()
  })
  
  
  output$optimalConditionDeviation <- renderUI({
    p_value <- summary(anova_result())[[1]]["group", "Pr(>F)"]
    
    if (p_value >= 0.05) {
      return("No hay COO, por lo tanto no se calculará la desviación asociada.")
    }
    
    p_value_res <- p_value_res()
    
    if (p_value_res >= 0.05) {
      ms_error <- summary(anova_result())[[1]]["Residuals", "Mean Sq"]
      deviation <- sqrt(ms_error)
      withMathJax(HTML(paste0("Todos los grupos tienen la misma desviación. La desviación asociada a la COO es la raiz del cuadrado medio residual del ANOVA de medias: $$\\sigma_{\\text{COO}} = \\sqrt{\\text{CMR}} = \\sqrt{", round(ms_error, 2), "} = ", round(deviation, 2), "$$")))
    } else {
      lsd_data <- lsd_intervals()
      if (input$optType == "Maximización") {
        max_mean <- max(lsd_data$mean)
        best_group <- lsd_data %>% filter(mean == max_mean) %>% slice(1)
      } else {
        min_mean <- min(lsd_data$mean)
        best_group <- lsd_data %>% filter(mean == min_mean) %>% slice(1)
      }
      residuals_squared <- residuals(anova_result())^2
      data <- d()
      data$residuals_squared <- residuals_squared
      group_residuals <- data %>% filter(group == best_group$group)
      mean_residuals_squared <- mean(group_residuals$residuals_squared)
      
      df_residuals <- summary(anova_result())[[1]]["Residuals", "Df"]
      n_total <- nrow(data)
      variance_corrected <- mean_residuals_squared * n_total / df_residuals
      deviation_corrected <- sqrt(variance_corrected)
      
      withMathJax(HTML(paste0(
        "La varianza corregida de la COO es $$\\sigma_{\\text{COO}}^{2} = \\frac{\\overline{x}_{\\text{COO res}^{2}} \\cdot N_{\\text{total}}}{gl_{residual}} = \\frac{", round(mean_residuals_squared, 2), " \\cdot ", n_total, "}{", df_residuals, "} = ", round(variance_corrected, 2), "$$",
        "<br>La desviación asociada a la COO es $$\\sigma_{\\text{COO}} = \\sqrt{", round(variance_corrected, 2), "} = ", round(deviation_corrected, 2), "$$"
      )))
    }
  })
  
  
  # Describir la COO como una distribución normal
  output$cooDistribution <- renderUI({
    p_value <- summary(anova_result())[[1]]["group", "Pr(>F)"]
    
    if (p_value >= 0.05) {
      return("No hay COO, ya que no hay diferencias significativas entre los grupos según el ANOVA.")
    }
    
    optimal_condition <- optimal_condition2()
    if (grepl("No hay COO", optimal_condition)) {
      return("No hay COO, por lo tanto no se calculará la desviación asociada.")
    }
    
    p_value_res <- p_value_res()
    
    if (p_value_res >= 0.05) {
      ms_error <- summary(anova_result())[[1]]["Residuals", "Mean Sq"]
      deviation <- sqrt(ms_error)
      mean_coo <- as.numeric(gsub(".*\\s(\\d+\\.\\d+).*", "\\1", optimal_condition)) # Extraer la media de la COO del texto
      withMathJax(HTML(paste0("La COO sigue una distribución $$N(", mean_coo, ", ", round(deviation, 2), ")$$")))
    } else {
      lsd_data <- lsd_intervals()
      if (input$optType == "Maximización") {
        max_mean <- max(lsd_data$mean)
        best_group <- lsd_data %>% filter(mean == max_mean) %>% slice(1)
      } else {
        min_mean <- min(lsd_data$mean)
        best_group <- lsd_data %>% filter(mean == min_mean) %>% slice(1)
      }
      residuals_squared <- residuals(anova_result())^2
      data <- d()
      data$residuals_squared <- residuals_squared
      group_residuals <- data %>% filter(group == best_group$group)
      mean_residuals_squared <- mean(group_residuals$residuals_squared)
      
      df_residuals <- summary(anova_result())[[1]]["Residuals", "Df"]
      n_total <- nrow(data)
      variance_corrected <- mean_residuals_squared * n_total / df_residuals
      deviation_corrected <- sqrt(variance_corrected)
      mean_coo <- as.numeric(gsub(".*\\s(\\d+\\.\\d+).*", "\\1", optimal_condition)) # Extraer la media de la COO del texto
      
      withMathJax(HTML(paste0("La COO sigue una distribución: $$N(", mean_coo, ", ", round(deviation_corrected, 2), ")$$")))
    }
  })
  ##################### MOSTRAR DATASETS ####################
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    head(d())
  })
  
  # Generate an HTML table view of the data ----
  output$dat <- renderTable({
    d()
  })
  
  output$dat_res <- renderTable({
    residuals_data()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
