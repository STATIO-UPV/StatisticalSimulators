
######################### LIBRERÍAS (PUEDES AÑADIR, NO BORRAR) #########################
# shiny → Construcción de la interfaz y del servidor
# shinyjs → Permite usar JS para mostrar/ocultar paneles y dinamismo
# tidyverse → Para manipulación cómoda de datos
# car/ agricolae → Tests estadísticos opcionales (ANOVA, etc.)
library(shiny)
library(shinyjs)
library(tidyverse)
library(car)
library(agricolae)
if (FALSE) {
  library(munsell)
}
############################ SISTEMA DE TEXTOS Y TRADUCCIÓN ############################
# showparams controla si se muestra el panel lateral de parámetros
showparams <- TRUE # Cambiarlo a FALSE si no se necesita barra de parámetros.
# Diccionario de textos (ES/EN/VAL)
texts <- list(
  # Cabecera
  title = c(
    ES = "GRÁFICOS DESCRIPTIVOS DE UNA MUESTRA",
    EN = "DESCRIPTIVE GRAPHS OF A SAMPLE",
    VAL = "GRÀFICS DESCRIPTIUS D'UNA MOSTRA"
  ),
  explanation = c(
    ES = "Genera una muestra con asimetría y se muestran: histograma, papel probabilístico normal, caja y bigotes, y una tabla de estadísticos.",
    EN = "Generate a sample with skewness and display: histogram, normal probability paper, box and whisker plot, and a table of statistics.",
    VAL = "Genera una mostra amb asimetria i es mostren: histograma, paper probabilístic normal, caixa i bigots, i una taula d'estadístics."
  ),
  userguide_btn = c(
    ES = "Guía de uso",
    EN = "User guide",
    VAL = "Guia d’ús"
  ),
  # Tabs
  panel1 = c(ES = "Gráficos y Parámetros", EN = "Plots & Parameters", VAL = "Gràfics i Paràmetres"),
  panel2 = c(ES = "Datos", EN = "Data", VAL = "Dades"),
  # Botón y panel lateral
  button_parameters = c(ES = "Parámetros", EN = "Parameters", VAL = "Paràmetres"),
  text_downmenu = c(ES = "Controles", EN = "Controls", VAL = "Controls"),
  slider1 = c(ES = "Asimetría", EN = "Skewness", VAL = "Asimetria"),
  slider2 = c(ES = "Tamaño de la muestra", EN = "Sample size", VAL = "Mida de la mostra"),
  slider3 = c(ES = "Nº de valores anómalos adicionales", EN = "# of additional anomalous values", VAL = "Nº de valors anòmals addicionals"),
  regen_btn = c(ES = "Generar nuevos datos", EN = "Generate new data", VAL = "Generar noves dades"),
  # Rótulos de gráficos
  plot = c(ES = "Gráfico", EN = "Plot", VAL = "Gràfic"),
  plot_x = c(ES = "Valores", EN = "Values", VAL = "Valors"),
  plot_y1 = c(ES = "Frecuencia", EN = "Frequency", VAL = "Freqüència"),
  plot_y = c(ES = "Cuantiles teóricos", EN = "Theoretical quantiles", VAL = "Quantils teòrics"),
  hist_title = c(ES = "Histograma", EN = "Histogram", VAL = "Histograma"),
  qq_title = c(ES = "Papel probabilístico normal", EN = "Normal probability plot", VAL = "Paper probabilístic normal"),
  box_title = c(ES = "Diagrama de caja y bigotes", EN = "Box-and-whisker plot", VAL = "Diagrama de caixa i bigots"),
  # Tabla de estadísticos
  sampleStats_title = c(ES = "Estadísticos muestrales:", EN = "Sample statistics:", VAL = "Estadístics de la mostra:"),
  table_header_stat = c(ES = "Estadístico", EN = "Statistic", VAL = "Estadístic"),
  table_header_value = c(ES = "Valor", EN = "Value", VAL = "Valor"),
  label_n = c(ES = "n", EN = "n", VAL = "n"),
  label_mean = c(ES = "Media", EN = "Mean", VAL = "Mitjana"),
  label_median = c(ES = "Mediana", EN = "Median", VAL = "Mediana"),
  label_sd = c(ES = "Desviación típica", EN = "Standard deviation", VAL = "Desviació típica"),
  label_iqr = c(ES = "Rango intercuartílico", EN = "Interquartile range", VAL = "Rang interquartílic"),
  label_skew = c(ES = "Asimetría", EN = "Skewness", VAL = "Asimetria"),
  label_kurt_excess = c(ES = "Apuntamiento", EN = "Kurtosis", VAL = "Curtosi"),
  # Créditos STATIO
  credits = c(
    ES = "STATIO es un Proyecto de Innovación y Mejora Educativa (PIME/25-26/562) desarrollado por el DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
    EN = "STATIO is an Educational Innovation and Improvement Project (PIME/25-26/562) developed by the DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
    VAL = "STATIO és un Projecte d'Innovació i Millora Educativa (PIME/25-26/562) desenrotllat pel DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>."
  ),
  # Rótulos para pestaña Datos
  data_index = c(ES = "Índice", EN = "Index", VAL = "Índex"),
  data_value = c(ES = "Valor", EN = "Value", VAL = "Valor")
  # *** NUEVO: Frase de la pestaña Guía ***
)
# Función de traducción: dado el ID y el idioma, devuelve el texto correcto. NO MODIFICAR.
tr <- function(id, lang) { texts[[id]][[lang]] }
rsn_manual <- function(n = 1, xi = 0, omega = 1, alpha = 0, tau = 0, dp = NULL) {
  
  if (!is.null(dp)) {
    if (!missing(alpha))
      stop("You cannot set both 'dp' and the component parameters")
    xi <- dp[1]
    omega <- dp[2]
    alpha <- dp[3]
    tau <- if (length(dp) > 3) dp[4] else 0
  }
  
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0) stop("'n' must be a non-negative number")
  n <- as.integer(n)
  if (!is.finite(omega) || omega <= 0) stop("'omega' must be > 0")
  
  delta <- alpha / sqrt(1 + alpha^2)
  
  if (tau == 0) {
    tn <- matrix(rnorm(2 * n), 2, n, byrow = FALSE)
    chi <- abs(tn[1, ])
    nrv <- tn[2, ]
    z <- delta * chi + sqrt(1 - delta^2) * nrv
  } else {
    truncN <- qnorm(runif(n, min = pnorm(-tau), max = 1))
    z <- delta * truncN + sqrt(1 - delta^2) * rnorm(n)
  }
  
  y <- as.vector(xi + omega * z)
  attr(y, "family") <- "SN"
  attr(y, "parameters") <- c(xi, omega, alpha, tau)
  y
}
##################### USER INTERFACE ######################################
ui <- fluidPage(
  useShinyjs(), # Activa funciones JS. NO MODIFICAR.
  # BOTONES PARA CAMBIAR DE IDIOMA EN LA PÁGINA. NO MODIFICARLOS.
  absolutePanel(
    top = 10, right = 10, fixed = TRUE,
    actionButton("lang_es", "ES"),
    actionButton("lang_en", "EN"),
    actionButton("lang_va", "VAL")
  ),
  # ------------------- CSS de la plantilla (sidebar oculto al inicio) -------------------
  tags$head(
    tags$style(HTML(" 
      #sidebarWrapper { 
        width: 300px; background: #f7f7f7; padding: 15px; border-right: 1px solid #ddd; 
        position: fixed; top: 0; bottom: 0; left: 0; overflow-y: auto; transition: transform .3s ease; z-index: 2000; 
        transform: translateX(-100%); /* oculto al inicio */ 
      } 
      #sidebarWrapper:not(.closed) { transform: translateX(0); } 
      #contentWrapper { transition: margin-left .3s ease; margin-left: 0px; } 
      #contentWrapper.shifted { margin-left: 300px; } 
      #toggleSidebar {
  position: static !important;
}
      .plot-panel{ background:#f2f2f2; border:1px solid #ddd; border-radius:6px; padding:6px; margin-bottom:14px; } 
      .table-panel{ background:#f2f2f2; border:1px solid #ddd; border-radius:6px; padding:8px; margin-bottom:14px; } 
      .table-panel table{ border-collapse:collapse; width:100%; font-size:1.05em; line-height:1.4; } 
      .table-panel th,.table-panel td{ border:1px solid #ccc; padding:8px; text-align:left; } 
      .table-panel th{ background:#f7f7f7; } 
      .credits-wrap{ margin-top:24px; text-align:center; margin-bottom:24px; } 
    "))
  ),
  # Botón que abre/cierra el panel lateral. NO MODIFICAR.
  if (showparams) {
    div(
      id = "topLeftButtons",
      style = "position:fixed; top:10px; left:10px; z-index:3000; display:flex; gap:10px;",
      
      actionButton("toggleSidebar", textOutput("button_parameters")),
      
      uiOutput("userguide_ui")
    )},
  # Panel lateral de parámetros
  div(id="sidebarWrapper", style="padding-top: 50px;",
      if (showparams) {
        div(
          h4(textOutput("text_downmenu")),
          uiOutput("slider1_ui"), uiOutput("slider2_ui"), uiOutput("slider3_ui"),
          actionButton("regen", label = textOutput("regen_label"), icon = icon("sync"))
        )
      }
  ),
  # CONTENIDO PRINCIPAL
  div(id="contentWrapper", class = "shifted",
      div(style="padding-top:50px; margin-bottom:30px",
          h2(textOutput("title"), align="center"),
          div(style="display:flex; justify-content:center;",
              div(style="border:2px solid #4a90e2; border-radius:12px; padding:12px; max-width:600px; background:white; text-align:center;",
                  uiOutput("explanation")
              )
          )
      ),
      # --------------------- TABS ---------------------
      tabsetPanel(
        # 1) Gráficos y Parámetros (en columna)
        tabPanel(textOutput("panel1_title"),
                 htmlOutput("plot_title"),
                 div(class = "plot-panel", plotOutput("histPlot", height = "340px")),
                 div(class = "plot-panel", plotOutput("boxPlot", height = "260px")),
                 div(class = "plot-panel", plotOutput("qqPlot", height = "260px")),
                 div(class = "table-panel", uiOutput("sampleStats")),
                 # Créditos al final de la pestaña
                 div(class = "credits-wrap",
                     div(style = "display:flex; justify-content:center; align-items:center; gap:40px;",
                         tags$img(src='UPV.png', style='height:85px; max-height:85px;'),
                         tags$img(src='DEIOAC.png', style='height:65px; max-height:70px;')
                     ),
                     div(style = "margin-top:15px;", htmlOutput("creditos"))
                 )
        ),
        # 3) Datos (tabla con los datos generados)
        tabPanel(textOutput("panel2_title"),
                 tableOutput("data"),
                 div(class = "credits-wrap",
                     div(style = "display:flex; justify-content:center; align-items:center; gap:40px;",
                         tags$img(src='UPV.png', style='height:85px; max-height:85px;'),
                         tags$img(src='DEIOAC.png', style='height:65px; max-height:70px;')
                     ),
                     div(style = "margin-top:15px;", htmlOutput("creditos"))
                 )
        )
      )
  )
)
############################ SERVER #######################################
server <- function(input, output, session) {
  # Toggle del panel lateral. NO MODIFICAR.
  observeEvent(input$toggleSidebar, {
    shinyjs::toggleClass(id = "sidebarWrapper", class = "closed")
    shinyjs::toggleClass(id = "contentWrapper", class = "shifted")
    shinyjs::runjs("setTimeout(function() { $(window).trigger('resize'); }, 350);")
  })
  # Idioma
  language <- reactiveVal("ES")
  observeEvent(input$lang_es, { language("ES") })
  observeEvent(input$lang_en, { language("EN") })
  observeEvent(input$lang_va, { language("VAL") })
  # Textos
  output$title <- renderText({ tr("title", language()) })
  output$explanation <- renderUI({ HTML(tr("explanation", language())) })
  output$button_parameters <- renderText({ tr("button_parameters", language()) })
  output$text_downmenu <- renderText({ tr("text_downmenu", language()) })
  output$plot_title <- renderUI({ HTML(paste0("<h3 style='font-size:22px; font-weight:bold'>", tr('plot', language()), "</h3>")) })
  output$regen_label <- renderText({ tr("regen_btn", language()) })
  output$panel1_title <- renderText({ tr("panel1", language()) })
  output$panel2_title <- renderText({ tr("panel2", language()) })
  output$creditos <- renderUI({ HTML(tr("credits", language())) })
  # *** NUEVO: Frase de la pestaña Guía ***
  output$userguide_ui <- renderUI({
    
    lang <- language()
    
    pdf_file <- switch(
      lang,
      ES  = "guia_ES.pdf",
      EN  = "guia_EN.pdf",
      VAL = "guia_VAL.pdf",
      "guia_ES.pdf"
    )
    
    tags$a(
      href = pdf_file,
      target = "_blank",
      class = "btn btn-outline-secondary btn-sm",
      tr("userguide_btn", lang)
    )
  })
  # Controles
  output$slider1_ui <- renderUI({ sliderInput("alpha", tr("slider1", language()), min=-10, max=10, value=0, step=1) })
  output$slider2_ui <- renderUI({ numericInput("n", tr("slider2", language()), value=100, min=10, max=500, step=1) })
  output$slider3_ui <- renderUI({ selectInput("anom", tr("slider3", language()), choices = 0:5, selected = 0) })
  # Generación de datos (con botón de regeneración)
  seed_val <- reactiveVal(123)
  observeEvent(input$regen, { seed_val(sample.int(.Machine$integer.max, 1)) })
  d <- reactive({
    req(input$n, input$alpha, input$anom)
    
    set.seed(seed_val())
    
    x <- rsn_manual(
      n = as.numeric(input$n),
      xi = 0,
      omega = 1,
      alpha = as.numeric(input$alpha)
    )
    
    anom_val <- as.integer(input$anom)
    
    if (!is.null(anom_val) && anom_val > 0) {
      Q1 <- quantile(x, 0.25)
      Q3 <- quantile(x, 0.75)
      IQR_val <- IQR(x)
      
      for (i in seq_len(anom_val)) {
        desplazamiento <- IQR_val * runif(1, 2.5, 4)
        valor_anomalo <- if (rbinom(1, 1, 0.5) == 1) Q3 + desplazamiento else Q1 - desplazamiento
        x <- c(x, valor_anomalo)
      }
    }
    
    x
  })
  # Gráficos apilados
  output$histPlot <- renderPlot({
    x <- d(); n <- length(x); nb <- ceiling(sqrt(n))
    op <- par(bg="#f2f2f2"); on.exit(par(op))
    hist(x, breaks=nb, main=tr("hist_title", language()), xlab=tr("plot_x", language()), ylab=tr("plot_y1", language()), col="lightgreen")
  })
  output$qqPlot <- renderPlot({
    x <- d(); n <- length(x)
    op <- par(bg="#f2f2f2"); on.exit(par(op))
    prob <- ppoints(n); teorico <- qnorm(prob); ordenados <- sort(x)
    plot(ordenados, teorico, main=tr("qq_title", language()), xlab=tr("plot_x", language()), ylab=tr("plot_y", language()))
    abline(lm(ordenados ~ teorico), col="red")
  })
  output$boxPlot <- renderPlot({
    x <- d(); op <- par(bg="#f2f2f2"); on.exit(par(op))
    boxplot(x, horizontal=TRUE, main=tr("box_title", language()), xlab=tr("plot_x", language()), col="lightgreen")
  })
  # Tabla de parámetros (estadísticos)
  output$sampleStats <- renderUI({
    x <- d(); n <- length(x)
    m <- mean(x); med <- median(x)
    sigma <- sqrt(mean((x - m)^2))
    sd_sample <- sd(x); iqr <- IQR(x)
    skew <- mean((x - m)^3) / (sigma^3)
    kurt_excess <- (mean((x - m)^4) / (sigma^4)) - 3
    fmt <- function(z) format(z, digits=4, decimal.mark=",")
    HTML(sprintf("<div>\n  <h5 style='margin:4px 0 8px 0;'>%s</h5>\n  <table>\n    <tr><th>%s</th><th>%s</th></tr>\n    <tr><td>%s</td><td>%s</td></tr>\n    <tr><td>%s</td><td>%s</td></tr>\n    <tr><td>%s</td><td>%s</td></tr>\n    <tr><td>%s</td><td>%s</td></tr>\n    <tr><td>%s</td><td>%s</td></tr>\n    <tr><td>%s</td><td>%s</td></tr>\n    <tr><td>%s</td><td>%s</td></tr>\n  </table>\n</div>",
                 tr("sampleStats_title", language()),
                 tr("table_header_stat", language()), tr("table_header_value", language()),
                 tr("label_n", language()), n,
                 tr("label_mean", language()), fmt(m),
                 tr("label_median", language()), fmt(med),
                 tr("label_sd", language()), fmt(sd_sample),
                 tr("label_iqr", language()), fmt(iqr),
                 tr("label_skew", language()), fmt(skew),
                 tr("label_kurt_excess", language()), fmt(kurt_excess)
    ))
  })
  # Datos (tabla)
  output$data <- renderTable({
    x <- d()
    df <- data.frame(Index = seq_along(x), Value = x)
    # Renombrado dinámico según idioma
    names(df) <- c(tr("data_index", language()), tr("data_value", language()))
    df
  }, digits = 4)
}
# Run app
shinyApp(ui, server)
