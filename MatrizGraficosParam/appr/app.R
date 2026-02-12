####### LIBRERÍAS (PUEDES AÑADIR, NO BORRAR) #######
library(shiny)
library(shinyjs)
library(tidyverse)
library(car)
library(agricolae)
if (FALSE) { library(munsell) }
library(sn)

######### SISTEMA DE TEXTOS Y TRADUCCIÓN #########
showparams <- TRUE
texts <- list(
  title = c(ES="GRÁFICOS DESCRIPTIVOS DE UNA MUESTRA", EN="DESCRIPTIVE GRAPHS OF A SAMPLE", VAL="GRÀFICS DESCRIPTIUS D'UNA MOSTRA"),
  explanation = c(ES="Genera una muestra con asimetría y se muestran: histograma, papel probabilístico normal, caja y bigotes, y una tabla de estadísticos.", EN="Generate a sample with skewness and display: histogram, normal probability paper, box and whisker plot, and a table of statistics.", VAL="Genera una mostra amb asimetria i es mostren: histograma, paper probabilístic normal, caixa i bigots, i una taula d'estadístics."),
  button_parameters = c(ES = "Parámetros", EN = "Parameters", VAL = "Paràmetres"),
  text_downmenu = c(ES = "Controles", EN = "Controls", VAL = "Controls"),
  slider1 = c(ES = "Asimetría (α)", EN = "Skewness (α)", VAL = "Asimetria (α)"),
  slider2 = c(ES = "Tamaño de la muestra", EN = "Sample size", VAL = "Mida de la mostra"),
  slider3 = c(ES = "Nº de valores anómalos adicionales", EN = "# of additional anomalous values", VAL = "Nº de valors anòmals addicionals"),
  regen_btn = c(ES = "Generar nuevos datos", EN = "Generate new data", VAL = "Generar noves dades"),
  plot = c(ES = "Gráfico", EN = "Plot", VAL = "Gràfic"),
  plot_x = c(ES = "Valores", EN = "Values", VAL = "Valors"),
  plot_y1 = c(ES = "Frecuencia", EN = "Frequency", VAL = "Freqüència"),
  plot_y = c(ES = "Cuantiles teóricos", EN = "Theoretical quantiles", VAL = "Quantils teòrics"),
  hist_title = c(ES = "Histograma", EN = "Histogram", VAL = "Histograma"),
  qq_title = c(ES = "Papel probabilístico normal", EN = "Normal probability plot", VAL = "Paper probabilístic normal"),
  box_title = c(ES = "Diagrama de caja y bigotes", EN = "Box-and-whisker plot", VAL = "Diagrama de caixa i bigots"),
  sampleStats_title = c(ES = "Estadísticos muestrales:", EN = "Sample statistics:", VAL = "Estadístics de la mostra:"),
  table_header_stat = c(ES = "Estadístico", EN = "Statistic", VAL = "Estadístic"),
  table_header_value = c(ES = "Valor", EN = "Value", VAL = "Valor"),
  label_n = c(ES = "n", EN = "n", VAL = "n"),
  label_mean = c(ES = "Media", EN = "Mean", VAL = "Mitjana"),
  label_median = c(ES = "Mediana", EN = "Median", VAL = "Mediana"),
  label_sd = c(ES = "Desviación típica", EN = "Standard deviation", VAL = "Desviació típica"),
  label_iqr = c(ES = "Rango intercuartílico", EN = "Interquartile range", VAL = "Rang interquartílic"),
  label_skew = c(ES = "Asimetría", EN = "Skewness", VAL = "Asimetria"),
  label_kurt_excess = c(ES = "Apuntamiento", EN = "Kurtosis", VAL = "Curtosi")
)
tr <- function(id, lang) { texts[[id]][[lang]] }

############# USER INTERFACE #############
ui <- fluidPage(
  useShinyjs(),
  absolutePanel(top = 10, right = 10, fixed = TRUE,
    actionButton("lang_es", "ES"), actionButton("lang_en", "EN"), actionButton("lang_va", "VAL")
  ),
  tags$head(tags$style(HTML(" 
    #sidebarWrapper{width:300px;background:#f7f7f7;padding:15px;border-right:1px solid #ddd;position:fixed;top:0;bottom:0;left:0;overflow-y:auto;transition:transform .3s ease;z-index:2000;}
    #sidebarWrapper.closed{transform:translateX(-100%);} 
    #contentWrapper{transition:margin-left .3s ease;margin-left:300px;} 
    #contentWrapper.shifted{margin-left:0px;} 
    #toggleSidebar{position:fixed;top:10px;left:10px;z-index:3000;}
    .plot-panel{background:#f2f2f2;border:1px solid #ddd;border-radius:6px;padding:6px;height:340px;}
    .plot-panel .shiny-plot-output{height:100% !important;}
    .table-panel{background:#f2f2f2;border:1px solid #ddd;border-radius:6px;padding:8px;}
    .table-panel table{border-collapse:collapse;width:100%;font-size:1.15em;line-height:1.6;}
    .table-panel th,.table-panel td{border:1px solid #ccc;padding:8px;text-align:left;}
    .table-panel th{background:#f7f7f7;}
  "))),
  if (showparams) { actionButton("toggleSidebar", textOutput("button_parameters")) },
  div(id = "sidebarWrapper", style = "padding-top:50px;",
      if (showparams) {
        div(h4(textOutput("text_downmenu")), uiOutput("slider1_ui"), uiOutput("slider2_ui"), uiOutput("slider3_ui"),
            actionButton("regen", label = textOutput("regen_label"), icon = icon("sync")))
      }
  ),
  # IMPORTANTE: TODO EL CONTENIDO QUE DEBE RESPETAR LA BARRA LATERAL
  # VA DENTRO DE contentWrapper
  div(id = "contentWrapper", style = "margin-left:300px;",
      div(style = "padding-top:50px; margin-bottom:30px",
          h2(textOutput("title"), align = "center"),
          div(style = "display:flex; justify-content:center;",
              div(style = "border:2px solid #4a90e2; border-radius:12px; padding:12px; max-width:600px; background:white; text-align:center;",
                  uiOutput("explanation")))) ,
      htmlOutput("plot_title"),
      fluidRow(
        column(6, div(class="plot-panel", plotOutput("histPlot", height = "100%"))),
        column(6, div(class="plot-panel", plotOutput("qqPlot", height = "100%")))
      ),
      fluidRow(
        column(6, div(class="plot-panel", plotOutput("boxPlot", height = "100%"))),
        column(6, div(class="table-panel", uiOutput("sampleStats")))
      ),
      div(style = "margin-top:30px; text-align:center; margin-bottom:30px;",
          div(style = "display:flex; justify-content:center; align-items:center; gap:40px;",
              tags$img(src='UPV.png', style='height:85px; max-height:85px;'),
              tags$img(src='DEIOAC.png', style='height:65px; max-height:70px;')
          ),
          div(style = "margin-top:15px;", htmlOutput("creditos"))
      )
  )
)

########### SERVER ###########
server <- function(input, output, session) {
  observeEvent(input$toggleSidebar, {
    shinyjs::toggleClass(id = "sidebarWrapper", class = "closed")
    shinyjs::toggleClass(id = "contentWrapper", class = "shifted")
    shinyjs::runjs("setTimeout(function(){ $(window).trigger('resize'); }, 350);")
  })

  language <- reactiveVal("ES")
  observeEvent(input$lang_es, { language("ES") })
  observeEvent(input$lang_en, { language("EN") })
  observeEvent(input$lang_va, { language("VAL") })
  output$title <- renderText({ tr("title", language()) })
  output$explanation <- renderUI({ HTML(tr("explanation", language())) })
  output$button_parameters <- renderText({ tr("button_parameters", language()) })
  output$text_downmenu <- renderText({ tr("text_downmenu", language()) })
  output$creditos <- renderUI({ HTML(tr("credits", language())) })
  output$plot_title <- renderUI({ HTML(paste0("<h3 style='font-size:22px; font-weight:bold'>", tr('plot', language()), "</h3>")) })
  output$regen_label <- renderText({ tr("regen_btn", language()) })

  output$slider1_ui <- renderUI({ sliderInput("alpha", tr("slider1", language()), min=-10, max=10, value=0, step=1) })
  output$slider2_ui <- renderUI({ numericInput("n", tr("slider2", language()), value=100, min=10, max=500, step=1) })
  output$slider3_ui <- renderUI({ selectInput("anom", tr("slider3", language()), choices = 0:5, selected = 0) })

  seed_val <- reactiveVal(123)
  observeEvent(input$regen, { seed_val(sample.int(.Machine$integer.max, 1)) })

  d <- reactive({
    set.seed(seed_val())
    x <- rsn(input$n, xi=0, omega=1, alpha=input$alpha)
    anom_val <- if (is.null(input$anom)) 0 else as.integer(input$anom)
    if (!is.null(anom_val) && anom_val > 0) {
      Q1 <- quantile(x, 0.25); Q3 <- quantile(x, 0.75); IQR_val <- IQR(x)
      for (i in seq_len(anom_val)) {
        desplazamiento <- IQR_val * runif(1, 2.5, 4)
        valor_anomalo <- if (rbinom(1, 1, 0.5) == 1) Q3 + desplazamiento else Q1 - desplazamiento
        x <- c(x, valor_anomalo)
      }
    }
    x
  })

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

  output$sampleStats <- renderUI({
    x <- d(); n <- length(x)
    m <- mean(x); med <- median(x)
    sigma <- sqrt(mean((x - m)^2))
    sd_sample <- sd(x); iqr <- IQR(x)
    skew <- mean((x - m)^3) / (sigma^3)
    kurt_excess <- (mean((x - m)^4) / (sigma^4)) - 3
    fmt <- function(z) format(z, digits=4, decimal.mark=",")
    HTML(sprintf("<div>\n <h5 style='margin:4px 0 8px 0;'>%s</h5>\n <table>\n <tr><th>%s</th><th>%s</th></tr>\n <tr><td>%s</td><td>%s</td></tr>\n <tr><td>%s</td><td>%s</td></tr>\n <tr><td>%s</td><td>%s</td></tr>\n <tr><td>%s</td><td>%s</td></tr>\n <tr><td>%s</td><td>%s</td></tr>\n <tr><td>%s</td><td>%s</td></tr>\n <tr><td>%s</td><td>%s</td></tr>\n </table>\n <div style='font-size:0.95em; color:#555; margin-top:6px;'>\n</div>\n</div>",
      tr("sampleStats_title", language()),
      tr("table_header_stat", language()), tr("table_header_value", language()),
      tr("label_n", language()), n,
      tr("label_mean", language()), fmt(m),
      tr("label_median", language()), fmt(med),
      tr("label_sd", language()), fmt(sd_sample),
      tr("label_iqr", language()), fmt(iqr),
      tr("label_skew", language()), fmt(skew),
      tr("label_kurt_excess", language()), fmt(kurt_excess)))
  })
}

shinyApp(ui, server)
