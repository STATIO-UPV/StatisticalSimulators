###### LIBRERÍAS (PUEDES AÑADIR, NO BORRAR) ###################################
# shiny → Construcción de la interfaz y del servidor
# shinyjs → Permite usar JS para mostrar/ocultar paneles y dinamismo
# tidyverse → Para manipulación cómoda de datos
# car/ agricolae → Tests estadísticos opcionales (ANOVA, etc.)
library(shiny)
library(shinyjs)
library(tidyverse)
library(car)
library(agricolae)
library(stringr)   # para manejo de mayúsculas/minúsculas de rótulos
if (FALSE) {
  library(munsell)
}

################ SISTEMA DE TEXTOS Y TRADUCCIÓN ##################################
# showparams controla si se muestra el panel lateral de parámetros
showparams <- TRUE # Cambiarlo a FALSE si no se necesita barra de parámetros.

# Lista de textos en cada idioma.
texts <- list(
  title = c(
    ES = "PAPEL PROBABILÍSTICO NORMAL CON MEZCLA DE POBLACIONES",
    EN = "NORMAL PROBABILITY PLOT WITH MIXTURE OF POPULATIONS",
    VAL = "PAPER PROBABILÍSTIC NORMAL AMB MESCLA DE POBLACIONS"
  ),
  explanation = c(
    ES = "Genera y visualiza una mezcla de 1 a 3 poblaciones normales en un papel probabilístico normal.",
    EN = "Generate and visualize a mixture of 1 to 3 normal populations on a normal probability plot.",
    VAL = "Genera i visualitza una mescla d'1 a 3 poblacions normals en un paper probabilístic normal."
  ),
  button_parameters = c(ES = "Parámetros", EN = "Parameters", VAL = "Paràmetres"),
  text_downmenu = c(ES = "Configuración de la mezcla", EN = "Mixture configuration", VAL = "Configuració de la mescla"),
  dropdown_label = c(ES = "Número de muestras", EN = "Number of samples", VAL = "Nombre de mostres"),
  option1 = c(ES = "1 muestra", EN = "1 sample", VAL = "1 mostra"),
  option2 = c(ES = "2 muestras", EN = "2 samples", VAL = "2 mostres"),
  option3 = c(ES = "3 muestras", EN = "3 samples", VAL = "3 mostres"),
  plot = c(ES = "Gráfico", EN = "Plot", VAL = "Gràfic"),
  plot_x = c(ES = "Datos", EN = "Data", VAL = "Dades"),
  plot_y = c(ES = "Cuantiles teóricos", EN = "Theoretical quantiles", VAL = "Quantils teòrics"),
  base_params = c(ES = "Parámetros de la muestra 1", EN = "Sample 1 parameters", VAL = "Paràmetres de la mostra 1"),
  mean_label = c(ES = "Media", EN = "Mean", VAL = "Mitjana"),
  sd_label = c(ES = "Desviación típica", EN = "Std. deviation", VAL = "Desviació típica"),
  n_label = c(ES = "Tamaño de muestra", EN = "Sample size", VAL = "Mida de mostra"),
  sample_label = c(ES = "muestra", EN = "Sample", VAL = "mostra"),
  sampleStats_title = c(
    ES = "Estadísticos por muestra (medias y desviaciones típicas):",
    EN = "Sample statistics (means and standard deviations):",
    VAL = "Estadístics per mostra (mitjanes i desviacions típiques):"
  ),
  credits= c(
    ES= "STATIO es un Proyecto de Innovación y Mejora Educativa (PIME/25-26/562) desarrollado por el DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
    EN= "STATIO is an Educational Innovation and Improvement Project (PIME/25-26/562) developed by the DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
    VAL= "STATIO és un Projecte d'Innovació i Millora Educativa (*PIME/25-26/562) desenrotllat pel DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>."
  )
)

# Función de traducción
tr <- function(id, lang) { texts[[id]][[lang]] }

############################# USER INTERFACE #####################################
ui <- fluidPage(
  useShinyjs(),

  # BOTONES DE IDIOMA
  absolutePanel(top = 10, right = 10, fixed = TRUE,
    actionButton("lang_es", "ES"),
    actionButton("lang_en", "EN"),
    actionButton("lang_va", "VAL")
  ),

  # CSS
  tags$head(tags$style(HTML(
    "#sidebarWrapper { width: 300px; background: #f7f7f7; padding: 15px; border-right: 1px solid #ddd; position: fixed; top: 0; bottom: 0; left: 0; overflow-y: auto; transition: transform .3s ease; z-index: 2000; transform: translateX(-100%);} \
    #sidebarWrapper:not(.closed){ transform: translateX(0);} \
    #contentWrapper { transition: margin-left .3s ease; margin-left: 0px;} \
    #contentWrapper.shifted { margin-left: 300px;} \
    #toggleSidebar { position: fixed; top: 10px; left: 10px; z-index: 3000;} "
  ))),
  tags$head(tags$style(HTML(".param-line{display:flex;align-items:center;gap:8px;margin-bottom:8px;} .param-line span{font-weight:bold;}"))),

  # Botón que abre/cierra el panel lateral
  if(showparams) {
    actionButton("toggleSidebar", textOutput("button_parameters"))
  },

  # LATERAL
  div(id = "sidebarWrapper", style = "padding-top:50px;",
      div(
        h4(textOutput("text_downmenu")),
        uiOutput("dropdown_ui"),
        h4(textOutput("base_params_title")),
        uiOutput("base_params_ui"),
        conditionalPanel(
          condition = "input.server_id == 'opt2' || input.server_id == 'opt3'",
          # TÍTULO LOCALIZADO PARA MUESTRA 2
          h4(textOutput("sample2_title")),
          uiOutput("sample2_ui")
        ),
        conditionalPanel(
          condition = "input.server_id == 'opt3'",
          # TÍTULO LOCALIZADO PARA MUESTRA 3
          h4(textOutput("sample3_title")),
          uiOutput("sample3_ui")
        )
      )
  ),

  # CONTENIDO PRINCIPAL
  div(id = "contentWrapper", class = "shifted",
      div(style = "padding-top:50px; margin-bottom:30px",
          h2(textOutput("title"), align = "center"),
          div(style = "display:flex; justify-content:center;",
              div(style = "border:2px solid #4a90e2; border-radius:12px; padding:12px; max-width:700px; background:white; text-align:center;",
                  uiOutput("explanation")
              )
          )
      ),
      div(
        htmlOutput("plot_title"),
        plotOutput("normalPlot", height = "520px"),
        uiOutput("sampleStats")
      ),
      div(
        style = "margin-top:40px; text-align:center; margin-bottom:40px;",
        div(
          style = "display:flex; justify-content:center; align-items:center; gap:40px;",
          tags$img(src = 'UPV.png',   style = "height:85px; max-height:85px;"),
          tags$img(src = 'DEIOAC.png',style = "height:65px; max-height:70px;")
        ),
        div(style = "margin-top:15px;", htmlOutput("creditos"))
      )
  )
)

# =============================== SERVER ===============================
server <- function(input, output) {
  # Barra lateral
  observeEvent(input$toggleSidebar, {
    shinyjs::toggleClass(id = "sidebarWrapper", class = "closed")
    shinyjs::toggleClass(id = "contentWrapper", class = "shifted")
    shinyjs::runjs("setTimeout(function(){ $(window).trigger('resize'); }, 350);")
  })

  # Idioma
  language <- reactiveVal("ES")
  observeEvent(input$lang_es, { language("ES") })
  observeEvent(input$lang_en, { language("EN") })
  observeEvent(input$lang_va, { language("VAL") })

  # Textos
  output$title             <- renderText({ tr("title", language()) })
  output$explanation       <- renderUI({ HTML(tr("explanation", language())) })
  output$button_parameters <- renderText({ tr("button_parameters", language()) })
  output$text_downmenu     <- renderText({ tr("text_downmenu", language()) })
  output$creditos          <- renderUI({ HTML(tr("credits", language())) })
  output$plot_title        <- renderUI({ HTML(paste0("<h3 style='font-size:22px; font-weight:bold'>",
                                                   tr('plot', language()),"</h3>")) })
  output$base_params_title <- renderText({ tr("base_params", language()) })

  # **TÍTULOS LOCALIZADOS DE LOS PANELES CONDICIONALES**
  output$sample2_title <- renderText({ paste(str_to_title(tr("sample_label", language())), "2") })
  output$sample3_title <- renderText({ paste(str_to_title(tr("sample_label", language())), "3") })

  # UI dinámico
  output$dropdown_ui <- renderUI({
    selectInput("server_id", tr("dropdown_label", language()),
               choices = setNames(c("opt1", "opt2", "opt3"),
                                 c(tr("option1", language()), tr("option2", language()), tr("option3", language())))
    )
  })

  output$base_params_ui <- renderUI({
    tagList(
      numericInput("m1", paste(tr("mean_label", language()), "1:"), value = 0, width = "140px"),
      numericInput("s1", paste(tr("sd_label", language()),   "1:"), value = 1, min = 0.0001, step = 0.1, width = "140px"),
      numericInput("n1", paste(tr("n_label", language()),    "1:"), value = 50, min = 1, step = 1, width = "140px")
    )
  })

  # Parámetros derivados
  output$sample2_ui <- renderUI({
    tagList(
      div(class="param-line", span(tr("mean_label", language())), selectInput("media_sel_2", NULL, choices = 1:5, selected = 1, width = "80px"), span("* s1 →"), textOutput("media_calc_2", inline = TRUE)),
      div(class="param-line", span(tr("sd_label", language())),   selectInput("sd_sel_2", NULL, choices = 1:3, selected = 1, width = "80px"),  span("* s1 →"), textOutput("sd_calc_2", inline = TRUE)),
      div(class="param-line", span(tr("n_label", language())),    span("n1/"), selectInput("n_sel_2", NULL, choices = 1:3, selected = 1, width = "80px"), span("→"), textOutput("n_calc_2", inline = TRUE))
    )
  })

  output$sample3_ui <- renderUI({
    tagList(
      div(class="param-line", span(tr("mean_label", language())), selectInput("media_sel_3", NULL, choices = 1:5, selected = 2, width = "80px"), span("* s1 →"), textOutput("media_calc_3", inline = TRUE)),
      div(class="param-line", span(tr("sd_label", language())),   selectInput("sd_sel_3", NULL, choices = 1:3, selected = 2, width = "80px"),  span("* s1 →"), textOutput("sd_calc_3", inline = TRUE)),
      div(class="param-line", span(tr("n_label", language())),    span("n1/"), selectInput("n_sel_3", NULL, choices = 1:3, selected = 2, width = "80px"), span("→"), textOutput("n_calc_3", inline = TRUE))
    )
  })

  observe({
    # Derivados muestra 2
    output$media_calc_2 <- renderText({ m1 <- input$m1; s1 <- input$s1; sel <- as.numeric(input$media_sel_2); round(m1 + sel * s1 * 2, 4) })
    output$sd_calc_2    <- renderText({ s1 <- input$s1; sel <- as.numeric(input$sd_sel_2); round(s1 * sel, 4) })
    output$n_calc_2     <- renderText({ n1 <- input$n1; sel <- as.numeric(input$n_sel_2); floor(n1/sel) })

    # Derivados muestra 3
    output$media_calc_3 <- renderText({ m1 <- input$m1; s1 <- input$s1; sel <- as.numeric(input$media_sel_3); round(m1 + sel * s1 * 2, 4) })
    output$sd_calc_3    <- renderText({ s1 <- input$s1; sel <- as.numeric(input$sd_sel_3); round(s1 * sel, 4) })
    output$n_calc_3     <- renderText({ n1 <- input$n1; sel <- as.numeric(input$n_sel_3); floor(n1/sel) })
  })

  # Datos simulados (1–3 muestras) — usando etiquetas de leyenda localizadas
  sample_label_for <- function(i, lang) { paste(str_to_title(tr("sample_label", lang)), i) }

  d <- reactive({
    req(input$m1, input$s1, input$n1)
    set.seed(123)
    m1 <- input$m1; s1 <- input$s1; n1 <- input$n1
    lang <- language()

    lab1 <- sample_label_for(1, lang)
    datos <- tibble(valor = rnorm(n1, m1, s1), muestra = lab1)

    if (input$server_id %in% c("opt2", "opt3")) {
      media2 <- m1 + as.numeric(input$media_sel_2) * s1 * 2
      sd2    <- s1 * as.numeric(input$sd_sel_2)
      n2     <- floor(n1 / as.numeric(input$n_sel_2))
      lab2   <- sample_label_for(2, lang)
      datos  <- bind_rows(datos, tibble(valor = rnorm(n2, media2, sd2), muestra = lab2))
    }
    if (input$server_id == "opt3") {
      media3 <- m1 + as.numeric(input$media_sel_3) * s1 * 2
      sd3    <- s1 * as.numeric(input$sd_sel_3)
      n3     <- floor(n1 / as.numeric(input$n_sel_3))
      lab3   <- sample_label_for(3, lang)
      datos  <- bind_rows(datos, tibble(valor = rnorm(n3, media3, sd3), muestra = lab3))
    }

    levels_vec <- c(sample_label_for(1, lang))
    if (input$server_id %in% c("opt2", "opt3")) levels_vec <- c(levels_vec, sample_label_for(2, lang))
    if (input$server_id == "opt3")              levels_vec <- c(levels_vec, sample_label_for(3, lang))
    datos$muestra <- factor(datos$muestra, levels = levels_vec)
    datos
  })

  # Gráfico
  output$normalPlot <- renderPlot({
    datos <- d() %>% arrange(valor)
    n     <- nrow(datos)
    prob  <- (seq_len(n) - 0.5) / n
    datos$q <- qnorm(prob)

    ggplot(datos, aes(x = valor, y = q, color = muestra)) +
      geom_point(size = 2, alpha = 0.85) +
      labs(
        x = tr("plot_x", language()),
        y = tr("plot_y", language()),
        color = str_to_title(tr("sample_label", language()))
      ) +
      theme_minimal() +
      theme(legend.position = "top")
  })
}

# Create Shiny app ----
shinyApp(ui, server)
