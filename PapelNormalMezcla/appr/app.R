############################################################
# PAPEL NORMAL (MEZCLA) — Tabs (ES/EN/VAL), ASCII-safe, balanced braces
############################################################

library(shiny)
library(shinyjs)
library(tidyverse)
library(car)
library(agricolae)
library(stringr)

showparams <- TRUE

texts <- list(
  title = c(
    ES = "PAPEL PROBABILISTICO NORMAL CON MEZCLA DE POBLACIONES",
    EN = "NORMAL PROBABILITY PLOT WITH MIXTURE OF POPULATIONS",
    VAL = "PAPER PROBABILISTIC NORMAL AMB MESCLA DE POBLACIONS"
  ),
  userguide_btn = c(
    ES = "Guía de uso",
    EN = "User guide",
    VAL = "Guia d’ús"
  ),
  explanation = c(
    ES = "Genera y visualiza una mezcla de 1 a 3 poblaciones normales en un papel probabilistico normal.",
    EN = "Generate and visualize a mixture of 1 to 3 normal populations on a normal probability plot.",
    VAL = "Genera i visualitza una mescla d'1 a 3 poblacions normals en un paper probabilistic normal."
  ),
  button_parameters = c(ES = "Parametros", EN = "Parameters", VAL = "Parametres"),
  text_downmenu = c(ES = "Configuracion de la mezcla", EN = "Mixture configuration", VAL = "Configuracio de la mescla"),
  dropdown_label = c(ES = "Numero de muestras", EN = "Number of samples", VAL = "Nombre de mostres"),
  option1 = c(ES = "1 muestra", EN = "1 sample", VAL = "1 mostra"),
  option2 = c(ES = "2 muestras", EN = "2 samples", VAL = "2 mostres"),
  option3 = c(ES = "3 muestras", EN = "3 samples", VAL = "3 mostres"),
  base_params = c(ES = "Parametros de la muestra 1", EN = "Sample 1 parameters", VAL = "Parametres de la mostra 1"),
  mean_label = c(ES = "Media", EN = "Mean", VAL = "Mitjana"),
  sd_label = c(ES = "Desviacion tipica", EN = "Std. deviation", VAL = "Desviacio tipica"),
  n_label = c(ES = "Tamano de muestra", EN = "Sample size", VAL = "Mida de mostra"),
  sample_label = c(ES = "muestra", EN = "sample", VAL = "mostra"),
  sampleStats_title = c(
    ES = "Estadisticos por muestra (medias y desviaciones tipicas):",
    EN = "Sample statistics (means and standard deviations):",
    VAL = "Estadistics per mostra (mitjanes i desviacions tipiques):"
  ),
  plot = c(ES = "Grafico", EN = "Plot", VAL = "Grafic"),
  plot_x = c(ES = "Datos", EN = "Data", VAL = "Dades"),
  plot_y = c(ES = "Cuantiles teoricos", EN = "Theoretical quantiles", VAL = "Quantils teorics"),
  credits= c(
    ES= "STATIO es un Proyecto de Innovacion y Mejora Educativa (PIME/25-26/562) desarrollado por el DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
    EN= "STATIO is an Educational Innovation and Improvement Project (PIME/25-26/562) developed by the DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
    VAL= "STATIO es un Projecte d'Innovacio i Millora Educativa (PIME/25-26/562) desenrotllat pel DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>."
  ),
  tab1 = c(ES = "Papel Normal", EN = "Normal Paper", VAL = "Paper Normal"),
  tab2 = c(ES = "Datos", EN = "Data", VAL = "Dades"),
  table_caption = c(ES = "Datos simulados", EN = "Simulated data", VAL = "Dades simulades"),
  table_col_value = c(ES = "Valor", EN = "Value", VAL = "Valor"),
  table_col_sample = c(ES = "Muestra", EN = "Sample", VAL = "Mostra")
)

tr <- function(id, lang) { texts[[id]][[lang]] }

ui <- fluidPage(
  useShinyjs(),
  absolutePanel(top = 10, right = 10, fixed = TRUE,
                actionButton("lang_es", "ES"),
                actionButton("lang_en", "EN"),
                actionButton("lang_va", "VAL")
  ),
  tags$head(tags$style(HTML(
    "#sidebarWrapper { width: 300px; background: #f7f7f7; padding: 15px; border-right: 1px solid #ddd; position: fixed; top: 0; bottom: 0; left: 0; overflow-y: auto; transition: transform .3s ease; z-index: 2000; transform: translateX(-100%);}\n#sidebarWrapper:not(.closed){ transform: translateX(0);}\n#contentWrapper { transition: margin-left .3s ease; margin-left: 0px;}\n#contentWrapper.shifted { margin-left: 300px;}\n#toggleSidebar {
  position: static !important;}"
  ))),
  if (showparams) {
    div(
      id = "topLeftButtons",
      style = "position:fixed; top:10px; left:10px; z-index:3000; display:flex; gap:10px;",
      
      actionButton("toggleSidebar", textOutput("button_parameters")),
      
      uiOutput("userguide_ui")
    )},
  
  div(id = "sidebarWrapper", style = "padding-top:50px;",
      div(
        h4(textOutput("text_downmenu")),
        uiOutput("dropdown_ui"),
        h4(textOutput("base_params_title")),
        uiOutput("base_params_ui"),
        conditionalPanel(
          condition = "input.server_id == 'opt2' || input.server_id == 'opt3'",
          h4(textOutput("sample2_title")),
          uiOutput("sample2_ui")
        ),
        conditionalPanel(
          condition = "input.server_id == 'opt3'",
          h4(textOutput("sample3_title")),
          uiOutput("sample3_ui")
        )
      )
  ),
  div(id = "contentWrapper", class = "shifted",
      div(style = "padding-top:50px; margin-bottom:30px",
          h2(textOutput("title"), align = "center"),
          div(style = "display:flex; justify-content:center;",
              div(style = "border:2px solid #4a90e2; border-radius:12px; padding:12px; max-width:700px; background:white; text-align:center;",
                  uiOutput("explanation")
              )
          )
      ),
      tabsetPanel(
        tabPanel(textOutput("tab1_title"),
                 htmlOutput("plot_title"),
                 plotOutput("normalPlot", height = "520px"),
                 uiOutput("sampleStats")
        ),
        tabPanel(textOutput("tab2_title"),
                 div(style = "padding: 10px;",
                     h4(textOutput("table_caption_out")),
                     tableOutput("datos_tabla")
                 )
        )
      ),
      div(style = "margin-top:40px; text-align:center; margin-bottom:40px;",
          div(style = "display:flex; justify-content:center; align-items:center; gap:40px;",
              tags$img(src = 'UPV.png', style = "height:85px; max-height:85px;"),
              tags$img(src = 'DEIOAC.png', style = "height:65px; max-height:70px;")
          ),
          div(style = "margin-top:15px;", htmlOutput("creditos"))
      )
  )
)

server <- function(input, output) {
  
  observeEvent(input$toggleSidebar, {
    shinyjs::toggleClass(id = "sidebarWrapper", class = "closed")
    shinyjs::toggleClass(id = "contentWrapper", class = "shifted")
    shinyjs::runjs("setTimeout(function(){ $(window).trigger('resize'); }, 350);")
  })
  
  language <- reactiveVal("ES")
  observeEvent(input$lang_es, { language("ES") })
  observeEvent(input$lang_en, { language("EN") })
  observeEvent(input$lang_va, { language("VAL") })
  
  output$title             <- renderText({ tr("title", language()) })
  output$explanation       <- renderUI({ HTML(tr("explanation", language())) })
  output$button_parameters <- renderText({ tr("button_parameters", language()) })
  output$text_downmenu     <- renderText({ tr("text_downmenu", language()) })
  output$creditos          <- renderUI({ HTML(tr("credits", language())) })
  output$plot_title        <- renderUI({ HTML(paste0("<h3 style='font-size:22px; font-weight:bold'>", tr('plot', language()),"</h3>")) })
  output$base_params_title <- renderText({ tr("base_params", language()) })
  
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
  output$tab1_title <- renderText({ tr("tab1", language()) })
  output$tab2_title <- renderText({ tr("tab2", language()) })
  
  output$table_caption_out <- renderText({ tr("table_caption", language()) })
  
  output$sample2_title <- renderText({ paste(str_to_title(tr("sample_label", language())), "2") })
  output$sample3_title <- renderText({ paste(str_to_title(tr("sample_label", language())), "3") })
  
  output$dropdown_ui <- renderUI({
    selectInput(
      inputId = "server_id",
      label   = tr("dropdown_label", language()),
      choices = setNames(
        c("opt1", "opt2", "opt3"),
        c(tr("option1", language()), tr("option2", language()), tr("option3", language()))
      )
    )
  })
  
  output$base_params_ui <- renderUI({
    tagList(
      numericInput("m1", paste(tr("mean_label", language()), "1:"), value = 0, width = "140px"),
      numericInput("s1", paste(tr("sd_label", language()), "1:"), value = 1, min = 0.0001, step = 0.1, width = "140px"),
      numericInput("n1", paste(tr("n_label", language()), "1:"), value = 50, min = 1, step = 1, width = "140px")
    )
  })
  
  output$sample2_ui <- renderUI({
    tagList(
      div(class = "param-line",
          span(tr("mean_label", language())),
          selectInput("media_sel_2", NULL, choices = 1:5, selected = 1, width = "80px"),
          span("* s1 ->"),
          textOutput("media_calc_2", inline = TRUE)
      ),
      div(class = "param-line",
          span(tr("sd_label", language())),
          selectInput("sd_sel_2", NULL, choices = 1:3, selected = 1, width = "80px"),
          span("* s1 ->"),
          textOutput("sd_calc_2", inline = TRUE)
      ),
      div(class = "param-line",
          span(tr("n_label", language())),
          span("n1/"),
          selectInput("n_sel_2", NULL, choices = 1:3, selected = 1, width = "80px"),
          span("->"),
          textOutput("n_calc_2", inline = TRUE)
      )
    )
  })
  
  output$sample3_ui <- renderUI({
    tagList(
      div(class = "param-line",
          span(tr("mean_label", language())),
          selectInput("media_sel_3", NULL, choices = 1:5, selected = 2, width = "80px"),
          span("* s1 ->"),
          textOutput("media_calc_3", inline = TRUE)
      ),
      div(class = "param-line",
          span(tr("sd_label", language())),
          selectInput("sd_sel_3", NULL, choices = 1:3, selected = 2, width = "80px"),
          span("* s1 ->"),
          textOutput("sd_calc_3", inline = TRUE)
      ),
      div(class = "param-line",
          span(tr("n_label", language())),
          span("n1/"),
          selectInput("n_sel_3", NULL, choices = 1:3, selected = 2, width = "80px"),
          span("->"),
          textOutput("n_calc_3", inline = TRUE)
      )
    )
  })
  
  observe({
    output$media_calc_2 <- renderText({
      m1 <- input$m1; s1 <- input$s1; sel <- as.numeric(input$media_sel_2)
      round(m1 + sel * s1 * 2, 4)
    })
    output$sd_calc_2 <- renderText({
      s1 <- input$s1; sel <- as.numeric(input$sd_sel_2)
      round(s1 * sel, 4)
    })
    output$n_calc_2 <- renderText({
      n1 <- input$n1; sel <- as.numeric(input$n_sel_2)
      floor(n1/sel)
    })
    
    output$media_calc_3 <- renderText({
      m1 <- input$m1; s1 <- input$s1; sel <- as.numeric(input$media_sel_3)
      round(m1 + sel * s1 * 2, 4)
    })
    output$sd_calc_3 <- renderText({
      s1 <- input$s1; sel <- as.numeric(input$sd_sel_3)
      round(s1 * sel, 4)
    })
    output$n_calc_3 <- renderText({
      n1 <- input$n1; sel <- as.numeric(input$n_sel_3)
      floor(n1/sel)
    })
  })
  
  sample_label_for <- function(i, lang) {
    paste(str_to_title(tr("sample_label", lang)), i)
  }
  
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
    if (input$server_id == "opt3")            levels_vec <- c(levels_vec, sample_label_for(3, lang))
    datos$muestra <- factor(datos$muestra, levels = levels_vec)
    datos
  })
  
  output$normalPlot <- renderPlot({
    datos <- d() %>% arrange(valor)
    n <- nrow(datos)
    prob <- (seq_len(n) - 0.5) / n
    datos$q <- qnorm(prob)
    
    ggplot(datos, aes(x = valor, y = q, color = muestra)) +
      geom_point(size = 2, alpha = 0.85) +
      labs(x = tr("plot_x", language()), y = tr("plot_y", language()), color = str_to_title(tr("sample_label", language()))) +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$sampleStats <- renderUI({
    datos <- d()
    stats <- datos %>% group_by(muestra) %>% summarise(media = mean(valor), sd = sd(valor), .groups = 'drop')
    txt <- paste(
      sprintf("%s ", tr("sampleStats_title", language())),
      paste(paste0(levels(datos$muestra), ": ", round(stats$media, 3), " +/- ", round(stats$sd, 3)), collapse = " | ")
    )
    HTML(paste0("<div style='margin-top:12px;'><i>", txt, "</i></div>"))
  })
  
  output$datos_tabla <- renderTable({
    df <- d()
    lang <- language()
    setNames(df, c(tr("table_col_value", lang), tr("table_col_sample", lang)))
  }, rownames = FALSE)
}

shinyApp(ui, server)
