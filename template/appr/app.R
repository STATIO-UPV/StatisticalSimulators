######################### LIBRERÍAS (PUEDES AÑADIR, NO BORRAR) #########################
# shiny       → Construcción de la interfaz y del servidor
# shinyjs     → Permite usar JS para mostrar/ocultar paneles y dinamismo
# tidyverse   → Para manipulación cómoda de datos
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


# Lista de textos en cada idioma.
# Cada entrada tiene ES / EN / VAL.
# Añadir tantas traducciones al dict como sean necesarias. 

texts <- list( 
  title = c( 
    ES = "Título de tu aplicación", 
    EN = "Application Title", 
    VAL = "Títol de la teua aplicació" 
    ), 
  explanation = c( 
    ES = "Breve explicación de la aplicación.", 
    EN = "Short explanation of the application.", 
    VAL = "Breu explicació de l'aplicació." 
    ), 
  button_parameters = c( 
    ES = "Parámetros", 
    EN = "Parameters", 
    VAL = "Paràmetres" 
    ), 
  text_downmenu= c( 
    ES = "1. Tu menú desplegable", 
    EN= "2. Your dropdown menu", 
    VAL= "3. El teu menú desplegable" 
    ), 
  dropdown_label = c( 
    ES = "Título del menú desplegable", 
    EN = "Title of drop-down menu", 
    VAL = "Títol del menú desplegable" 
    ), 
  option1 = c( 
    ES = "OPCIÓN 1", 
    EN = "OPTION 1", 
    VAL = "OPCIÓ 1" 
    ), 
  option2 = c( 
    ES = "OPCIÓN 2", 
    EN = "OPTION 2", 
    VAL = "OPCIÓ 2" 
    ), 
  slider1= c( 
    ES = "BARRA DESLIZANTE 1", 
    EN= "SLIDER 1", 
    VAL= "Barra lliscant 1" 
    ), 
  slider2= c( 
    ES = "BARRA DESLIZANTE 2", 
    EN= "SLIDER 2", 
    VAL= "Barra lliscant 2" 
    ), 
  slider3= c( 
    ES = "BARRA DESLIZANTE 3", 
    EN= "SLIDER 3", 
    VAL= "Barra lliscant 3" 
    ), 
  slider4= c( 
    ES = "BARRA DESLIZANTE 4", 
    EN= "SLIDER 4", 
    VAL= "Barra lliscant 4" 
    ), 
  slider5= c( 
    ES = "BARRA DESLIZANTE 5", 
    EN= "SLIDER 5", 
    VAL= "Barra lliscant 5" 
    ), 
  slider6= c( 
    ES = "BARRA DESLIZANTE 6", 
    EN= "SLIDER 6", 
    VAL= "Barra lliscant 6" 
    ), 
  panel1 = c( 
    ES = "Panel 1", 
    EN = "Panel 1", 
    VAL = "Panell 1" ), 
  panel2 = c( 
    ES = "Panel 2", 
    EN = "Panel 2", 
    VAL = "Panell 2" 
    ), 
  panel3 = c( 
    ES = "Panel 3", 
    EN = "Panel 3", 
    VAL = "Panell 3" 
    ), 
  plot= c(
    ES= "Gráfico",
    EN= "Plot", 
    VAL= "Gràfic"
    ),
  plot_x= c(
    ES= "Valor_X",
    EN= "X_value",
    VAL= "Valor X"
  ),
  plot_y= c(
    ES= "Valor_y",
    EN= "y_value",
    VAL= "Valor_y"
  ),
  sampleStats_title = c(
    ES = "Ejemplo de datos (medias, desviación típica):",
    EN = "Example of data (means, std):",
    VAL = "Exemple de dades (mitjanes, desviació típica):"
  ),
  equation_label = c(
    ES = "Ecuación",
    EN = "Equation",
    VAL = "Equació"
  ),
  tabla= c(
    ES= "Tabla Ejemplo: Este es un ejemplo de tabla (para ANOVA)", 
    EN= "Example Table: This is a table example (for ANOVA)", 
    VAL= "Taula Exemple: Esta és una taula d'exemple (per a ANOVA)" 
  ),
  interpretation= c(
    ES= "Interpretación", 
    EN= "Interpretation", 
    VAL= "Interpretació"
  ),
  conclusion1= c(
    ES= "Esta es una de las conclusiones", 
    EN= "This is one of the conclussions", 
    VAL= "Esta és una de les conclusions"
  ),
  conclusion2= c(
    ES= "Esta es otra de las conclusiones", 
    EN= "This is another conclussion", 
    VAL= "Esta és una altra conclusió"
  ),
  resultsMessage = 
    c( 
      ES = "AÑADE LOS RESULTADOS QUE QUIERAS", 
      EN = "ADD ANY RESULTS YOU WANT", 
      VAL = "AFIG ELS RESULTATS QUE VULGUES" ), 
  credits= c( 
    ES= "STATIO es un Proyecto de Innovación y Mejora Educativa (PIME/25-26/562) 
    desarrollado por el DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.", 
    EN= "STATIO is an Educational Innovation and Improvement Project (PIME/25-26/562) 
    developed by the DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.", 
    VAL= "STATIO és un Projecte d'Innovació i Millora Educativa (*PIME/25-26/562) 
    desenrotllat pel DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>." 
    )
)


# Función de traducción: dado el ID y el idioma, devuelve el texto correcto. NO MODIFICAR.

tr <- function(id, lang) { texts[[id]][[lang]] }

##################### USER INTERFACE ######################################

ui <- fluidPage(
  
  useShinyjs(), # Activa funciones JS. NO MODIFICAR.
  
  # BOTONES PARA CAMBIAR DE IDIOMA EN LA PÁGINA. NO MODIFICARlOS. 
  
  absolutePanel(
    top = 10, right = 10, fixed = TRUE,
    actionButton("lang_es", "ES"),
    actionButton("lang_en", "EN"),
    actionButton("lang_va", "VAL")
  ),
  
  # ---------------------------------------------------------------------------
  # 1. CSS PARA LA ESTRUCTURA VISUAL DE LA TEMPLATE. NO MODIFICAR. 
  
  tags$head(
    tags$style(HTML("
        #sidebarWrapper {
          width: 300px;
          background: #f7f7f7;
          padding: 15px;
          border-right: 1px solid #ddd;
          position: fixed;
          top: 0;
          bottom: 0;
          left: 0;
          overflow-y: auto;
          transition: transform .3s ease;
          z-index: 2000;
          transform: translateX(-100%); /* oculto al inicio */
        }
        
        #sidebarWrapper:not(.closed) {
          transform: translateX(0); /* visible */
        }
        
        #contentWrapper {
          transition: margin-left .3s ease;
          margin-left: 0px;
        }
        
        #contentWrapper.shifted {
          margin-left: 300px;
        }
        
        #toggleSidebar {
          position: fixed;
          top: 10px;
          left: 10px;
          z-index: 3000; /* siempre por encima */
              }"
    )
    )
  ),
  
  tags$style(HTML("
  
    /* Fila de logos */
    .logo-row {
      display: flex;
      gap: 6px;
    }
  
    /* Tamaño de los logos */
    .logo-img {
      height: 80px;
      transition: height 0.3s ease;
    }
  
    /* Caja del texto */
    .text-box {
      max-width: 260px;        /* ancho base del cuadro */
      text-align: center;
      transition: max-width 0.3s ease;
    }
  
    .text-box h5 {
      margin: 7px 0 4px 0;
      font-size: 1em;
      transition: font-size 0.3s ease;
    }
  
    /*Pantallas medianas */
    @media (max-width: 1100px) {
      .logo-img { height: 70px; }
      .text-box { max-width: 220px; }      
      .text-box h5 { font-size: 0.9em; }
    }
  
    /*Pantallas pequeñas */
    @media (max-width: 900px) {
      .logo-img { height: 70px; }
      .text-box { max-width: 180px; }     
      .text-box h5 { font-size: 0.8em; }
    }
  
    /* Móviles muy pequeños */
    @media (max-width: 480px) {
      .logo-img { height: 50px; }
      .text-box { max-width: 150px; }     
      .text-box h5 { font-size: 0.7em; }
    }
  ")),
  
  tags$head(
    tags$style(HTML("
    .tab-content h4 {
      font-weight: bold;
    }
  "))
  ),
  # -------------------------------FIN DEL CSS---------------------------------
  
  # Botón que abre/cierra el panel lateral. No MODIFICAR.
  
  if(showparams) {
    actionButton("toggleSidebar", textOutput("button_parameters"))
  },
  
  
  div(id="sidebarWrapper",
      style="padding-top: 50px;",
      if(showparams) {
        div(
          
          # PANEL LATERAL DE PARÁMETROS. SE PUEDE MODIFICAR. 
          
          h4(textOutput("text_downmenu")), 
          uiOutput("dropdown_ui"),
          
          # Sliders only if OPTION 1 is selected in dropdown. You can delete one and only select sliders if needed.
          # This is the way to link dropdown menus to parameters and other functions.
          
          conditionalPanel( 
            condition = "input.server_id == 'opt1'", 
            uiOutput("slider1_ui"), 
            uiOutput("slider2_ui"), 
            uiOutput("slider3_ui")), 
          
          conditionalPanel( 
            condition = "input.server_id == 'opt2'", 
            uiOutput("slider4_ui"), 
            uiOutput("slider5_ui"), 
            uiOutput("slider6_ui") )
        )
      }
  ),
  
  
  # -------------------- CONTENIDO PRINCIPAL -------------------------------
  
  div(id="contentWrapper", class = "shifted",
      
      # Título y explicación. NO MODIFICAR aquí, solo en el diccionario. 
      
        div(style="padding-top:50px; margin-bottom:30px",
            h2(textOutput("title"), align="center"),
            div(style="display:flex; justify-content:center;",
                div(style="border:2px solid #4a90e2; border-radius:12px; padding:12px; 
                         max-width:600px; background:white; text-align:center;",
                    uiOutput("explanation")
                )
            )
        ),
      
      # --------------------- TABS DE LA APLICACIÓN ---------------------
      
      # Se puede modificar. 
      
      tabsetPanel(
        tabPanel(textOutput("panel1_title"),
                 htmlOutput("plot_title"),
                 plotOutput("Plot_ID"),
                 uiOutput("sampleStats"),
                 htmlOutput("table_name"), verbatimTextOutput("aov"),
                 textOutput("pValueText"), 
                 htmlOutput("interpretation_text"), textOutput("conclusionText"),
                 uiOutput("resultsMessage")
        ),
        
        tabPanel(textOutput("panel2_title")),
        tabPanel(textOutput("panel3_title")),
        tabPanel("Data", tableOutput("data"))
      ),
      
      # Créditos y logos. NO MODIFICAR. 
      div(style="margin-top:40px; text-align:center;margin-bottom:40px;",
          tags$img(src='DEIOAC.png', height=80),
          tags$img(src='UPV.png', height=80),
          div(style="margin-top:15px;", 
              htmlOutput("creditos"))
      )
  )
)


############################ SERVER #######################################

server <- function(input, output) {
  
  # ---------------- Manejo del panel lateral. NO MODIFICAR. --------------------
  observeEvent(input$toggleSidebar, {
    shinyjs::toggleClass(id = "sidebarWrapper", class = "closed")
    shinyjs::toggleClass(id = "contentWrapper", class = "shifted")
    shinyjs::runjs("setTimeout(function() { $(window).trigger('resize'); }, 350);")
  })
  
  # ---------------- IDIOMA. NO MODIFICAR. -----------------
  language <- reactiveVal("ES") 
  observeEvent(input$lang_es, { language("ES") }) 
  observeEvent(input$lang_en, { language("EN") }) 
  observeEvent(input$lang_va, { language("VAL") })
  
  # -------- TEXTOS TRADUCIBLES. SE PUEDEN AÑADIR/ELIMINAR SI HACEN FALTA. ----
  
  output$title <- renderText({ tr("title", language()) })
  output$explanation <- renderUI({HTML(tr("explanation", language())) })
  output$panel1_title <- renderText({ tr("panel1", language()) })
  output$panel2_title <- renderText({ tr("panel2", language()) })
  output$panel3_title <- renderText({ tr("panel3", language()) })
  output$button_parameters <- renderText({tr("button_parameters", language())})
  
  output$text_downmenu <- renderText({ tr("text_downmenu", language()) })
  
  output$creditos <- renderUI({ HTML(tr("credits", language())) })
  output$plot_title <- renderUI({
    HTML(paste0("<h3 style='font-size:22px; font-weight:bold'>",
                tr('plot', language()),
                "</h3>"))
  })
  
  # ---------------- PARÁMETROS DINÁMICOS. SE PUEDEN AÑADIR/ELIMINAR SI HACEN FALTA. ----------------
  
  output$dropdown_ui <- renderUI({ 
    selectInput( "server_id", tr("dropdown_label", language()), 
                 choices = setNames( 
                   c("opt1", "opt2"), 
                   c(tr("option1", language()), tr("option2", language())) ) ) }
  )
  
  # Sliders generados dinámicamente. SE PUEDEN AÑADIR/ELIMINAR SI HACEN FALTA.
  
  output$slider1_ui <- renderUI({
    sliderInput("sliderId", tr("slider1", language()), min = 1, max = 100, value = 30)
  })
  output$slider2_ui <- renderUI({
    sliderInput("sliderId2", tr("slider2", language()), min = 1, max = 100, value = 30)
  })
  output$slider3_ui <- renderUI({
    sliderInput("sliderId3", tr("slider3", language()), min = 1, max = 100, value = 30)
  })
  
  
  output$slider4_ui <- renderUI({
    sliderInput("sliderId4", tr("slider4", language()), min = 1, max = 20, value = 10)
  })
  output$slider5_ui <- renderUI({
    sliderInput("sliderId5", tr("slider5", language()), min = 1, max = 10, value = 5)
  })
  output$slider6_ui <- renderUI({
    sliderInput("sliderId6", tr("slider6", language()), min = 1, max = 70, value = 30)
  })
  
  
  # -------------------- GENERACIÓN DE DATOS -------------------------
  # Dataset generado pseudoaleatoriamente. Modificar con dataset propio si se desea. 
  
  d <- reactive({
    n <- 50 # Número de individuos en el dataset
    uA <- input$sliderId # Linkeamos ciertos parámetros a los sliders.  
    uB <- input$sliderId2
    uC <- input$sliderId3
    sdA <- 10
    sdB <- 10
    sdC <- 10
    
    set.seed(123)
    
    df <- bind_rows(
      data.frame(group="A", value=rnorm(n, uA, 10)),
      data.frame(group="B", value=rnorm(n, uB, 10)),
      data.frame(group="C", value=rnorm(n, uC, 10))
    )
    df
  })
  
  # ----------------------------- PLOT(S) ------------------------------------
  
  output$Plot_ID <- renderPlot({
    ggplot(d(), aes(group, value, fill=group)) +
      geom_violin(trim=FALSE) +
      geom_jitter(width=0.1, size=0.6) +
      labs(x=tr("plot_x", language()), y=tr("plot_y", language())) +
      theme_minimal()
  })
  
  
  # ---------------------- ESTADÍSTICAS -------------------------
  
  output$sampleStats <- renderUI({
    means <- d() %>% group_by(group) %>% summarise(mean = mean(value))
    
    withMathJax(HTML(paste0(
      "<h5>", tr("sampleStats_title", language()), "</h5>",
      "$$ \\text{Media A}=", round(means$mean[1],2),
      ",\\; \\text{Media B}=", round(means$mean[2],2),
      ",\\; \\text{Media C}=", round(means$mean[3],2), "$$"
    )))
  })
  # ---------------------- EJEMPLO ------------------------- 
  
  output$table_name <- renderUI({
    HTML(paste0("<h3 style='font-size:22px; font-weight:bold'>",
                tr('tabla', language()),
                "</h3>"))
  })
  
  test_result <- reactive(aov(value ~ group, data=d()))
  output$aov <- renderPrint(summary(test_result()))
  
  p_value <- reactive(summary(test_result())[[1]][["Pr(>F)"]][1])
  
  output$conclusionText <- renderText({
    if (p_value() < 0.05) tr("conclusion1", language()) else tr("conclusion2", language())
  })
  
  output$aov <- renderPrint({
    anova_result <- test_result()
    summary(anova_result)
  })
  
  output$pValueText <- renderText({
    paste("p-value or other parameter: ", format(p_value(), digits = 5))
  })
  
  output$interpretation_text <- renderUI({
    HTML(paste0("<h3 style='font-size:22px; font-weight:bold'>",
                tr('interpretation', language()),
                "</h3>"))
  })

  
  # DATOS DEL PANEL 4
  output$data <- renderTable({
    d()
  })
  
# ELIMINAR ESTO CUANDO SE HAGA LA TEMPLATE. 
  output$resultsMessage <- renderUI({
    HTML(paste0("<h3 style='color: gray; text-align: center;'>", tr("resultsMessage", language()), "</h3>"))
  })
}

# Create Shiny app ----
shinyApp(ui, server)