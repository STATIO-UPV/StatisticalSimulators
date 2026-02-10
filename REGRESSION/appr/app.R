######################### LIBRERÍAS (PUEDES AÑADIR, NO BORRAR) #########################
# shiny       → Construcción de la interfaz y del servidor
# shinyjs     → Permite usar JS para mostrar/ocultar paneles y dinamismo
# tidyverse   → Para manipulación cómoda de datos
# car/ agricolae → Tests estadísticos opcionales (ANOVA, etc.)

library(shiny)
library(shinyjs)
library(tidyverse)
#library(car)
#library(agricolae)

# My needed libraries
#library(ggplot2)
#library(dplyr)

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
    ES = "Simulador interactivo de regresión (Variable Cuantitativa X y Cualitativas binarias A y B)", 
    EN = "Regression interactive simulator (Quantitative X and binary A and B variables", 
    VAL = "Simulador interactiu de regresió (Variable quantitativa X i Qualitatives binàries A i B)" 
  ), 
  explanation = c( 
    ES = "$$E(Y) = \\beta_0 + \\beta_1 X + \\beta_2 X^2 + \\beta_3 A + \\beta_4 B + \\beta_5 X A + \\beta_6 X B + \\beta_7 A B$$", 
    EN = "$$E(Y) = \\beta_0 + \\beta_1 X + \\beta_2 X^2 + \\beta_3 A + \\beta_4 B + \\beta_5 X A + \\beta_6 X B + \\beta_7 A B$$", 
    VAL = "$$E(Y) = \\beta_0 + \\beta_1 X + \\beta_2 X^2 + \\beta_3 A + \\beta_4 B + \\beta_5 X A + \\beta_6 X B + \\beta_7 A B$$" 
  ), 
  button_parameters = c( 
    ES = "Parámetros", 
    EN = "Parameters", 
    VAL = "Paràmetres" 
  ), 
  dropdown_label = c( 
    ES = "Título del menú desplegable", 
    EN = "Title of drop-down menu", 
    VAL = "Títol del menú desplegable" 
  ), 
  slider_intercept= c( 
    ES  = "Intercepto \\( (\\beta_0) \\)", 
    EN  = "Intercept \\( (\\beta_0) \\)", 
    VAL = "Intercepte \\( (\\beta_0) \\)" 
  ), 
  slider_beta_x= c( 
    ES = "Coeficiente X \\( (\\beta_1) \\)", 
    EN = "X Coefficient \\( (\\beta_1) \\)", 
    VAL= "Coeficient X \\( (\\beta_1) \\)" 
  ), 
  slider_beta_x2= c( 
    ES = "Coeficiente \\(X^2\\) \\( (\\beta_2) \\)", 
    EN = "\\(X^2\\) Coefficient \\( (\\beta_2) \\)", 
    VAL= "Coeficient \\(X^2\\) \\( (\\beta_2) \\)" 
  ), 
  slider_beta_A= c( 
    ES = "Efecto A \\( (\\beta_3) \\)", 
    EN = "A Effect \\( (\\beta_3) \\)", 
    VAL= "Efecte A \\( (\\beta_3) \\)" 
  ),
  slider_beta_B= c( 
    ES = "Efecto B \\( (\\beta_4) \\)", 
    EN = "B Effect \\( (\\beta_4) \\)", 
    VAL= "Efecte B \\( (\\beta_4) \\)" 
  ), 
  slider_beta_xA= c( 
    ES = "Interacción X*A \\( (\\beta_5) \\)", 
    EN = "X*A Interaction \\( (\\beta_5) \\)", 
    VAL= "Interacció X*A \\( (\\beta_5) \\)" 
  ), 
  slider_beta_xB= c( 
    ES = "Interacción X*B \\( (\\beta_6) \\)", 
    EN = "X*B Interaction \\( (\\beta_6) \\)", 
    VAL= "Interacció X*B \\( (\\beta_6) \\)" 
  ), 
  slider_beta_AB= c( 
    ES = "Interacción A*B \\( (\\beta_7) \\)", 
    EN = "A*B Interaction \\( (\\beta_7) \\)", 
    VAL= "Interacció A*B \\( (\\beta_7) \\)" 
  ), 
  slider_sd_error= c( 
    ES = "Error \\( (\\epsilon) \\)", 
    EN = "Error \\( (\\epsilon) \\)", 
    VAL= "Error \\( (\\epsilon) \\)" 
  ), 
  slider_npoints= c( 
    ES = "Número de puntos", 
    EN = "Number of points", 
    VAL= "Nombre de punts" 
  ), 
  button_reset_params= c( 
    ES = "Resetear parámetros", 
    EN = "Reset parameters", 
    VAL= "Resetetjar paràmetres" 
  ), 
  
  panel1 = c( 
    ES = "Simulación", 
    EN = "Simulation", 
    VAL= "Simulació" ), 
  plot= c(
    ES = "Gráfico",
    EN = "Plot", 
    VAL= "Gràfic"
  ),
  plot_title = c(
    ES = "Simulación de regresión con variables binarias A y B",
    EN = "Regression simulation with binary A & B variables",
    VAL= "Simulació de regresió amb variables binàries A i B"
  ),
  plot_x= c(
    ES = "Variable Cuantiativa X",
    EN = "Quantitative Variable X",
    VAL= "Variable Quantitativa X"
  ),
  plot_y= c(
    ES = "Variable Dependiente Y",
    EN = "Dependent Variable Y",
    VAL= "Variable Dependient Y"
  ),
  plot_vA= c(
    ES = "Variable A",
    EN = "Variable A",
    VAL= "Variable A"
  ),
  plot_vB= c(
    ES = "Variable B",
    EN = "Variable B",
    VAL= "Variable B"
  ),
  sampleStats_title = c(
    ES = "Ejemplo de datos (medias, desviación típica):",
    EN = "Example of data (means, std):",
    VAL= "Exemple de dades (mitjanes, desviació típica):"
  ),
  equation_label = c(
    ES = "Ecuación",
    EN = "Equation",
    VAL = "Ecuació"
  ),
  interpretation= c(
    ES= "Interpretación", 
    EN= "Interpretation", 
    VAL= "Interpretació"
  ),
  modelo_estimado = c(
    ES = "Modelo Estimado:",
    EN = "Estimated Model:",
    VAL= "Model Estimat:"
  ),
  submodelos_estimados = c(
    ES = "Submodelos según valores de A y B:",
    EN = "Submodels considering A & B values:",
    VAL= "Submodels segons els valors de A i B:"
  ),
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
  withMathJax(),
  
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
          uiOutput("actionButton_reset_ui"),
          uiOutput("slider_intercept_ui"), 
          uiOutput("slider_beta_x_ui"), 
          uiOutput("slider_beta_x2_ui"), 
          uiOutput("slider_beta_A_ui"), 
          uiOutput("slider_beta_B_ui"), 
          uiOutput("slider_beta_xA_ui"), 
          uiOutput("slider_beta_xB_ui"), 
          uiOutput("slider_beta_AB_ui"),
          uiOutput("slider_sd_error_ui"), 
          uiOutput("slider_npoints_ui")
            
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
        
        #tabPanel(textOutput("panel2_title")),
        #tabPanel(textOutput("panel3_title")),
        tabPanel("Data", tableOutput("data"))
      ),
      
      # Créditos y logos. NO MODIFICAR. 
      div(
        style = "margin-top:40px; text-align:center; margin-bottom:40px;",
        
        # Contenedor horizontal obligatorio: marca UPV (izquierda) + subemisor (derecha)
        div(
          style = "display:flex; justify-content:center; align-items:center; gap:40px;",
          
          # Marca oficial UPV en composición horizontal (no el escudo solo)
          tags$img(
            src = 'UPV.png', 
            style = "height:85px; max-height:85px;"
          ),
          
          # Logotipo del subemisor DEIOAC (nunca mayor que la marca UPV)
          tags$img(
            src = 'DEIOAC.png',
            style = "height:65px; max-height:70px;"
          )
        ),
        
        # Créditos
        div(
          style = "margin-top:15px;",
          htmlOutput("creditos")
        )
      )
      
  )
)


############################ SERVER #######################################

server <- function(input, output, session) {
  
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
  output$button_parameters <- renderText({tr("button_parameters", language())})
  
  output$text_downmenu <- renderText({ tr("text_downmenu", language()) })
  
  output$creditos <- renderUI({ HTML(tr("credits", language())) })
  output$plot_title <- renderUI({
    HTML(paste0("<h3 style='font-size:22px; font-weight:bold'>",
                tr('plot', language()),
                "</h3>"))
  })
  
  # ---------------- PARÁMETROS DINÁMICOS. SE PUEDEN AÑADIR/ELIMINAR SI HACEN FALTA. ----------------
  
  # Sliders generados dinámicamente. SE PUEDEN AÑADIR/ELIMINAR SI HACEN FALTA.
  
  output$slider_intercept_ui <- renderUI({
    sliderInput("intercept", label = withMathJax(HTML(tr("slider_intercept", language()))), min = -5, max = 5, value = 0, step = 0.1)
  })
  output$slider_beta_x_ui<- renderUI({
    sliderInput("beta_x", label = withMathJax(HTML(tr("slider_beta_x", language()))), min = -5, max = 5, value = 0, step = 0.1)
  })
  output$slider_beta_x2_ui<- renderUI({
    sliderInput("beta_x2", label = withMathJax(HTML(tr("slider_beta_x2", language()))), min = -5, max = 5, value = 0, step = 0.1)
  })
  output$slider_beta_A_ui<- renderUI({
    sliderInput("beta_A", label = withMathJax(HTML(tr("slider_beta_A", language()))), min = -5, max = 5, value = 0, step = 0.1)
  })
  output$slider_beta_B_ui<- renderUI({
    sliderInput("beta_B", label = withMathJax(HTML(tr("slider_beta_B", language()))), min = -5, max = 5, value = 0, step = 0.1)
  })
  output$slider_beta_xA_ui<- renderUI({
    sliderInput("beta_xA", label = withMathJax(HTML(tr("slider_beta_xA", language()))), min = -5, max = 5, value = 0, step = 0.1)
  })
  output$slider_beta_xB_ui<- renderUI({
    sliderInput("beta_xB", label = withMathJax(HTML(tr("slider_beta_xB", language()))), min = -5, max = 5, value = 0, step = 0.1)
  })
  output$slider_beta_AB_ui<- renderUI({
    sliderInput("beta_AB", label = withMathJax(HTML(tr("slider_beta_AB", language()))), min = -5, max = 5, value = 0, step = 0.1)
  })
  output$slider_sd_error_ui<- renderUI({
    sliderInput("sd_error", label = withMathJax(HTML(tr("slider_sd_error", language()))), min = 0, max = 5, value = 1, step = 0.1)
  })
  output$slider_npoints_ui<- renderUI({
    sliderInput("npoints", label = withMathJax(HTML(tr("slider_npoints", language()))), min = 10, max = 200, value = 10, step = 0.1)
  })
  output$actionButton_reset_ui <- renderUI({
    actionButton("reset", withMathJax(HTML(tr("button_reset_params", language()))))
  })
  
  output$modelo_teorico_teorico <- renderUI({
    math_text <- "$$E(Y) = \\beta_0 + \\beta_1 X + \\beta_2 X^2 + \\beta_3 A + \\beta_4 B + \\beta_5 X A + \\beta_6 X B + \\beta_7 A B$$"
    withMathJax(HTML(math_text))
  })
  
  output$modelo_teorico_estimado <- renderUI({
    formula_line <- sprintf("$$\\hat{Y} = %.2f + %.2f X + %.2f X^2 + %.2f A + %.2f B + %.2f X A + %.2f X B + %.2f A B + \\epsilon$$",
                            input$intercept, input$beta_x, input$beta_x2, input$beta_A, input$beta_B,
                            input$beta_xA, input$beta_xB, input$beta_AB)
    withMathJax(HTML(formula_line))
  })
  
  observeEvent(input$reset, {
    for (p in c("intercept","beta_x","beta_x2","beta_A","beta_B","beta_xA","beta_xB","beta_AB","sd_error","npoints")) {
      updateSliderInput(session, p, value = ifelse(p == "sd_error", 1, 0))
    }
  })
  
  
  output$submodelos <- renderUI({
    combs <- expand.grid(A=0:1,B=0:1)
    textos <- apply(combs, 1, function(row) {
      A <- as.numeric(row["A"])
      B <- as.numeric(row["B"])
      intercept <- round(input$intercept + input$beta_A * A + input$beta_B * B + input$beta_AB * A * B, 2)
      beta_x <- round(input$beta_x + input$beta_xA * A + input$beta_xB * B, 2)
      beta_x2 <- round(input$beta_x2, 2)
      paste0(
        '<div style="border: 1px solid #007BFF; border-radius: 5px; padding: 8px; margin-bottom: 5px; background-color:#F0F8FF;">',
        sprintf("<b>A = %d, B = %d:</b><br>$$\\hat{Y} = %.2f + %.2f X + %.2f X^2$$",
                A, B, intercept, beta_x, beta_x2),
        "</div>"
      )
    })
    HTML(paste(textos, collapse=""))
  })

  # -------------------- GENERACIÓN DE DATOS -------------------------
  # Dataset generado pseudoaleatoriamente. Modificar con dataset propio si se desea. 
  
  # Cambiar sim_data para que sea reactivo con los parámetros directamente
  d <- reactive({
    grid <- expand.grid(A=0:1,B=0:1)
    do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
      row <- grid[i,]
      x <- runif(input$npoints, 0, 10)
      A <- row$A
      B <- row$B
      mu <- input$intercept + input$beta_x*x + input$beta_x2*x^2 + 
        input$beta_A*A + input$beta_B*B + input$beta_xA*x*A + 
        input$beta_xB*x*B + input$beta_AB*A*B
      y <- rnorm(input$npoints, mean=mu, sd=input$sd_error)
      data.frame(x=x, A=factor(A, levels=0:1, labels=c("A=0", "A=1")), B=factor(B, levels=0:1, labels=c("B=0", "B=1")), y=y)
    }))
  })
  
  lines_model <- reactive({
    x_seq <- seq(0, 10, length.out = 200)
    do.call(rbind, lapply(0:1, function(A) {
      lapply(0:1, function(B) {
        y <- input$intercept + input$beta_x*x_seq + input$beta_x2*x_seq^2 + 
          input$beta_A*A + input$beta_B*B + input$beta_xA*x_seq*A + 
          input$beta_xB*x_seq*B + input$beta_AB*A*B
        data.frame(x=x_seq, y=y, A=factor(A, levels=0:1, labels=c("A=0","A=1")), B=factor(B, levels=0:1, labels=c("B=0","B=1")))
      }) %>% bind_rows()
    }))
  })
  
  
  # ----------------------------- PLOT(S) ------------------------------------
  output$Plot_ID <- renderPlot({
    df <- d()
    df_lines <- lines_model()
    ggplot() +
      geom_point(data=df, aes(x=x, y=y, color=A, shape=B), alpha=0.9, size=4) +
      geom_line(data=df_lines, aes(x=x, y=y, color=A, linetype=B), size=1.2) +
      scale_color_manual(name = tr("plot_vA", language()), values = c("A=0"="red", "A=1"="blue")) +
      scale_shape_manual(name = tr("plot_vB", language()), values = c(16,17), labels=c("B=0", "B=1")) +
      scale_linetype_manual(name = tr("plot_vB", language()), values = c("solid", "dashed"), labels=c("B=0", "B=1")) +
      labs(x=tr("plot_x", language()), y=tr("plot_y", language()),
           title=tr("plot_title", language())) +
      theme_minimal(base_size=16) +
      theme(legend.position = "right")
  })
  

  output$interpretation_text <- renderUI({
    HTML(paste0("<h3 style='font-size:22px; font-weight:bold'>",
                tr('interpretation', language()),
                "</h3>",
                "<b>",
                tr("modelo_estimado", language()),
                "</b>",
                uiOutput("modelo_teorico_estimado"),
                "<b>",
                tr("submodelos_estimados",language()),
                "</b>",
                uiOutput("submodelos")
                )
         )
  })
  
  
  # DATOS DEL PANEL 4
  output$data <- renderTable({
    d()
  })
  
  # ELIMINAR ESTO CUANDO SE HAGA LA TEMPLATE. 
  #output$resultsMessage <- renderUI({
  #  HTML(paste0("<h3 style='color: gray; text-align: center;'>", tr("resultsMessage", language()), "</h3>"))
  #})
}

# Create Shiny app ----
shinyApp(ui, server)