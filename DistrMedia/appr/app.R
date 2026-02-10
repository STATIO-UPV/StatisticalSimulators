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
    ES = "Distribución muestral de la media", 
    EN = "Sampling distribution of the average", 
    VAL = "Distribució mostral de la mitjana" 
  ), 
  explanation = c( 
    ES = "Predecir el futuro: ¿permiten las técnicas estadísticas predecir qué va a ocurrir?
    La respuesta es que depende. Bajo ciertas circumstancias SÍ.
    Con esta aplicación entenderás qué sucede al tomar muchas 
    muestras de tamaño *n* de una población objetivo y calcular sus medias.
    ¿Qué valores toman las medias de esas muestras? 
    ¿Qué valor toma la media de todas esas medias?
    ¿Qué dispersión presentan?
    ¿Qué modelo de distribución podría utilizarse para estudiarlas?",
    EN = "Predicting the future: can statistical techniques predict what will happen?
    The answer is: it depends. Under certain conditions, YES.
    With this app, you will understand what happens when you take many samples of size *n* 
    from a target population and calculate their means.
    What values do the means of these samples take? 
    What value does the mean of all these means take?
    What level of variation do they show?
    What distribution model could be used to study them?", 
    VAL = "Predir el futur: permeten les tècniques estadístiques predir què passarà?
La resposta és que depèn. En certes circumstàncies, SÍ.
Amb aquesta app entendràs què passa en prendre moltes
mostres de grandaria *n* d’una població objectiu i calcular-ne les mitjanes.
Quins valors prenen les mitjanes d’aquestes mostres?
Quin valor pren la mitjana de totes aquestes mitjanes?
Quina dispersió presenten?
Quin model de distribució es podria utilitzar per estudiar-les?
" 
  ), 
  qqPlotMain=c(ES = "¿Tienen las medias una dist. Normal?",
               EN = "Do averages have a Normal distribution?",
               VAL = "Tenen les mitjanes una dist. Normal?"
  ),
  qqPlotYlab=c(ES= "Cuantiles empíricos",
               EN= "Empirical quantiles",
               VAL= "Quantils empírics"
  ),
  qqPlotXlab=c(ES= "Cuantiles teóricos estandarizados",
               EN= "Standardized theoretical quantiles",
               VAL= "Quantils teòrics estandaritzats"
  ),
  button_parameters = c( 
    ES = "Parámetros", 
    EN = "Parameters", 
    VAL = "Paràmetres" 
  ), 
  text_downmenu= c( 
    ES = "Población", 
    EN = "Population", 
    VAL = "Població" 
  ), 
  dropdown_label = c( 
    ES = "Población", 
    EN = "Population", 
    VAL = "Població" 
  ), 
  Asimetría = c( 
    ES = "Asimetría +", 
    EN = "Asymmetry +", 
    VAL = "Asimetria +" 
  ), 
  Normal = c( 
    ES = "Normal", 
    EN = "Normal", 
    VAL = "Normal" 
  ), 
  Uniforme= c( 
    ES = "Uniforme", 
    EN= "Uniform", 
    VAL= "Uniforme" 
  ), 
  panel1 = c( 
    ES = "Panel 1", 
    EN = "Panel 1", 
    VAL = "Panell 1" ), 
  
  Hist= c(
    ES= "Histograma de la Población",
    EN= "Population histogram", 
    VAL= "Histograma de la població"
  ),
  X= c(
    ES= "Valor_X",
    EN= "X_value",
    VAL= "Valor X"
  ),
  Frecuencia= c(
    ES= "Frecuencia",
    EN= "Frequency",
    VAL= "Freqüència"
  ),
  PopButton= c(
    ES= "Genera la población",
    EN= "Generate population", 
    VAL= "Genera la població"
  ), 
  Elements= c(
    ES= "¿Cuántos elementos en cada muestra?",
    EN= "How many elements in each SAMPLE?",
    VAL= "Quants elements en cada mostra?"
  ),
  credits= c( 
    ES= "STATIO es un Proyecto de Innovación y Mejora Educativa (PIME/25-26/562) 
    desarrollado por el DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.", 
    EN= "STATIO is an Educational Innovation and Improvement Project (PIME/25-26/562) 
    developed by the DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.", 
    VAL= "STATIO és un Projecte d'Innovació i Millora Educativa (*PIME/25-26/562) 
    desenrotllat pel DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>." 
  ),
  SampleButton = c(
    ES = "Genera 1000 muestras",
    EN = "Generate 1000 samples",
    VAL = "Genera 1000 mostres"
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
          h4(textOutput("text_downmenu")), 
          uiOutput("dropdown_ui")
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
        tabPanel(
          textOutput("panel1_title"),
          actionButton("genPob", textOutput("PopButton")),
          plotOutput("histPob"),
          
          #tableOutput("pobData")
          radioButtons("nelms", 
                       textOutput("Elements"), 
                       c(2,10,25,50,100),inline=TRUE),
          
          #numericInput("nelms", "Tamaño de las muestras", value = 25, min = 2, max = 100),
          actionButton("genMedias", textOutput("SampleButtonText")),
          #plotOutput("diagp")
          plotOutput("histMedias"),
          plotOutput("qqmedias")),
        
        
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
  ))


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
  output$button_parameters <- renderText({tr("button_parameters", language())})
  output$SampleButtonText <- renderText({ tr("SampleButton", language()) })
  
  output$text_downmenu <- renderText({ tr("text_downmenu", language()) })
  
  output$creditos <- renderUI({ HTML(tr("credits", language())) })
  output$PopButton <- renderText({tr("PopButton", language())})
  output$Elements <- renderText({tr("Elements", language())})
  
  # ---------------- PARÁMETROS DINÁMICOS. SE PUEDEN AÑADIR/ELIMINAR SI HACEN FALTA. ----------------
  
  output$dropdown_ui <- renderUI({ 
    selectInput("pdist", tr(("Población"), language()),
                choices= setNames(
                  c("Asimetría +", "Normal", "Uniforme"),
                  c(tr("Asimetría", language()), tr("Normal", language()), tr("Uniforme", language()))))}
  )
  
  # -------------------- GENERACIÓN DE DATOS -------------------------
  # Dataset generado pseudoaleatoriamente. Modificar con dataset propio si se desea. 
  
  pob <- eventReactive(input$genPob, {
    switch(input$pdist,
           "Asimetría +" = rexp(10000, rate = 1/10),
           "Normal"      = rnorm(10000, mean = 10, sd = 2),
           "Uniforme" = runif(10000, min = 2, max = 18))
  })
  
  ## Flag reactivo
  mostrarMedias <- reactiveVal(FALSE)
  
  ## Efectos (estado)
  observeEvent(input$genPob, {mostrarMedias(FALSE)}) # borrar gráf medias
  observeEvent(input$genMedias, {mostrarMedias(TRUE)}) # mostrar gráf medias
  
  
  
  medias <- eventReactive(input$genMedias, {
    
    # Obtenemos 1000 muestras de tamaño input$nelms
    meds <- numeric(1000)
    for(i in 1:1000) meds[i] <- mean(sample(pob(), size=input$nelms, replace = F))
    meds
  })
  
  
  #output$pobData <- renderTable(head(pob()))
  
  # ----------------------------- PLOT(S) ------------------------------------
  
  
  output$histPob <- renderPlot({
    req(pob())
    h <-hist(pob(),main=tr("Hist", language()),
             xlab= tr("X", language()),
             ylab=tr("Frecuencia", language()),
             breaks=50) 
    h
    mtext(bquote(mu == .(round(mean(pob()) , 1)) ~ ~ ~ sigma == .(round(sd(pob()), 1))), side=3, line=0,col="blue")
    
    abline(v=mean(pob()), col="blue", lwd=4)
    text(mean(pob()), 
         max(h$counts)/2, 
         bquote(mu == .(round(mean(pob()), 1))),
         col = "blue", pos = 4)
    
    # text(par("usr")[2] / 2, 
    #   max(h$counts)/2, 
    #   bquote(sigma == .(round(sd(pob()), 1))),
    #   col = "blue", pos = 2)
    #text(mean(pob()), 1000, paste("media: ", round(mean(pob()),0)))
  },res = 96)
  
  # output$diagp <- renderPlot(stripchart(medias()[1:10], method="stack", pch=16, 
  #                                       main="Primeras 10 medias", frame=FALSE,
  #                                       xlab="Valores de las medias", ylim=c(0,60),
  #                                       ylab="Frecuencia"), res = 96)
  
  output$histMedias <- renderPlot({
    req(mostrarMedias())
    hm <- hist(medias(), 
               main=paste("Medias de 1000 muestras de ", input$nelms, " datos"),
               xlab="Valores de las medias",
               ylab="Frecuencia",
               breaks=30)
    hm
    mtext(bquote(bar(X)[bar(X)] == .(round(mean(medias()) , 1)) ~ ~ ~ S[bar(X)] == .(round(sd(medias()), 1))), side=3, line=0,col="red")
    # mtext(paste(
    #   "media=", round(mean(medias()),1),
    #   ", desv. típica=", round(sd(medias()),1)), 
    #   side = 3, line = 0)
    
    abline(v=mean(medias()), col="red", lwd=4)
    
    # text(mean(medias()), 
    #   max(hm$counts)/2, 
    #   paste(round(mean(medias()),1)),
    #   col = "red", pos = 4)
    # 
    # text(par("usr")[2] / 2, 
    #   max(hm$counts)/2, 
    #   bquote(S[bar(X)] == .(round(sd(medias()), 1))),
    #   col = "red", pos = 2)
    # 
    # parámetros de la normal a superponer al gráfico
    m <- mean(medias()) 
    s <- sd(medias())
    
    # ancho de las barras
    binwidth <- diff(hm$breaks)[1]
    
    # curva normal escalada
    x <- seq(min(hm$breaks), max(hm$breaks), length.out = 200)
    
    lines(x,
          dnorm(x, mean = m, sd = s) * length(medias()) * binwidth,
          col = "blue", lwd = 3)
  }, res = 96)
  
  output$qqmedias<- renderPlot({
    req(mostrarMedias())
    qqPlot(medias(), id=FALSE,
           main= tr("qqPlotMain", language()),
           ylab= tr("qqPlotYlab", language()),
           xlab= tr("qqPlotXlab", language()))
  }, res=96)
}

# Create Shiny app ----
shinyApp(ui, server)


