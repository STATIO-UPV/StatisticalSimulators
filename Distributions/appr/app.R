######################### LIBRERÍAS (PUEDES AÑADIR, NO BORRAR) #########################
# shiny       → Construcción de la interfaz y del servidor
# shinyjs     → Permite usar JS para mostrar/ocultar paneles y dinamismo

library(ggplot2)
library(plotly)
library(shiny)
library(shinyjs)

if (FALSE) {
  library(munsell)
}

############################ SISTEMA DE TEXTOS Y TRADUCCIÓN ############################

# showparams controla si se muestra el panel lateral de parámetros
showparams <- FALSE # Cambiarlo a FALSE si no se necesita barra de parámetros.


# Lista de textos en cada idioma.
# Cada entrada tiene ES / EN / VAL.
# Añadir tantas traducciones al dict como sean necesarias.

texts <- list(
  title = c(
    ES = "Distribuciones de probabilidad",
    EN = "Probability distributions",
    VAL = "Distribucions de probabilitat"
  ),
  explanation = c(
    ES = "Esta aplicación es un visor interactivo para explorar diversas distribuciones de probabilidad
    comunes y cómo cambian sus formas con diferentes parámetros y rangos. Muestra seis distribuciones:
    binomial, binomial negativa (que incluye la geométrica como un caso especial), Poisson,
    hipergeométrica, normal y exponencial. Estas distribuciones se presentan en gráficos interactivos
    independientes que muestran las funciones de masa/densidad de probabilidad o las funciones de
    distribución. En cada gráfico, pasando el ratón por encima, se pueden ver los valores de probabilidad
    correspondientes para un determinado valor de X. Para cada distribución, el usuario puede definir el rango del eje x (X Mín, X Máx)
    y ajustar los parámetros correspondientes (por ejemplo, el número de repeticiones y la probabilidad
    de éxito para la binomial, o la tasa para la de Poisson).",
    EN = "This application is an interactive viewer to explore several common probability
    distributions and how their shapes change with different parameters and ranges. It displays
    six distributions: binomial, negative binomial (including geometric as a special case), Poisson,
    hypergeometric, normal, and exponential. These distributions are shown in separate interactive
    graphs, showing either probability mass functions/density functions or distribution functions.
    In each graph, by hovering over it, you can see the corresponding probability values for a given value of X.
    For each distribution, the user can set the x-axis range (X Min, X Max) and adjust the corresponding
    parameters (for example, number of repetitions and probability of succes for the binomial, or rate
    for the Poisson).",
    VAL = "Aquesta aplicació és un visor interactiu per a explorar diverses distribucions de
    probabilitat i com canvien les seues formes amb diferents paràmetres i rangs. Mostra sis
    distribucions: binomial, binomial negativa (incloent-hi la geomètrica com un cas especial),
    Poisson, hipergeomètrica, normal i exponencial. Aquestes distribucions es presenten en gràfics
    interactius separats que mostren les funcions de massa/densitat de probabilitat o les funcions
    de distribució. En cada gràfic, passant el ratolí per damunt, es poden veure els valors de
    probabilitat corresponents per a un determinat valor de X. Per a cada distribució, l'usuari pot establir el rang de l'eix x (X Mín, X Màx) i
    ajustar els paràmetres corresponents (per exemple, el nombre de repeticions i la probabilitat
    d'èxit per a la binomial, o la taxa per a la de Poisson)."
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
  binomial = c(
    ES = "Binomial",
    EN = "Binomial",
    VAL = "Binomial"
  ),
  poisson = c(
    ES = "Poisson",
    EN = "Poisson",
    VAL = "Poisson"
  ),
  binomial_neg= c(
    ES = "Binomial negativa",
    EN= "Negative binomial",
    VAL= "Binomial negativa"
  ),
  hipergeom= c(
    ES = "Hipergeométrica",
    EN= "Hypergeometric",
    VAL= "Hipergeomètrica"
  ),
  normal= c(
    ES = "Normal",
    EN= "Normal",
    VAL= "Normal"
  ),
  exponencial= c(
    ES = "Exponencial",
    EN= "Exponential",
    VAL= "Exponencial"
  ),
  n_rep= c(
    ES = "N. repeticiones (n)",
    EN= "N. repetitions (n)",
    VAL= "N. repeticions (n)"
  ),
  p_exito= c(
    ES = "Prob. éxito (p)",
    EN= "Prob. success (p)",
    VAL= "Prob. èxit (p)"
  ),
  n_eventos = c(
    ES = "N. eventos (k)",
    EN = "N. events (k)",
    VAL = "N. esdeveniments (k)" ),
  tasa = c(
    ES = "Tasa",
    EN = "Rate",
    VAL = "Taxa"
  ),
  panel3 = c(
    ES = "Panel 3",
    EN = "Panel 3",
    VAL = "Panell 3"
  ),
  N= c(
    ES= "Población (N)",
    EN= "Population (N)",
    VAL= "Població (N)"
  ),
  media= c(
    ES= "Media",
    EN= "Mean",
    VAL= "Mitjana"
  ),
  sd= c(
    ES= "Desviación típica",
    EN= "Standard deviation",
    VAL= "Desviació típica"
  ),
  checkbox_dist = c(
    ES = "Cambiar probabilidad/densidad por funciones de distribución",
    EN = "Change probability/density for distribution functions",
    VAL = "Canviar probabilitat/densitat per funcions de distribució"
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

  # if(showparams) {
  #   actionButton("toggleSidebar", textOutput("button_parameters"))
  # },


  # div(id="sidebarWrapper",
  #     style="padding-top: 50px;",
  #     if(showparams) {
  #       div(
  #
  #         # PANEL LATERAL DE PARÁMETROS. SE PUEDE MODIFICAR.
  #
  #         h4(textOutput("text_downmenu")),
  #         uiOutput("dropdown_ui"),
  #
  #         # Sliders only if OPTION 1 is selected in dropdown. You can delete one and only select sliders if needed.
  #         # This is the way to link dropdown menus to parameters and other functions.
  #
  #         conditionalPanel(
  #           condition = "input.server_id == 'opt1'",
  #           uiOutput("slider1_ui"),
  #           uiOutput("slider2_ui"),
  #           uiOutput("slider3_ui")),
  #
  #         conditionalPanel(
  #           condition = "input.server_id == 'opt2'",
  #           uiOutput("slider4_ui"),
  #           uiOutput("slider5_ui"),
  #           uiOutput("slider6_ui") )
  #       )
  #     }
  # ),


  # -------------------- CONTENIDO PRINCIPAL -------------------------------

  div(id="contentWrapper",

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

      #First row
      fixedRow(
        column(4, plotlyOutput("binomplot")),
        column(4, plotlyOutput("negbinomplot")),
        column(4, plotlyOutput("poisplot")
        )),
      #Second row
      fixedRow(
        column(4,
               splitLayout(
                 numericInput("minbinom", "X Min.", 0, step = 1),
                 numericInput("maxbinom", "X Max.", 20, step = 1))),
        # sliderInput("rangebinom",
        #           "Rango de X",
        #           min = 0,
        #           max = 100,
        #           step = 1,
        #           value = c(0, 20)
        #           )),
        column(4,
               splitLayout(
                 numericInput("mingeom", "X Min.", 0, step = 1),
                 numericInput("maxgeom", "X Max.", 20, step = 1))),
        column(4,
               splitLayout(
                 numericInput("minpois", "X Min.", 0, step = 1),
                 numericInput("maxpois", "X Max.", 20, step = 1)))
      ),
      #Third row
      fixedRow(
        column(4,
               splitLayout(
                 numericInput("nbinom", tr("n_rep", "ES"), value = 20, step = 1),
                 numericInput("pbinom", tr("p_exito", "ES"), value = 0.5, step = 0.1))
        ),
        column(4,
               splitLayout(
                 numericInput("pnegbinom", tr("p_exito", "ES"), value = 0.5, step = 0.1),
                 numericInput("knegbinom", tr("n_eventos", "ES"), value = 1, step = 1))
        ),
        column(4,
               numericInput("lambdapois",
                            HTML(tr('n_eventos', "ES"), "λ"),
                            value = 5, step = 0.5
               )
        )
      ),
      #Fifth row
      fixedRow(
        column(width=4,
               plotlyOutput("hyperplot")
        ),
        column(width=4,
               plotlyOutput("gausplot")
        ),
        column(width=4,
               plotlyOutput("expplot")
        )
      ),
      #Sixth row
      fixedRow(
        column(width=4,
               splitLayout(
                 numericInput("minhyper", "X Min.", value = 0, step = 1),
                 numericInput("maxhyper", "X Max.", value = 20, step = 1))
        ),
        column(width=4,
               splitLayout(
                 numericInput("mingaus", "X Min.", value = -5, step = 1),
                 numericInput("maxgaus", "X Max.", value = 5, step = 1))
        ),
        column(width=4,
               splitLayout(
                 numericInput("minexp", "X Min.", value = 0, step = 1),
                 numericInput("maxexp", "X Max.", value = 20, step = 1))
        )
      ),
      #Seventh row
      fixedRow(
        column(width=4,
               splitLayout(
                 numericInput("Nhyper", tr("N", "ES"), value = 40, step = 1),
                 numericInput("nhyper", tr("n_rep", "ES"), value = 20, step = 1),
                 numericInput("phyper", tr("p_exito", "ES"), value = 0.5, step = 0.1))

        ),
        column(width=4,
               splitLayout(
                 numericInput("mugaus", tr("media", "ES"), value = 0, step = 0.1),
                 numericInput("sdgaus", tr("sd", "ES"), value = 1, step = 0.1))
        ),
        column(width=4,
               numericInput("rateexp",
                            HTML("Rate (&lambda;)"),
                            value = 0.5, step = 0.5
               )
        )
      ),

      #Tenth row
      fixedRow(
        column(width=8, offset = 4,
               checkboxInput("fdistr",
                             tr("checkbox_dist", "ES"),
                             value = FALSE
               )
        )
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

  # --------- TRADUCIR BOTONES INPUT ----------------------
  observeEvent(language(), {
    lang <- language()
    updateNumericInput(session, "nbinom", label = tr("n_rep", lang))
    updateNumericInput(session, "pbinom", label = tr("p_exito", lang))
    updateNumericInput(session, "pnegbinom", label = tr("p_exito", lang))
    updateNumericInput(session, "knegbinom", label = tr("n_eventos", lang))
    updateNumericInput(session, "lambdapois", label = paste0(tr("tasa", lang), " (λ)"))

    updateNumericInput(session, "Nhyper", label = tr("N", lang))
    updateNumericInput(session, "nhyper", label = tr("n_rep", lang))
    updateNumericInput(session, "phyper", label = tr("p_exito", lang))
    updateNumericInput(session, "mugaus", label = tr("media", lang))
    updateNumericInput(session, "sdgaus", label = tr("sd", lang))
    updateNumericInput(session, "rateexp", label = paste0(tr("tasa", lang), " (λ)"))
    updateCheckboxInput(session, "fdistr", label = tr("checkbox_dist", lang))
  })

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

  output$binomplot <- renderPlotly({
    x <- seq(input$minbinom, input$maxbinom)
    if(input$fdistr){
      y <- pbinom(x, input$nbinom, input$pbinom)
    } else{
      y <- dbinom(x, input$nbinom, input$pbinom)
    }
    df <- signif(data.frame(x, y), 3)
    ggplotly(ggplot(df, aes(x=x, y=y)) + geom_point() + ggtitle(tr("binomial", language())) + xlab("X") + ylab(ifelse(input$fdistr, "P(X\u2264x)", "P(X=x)")))
    #plot(x, y, type="h", las=1, ylab="P(X=x)", xlab="X", main="Binomial")
  })

  output$negbinomplot <- renderPlotly({
    x <- seq(input$mingeom, input$maxgeom)
    if(input$fdistr){
      y <- pnbinom(x, size=input$knegbinom, prob=input$pnegbinom)
    } else{
      y <- dnbinom(x, size=input$knegbinom, prob=input$pnegbinom)
    }
    df <- signif(data.frame(x, y), 3)
    ggplotly(ggplot(df, aes(x=x, y=y)) + geom_point() + ggtitle(tr("binomial_neg", language())) + xlab("X") + ylab(ifelse(input$fdistr, "P(X\u2264x)", "P(X=x)")))
    #plot(x, y, type="h", las=1, ylab="P(X=x)", xlab="X", main="Binomial negativa \n (Geométrica para k=1)")
  })

  output$poisplot <- renderPlotly({
    x <- seq(input$minpois, input$maxpois)
    if(input$fdistr){
      y <- ppois(x, input$lambdapois)
    } else{
      y <- dpois(x, input$lambdapois)
    }
    df <- signif(data.frame(x, y), 3)
    ggplotly(ggplot(df, aes(x=x, y=y)) + geom_point() + ggtitle(tr("poisson", language())) + xlab("X") + ylab(ifelse(input$fdistr, "P(X\u2264x)", "P(X=x)")))
    #plot(x, y, type="h", las=1, ylab="P(X=x)", xlab="X", main="Poisson")
  })

  output$hyperplot <- renderPlotly({
    x <- seq(input$minhyper, input$maxhyper)
    if(input$fdistr){
      y <- phyper(x, input$Nhyper*input$phyper, input$Nhyper*(1-input$phyper), input$nhyper)
    } else{
      y <- dhyper(x, input$Nhyper*input$phyper, input$Nhyper*(1-input$phyper), input$nhyper)
    }
    df <- signif(data.frame(x, y), 3)
    ggplotly(ggplot(df, aes(x=x, y=y)) + geom_point() + ggtitle(tr("hipergeom", language())) + xlab("X") + ylab(ifelse(input$fdistr, "P(X\u2264x)", "P(X=x)")))
    #plot(x, y, type="h", las=1, ylab="P(X=x)", xlab="X", main="Hipergeométrica")
  })

  output$gausplot <- renderPlotly({
    x <- seq(input$mingaus, input$maxgaus, 0.1)
    if(input$fdistr){
      y <- pnorm(x, input$mugaus, input$sdgaus)
    } else{
      y <- dnorm(x, input$mugaus, input$sdgaus)
    }
    df <- signif(data.frame(x, y), 3)
    ggplotly(ggplot(df, aes(x=x, y=y)) + geom_line() + ggtitle(tr("normal", language())) + xlab("X") + ylab(ifelse(input$fdistr, "P(X\u2264x)", "f(x)")))
    #plot(x, y, type="l", las=1, ylab="Densidad", xlab="X", main="Normal")
  })

  output$expplot <- renderPlotly({
    x <- seq(input$minexp, input$maxexp, 0.1)
    if(input$fdistr){
      y <- pexp(x, input$rateexp)
    } else{
      y <- dexp(x, input$rateexp)
    }
    df <- signif(data.frame(x, y), 3)
    ggplotly(ggplot(df, aes(x=x, y=y)) + geom_line() + ggtitle(tr("exponencial", language())) + xlab("X") + ylab(ifelse(input$fdistr, "P(X\u2264x)", "f(x)")))
    #plot(x, y, type="l", las=1, ylab="Densidad", xlab="X", main="Exponencial")
  })
}

# Create Shiny app ----
shinyApp(ui, server)
