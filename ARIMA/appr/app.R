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
#library(forecast)



if (FALSE) {
  library(munsell)
}


############################ SISTEMA DE TEXTOS Y TRADUCCIÓN ############################
############################ SISTEMA DE TEXTOS Y TRADUCCIÓN ############################

showparams <- TRUE

texts <- list(
  title = c(
    ES = "SARIMA: simulación + ACF/PACF",
    EN = "SARIMA: simulation + ACF/PACF",
    VAL = "SARIMA: simulació + ACF/PACF"
  ),
  explanation = c(
    ES = "Elige (p,d,q)(P,D,Q)[s] y simula una serie. ACF/PACF se calculan sobre la serie diferenciada.",
    EN = "Choose (p,d,q)(P,D,Q)[s] and simulate a series. ACF/PACF are computed on the differenced series.",
    VAL = "Tria (p,d,q)(P,D,Q)[s] i simula una sèrie. ACF/PACF es calculen sobre la sèrie diferenciada."
  ),
  button_parameters = c(
    ES = "Parámetros",
    EN = "Parameters",
    VAL = "Paràmetres"
  ),
  text_downmenu = c(
    ES = "1. Parámetros del modelo",
    EN = "1. Model parameters",
    VAL = "1. Paràmetres del model"
  ),
  
  # Reutilizamos slider1..6 del template para órdenes SARIMA
  slider1 = c(ES = "p (AR)", EN = "p (AR)", VAL = "p (AR)"),
  slider2 = c(ES = "d", EN = "d", VAL = "d"),
  slider3 = c(ES = "q (MA)", EN = "q (MA)", VAL = "q (MA)"),
  slider4 = c(ES = "P (SAR)", EN = "P (SAR)", VAL = "P (SAR)"),
  slider5 = c(ES = "D", EN = "D", VAL = "D"),
  slider6 = c(ES = "Q (SMA)", EN = "Q (SMA)", VAL = "Q (SMA)"),
  
  panel1 = c(ES = "Simulación", EN = "Simulation", VAL = "Simulació"),
  panel2 = c(ES = "Guía", EN = "Guide", VAL = "Guia"),
  panel3 = c(ES = "Detalles", EN = "Details", VAL = "Detalls"),
  
  plot = c(ES = "Resultados", EN = "Results", VAL = "Resultats"),
  plot_x = c(ES = "Tiempo", EN = "Time", VAL = "Temps"),
  plot_y = c(ES = "Valor", EN = "Value", VAL = "Valor"),
  
  sampleStats_title = c(
    ES = "Resumen",
    EN = "Summary",
    VAL = "Resum"
  ),
  
  credits = c(
    ES = "STATIO es un Proyecto de Innovación y Mejora Educativa (PIME/25-26/562) desarrollado por el DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
    EN = "STATIO is an Educational Innovation and Improvement Project (PIME/25-26/562) developed by the DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
    VAL = "STATIO és un Projecte d'Innovació i Millora Educativa (PIME/25-26/562) desenrotllat pel DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>."
  )
)

tr <- function(id, lang) { texts[[id]][[lang]] }






simulate_sarima_params <- function(
    n, s,
    p=0, d=0, q=0,
    P=0, D=0, Q=0,
    phi=numeric(0), theta=numeric(0), Phi=numeric(0), Theta=numeric(0),
    sd=1, burnin=200, seed=NULL
){
  if (!is.null(seed)) set.seed(seed)
  stopifnot(length(phi)==p, length(theta)==q, length(Phi)==P, length(Theta)==Q)
  
  mult_poly <- function(a,b) as.numeric(stats::convolve(a, rev(b), type="open"))
  
  # AR multiplicativa: (1 - phi(B))*(1 - Phi(B^s))
  ar0  <- c(1, -phi)
  sar0 <- c(1, rep(0, P*s))
  if (P > 0) sar0[1 + (1:P)*s] <- -Phi
  ar_poly <- mult_poly(ar0, sar0)
  ar_rec  <- -ar_poly[-1]
  
  # MA multiplicativa: (1 + theta(B))*(1 + Theta(B^s))
  ma0  <- c(1, theta)
  sma0 <- c(1, rep(0, Q*s))
  if (Q > 0) sma0[1 + (1:Q)*s] <- Theta
  ma_poly <- mult_poly(ma0, sma0)
  ma_rec  <- ma_poly[-1]
  
  # Simula parte estacionaria
  n_stat <- n + burnin + d + D*s + 5
  x_stat <- as.numeric(stats::arima.sim(
    model = list(ar = ar_rec, ma = ma_rec),
    n = n_stat,
    sd = sd
  ))
  x <- tail(x_stat, n)
  
  # Integra (inversa de las diferencias)
  if (D > 0) for (k in 1:D) for (t in (s+1):n) x[t] <- x[t] + x[t - s]
  if (d > 0) for (k in 1:d) x <- cumsum(x)
  
  ts(x, frequency = s)
}



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
  # CSS TEMPLATE. NO MODIFICAR.
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
    ))
  ),
  
  tags$style(HTML("
    .logo-row { display: flex; gap: 6px; }
    .logo-img { height: 80px; transition: height 0.3s ease; }
    .text-box { max-width: 260px; text-align: center; transition: max-width 0.3s ease; }
    .text-box h5 { margin: 7px 0 4px 0; font-size: 1em; transition: font-size 0.3s ease; }

    @media (max-width: 1100px) {
      .logo-img { height: 70px; }
      .text-box { max-width: 220px; }
      .text-box h5 { font-size: 0.9em; }
    }

    @media (max-width: 900px) {
      .logo-img { height: 70px; }
      .text-box { max-width: 180px; }
      .text-box h5 { font-size: 0.8em; }
    }

    @media (max-width: 480px) {
      .logo-img { height: 50px; }
      .text-box { max-width: 150px; }
      .text-box h5 { font-size: 0.7em; }
    }
  ")),
  
  tags$head(
    tags$style(HTML("
      .tab-content h4 { font-weight: bold; }
    "))
  ),
  # -------------------------------FIN DEL CSS---------------------------------
  
  # Botón que abre/cierra el panel lateral. No MODIFICAR.
  if (showparams) {
    actionButton("toggleSidebar", textOutput("button_parameters"))
  },
  
  # -------------------- SIDEBAR --------------------
  div(
    id = "sidebarWrapper",
    style = "padding-top: 50px;",
    if (showparams) {
      div(
        h4(textOutput("text_downmenu")),
        
        # Órdenes SARIMA
        fluidRow(
          column(4, uiOutput("p_ui")),
          column(4, uiOutput("d_ui")),
          column(4, uiOutput("q_ui"))
        ),
        fluidRow(
          column(4, uiOutput("P_ui")),
          column(4, uiOutput("D_ui")),
          column(4, uiOutput("Q_ui"))
        ),
        
        
        tags$hr(),
        
        
        # Botón + panel avanzado (coeficientes)
        actionButton("toggleAdvanced", "Avanzado"),
        div(
          id = "advancedPanel",
          style = "display:none; margin-top:10px;",
          uiOutput("phi_ui"),
          uiOutput("theta_ui"),
          uiOutput("Phi_ui"),
          uiOutput("Theta_ui"),
          tags$hr(),
          uiOutput("n_ui"),
          uiOutput("lagmax_ui"),
          uiOutput("sigma_ui")
        )
      )
    }
  ),
  
  # -------------------- CONTENIDO PRINCIPAL -------------------------------
  div(
    id = "contentWrapper", class = "shifted",
    
    # Título y explicación (diccionario)
    div(
      style = "padding-top:50px; margin-bottom:30px",
      h2(textOutput("title"), align = "center"),
      div(
        style = "display:flex; justify-content:center;",
        div(
          style = "border:2px solid #4a90e2; border-radius:12px; padding:12px;
                   max-width:700px; background:white; text-align:center;",
          uiOutput("explanation")
        )
      )
    ),
    
    tabsetPanel(
      tabPanel(
        textOutput("panel1_title"),
        plotOutput("plot_orig", height = "260px"),
        plotOutput("plot_diff", height = "260px"),
        fluidRow(
          column(width = 6, plotOutput("plot_acf",  height = "260px")),
          column(width = 6, plotOutput("plot_pacf", height = "260px"))
        ),
        uiOutput("sampleStats")
      ),
      
      tabPanel(textOutput("panel2_title")),
      tabPanel(textOutput("panel3_title")),
      tabPanel("Data", tableOutput("data"))
    ),
    
    # Créditos y logos. NO MODIFICAR.
    div(
      style = "margin-top:40px; text-align:center; margin-bottom:40px;",
      
      div(
        style = "display:flex; justify-content:center; align-items:center; gap:40px;",
        tags$img(src = "UPV.png", style = "height:85px; max-height:85px;"),
        tags$img(src = "DEIOAC.png", style = "height:65px; max-height:70px;")
      ),
      
      div(style = "margin-top:15px;", htmlOutput("creditos"))
    )
  )
)



############################ SERVER #######################################

############################ SERVER #######################################

server <- function(input, output, session) {
  
  # --- Sidebar toggle (NO TOCAR)
  observeEvent(input$toggleSidebar, {
    shinyjs::toggleClass(id = "sidebarWrapper", class = "closed")
    shinyjs::toggleClass(id = "contentWrapper", class = "shifted")
    shinyjs::runjs("setTimeout(function() { $(window).trigger('resize'); }, 350);")
  })
  
  # --- Idioma (NO TOCAR)
  language <- reactiveVal("ES")
  observeEvent(input$lang_es, { language("ES") })
  observeEvent(input$lang_en, { language("EN") })
  observeEvent(input$lang_va, { language("VAL") })
  
  # --- Textos
  output$title <- renderText(tr("title", language()))
  output$explanation <- renderUI(HTML(tr("explanation", language())))
  output$panel1_title <- renderText(tr("panel1", language()))
  output$panel2_title <- renderText(tr("panel2", language()))
  output$panel3_title <- renderText(tr("panel3", language()))
  output$button_parameters <- renderText(tr("button_parameters", language()))
  output$text_downmenu <- renderText(tr("text_downmenu", language()))
  output$creditos <- renderUI(HTML(tr("credits", language())))
  
  # --- Inputs SARIMA (estos eran los que te faltaban a veces)
  output$p_ui <- renderUI(selectInput("p", tr("slider1", language()), choices = 0:3, selected = 1))
  output$d_ui <- renderUI(selectInput("d", tr("slider2", language()), choices = 0:2, selected = 0))
  output$q_ui <- renderUI(selectInput("q", tr("slider3", language()), choices = 0:3, selected = 0))
  
  output$P_ui <- renderUI(selectInput("P", tr("slider4", language()), choices = 0:3, selected = 0))
  output$D_ui <- renderUI(selectInput("D", tr("slider5", language()), choices = 0:1, selected = 0))
  output$Q_ui <- renderUI(selectInput("Q", tr("slider6", language()), choices = 0:3, selected = 0))
  
  output$s_ui <- renderUI(numericInput("s", "Periodo estacional (s)", value = 12, min = 1, step = 1))
  output$n_ui <- renderUI(sliderInput("n", "Longitud (n)", min = 80, max = 500, value = 200, step = 10))
  output$lagmax_ui <- renderUI(sliderInput("lagmax", "Lags ACF/PACF", min = 10, max = 60, value = 24, step = 1))
  output$sigma_ui <- renderUI(sliderInput("sigma", "Ruido (sd)", min = 0.1, max = 5, value = 1, step = 0.1))
  
  # --- Avanzado (solo este mecanismo)
  advanced_on <- reactiveVal(FALSE)
  observeEvent(input$toggleAdvanced, {
    advanced_on(!advanced_on())
    shinyjs::toggle(id = "advancedPanel")
  })
  
  # --- Casting centralizado (NO BLOQUEANTE: no uses req aquí)
  orders <- reactive({
    list(
      p = as.integer(input$p %||% 1),
      d = as.integer(input$d %||% 0),
      q = as.integer(input$q %||% 0),
      P = as.integer(input$P %||% 0),
      D = as.integer(input$D %||% 0),
      Q = as.integer(input$Q %||% 0),
      s = as.integer(input$s %||% 12),
      n = as.integer(input$n %||% 200)
    )
  })
  
  # --- Defaults
  default_phi   <- function(p){ if(p==0) numeric(0) else if(p==1) c(0.7) else if(p==2) c(0.6,-0.2) else c(0.5,-0.2,0.1)[1:p] }
  default_theta <- function(q){ if(q==0) numeric(0) else if(q==1) c(-0.6) else if(q==2) c(-0.5,0.2) else c(-0.4,0.2,-0.1)[1:q] }
  default_Phi   <- function(P){ if(P==0) numeric(0) else rep(0.5, P) }
  default_Theta <- function(Q){ if(Q==0) numeric(0) else rep(-0.5, Q) }
  
  # --- UI coeficientes (solo una vez, sin duplicados)
  output$phi_ui <- renderUI({
    ord <- orders()
    if (!advanced_on() || ord$p == 0) return(NULL)
    tagList(lapply(1:ord$p, function(i)
      sliderInput(paste0("phi_", i), paste0("phi", i),
                  min=-0.99, max=0.99, value=default_phi(ord$p)[i], step=0.01)
    ))
  })
  
  output$theta_ui <- renderUI({
    ord <- orders()
    if (!advanced_on() || ord$q == 0) return(NULL)
    tagList(lapply(1:ord$q, function(i)
      sliderInput(paste0("theta_", i), paste0("theta", i),
                  min=-0.99, max=0.99, value=default_theta(ord$q)[i], step=0.01)
    ))
  })
  
  output$Phi_ui <- renderUI({
    ord <- orders()
    if (!advanced_on() || ord$P == 0) return(NULL)
    tagList(lapply(1:ord$P, function(i)
      sliderInput(paste0("Phi_", i), paste0("Phi", i, " (lag ", i*ord$s, ")"),
                  min=-0.99, max=0.99, value=default_Phi(ord$P)[i], step=0.01)
    ))
  })
  
  output$Theta_ui <- renderUI({
    ord <- orders()
    if (!advanced_on() || ord$Q == 0) return(NULL)
    tagList(lapply(1:ord$Q, function(i)
      sliderInput(paste0("Theta_", i), paste0("Theta", i, " (lag ", i*ord$s, ")"),
                  min=-0.99, max=0.99, value=default_Theta(ord$Q)[i], step=0.01)
    ))
  })
  
  # --- Coeficientes efectivos
  get_params <- reactive({
    ord <- orders()
    if (!advanced_on()) {
      list(
        phi   = default_phi(ord$p),
        theta = default_theta(ord$q),
        Phi   = default_Phi(ord$P),
        Theta = default_Theta(ord$Q)
      )
    } else {
      list(
        phi   = if (ord$p>0) sapply(1:ord$p, function(i) input[[paste0("phi_", i)]]) else numeric(0),
        theta = if (ord$q>0) sapply(1:ord$q, function(i) input[[paste0("theta_", i)]]) else numeric(0),
        Phi   = if (ord$P>0) sapply(1:ord$P, function(i) input[[paste0("Phi_", i)]]) else numeric(0),
        Theta = if (ord$Q>0) sapply(1:ord$Q, function(i) input[[paste0("Theta_", i)]]) else numeric(0)
      )
    }
  })
  
  # --- Simulación + diferenciación
  x_sim <- reactive({
    ord <- orders(); co <- get_params()
    simulate_sarima_params(
      n = ord$n, s = ord$s,
      p = ord$p, d = ord$d, q = ord$q,
      P = ord$P, D = ord$D, Q = ord$Q,
      phi = co$phi, theta = co$theta, Phi = co$Phi, Theta = co$Theta,
      sd = input$sigma %||% 1,
      burnin = 200,
      seed = 123
    )
  })
  
  x_diff <- reactive({
    ord <- orders()
    x <- x_sim()
    if (ord$D > 0) x <- diff(x, lag = ord$s, differences = ord$D)
    if (ord$d > 0) x <- diff(x, differences = ord$d)
    x
  })
  
  # --- Plots
  output$plot_orig <- renderPlot({
    ord <- orders()
    x <- x_sim()
    
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(mar = c(4, 4, 2.5, 1))  # bottom, left, top, right
    
    main <- paste0("SARIMA(", ord$p,",",ord$d,",",ord$q,")(",
                   ord$P,",",ord$D,",",ord$Q,")[",ord$s,"]")
    
    plot(x, main = main, xlab = tr("plot_x", language()), ylab = tr("plot_y", language()))
  })
  
  output$plot_diff <- renderPlot({
    xd <- x_diff()
    
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(mar = c(4, 4, 2.5, 1))
    
    plot(xd, main = "Serie diferenciada", xlab = tr("plot_x", language()), ylab = tr("plot_y", language()))
  })
  
  
  output$plot_acf <- renderPlot({
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(mar = c(4, 4, 2.5, 1))
    stats::acf(as.numeric(x_diff()), lag.max = input$lagmax %||% 24, main = "")
  })
  
  output$plot_pacf <- renderPlot({
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(mar = c(4, 4, 2.5, 1))
    stats::pacf(as.numeric(x_diff()), lag.max = input$lagmax %||% 24, main = "")
  })
  

  
  # --- Resumen + Data
  output$sampleStats <- renderUI({
    xd <- as.numeric(x_diff())
    HTML(paste0(
      "<div style='margin-top:10px; padding:10px; border:1px solid #ddd; border-radius:10px; background:#fff;'>",
      "<b>", tr("sampleStats_title", language()), ":</b> ",
      "n=", length(xd),
      " | media=", round(mean(xd), 3),
      " | sd=", round(sd(xd), 3),
      "</div>"
    ))
  })
  
  output$data <- renderTable({
    x <- x_sim()
    data.frame(t = seq_along(x), x = as.numeric(x))
  })
}


# Create Shiny app ----
shinyApp(ui, server)
