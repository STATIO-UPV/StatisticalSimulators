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
    ES = "Modelos SARIMA",
    EN = "SARIMA Models",
    VAL = "Models SARIMA"
  ),
  plot_diff_title = c(
    ES = "Serie diferenciada",
    EN = "Differenced series",
    VAL = "Sèrie diferenciada"
  ),
  
  acf_title = c(
    ES = "ACF de la serie diferenciada",
    EN = "ACF of the differenced series",
    VAL = "ACF de la sèrie diferenciada"
  ),
  
  pacf_title = c(
    ES = "PACF de la serie diferenciada",
    EN = "PACF of the differenced series",
    VAL = "PACF de la sèrie diferenciada"
  ),
  button_advanced = c(
    ES = "Avanzado",
    EN = "Advanced",
    VAL = "Avançat"
  ),
  explanation = c(
    ES = "Aplicación docente para explorar modelos SARIMA mediante simulación, analizando la estructura temporal y estacional a través de las funciones ACF y PACF.",
    EN = "Educational tool to explore SARIMA models through simulation, analyzing temporal and seasonal structure using ACF and PACF.",
    VAL = "Aplicació docent per a explorar models SARIMA mitjançant simulació, analitzant l’estructura temporal i estacional amb les funcions ACF i PACF."
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
  download_guide_text = c(
    ES = "Descargar guía en PDF",
    EN = "Download PDF guide",
    VAL = "Descarregar guia en PDF"
  ),
  
  guide_button = c(
    ES = "Guía",
    EN = "Guide",
    VAL = "Guia"
  ), 
  download_data_text = c(
    ES = "Descargar datos",
    EN = "Download data",
    VAL = "Descarregar dades"
  ),
  panel_data = c(
    ES = "Datos",
    EN = "Data",
    VAL = "Dades"
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
        actionButton("toggleAdvanced", textOutput("button_advanced")),
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
        )
      )
    }
  ),
  
  # -------------------- CONTENIDO PRINCIPAL -------------------------------
  div(
    id = "contentWrapper", class = "shifted",
    
    div(
      style = "position:fixed; top:10px; left:320px; z-index:3000;",
      downloadButton(
        "download_guide",
        textOutput("guide_button"),
        class = "btn btn-default no-icon"
      )
    ),
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
      
      # tabPanel(
      #   textOutput("panel2_title"),
      #   br(),
      #   downloadButton("download_guide", textOutput("download_guide_text"))
      #   ),
      
      tabPanel(
        textOutput("panel_data_title"),
        br(),
        downloadButton("download_data", textOutput("download_data_text")),
        br(), br(),
        tableOutput("data")
      )
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
  output$panel_data_title <- renderText(
    tr("panel_data", language())
  )
  output$button_parameters <- renderText(tr("button_parameters", language()))
  output$button_advanced <- renderText(tr("button_advanced", language()))
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
  output$n_ui <- renderUI(sliderInput("n", "n", min = 80, max = 500, value = 200, step = 10))
  output$lagmax_ui <- renderUI(sliderInput("lagmax", "Lags ACF/PACF", min = 10, max = 30, value = 24, step = 1))
  
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
  # output$phi_ui <- renderUI({
  #   ord <- orders()
  #   if (!advanced_on() || ord$p == 0) return(NULL)
  #   tagList(lapply(1:ord$p, function(i)
  #     sliderInput(paste0("phi_", i), paste0("phi", i),
  #                 min=-0.99, max=0.99, value=default_phi(ord$p)[i], step=0.01)
  #   ))
  # })
  # 
  # output$theta_ui <- renderUI({
  #   ord <- orders()
  #   if (!advanced_on() || ord$q == 0) return(NULL)
  #   tagList(lapply(1:ord$q, function(i)
  #     sliderInput(paste0("theta_", i), paste0("theta", i),
  #                 min=-0.99, max=0.99, value=default_theta(ord$q)[i], step=0.01)
  #   ))
  # })
  # 
  # output$Phi_ui <- renderUI({
  #   ord <- orders()
  #   if (!advanced_on() || ord$P == 0) return(NULL)
  #   tagList(lapply(1:ord$P, function(i)
  #     sliderInput(paste0("Phi_", i), paste0("Phi", i, " (lag ", i*ord$s, ")"),
  #                 min=-0.99, max=0.99, value=default_Phi(ord$P)[i], step=0.01)
  #   ))
  # })
  # 
  # output$Theta_ui <- renderUI({
  #   ord <- orders()
  #   if (!advanced_on() || ord$Q == 0) return(NULL)
  #   tagList(lapply(1:ord$Q, function(i)
  #     sliderInput(paste0("Theta_", i), paste0("Theta", i, " (lag ", i*ord$s, ")"),
  #                 min=-0.99, max=0.99, value=default_Theta(ord$Q)[i], step=0.01)
  #   ))
  # })
  
  output$phi_ui <- renderUI({
    ord <- orders()
    if (!advanced_on() || ord$p == 0) return(NULL)
    tagList(lapply(1:ord$p, function(i)
      sliderInput(
        paste0("phi_", i),
        label = HTML(paste0("&phi;<sub>", i, "</sub>")),
        min = -0.99, max = 0.99,
        value = default_phi(ord$p)[i],
        step = 0.01
      )
    ))
  })
  
  output$theta_ui <- renderUI({
    ord <- orders()
    if (!advanced_on() || ord$q == 0) return(NULL)
    tagList(lapply(1:ord$q, function(i)
      sliderInput(
        paste0("theta_", i),
        label = HTML(paste0("&theta;<sub>", i, "</sub>")),
        min = -0.99, max = 0.99,
        value = default_theta(ord$q)[i],
        step = 0.01
      )
    ))
  })
  
  output$Phi_ui <- renderUI({
    ord <- orders()
    if (!advanced_on() || ord$P == 0) return(NULL)
    tagList(lapply(1:ord$P, function(i)
      sliderInput(
        paste0("Phi_", i),
        label = HTML(paste0(
          "&Phi;<sub>", i, "</sub> ",
          "<span style='font-weight:normal;'>(lag ", i * ord$s, ")</span>"
        )),
        min = -0.99, max = 0.99,
        value = default_Phi(ord$P)[i],
        step = 0.01
      )
    ))
  })
  
  output$Theta_ui <- renderUI({
    ord <- orders()
    if (!advanced_on() || ord$Q == 0) return(NULL)
    tagList(lapply(1:ord$Q, function(i)
      sliderInput(
        paste0("Theta_", i),
        label = HTML(paste0(
          "&Theta;<sub>", i, "</sub> ",
          "<span style='font-weight:normal;'>(lag ", i * ord$s, ")</span>"
        )),
        min = -0.99, max = 0.99,
        value = default_Theta(ord$Q)[i],
        step = 0.01
      )
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
    ord <- orders()
    co <- get_params()
    
    tryCatch({
      simulate_sarima_params(
        n = ord$n, s = ord$s,
        p = ord$p, d = ord$d, q = ord$q,
        P = ord$P, D = ord$D, Q = ord$Q,
        phi = co$phi, theta = co$theta,
        Phi = co$Phi, Theta = co$Theta,
        sd = 1,
        burnin = 200,
        seed = 123
      )
    }, error = function(e) {
      validate(
        need(FALSE, "Parámetros no válidos (proceso no estacionario). Ajusta los coeficientes.")
      )
    })
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
    t <- seq_along(x)
    
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(mar = c(4, 4, 2.5, 1), bg = "#FAFAFA")
    
    main <- paste0("SARIMA(", ord$p,",",ord$d,",",ord$q,")(",
                   ord$P,",",ord$D,",",ord$Q,")[",ord$s,"]")
    
    plot(
      t, as.numeric(x),
      type = "n",
      xaxt = "n",
      main = main,
      xlab = tr("plot_x", language()),
      ylab = tr("plot_y", language())
    )
    
    lines(t, as.numeric(x), col = "#5A5A5A", lwd = 2.5)
    abline(h = 0, col = "gray75", lty = 2)
    
    ticks <- seq(50, length(x), by = 50)
    axis(1, at = ticks, labels = ticks)
  })
  
  output$plot_diff <- renderPlot({
    xd <- x_diff()
    t <- seq_along(xd)
    
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(mar = c(4, 4, 2.5, 1), bg = "#FAFAFA")
    
    plot(
      t, as.numeric(xd),
      type = "n",
      xaxt = "n",
      main = tr("plot_diff_title", language()),
      xlab = tr("plot_x", language()),
      ylab = tr("plot_y", language())
    )
    
    lines(t, as.numeric(xd), col = "#8A8A8A", lwd = 2, lty = 2)
    abline(h = 0, col = "gray75", lty = 2)
    
    ticks <- seq(50, length(xd), by = 50)
    axis(1, at = ticks, labels = ticks)
  })
  
  
  acf_colors <- function(lags, vals, ci, s = 12, trend_max = 6) {
    significant <- abs(vals) > ci
    seasonal <- lags %% s == 0 & lags > 0
    trend <- lags >= 1 & lags <= trend_max
    
    ifelse(
      significant & seasonal, "#E69F00",      # amarillo
      ifelse(
        significant & trend, "#0072B2",       # azul
        "gray70"
      )
    )
  }
  
  output$plot_acf <- renderPlot({
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(mar = c(4, 4, 4, 1))
    
    x <- na.contiguous(as.numeric(x_diff()))
    ord <- orders()
    
    ac <- stats::acf(
      x,
      lag.max = input$lagmax %||% 24,
      plot = FALSE
    )
    
    # quitar lag 0
    lags <- as.numeric(ac$lag)[-1]
    vals <- as.numeric(ac$acf)[-1]
    
    ci <- 1.96 / sqrt(length(x))
    ylim <- range(c(-3/sqrt(length(x)), 3/sqrt(length(x)), vals))
    
    plot(
      lags, vals,
      type = "n",
      ylim = ylim,
      xaxt = "n",
      main = tr("acf_title", language()),
      xlab = "Lag",
      ylab = "ACF"
    )
    
    cols <- acf_colors(lags, vals, ci, s = ord$s, trend_max = 6)
    
    segments(
      x0 = lags, y0 = 0,
      x1 = lags, y1 = vals,
      lwd = 3,
      col = cols
    )
    
    abline(h = 0, lwd = 1.2)
    abline(h = c(-ci, ci), lty = 2, col = "darkgray", lwd = 2)
    
    axis(1, at = lags, labels = lags)
  })
  
  
  output$plot_pacf <- renderPlot({
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(mar = c(4, 4, 4, 1))
    
    x <- na.contiguous(as.numeric(x_diff()))
    ord <- orders()
    
    pc <- stats::pacf(
      x,
      lag.max = input$lagmax %||% 24,
      plot = FALSE
    )
    
    lags <- as.numeric(pc$lag)
    vals <- as.numeric(pc$acf)
    
    ci <- 1.96 / sqrt(length(x))
    ylim <- range(c(-3/sqrt(length(x)), 3/sqrt(length(x)), vals))
    
    plot(
      lags, vals,
      type = "n",
      ylim = ylim,
      xaxt = "n",
      main = tr("pacf_title", language()),
      xlab = "Lag",
      ylab = "PACF"
    )
    
    cols <- acf_colors(lags, vals, ci, s = ord$s, trend_max = 6)
    
    segments(
      x0 = lags, y0 = 0,
      x1 = lags, y1 = vals,
      lwd = 3,
      col = cols
    )
    
    abline(h = 0, lwd = 1.2)
    abline(h = c(-ci, ci), lty = 2, col = "darkgray", lwd = 2)
    
    axis(1, at = lags, labels = lags)
  })
  
  # --- Resumen + Data
  # output$sampleStats <- renderUI({
  #   xd <- as.numeric(x_diff())
  #   HTML(paste0(
  #     "<div style='margin-top:10px; padding:10px; border:1px solid #ddd; border-radius:10px; background:#fff;'>",
  #     "<b>", tr("sampleStats_title", language()), ":</b> ",
  #     "n=", length(xd),
  #     " | media=", round(mean(xd), 3),
  #     " | sd=", round(sd(xd), 3),
  #     "</div>"
  #   ))
  # })
  
  output$data <- renderTable({
    x <- x_sim()
    data.frame(t = seq_along(x), x = as.numeric(x))
  })
  
  output$download_data_text <- renderText(
    tr("download_data_text", language())
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("sarima_data_", language(), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      
      x <- x_sim()
      
      # Nombres de columnas según idioma
      datos <- switch(
        language(),
        ES = data.frame(Tiempo = seq_along(x), Valor = as.numeric(x)),
        EN = data.frame(Time   = seq_along(x), Value = as.numeric(x)),
        VAL = data.frame(Temps = seq_along(x), Valor = as.numeric(x))
      )
      
      # Escritura según estándar regional
      if (language() == "EN") {
        write.csv(datos, file, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        write.csv2(datos, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
  
  output$data <- renderTable({
    x <- x_sim()
    data.frame(t = seq_along(x), x = as.numeric(x))
  })
  
  output$guide_button <- renderText(
    tr("guide_button", language())
  )
  
  output$download_guide <- downloadHandler(
    filename = function() {
      paste0("guia_sarima_", language(), "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      guia <- switch(
        language(),
        ES  = file.path("www", "guia_sarima_ES.pdf"),
        EN  = file.path("www", "guia_sarima_EN.pdf"),
        VAL = file.path("www", "guia_sarima_VAL.pdf")
      )
      
      file.copy(guia, file, overwrite = TRUE)
    }
  )
}


# Create Shiny app ----
shinyApp(ui, server)
