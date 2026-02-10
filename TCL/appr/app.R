######################### LIBRERÍAS (PUEDES AÑADIR, NO BORRAR) #########################
library(shiny)
library(shinyjs)
library(tidyverse)
library(car)
library(agricolae)

if (FALSE) {
  library(munsell)
}

############################ SISTEMA DE TEXTOS Y TRADUCCIÓN ############################

showparams <- TRUE 

texts <- list( 
  title = c( 
    ES = "Teorema Central del Límite (TCL)", 
    EN = "Central Limit Theorem (CLT)", 
    VAL = "Teorema Central del Límit (TCL)" 
  ), 
  explanation = c( 
    ES = "Visualiza en tiempo real cómo se comporta la media muestral. Observa cómo el intervalo de confianza (líneas rojas) se estrecha a medida que aumentas N, 'atrapando' cada vez más medias muestrales.", 
    EN = "Visualize in real-time how the sample mean behaves. Observe how the confidence interval (red lines) narrows as you increase N, 'trapping' more and more sample means.", 
    VAL = "Visualitza en temps real com es comporta la mitja mostral. Observa com l'interval de confiança (línies roges) s'estreix a mesura que augmentes N, 'atrapant' cada vegada més mitges mostrals." 
  ), 
  button_parameters = c( 
    ES = "Parámetros", 
    EN = "Parameters", 
    VAL = "Paràmetres" 
  ), 
  text_downmenu= c( 
    ES = "Configuración", 
    EN= "Settings", 
    VAL= "Configuració" 
  ), 
  dropdown_label = c( 
    ES = "Distribución de partida", 
    EN = "Source Distribution", 
    VAL = "Distribució de partida" 
  ), 
  dist_norm = c( ES = "Normal (mu=0, sd=1)", EN = "Normal (mu=0, sd=1)", VAL = "Normal (mu=0, sd=1)" ),
  dist_exp  = c( ES = "Exponencial (rate=1)", EN = "Exponential (rate=1)", VAL = "Exponencial (rate=1)" ),
  dist_unif = c( ES = "Uniforme (min=0, max=10)", EN = "Uniform (min=0, max=10)", VAL = "Uniforme (min=0, max=10)" ),
  dist_pois = c( ES = "Poisson (lambda=3)", EN = "Poisson (lambda=3)", VAL = "Poisson (lambda=3)" ),
  
  slider_n = c( 
    ES = "Tamaño muestral (N)", 
    EN= "Sample size (N)", 
    VAL= "Grandària mostral (N)" 
  ), 
  slider_k = c( 
    ES = "Número de muestras (k)", 
    EN= "Number of samples (k)", 
    VAL= "Nombre de mostres (k)" 
  ), 
  
  seed_label = c(
    ES = "Semilla (Seed)",
    EN = "Seed",
    VAL = "Llavors (Seed)"
  ),
  resample_btn = c(
    ES = "Remuestrear",
    EN = "Resample",
    VAL = "Remostrejar"
  ),
  
  plot1_title = c(
    ES= "1. Población Original e Intervalo de Confianza de la Media",
    EN= "1. Original Population & Mean Confidence Interval", 
    VAL= "1. Població Original i Interval de Confiança de la Mitja"
  ),
  plot2_title = c(
    ES= "2. Muestras Extraídas (Puntos) y sus Medias (X)",
    EN= "2. Extracted Samples (Points) and their Means (X)", 
    VAL= "2. Mostres Extretes (Punts) i les seues Mitges (X)"
  ),
  plot3_title = c(
    ES= "3. Distribución de las Medias: Empírica vs Teórica",
    EN= "3. Distribution of Means: Empirical vs Theoretical", 
    VAL= "3. Distribució de les Mitges: Empírica vs Teòrica"
  ),
  
  shapiro_title = c(
    ES = "Test de Normalidad (Shapiro-Wilk) sobre las medias:",
    EN = "Normality Test (Shapiro-Wilk) on means:",
    VAL = "Test de Normalitat (Shapiro-Wilk) sobre les mitges:"
  ),
  
  shapiro_reject = c(
    ES = "Resultado: p < 0.05 → Rechazamos la hipótesis de normalidad.",
    EN = "Result: p < 0.05 → We reject the normality hypothesis.",
    VAL = "Resultat: p < 0.05 → Rebutgem la hipòtesi de normalitat."
  ),
  shapiro_accept = c(
    ES = "Resultado: p > 0.05 → No hay evidencias para rechazar la normalidad.",
    EN = "Result: p > 0.05 → No evidence to reject normality.",
    VAL = "Resultat: p > 0.05 → No hi ha evidències per a rebutjar la normalitat."
  ),
  
  didactic_note_title = c(
    ES = "Nota didáctica: Entendiendo el TCL y sus límites",
    EN = "Didactic Note: Understanding CLT and its limits",
    VAL = "Nota didàctica: Entenent el TCL i els seus límits"
  ),
  
  didactic_note_text = c(
    ES = "Recuerda que el TCL garantiza normalidad <b>asintótica</b> (N → ∞). Mientras N no se acerque a infinito, la distribución de las medias es solo una <i>aproximación</i> a la normal, aunque suele ser una aproximación excelente.<br><br>
          <b>¿Qué pasa con N baja?</b> El caso extremo es N=1: la distribución de medias es idéntica a la original. Si la población no es normal, las medias tampoco lo serán. Al aumentar N, los efectos aleatorios hacen que las medias se aglutinen en torno al centro, formando la campana de Gauss.<br><br>
          <b>¿Qué pasa si aumentamos 'k' (potencia)?</b> Si aumentas mucho 'k' (ej. k=1000) sin aumentar 'N', el test de Shapiro-Wilk gana mucha potencia. Se vuelve tan preciso que detecta que esa <i>aproximación</i> (por ser N finito) no es matemáticamente perfecta y rechaza la normalidad. No es que el TCL falle, es que el test detecta las pequeñas imperfecciones de no tener N infinito.<br><br>
          <b>Conclusión:</b> Aunque el límite es asintótico, la convergencia es muy rápida. Generalmente, se asume que con <b>N > 30</b>, es razonable afirmar que la distribución de las medias sigue una \\( N(\\mu, \\sigma/\\sqrt{N}) \\).",
    
    EN = "Remember that the CLT guarantees <b>asymptotic</b> normality (N → ∞). As long as N is not close to infinity, the distribution of means is only an <i>approximation</i> to the normal, although usually an excellent one.<br><br>
          <b>What happens with low N?</b> The extreme case is N=1: the distribution of means is identical to the original. If the population is not normal, the means won't be either. As N increases, random effects cause means to cluster around the center, forming the Gaussian bell.<br><br>
          <b>What happens if we increase 'k' (power)?</b> If you increase 'k' significantly (e.g., k=1000) without increasing 'N', the Shapiro-Wilk test gains a lot of power. It becomes so precise that it detects that the <i>approximation</i> (due to finite N) is not mathematically perfect and rejects normality. It's not that CLT fails, but that the test detects the small imperfections of not having infinite N.<br><br>
          <b>Conclusion:</b> Although the limit is asymptotic, convergence is very fast. Generally, it is assumed that with <b>N > 30</b>, it is reasonable to state that the distribution of means follows a \\( N(\\mu, \\sigma/\\sqrt{N}) \\).",
    
    VAL = "Recorda que el TCL garanteix normalitat <b>asimptòtica</b> (N → ∞). Mentre N no s'acoste a infinit, la distribució de les mitges és només una <i>aproximació</i> a la normal, encara que sol ser una aproximació excel·lent.<br><br>
          <b>Què passa amb N baixa?</b> El cas extrem és N=1: la distribució de mitges és idèntica a l'original. Si la població no és normal, les mitges tampoc ho seran. En augmentar N, els efectes aleatoris fan que les mitges s'aglutinen entorn del centre, formant la campana de Gauss.<br><br>
          <b>Què passa si augmentem 'k' (potència)?</b> Si augmentes molt 'k' (ex. k=1000) sense augmentar 'N', el test de Shapiro-Wilk guanya molta potència. Es torna tan precís que detecta que eixa <i>aproximació</i> (per ser N finit) no és matemàticament perfecta i rebutja la normalitat. No és que el TCL falle, és que el test detecta les xicotetes imperfeccions de no tindre N infinit.<br><br>
          <b>Conclusió:</b> Encara que el límit és asimptòtic, la convergència és molt ràpida. Generalment, s'assumeix que amb <b>N > 30</b>, és raonable afirmar que la distribució de les mitges segueix una \\( N(\\mu, \\sigma/\\sqrt{N}) \\)."
  ),
  
  exercise1_title = c(
    ES = "Ejercicio Conceptual 1: Intervalos de Confianza",
    EN = "Conceptual Exercise 1: Confidence Intervals",
    VAL = "Exercici Conceptual 1: Intervals de Confiança"
  ),
  
  exercise1_text = c(
    ES = "Pon a prueba tu intuición sobre la cobertura del intervalo:<br>
          1. Selecciona la distribución <b>Uniforme</b>, fija <b>N = 50</b> y <b>k = 20</b>. Observa las líneas rojas discontinuas. ¿Qué representan?<br>
          Respuesta: <span class='spoiler'>Representan los límites teóricos donde esperamos encontrar el 95% de las medias muestrales (\\(\\mu \\pm 1.96 \\frac{\\sigma}{\\sqrt{N}}\\)).</span><br>
          2. Si hemos extraído <b>k = 20</b> muestras, ¿cuántas medias muestrales ('X') esperarías ver FUERA de las líneas rojas?<br>
          Respuesta: <span class='spoiler'>Como el nivel de confianza es 95%, esperamos que el 5% de las medias queden fuera. El 5% de 20 es 1. Así que deberías ver aprox. 1 'X' fuera.</span><br>
          3. Pulsa el botón <b>'Remuestrear'</b> varias veces. ¿Ocurre siempre que queda exactamente 1 fuera?<br>
          Respuesta: <span class='spoiler'>No siempre. Es un proceso aleatorio. A veces verás 0, a veces 2 o 3, pero 'a la larga' el promedio será de 1 muestra fuera por cada 20.</span>",
    
    EN = "Test your intuition on interval coverage:<br>
          1. Select the <b>Uniform</b> distribution, set <b>N = 50</b> and <b>k = 20</b>. Observe the dashed red lines. What do they represent?<br>
          Answer: <span class='spoiler'>They represent the theoretical limits where we expect to find 95% of sample means (\\(\\mu \\pm 1.96 \\frac{\\sigma}{\\sqrt{N}}\\)).</span><br>
          2. If we extracted <b>k = 20</b> samples, how many sample means ('X') would you expect to see OUTSIDE the red lines?<br>
          Answer: <span class='spoiler'>Since the confidence level is 95%, we expect 5% of means to fall outside. 5% of 20 is 1. So you should see approx. 1 'X' outside.</span><br>
          3. Click the <b>'Resample'</b> button several times. Does exactly 1 mean fall outside every time?<br>
          Answer: <span class='spoiler'>Not always. It is a random process. Sometimes you will see 0, sometimes 2 or 3, but 'in the long run' the average will be 1 sample outside for every 20.</span>",
    
    VAL = "Posa a prova la teua intuïció sobre la cobertura de l'interval:<br>
          1. Selecciona la distribució <b>Uniforme</b>, fixa <b>N = 50</b> i <b>k = 20</b>. Observa les línies roges discontínues. Què representen?<br>
          Resposta: <span class='spoiler'>Representen els límits teòrics on esperem trobar el 95% de les mitges mostrals (\\(\\mu \\pm 1.96 \\frac{\\sigma}{\\sqrt{N}}\\)).</span><br>
          2. Si hem extret <b>k = 20</b> mostres, quantes mitges mostrals ('X') esperaries vore FORA de les línies roges?<br>
          Resposta: <span class='spoiler'>Com el nivell de confiança és 95%, esperem que el 5% de les mitges queden fora. El 5% de 20 és 1. Així que hauries de vore aprox. 1 'X' fora.</span><br>
          3. Prem el botó <b>'Remostrejar'</b> diverses vegades. Ocorre sempre que queda exactament 1 fora?<br>
          Resposta: <span class='spoiler'>No sempre. És un procés aleatori. A vegades voràs 0, a vegades 2 o 3, però 'a la llarga' la mitjana serà d'1 mostra fora per cada 20.</span>"
  ),
  
  exercise2_title = c(
    ES = "Ejercicio Conceptual 2: Remuestreo y Estabilidad",
    EN = "Conceptual Exercise 2: Resampling and Stability",
    VAL = "Exercici Conceptual 2: Remostreig i Estabilitat"
  ),
  
  exercise2_text = c(
    ES = "Pon a prueba la convergencia a la normalidad:<br>
          1. Selecciona la distribución <b>Exponencial</b> y fija <b>k = 100</b>.<br>
          2. Pon <b>N = 10</b>. Pulsa el botón <b>'Remuestrear'</b> varias veces seguidas observando el p-valor. ¿Qué ocurre?<br>
          Respuesta: <span class='spoiler'>Verás que el test rechaza la normalidad (p < 0.05) con frecuencia. Con N=10 la asimetría de la exponencial es aún demasiado fuerte.</span><br>
          3. Ahora sube a <b>N = 50</b> y sigue remuestreando. ¿Es la distribución empírica compatible con la normal más veces ahora?<br>
          Respuesta: <span class='spoiler'>Sí. Al aumentar N, el Teorema Central del Límite actúa con más fuerza, la distribución de medias se simetriza y el test acepta la normalidad casi siempre.</span>",
    
    EN = "Test the convergence to normality:<br>
          1. Select the <b>Exponential</b> distribution and set <b>k = 100</b>.<br>
          2. Set <b>N = 10</b>. Click the <b>'Resample'</b> button several times watching the p-value. What happens?<br>
          Answer: <span class='spoiler'>You will see the test frequently rejects normality (p < 0.05). At N=10 the skewness of the exponential is still too strong.</span><br>
          3. Now increase to <b>N = 50</b> and keep resampling. Is the empirical distribution compatible with normality more often now?<br>
          Answer: <span class='spoiler'>Yes. As N increases, the Central Limit Theorem acts more strongly, the distribution of means becomes symmetric, and the test accepts normality almost always.</span>",
    
    VAL = "Posa a prova la convergència a la normalitat:<br>
          1. Selecciona la distribució <b>Exponencial</b> i fixa <b>k = 100</b>.<br>
          2. Posa <b>N = 10</b>. Prem el botó <b>'Remostrejar'</b> diverses vegades seguidas observant el p-valor. Què ocorre?<br>
          Resposta: <span class='spoiler'>Voràs que el test rebutja la normalitat (p < 0.05) amb freqüència. Amb N=10 l'asimetria de l'exponencial és encara massa forta.</span><br>
          3. Ara puja a <b>N = 50</b> i segueix remostrejant. És la distribució empírica compatible amb la normal més vegades ara?<br>
          Resposta: <span class='spoiler'>Sí. En augmentar N, el Teorema Central del Límit actua amb més força, la distribució de mitges se simetritza i el test accepta la normalitat quasi sempre.</span>"
  ),
  
  credits= c( 
    ES= "STATIO es un Proyecto de Innovación y Mejora Educativa (PIME/25-26/562) desarrollado por el DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.", 
    EN= "STATIO is an Educational Innovation and Improvement Project (PIME/25-26/562) developed by the DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.", 
    VAL= "STATIO és un Projecte d'Innovació i Millora Educativa (*PIME/25-26/562) desenrotllat pel DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>." 
  )
)

tr <- function(id, lang) { texts[[id]][[lang]] }

##################### USER INTERFACE ######################################

ui <- fluidPage(
  useShinyjs(), 
  
  absolutePanel(
    top = 10, right = 10, fixed = TRUE,
    actionButton("lang_es", "ES"),
    actionButton("lang_en", "EN"),
    actionButton("lang_va", "VAL")
  ),
  
  tags$head(
    tags$script(HTML("
      $(document).on('click', '.spoiler', function() {
        $(this).toggleClass('revealed');
      });
    ")),
    tags$style(HTML("
        #sidebarWrapper {
          width: 300px; background: #f7f7f7; padding: 15px; border-right: 1px solid #ddd;
          position: fixed; top: 0; bottom: 0; left: 0; overflow-y: auto;
          transition: transform .3s ease; z-index: 2000; transform: translateX(-100%);
        }
        #sidebarWrapper:not(.closed) { transform: translateX(0); }
        #contentWrapper { transition: margin-left .3s ease; margin-left: 0px; }
        #contentWrapper.shifted { margin-left: 300px; }
        #toggleSidebar { position: fixed; top: 10px; left: 10px; z-index: 3000; }
        .logo-row { display: flex; gap: 6px; }
        .logo-img { height: 80px; transition: height 0.3s ease; }
        
        /* ESTILOS DE CAJAS INFORMATIVAS */
        .result-box { 
          background-color: #fff; 
          border: 1px solid #ccc; 
          padding: 15px; 
          border-radius: 8px; 
          margin-top: 30px; 
          box-shadow: 0 2px 5px rgba(0,0,0,0.05); 
        }
        
        .didactic-box { 
          background-color: #e8f4f8; 
          border-left: 5px solid #4a90e2; 
          padding: 15px; 
          border-radius: 4px; 
          margin-top: 15px; 
          font-size: 0.95em; 
          line-height: 1.5; 
          color: #333; 
        }
        
        .exercise1-box { 
          background-color: #e8f5e9; /* Verde muy claro */
          border-left: 5px solid #4caf50; /* Verde */
          padding: 15px; 
          border-radius: 4px; 
          margin-top: 15px; 
          font-size: 0.95em; 
          line-height: 1.5; 
          color: #333; 
        }
        
        .exercise2-box { 
          background-color: #fff8e1; /* Ámbar muy claro */
          border-left: 5px solid #ffc107; /* Ámbar/Naranja */
          padding: 15px; 
          border-radius: 4px; 
          margin-top: 15px; 
          font-size: 0.95em; 
          line-height: 1.5; 
          color: #333; 
        }
        
        /* ESTILO SPOILER */
        .spoiler {
          background-color: #000;
          color: #000;
          padding: 0 4px;
          border-radius: 3px;
          cursor: pointer;
          user-select: none;
        }
        
        .spoiler:hover {
          background-color: #333;
        }
        
        .spoiler.revealed {
          background-color: transparent;
          color: #000;
          font-weight: bold;
          cursor: text;
          user-select: text;
        }

        .exercise1-box strong { color: #2e7d32; }
        .exercise2-box strong { color: #d39e00; }
        .didactic-box strong { color: #2c5e8f; }
    "))
  ),
  
  if(showparams) {
    actionButton("toggleSidebar", textOutput("button_parameters"))
  },
  
  div(id="sidebarWrapper", style="padding-top: 50px;",
      if(showparams) {
        div(
          h4(textOutput("text_downmenu")), 
          
          # Inputs de control
          uiOutput("dropdown_ui"),
          uiOutput("slider_n_ui"), 
          uiOutput("slider_k_ui"),
          
          hr(),
          # Control de semilla
          uiOutput("seed_ui"),
          actionButton("resample_btn", textOutput("resample_btn_text"), 
                       style="width:100%; background-color:#4a90e2; color:white; font-weight:bold;")
        )
      }
  ),
  
  div(id="contentWrapper", class = "shifted",
      
      div(style="padding-top:50px; margin-bottom:30px",
          h2(textOutput("title"), align="center"),
          div(style="display:flex; justify-content:center;",
              div(style="border:2px solid #4a90e2; border-radius:12px; padding:12px; 
                         max-width:800px; background:white; text-align:center;",
                  uiOutput("explanation")
              )
          )
      ),
      
      # CONTENIDO UNIFICADO EN UNA PÁGINA
      div(style="padding: 0 20px 50px 20px;",
          
          # --- PLOT 1: POBLACIÓN ---
          h4(textOutput("plot1_label"), style="text-align:center; color:#444; margin-top:20px; font-weight:bold;"),
          plotOutput("Plot_Population", height = "250px"),
          
          # --- PLOT 2: MUESTRAS (Altura dinámica) ---
          h4(textOutput("plot2_label"), style="text-align:center; color:#444; margin-top:30px; font-weight:bold;"),
          uiOutput("dynamic_plot_samples_ui"),
          
          # --- PLOT 3: DENSIDADES ---
          h4(textOutput("plot3_label"), style="text-align:center; color:#444; margin-top:30px; font-weight:bold;"),
          plotOutput("Plot_Density_Compare", height = "350px"),
          
          # RESULTADOS Y NOTAS
          uiOutput("shapiro_res"),
          
          # CRÉDITOS
          div(style="margin-top:60px; text-align:center; border-top: 1px solid #eee; padding-top:20px;",
              tags$img(src='DEIOAC.png', height=60, onerror="this.style.display='none'"), 
              tags$img(src='UPV.png', height=60, onerror="this.style.display='none'"),
              div(style="margin-top:15px;", htmlOutput("creditos"))
          )
      )
  )
)


############################ SERVER #######################################

server <- function(input, output, session) {
  
  # --- UI INTERACTION ---
  observeEvent(input$toggleSidebar, {
    shinyjs::toggleClass(id = "sidebarWrapper", class = "closed")
    shinyjs::toggleClass(id = "contentWrapper", class = "shifted")
    shinyjs::runjs("setTimeout(function() { $(window).trigger('resize'); }, 350);")
  })
  
  language <- reactiveVal("ES") 
  observeEvent(input$lang_es, { language("ES") }) 
  observeEvent(input$lang_en, { language("EN") }) 
  observeEvent(input$lang_va, { language("VAL") })
  
  # --- TEXTOS REACTIVOS ---
  output$title <- renderText({ tr("title", language()) })
  output$explanation <- renderUI({HTML(tr("explanation", language())) })
  output$button_parameters <- renderText({tr("button_parameters", language())})
  output$text_downmenu <- renderText({ tr("text_downmenu", language()) })
  output$creditos <- renderUI({ HTML(tr("credits", language())) })
  output$resample_btn_text <- renderText({ tr("resample_btn", language()) })
  
  output$plot1_label <- renderText({ tr("plot1_title", language()) })
  output$plot2_label <- renderText({ tr("plot2_title", language()) })
  output$plot3_label <- renderText({ tr("plot3_title", language()) })
  
  
  # --- CONTROLES PARÁMETROS ---
  output$dropdown_ui <- renderUI({ 
    selectInput("dist", tr("dropdown_label", language()), 
                choices = setNames(c("norm", "exp", "unif", "pois"), 
                                   c(tr("dist_norm", language()), tr("dist_exp", language()), tr("dist_unif", language()), tr("dist_pois", language()))))
  })
  
  # N de 1 a 50 (Didáctico)
  output$slider_n_ui <- renderUI({ sliderInput("n", tr("slider_n", language()), min = 1, max = 50, value = 10, step=1) })
  
  # k de 1 a 100
  output$slider_k_ui <- renderUI({ sliderInput("k", tr("slider_k", language()), min = 1, max = 100, value = 20, step=1) })
  
  # Semilla input
  output$seed_ui <- renderUI({ 
    numericInput("seed_val", tr("seed_label", language()), value = 1234, min = 0, step = 1) 
  })
  
  # Lógica del botón Remuestrear: Genera un nuevo número aleatorio y actualiza el input de semilla
  observeEvent(input$resample_btn, {
    new_seed <- sample(1:9999, 1)
    updateNumericInput(session, "seed_val", value = new_seed)
  })
  
  
  # --- DATOS Y CÁLCULOS ---
  simulation_data <- reactive({
    req(input$n, input$k, input$dist, input$seed_val)
    n <- input$n
    k <- input$k
    type <- input$dist
    
    # Usar la semilla del input (que cambia con el botón remuestrear)
    set.seed(input$seed_val) 
    
    # Configuración según distribución
    if(type == "norm") {
      vals <- matrix(rnorm(n*k, 0, 1), nrow=n, ncol=k)
      theo_mean <- 0; theo_sd <- 1
      xlims <- c(-4, 4)
      max_dens_pop <- 0.4
    } else if (type == "exp") {
      vals <- matrix(rexp(n*k, 1), nrow=n, ncol=k)
      theo_mean <- 1; theo_sd <- 1
      xlims <- c(0, 5) 
      max_dens_pop <- 1
    } else if (type == "unif") {
      vals <- matrix(runif(n*k, 0, 10), nrow=n, ncol=k)
      theo_mean <- 5; theo_sd <- sqrt((10-0)^2/12)
      xlims <- c(-1, 11)
      max_dens_pop <- 0.1
    } else { # pois
      vals <- matrix(rpois(n*k, 3), nrow=n, ncol=k)
      theo_mean <- 3; theo_sd <- sqrt(3)
      xlims <- c(0, 10)
      max_dens_pop <- 0.25
    }
    
    means_vec <- colMeans(vals)
    
    # Intervalo de confianza (error estándar)
    sem <- theo_sd / sqrt(n)
    ci_lower <- theo_mean - 1.96 * sem
    ci_upper <- theo_mean + 1.96 * sem
    
    # Colores pastel continuos
    palette_fn <- colorRampPalette(c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD"))
    sample_colors <- palette_fn(k)
    
    list(raw = vals, means = means_vec, 
         params = list(mean=theo_mean, sd=theo_sd, n=n, k=k, type=type, xlims=xlims, 
                       ci_lower=ci_lower, ci_upper=ci_upper, sem=sem, max_dens=max_dens_pop),
         colors = sample_colors)
  })
  
  
  # --- PLOT 1: POBLACIÓN E INTERVALOS ---
  output$Plot_Population <- renderPlot({
    req(simulation_data())
    d <- simulation_data()
    p_params <- d$params
    
    p <- ggplot() + theme_minimal() + xlim(p_params$xlims) + 
      labs(y="Densidad", x=NULL) 
    
    
    # Dibujar distribución poblacional
    type <- p_params$type
    if(type == "norm") p <- p + stat_function(fun=dnorm, args=list(0,1), size=1, color="black")
    else if(type == "exp") p <- p + stat_function(fun=dexp, args=list(1), size=1, color="black")
    else if(type == "unif") p <- p + stat_function(fun=dunif, args=list(0,10), size=1, color="black")
    else {
      df_p <- data.frame(x=0:15, y=dpois(0:15, 3))
      p <- p + geom_segment(data=df_p, aes(x=x, xend=x, y=0, yend=y)) + geom_point(data=df_p, aes(x,y))
    }
    
    # Añadir las líneas de IC verticales
    p <- p + geom_vline(xintercept = c(p_params$ci_lower, p_params$ci_upper), 
                        color="red", linetype="dashed", size=0.5, alpha=0.5) +
      geom_vline(xintercept = p_params$mean, color="red", size=0.5, alpha=0.5)
    
    # Añadir flechas y anotación
    y_arrow <- p_params$max_dens * 0.5
    
    # parse=TRUE para notación matemática
    label_expr <- paste0("'1.96' %.% sigma / sqrt(", p_params$n, ")")
    
    p <- p + annotate("segment", x = p_params$ci_lower, xend = p_params$ci_upper, 
                      y = y_arrow, yend = y_arrow, 
                      colour = "red", arrow = arrow(length = unit(0.2, "cm"), ends = "both")) +
      annotate("text", x = p_params$mean, y = y_arrow * 1.2, 
               label = label_expr, parse = TRUE,
               color="red", fontface="bold")
    
    p + theme(axis.text.x = element_blank()) 
  })
  
  
  # --- PLOT 2: MUESTRAS (STRIP CHART) ---
  
  # Altura dinámica: ~25px por muestra, mínimo 300px
  output$dynamic_plot_samples_ui <- renderUI({
    req(input$k)
    pixel_height <- max(300, input$k * 25) 
    plotOutput("Plot_Samples_Detail", height = paste0(pixel_height, "px"))
  })
  
  output$Plot_Samples_Detail <- renderPlot({
    req(simulation_data())
    d <- simulation_data()
    
    # Preparar datos
    plot_df <- data.frame()
    mean_df <- data.frame()
    
    # Mostrar TODAS las k muestras
    for(i in 1:d$params$k) {
      tmp <- data.frame(val = d$raw[,i], sample_id = i, col = d$colors[i])
      plot_df <- rbind(plot_df, tmp)
      mean_df <- rbind(mean_df, data.frame(val = d$means[i], sample_id = i, col = d$colors[i]))
    }
    
    ggplot() +
      # Puntos de la muestra menos transparentes (más opacos, alpha=0.6)
      geom_point(data=plot_df, aes(x=val, y=factor(sample_id), color=I(col)), shape=1, size=2, alpha=0.9) +
      # Medias (X) casi opacas (alpha=0.9)
      geom_point(data=mean_df, aes(x=val, y=factor(sample_id), color=I(col)), shape=4, size=5, stroke=2, alpha=0.9) +
      
      # Líneas de IC verticales
      geom_vline(xintercept = c(d$params$ci_lower, d$params$ci_upper), 
                 color="red", linetype="dashed", size=0.5, alpha=0.5) +
      geom_vline(xintercept = d$params$mean, color="red", size=0.5, alpha=0.5) +
      
      xlim(d$params$xlims) +
      labs(x="Valor", y="Muestra #") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  
  # --- PLOT 3: DISTRIBUCIÓN MEDIAS (DENSIDADES) ---
  output$Plot_Density_Compare <- renderPlot({
    req(simulation_data())
    d <- simulation_data()
    
    theo_sd_sampling <- d$params$sd / sqrt(d$params$n)
    
    ggplot() +
      # 1. Densidad Empírica (Rellena)
      geom_density(data=data.frame(m=d$means), aes(x=m, fill="Empírica"), 
                   alpha=0.4, color="#1a508b") +
      
      # 2. Densidad Teórica Normal (Línea Roja)
      stat_function(data=data.frame(x=d$params$xlims), aes(x=x, color="Teórica"), 
                    fun = dnorm, args = list(mean=d$params$mean, sd=theo_sd_sampling), 
                    size=1.2, linetype="solid") +
      
      # Líneas de referencia
      geom_vline(xintercept = c(d$params$ci_lower, d$params$ci_upper), 
                 color="red", linetype="dashed", size=0.5, alpha=0.5) +
      
      xlim(d$params$xlims) +
      labs(x = "Media Muestral", y = "Densidad", fill="", color="") +
      scale_fill_manual(values=c("Empírica"="#4a90e2")) +
      scale_color_manual(values=c("Teórica"="red")) +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  
  # --- SHAPIRO WILK + NOTAS ---
  output$shapiro_res <- renderUI({
    req(simulation_data())
    means <- simulation_data()$means
    
    st <- shapiro.test(means)
    pval <- st$p.value
    
    if(pval > 0.05) {
      res_color <- "green"
      res_decision <- tr("shapiro_accept", language())
    } else {
      res_color <- "#d9534f" # Rojo suave
      res_decision <- tr("shapiro_reject", language())
    }
    
    withMathJax(HTML(paste0(
      # CAJA 1: RESULTADO TÉCNICO
      "<div class='result-box'>",
      "<h5>", tr("shapiro_title", language()), "</h5>",
      "<p style='font-family: monospace; font-size: 1.2em;'>",
      "W = ", round(st$statistic, 4), ", ",
      "<span style='color:", res_color, "; font-weight:bold'>p-value = ", format.pval(pval, digits=4), "</span>",
      "</p>",
      "<p style='font-weight:bold; color:", res_color, "'>", res_decision, "</p>",
      "</div>",
      
      # CAJA 2: NOTA DIDÁCTICA (Azul)
      "<div class='didactic-box'>",
      "<h5 style='margin-top:0; font-weight:bold;'>", tr("didactic_note_title", language()), "</h5>",
      tr("didactic_note_text", language()),
      "</div>",
      
      # CAJA 3: EJERCICIO CONCEPTUAL 1 (Verde) - Intervalos
      "<div class='exercise1-box'>",
      "<h5 style='margin-top:0; font-weight:bold;'>", tr("exercise1_title", language()), "</h5>",
      tr("exercise1_text", language()),
      "</div>",
      
      # CAJA 4: EJERCICIO CONCEPTUAL 2 (Ámbar/Naranja) - Normalidad
      "<div class='exercise2-box'>",
      "<h5 style='margin-top:0; font-weight:bold;'>", tr("exercise2_title", language()), "</h5>",
      tr("exercise2_text", language()),
      "</div>"
    )))
  })
}

shinyApp(ui, server)