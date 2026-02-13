# Librerías------------------------------------------------------------------
# shiny       → Construcción de la interfaz y del servidor
# shinyjs     → Permite usar JS para mostrar/ocultar paneles y dinamismo
# tidyverse   → Para manipulación cómoda de datos
# car/ agricolae → Tests estadísticos opcionales (ANOVA, etc.)
library(shiny)
library(shinyjs)
library(ggplot2)
# library(car)
# library(agricolae)
if (FALSE) {
  library(munsell)
}

# Poner el punto como decimal
options(OutDec=".")

# Sístema de textos y traducción---------------------------------------------
# función de traducción: dado el ID y el idioma, devuelve el texto correcto--
tr <- function(id, lang) { texts[[id]][[lang]] }

# showparams controla si se muestra el panel lateral de parámetros
showparams <- TRUE # Cambiarlo a FALSE si no se necesita barra de parámetros.

# Lista de textos en cada idioma.
# Cada entrada tiene ES / EN / VAL.
# Añadir tantas traducciones al dict como sean necesarias. 

texts <- list( 
## Común
    title = c( 
    ES = "Contraste de hipótesis: p-valor y región crítica", 
    EN = "Hypothesis testing: p-value and critical region", 
    VAL = "Contrast d'hipòtesis: p-valor i regió crítica" 
  ), 

  explanation = c(
    ES = "Simulador interactivo para visualizar el p-valor, la región crítica y la decisión de un contraste de hipótesis según el nivel de significación.",
    
    EN = "Interactive simulator to visualise the p-value, the critical region and the decision of a hypothesis test according to the significance level.",
    
    VAL = "Simulador interactiu per visualitzar el p-valor, la regió crítica i la decisió d’un contrast d’hipòtesis segons el nivell de significació."
  ),  
  
  button_parameters = c( 
    ES = "Parámetros", 
    EN = "Parameters", 
    VAL = "Paràmetres" 
  ), 
  
  text_downmenu= c( 
    ES = "Selección de parámetros", 
    EN=  "Parameter selection", 
    VAL= "Selecció de paràmetres" 
  ),
  
  dropdown_label = c(
    ES = "Tipo de contraste",
    EN = "Type of test",
    VAL = "Tipus de contrast"
  ),
  
  slider_alpha = c(
    ES = "Nivel de significación α",
    EN = "Significance level α",
    VAL = "Nivell de significació α"
  ),  

  interpretacion = c(
    ES = "La curva representa la distribución del estadístico de contraste bajo la hipótesis nula. La línea discontinua señala el valor observado en la muestra. El área roja corresponde al p-valor: la probabilidad de obtener un resultado tan extremo o más extremo que el observado, asumiendo que la hipótesis nula es cierta. El área azul delimita la región de rechazo determinada por el nivel de significación α. Si el p-valor es menor que α, se rechaza la hipótesis nula porque el resultado observado es suficientemente improbable bajo dicha hipótesis.",
    
    EN = "The curve represents the distribution of the test statistic under the null hypothesis. The dashed line marks the value observed in the sample. The red area corresponds to the p-value: the probability of obtaining a result as extreme or more extreme than the observed one, assuming the null hypothesis is true. The blue area delimits the rejection region determined by the significance level α. If the p-value is smaller than α, the null hypothesis is rejected because the observed result is sufficiently unlikely under that hypothesis.",
    
    VAL = "La corba representa la distribució de l'estadístic de contrast sota la hipòtesi nul·la. La línia discontínua assenyala el valor observat en la mostra. L'àrea roja correspon al p-valor: la probabilitat d'obtindre un resultat tan extrem o més extrem que l'observat, assumint que la hipòtesi nul·la és certa. L'àrea blava delimita la regió de rebuig determinada pel nivell de significació α. Si el p-valor és menor que α, es rebutja la hipòtesi nul·la perquè el resultat observat és prou improbable sota aquesta hipòtesi."
  ),  
  
  credits= c( 
    ES= "STATIO es un Proyecto de Innovación y Mejora Educativa (PIME/25-26/562) 
    desarrollado por el DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.", 
    EN= "STATIO is an Educational Innovation and Improvement Project (PIME/25-26/562) 
    developed by the DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.", 
    VAL= "STATIO és un Projecte d'Innovació i Millora Educativa (*PIME/25-26/562) 
    desenrotllat pel DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>." 
  ),
  
## tab 1---------------------------------------------------------------------
panel1 = c(
  ES = "p-valor",
  EN = "p-value",
  VAL = "p-valor"
),

title_tab1 = c(
  ES = "El concepto del p-valor",
  EN = "The concept of the p-value",
  VAL = "El concepte del p-valor"
),

option1_tab1 = c(
  ES = "Cola derecha",
  EN = "Right-tailed",
  VAL = "Cua dreta"
),

option2_tab1 = c(
  ES = "Cola izquierda",
  EN = "Left-tailed",
  VAL = "Cua esquerra"
),

option3_tab1 = c(
  ES = "Bilateral",
  EN = "Two-sided",
  VAL = "Bilateral"
),

slider_z = c(
  ES = "Estadístico z observado",
  EN = "Observed z statistic",
  VAL = "Estadístic z observat"
),

plot_y_tab1 = c(
  ES= "Densidad",
  EN= "Density",
  VAL= "Densitat"
),

res_contrast = c(
  ES = "Resultados del contraste",
  EN = "Test results",
  VAL = "Resultats del contrast"
),

reject_model = c(
  ES = "→ Rechazar H0 (el modelo es significativo)",
  EN = "→ Reject H0 (model is significant)",
  VAL = "→ Rebutjar H0 (el model és significatiu)"
),

noreject_model = c(
  ES = "→ No rechazar H0 (el modelo no es significativo)",
  EN = "→ Do not reject H0 (model is not significant)",
  VAL = "→ No rebutjar H0 (el model no és significatiu)"
),

reject_subset = c(
  ES = "→ Rechazar H0 (el conjunto de variables es significativo)",
  EN = "→ Reject H0 (subset of variables is significant)",
  VAL = "→ Rebutjar H0 (el conjunt de variables és significatiu)"
),

noreject_subset = c(
  ES = "→ No rechazar H0 (el conjunto no es significativo)",
  EN = "→ Do not reject H0 (subset is not significant)",
  VAL = "→ No rebutjar H0 (el conjunt no és significatiu)"
),

reject = c(
  ES = "Se rechaza H0",
  EN = "Reject H0",
  VAL = "Es rebutja H0"
),

not_reject = c(
  ES = "No se rechaza H0",
  EN = "Do not reject H0",
  VAL = "No es rebutja H0"
),

subtitle_graph_tab1 = c(
  ES = "Descripción del gráfico",
  EN = "Graph description",
  VAL = "Descripció del gràfic"
),

subtitle_results_tab1 = c(
  ES = "Resultados del contraste",
  EN = "Test results",
  VAL = "Resultats del contrast"
),

subtitle_econometric_tab1 = c(
  ES = "Interpretación econométrica",
  EN = "Econometric interpretation",
  VAL = "Interpretació economètrica"
),

## tab 2---------------------------------------------------------------------
panel2 = c(
  ES = "Test t (coeficiente)",
  EN = "t test (coefficient)",
  VAL = "Test t (coeficient)"
),

title_tab2 = c(
  ES = "Contraste sobre un parámetro",
  EN = "Test on a coefficient",
  VAL = "Contrast sobre un paràmetre"
),

option1_tab2 = c(
  ES = "Cola derecha (H1: β > 0)",
  EN = "Right-tailed (H1: β > 0)",
  VAL = "Cua dreta (H1: β > 0)"
),

option2_tab2 = c(
  ES = "Cola izquierda (H1: β < β₀)",
  EN = "Left-tailed (H1: β < β₀)",
  VAL = "Cua esquerra (H1: β < β₀)"
),

option3_tab2 = c(
  ES = "Bilateral (H1: β ≠ β₀)",
  EN = "Two-sided (H1: β ≠ β₀)",
  VAL = "Bilateral (H1: β ≠ β₀)"
),

beta_tab2 = c(
  ES = "Estimación del coeficiente (β̂)",
  EN = "Coefficient Estimate (β̂)",
  VAL = "Estimació del coeficient (β̂)"
),

beta0_tab2 = c(
  ES = "Valor hipotetizado del coeficiente (β₀)",
  EN = "Hypothesised coefficient value (β₀)",
  VAL = "Valor hipotetitzat del coeficient (β₀)"
),

se_tab2 = c(
  ES = "Error estándar del coeficiente (se)",
  EN = "Standard error of the coefficient (se)",
  VAL = "Error estàndard del coeficient (se)"
),

n_obs_tab2 = c(
  ES = "Número de observaciones (n)",
  EN = "Number of observations (n)",
  VAL = "Número d'observacions (n)"
),

k_var_tab2 = c(
  ES = "Número de variables explicativas (k)",
  EN = "Number of explanatory variables (k)",
  VAL = "Número de variables explicatives (k)"
),

subtitle_graph_tab2 = c(
  ES = "Descripción del gráfico",
  EN = "Graph description",
  VAL = "Descripció del gràfic"
),

subtitle_results_tab2 = c(
  ES = "Resultados del contraste",
  EN = "Test results",
  VAL = "Resultats del contrast"
),

subtitle_econometric_tab2 = c(
  ES = "Interpretación econométrica",
  EN = "Econometric interpretation",
  VAL = "Interpretació economètrica"
),

interpretation_tab2 = c(
  ES = "Este contraste se utiliza para evaluar si un coeficiente de regresión es distinto a β₀.<br><br>
H0: β = β₀ (Cuando β₀ = 0 → H0: la variable no tiene efecto)<br>",
  
  EN = "This test evaluates whether a regression coefficient differs from β₀.<br><br>
H0: β = β₀ (When β₀ = 0 → H0: the variable has no effect)<br>",
  
  VAL = "Aquest contrast avalua si un coeficient de regressió és diferent de β₀.<br><br>
H0: β = β₀ (Quan β₀ = 0 → H0: la variable no té efecte)<br>"
),

significant_tab2 = c(
  ES = "Si el p-valor es pequeño, el coeficiente se considera estadísticamente significativo.",
  EN = "If the p-value is small, the coefficient is considered statistically significant.",
  VAL = "Si el p-valor és xicotet, el coeficient es considera estadísticament significatiu."
),


decision_rechazar_tab2 = c(
  ES = "→  Rechazar H0.",
  EN = "→  Reject H0.",
  VAL = "→  Rebutjar H0."
),

decision_no_rechazar_tab2 = c(
  ES = "→  No rechazar H0.",
  EN = "→  Do not reject H0.",
  VAL = "→  No rebutjar H0."
),

## tab 3---------------------------------------------------------------------
panel3 = c(
  ES = "Test F (modelo)",
  EN = "F test (model)",
  VAL = "Test F (model)"
),

title_tab3 = c(
  ES = "Contraste sobre todo el modelo",
  EN = "Test on the whole model",
  VAL = "Contrast sobre tot el model"
),

r2_tab3 = c(
  ES = "Coeficiente de determinación del modelo (R²)",
  EN = "Coefficient of determination of the model, (R²)",
  VAL = "Coeficient de determinació del model, (R²)"
),

n_obs_tab3 = c(
  ES = "Número de observaciones (n)",
  EN = "Number of observations (n)",
  VAL = "Número d'observacions (n)"
),

k_var_tab3 = c(
  ES = "Número de variables explicativas (k)",
  EN = "Number of explanatory variables (k)",
  VAL = "Número de variables explicatives (k)"
),

subtitle_graph_tab3 = c(
  ES = "Descripción del gráfico",
  EN = "Graph description",
  VAL = "Descripció del gràfic"
),

subtitle_results_tab3 = c(
  ES = "Resultados del contraste",
  EN = "Test results",
  VAL = "Resultats del contrast"
),

subtitle_econometric_tab3 = c(
  ES = "Interpretación econométrica",
  EN = "Econometric interpretation",
  VAL = "Interpretació economètrica"
),

interpretation_tab3 = c(
  ES = "Este contraste evalúa si el conjunto de variables explicativas del modelo tiene capacidad explicativa conjunta.<br><br>
H0: β1 = β2 = ... = βk = 0<br>
(el modelo no tiene capacidad explicativa)<br><br>
H1: Al menos un coeficiente es distinto de cero<br>
(el modelo tiene capacidad explicativa)",
  
  EN = "This test evaluates whether the explanatory variables jointly have explanatory power.<br><br>
H0: β1 = β2 = ... = βk = 0<br>
(the model has no explanatory power)<br><br>
H1: At least one coefficient differs from zero<br>
(the model has explanatory power)",
  
  VAL = "Aquest contrast avalua si el conjunt de variables explicatives té capacitat explicativa conjunta.<br><br>
H0: β1 = β2 = ... = βk = 0<br>
(el model no té capacitat explicativa)<br><br>
H1: Almenys un coeficient és diferent de zero<br>
(el model té capacitat explicativa)"
),


## tab 4---------------------------------------------------------------------
panel4 = c(
  ES = "Test F (submodelo)",
  EN = "F test (submodel)",
  VAL = "Test F (submodel)"
),

title_tab4 = c(
  ES = "Contraste sobre un conjunto de parámetros",
  EN = "Test on a subset of parameters",
  VAL = "Contrast sobre un conjunt de paràmetres"
),

r2_full_tab4 = c(
  ES = "Coeficiente de determinación del modelo completo (R²)",
  EN = "Coefficient of determination of the full model (R²)",
  VAL = "Coeficient de determinació del model complet, (R²)"
),

r2_restr_tab4 = c(
  ES = "Coeficiente de determinación del modelo restringido (R²)",
  EN = "Coefficient of determination of the constrained model (R²)",
  VAL = "Coeficient de determinació del model restringit, (R²)"
),

n_obs_tab4 = c(
  ES = "Número de observaciones (n)",
  EN = "Number of observations (n)",
  VAL = "Número d'observacions (n)"
),

k_var_tab4 = c(
  ES = "Número de variables explicativas del modelo completo (k)",
  EN = "Number of explanatory variables of the full model (k)",
  VAL = "Número de variables explicatives del model complet (k)"
),

q_tab4 = c(
  ES = "Número de restricciones (q)",
  EN = "Number of restrictions (q)",
  VAL = "Número de restriccions (q)"
),

subtitle_graph_tab4 = c(
  ES = "Descripción del gráfico",
  EN = "Graph description",
  VAL = "Descripció del gràfic"
),

subtitle_results_tab4 = c(
  ES = "Resultados del contraste",
  EN = "Test results",
  VAL = "Resultats del contrast"
),

subtitle_econometric_tab4 = c(
  ES = "Interpretación econométrica",
  EN = "Econometric interpretation",
  VAL = "Interpretació economètrica"
),

interpretation_tab4 = c(
  ES = "Este contraste evalúa si un subconjunto de variables es conjuntamente significativo.<br><br>
H0: las q restricciones se cumplen<br>
(el subconjunto no aporta capacidad explicativa adicional)<br><br>
H1: al menos una restricción no se cumple<br>
(el subconjunto mejora el modelo)",
  
  EN = "This test evaluates whether a subset of variables is jointly significant.<br><br>
H0: the q restrictions hold<br>
(the subset does not add explanatory power)<br><br>
H1: at least one restriction does not hold<br>
(the subset improves the model)",
  
  VAL = "Aquest contrast avalua si un subconjunt de variables és conjuntament significatiu.<br><br>
H0: les q restriccions es compleixen<br>
(el subconjunt no aporta capacitat explicativa)<br><br>
H1: almenys una restricció no es compleix<br>
(el subconjunt millora el model)"
)
)


#ui--------------------------------------------------------------------------

ui <- fluidPage(
    useShinyjs(), # Activa funciones JS. NO MODIFICAR.
  
  # BOTONES PARA CAMBIAR DE IDIOMA EN LA PÁGINA. NO MODIFICARlOS. 
  
  absolutePanel(
    top = 10, right = 10, fixed = TRUE,
    actionButton("lang_es", "ES"),
    actionButton("lang_en", "EN"),
    actionButton("lang_va", "VAL")
  ),
  
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
  
  tags$head(
    tags$script(HTML("
    $(document).on('shiny:connected', function() {
      $('input[type=number]').on('keypress', function(e) {
        if (e.which === 44) {
          e.preventDefault();
          var val = $(this).val();
          var cursorPos = this.selectionStart;
          $(this).val(val.substring(0, cursorPos) + '.' + val.substring(cursorPos));
          this.selectionStart = this.selectionEnd = cursorPos + 1;
        }
      });
      $('input[type=number]').on('paste', function(e) {
        setTimeout(() => {
          $(this).val($(this).val().replace(/,/g, '.'));
        }, 10);
      });
    });
  "))
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
  ), # FIN DEL CSS
  
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
          
          uiOutput("slider_alpha_ui"),
          
          conditionalPanel(
            condition = "input.tabs == 'pvalor'",
            uiOutput("slider_z_ui")
          ),
          
          conditionalPanel(
            condition = "input.tabs == 'tcoef'",
            uiOutput("slider_t_ui")
          ),
          conditionalPanel(
            condition = "input.tabs == 'Fmodel'",
            uiOutput("slider_F_ui")
          ),
          
          conditionalPanel(
            condition = "input.tabs == 'Fsub'",
            uiOutput("slider_F_sub_ui")
          )
        )
      }
  ),
  
  
# CONTENIDO PRINCIPAL
  
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
      
# Tabs de la aplicación------------------------------------------------------
  
  ## tab 1-------------------------------------------------------------------    
      tabsetPanel(id="tabs",
                  
                  tabPanel(textOutput("panel1_title"), value="pvalor",
                           br(),
                           h3(textOutput("title_tab1"), align="center"),
                           br(),
                           plotOutput("plot_tab1", height="500px", width="100%"),
                           br(),
                           h4(strong(textOutput("subtitle_graph_tab1"))),
                           htmlOutput("interpretacion_text1"),
                           br(),
                           br(),
                           h4(strong(textOutput("subtitle_results_tab1"))),
                           textOutput("decision_text1")
                  ),
  ## tab 2-------------------------------------------------------------------    
                  tabPanel(textOutput("panel2_title"), value="tcoef",
                           br(),
                           h3(textOutput("title_tab2"), align="center"),
                           br(),
                           plotOutput("plot_t", height="500px", width="100%"),
                           br(),
                           h4(strong(textOutput("subtitle_graph_tab2"))),
                           htmlOutput("interpretacion_text2"),
                           br(),
                           br(),
                           h4(strong(textOutput("subtitle_econometric_tab2"))),
                           htmlOutput("texto_t"),
                           br(), 
                           br(),
                           h4(strong(textOutput("subtitle_results_tab2"))),
                           textOutput("decision_t")
                  ),

  ## tab 3-------------------------------------------------------------------    
                  tabPanel(textOutput("panel3_title"), value="Fmodel",
                           br(),
                           h3(textOutput("title_tab3"), align="center"),
                           br(),
                           plotOutput("plot_F", height="500px", width="100%"),
                           br(),
                           h4(strong(textOutput("subtitle_graph_tab3"))),
                           htmlOutput("interpretacion_text3"),
                           br(),
                           br(),
                           h4(strong(textOutput("subtitle_econometric_tab3"))),
                           htmlOutput("texto_F_model"),
                           br(),
                           br(),
                           h4(strong(textOutput("subtitle_results_tab3"))),
                           htmlOutput("decision_F")
                  ),
  ## tab 4-------------------------------------------------------------------    
                  tabPanel(textOutput("panel4_title"), value="Fsub",
                           br(),
                           h3(textOutput("title_tab4"), align="center"),
                           br(),
                           plotOutput("plot_F_sub", height="500px", width="100%"),
                           br(),
                           h4(strong(textOutput("subtitle_graph_tab4"))),
                           htmlOutput("interpretacion_text4"),
                           br(),
                           br(),
                           h4(strong(textOutput("subtitle_econometric_tab4"))),
                           htmlOutput("texto_F_sub"),
                           br(),
                           br(),
                           h4(strong(textOutput("subtitle_results_tab4"))),
                           htmlOutput("decision_F_sub")
                  )
      ),
      
      # Créditos y logos (obligatorio en plantilla STATIO)
      div(
        style = "margin-top:40px; text-align:center; margin-bottom:40px;",
        
        div(
          style = "display:flex; justify-content:center; align-items:center; gap:40px;",
          
          tags$img(
            src = 'UPV.png', 
            style = "height:85px; max-height:85px;"
          ),
          
          tags$img(
            src = 'DEIOAC.png',
            style = "height:65px; max-height:70px;"
          )
        ),
        
        div(
          style = "margin-top:15px;",
          htmlOutput("creditos")
        )
      )
    )   
  )

# Server---------------------------------------------------------------------
server <- function(input, output, session) {

# Opciones (no modificar)----------------------------------------------------
  observeEvent(input$toggleSidebar, {
    shinyjs::toggleClass(id = "sidebarWrapper", class = "closed")
    shinyjs::toggleClass(id = "contentWrapper", class = "shifted")
    shinyjs::runjs("setTimeout(function() { $(window).trigger('resize'); }, 350);")
  })
  
  language <- reactiveVal("ES") 
  observeEvent(input$lang_es, { language("ES") }) 
  observeEvent(input$lang_en, { language("EN") }) 
  observeEvent(input$lang_va, { language("VAL") })
  
  output$title_tab1 <- renderText({ tr("title_tab1", language()) })
  output$title_tab2 <- renderText({ tr("title_tab2", language()) })
  output$title_tab3 <- renderText({ tr("title_tab3", language()) })
  output$title_tab4 <- renderText({ tr("title_tab4", language()) })
  
  output$title <- renderText({ tr("title", language()) })
  output$explanation <- renderUI({HTML(tr("explanation", language())) })
  output$panel1_title <- renderText({ tr("panel1", language()) })
  output$panel2_title <- renderText({ tr("panel2", language()) })
  output$panel3_title <- renderText({ tr("panel3", language()) })
  output$panel4_title <- renderText({ tr("panel4", language()) })
  output$button_parameters <- renderText({tr("button_parameters", language())})
  
  output$text_downmenu <- renderText({ tr("text_downmenu", language()) })

  output$creditos <- renderUI({ HTML(tr("credits", language())) })

# Parámetros dinámicos-----------------------------------------------------
  ## común-----------------------------------------------------------------
  # desplegable
  output$dropdown_ui <- renderUI({
    
    # tab1
    if(input$tabs == "pvalor"){
      
      selectInput(
        "server_id_tab1",
        tr("dropdown_label", language()),
        choices = setNames(
          c("opt1","opt2","opt3"),
          c(
            tr("option1_tab1", language()),
            tr("option2_tab1", language()),
            tr("option3_tab1", language())
          )
        ),
        selected = "opt1"
      )
    }
    
    # tab2
    else if(input$tabs == "tcoef"){
      
      selectInput(
        "server_id_tab2",
        tr("dropdown_label", language()),
        choices = setNames(
          c("opt1","opt2","opt3"),
          c(
            tr("option1_tab2", language()),
            tr("option2_tab2", language()),
            tr("option3_tab2", language())
          )
        ),
        selected = "opt3"   # bilateral por defecto
      )
    }
    
    # tab3
    else if(input$tabs == "Fmodel"){
      
      selectInput(
        "server_id_tab3",
        tr("dropdown_label", language()),
        choices = setNames(
          "opt1",
          tr("option1_tab1", language())
        ),
        selected = "opt1"
      )
    }
    
    # tab4
    else if(input$tabs == "Fsub"){
      
      selectInput(
        "server_id_tab4",
        tr("dropdown_label", language()),
        choices = setNames(
          "opt1",
          tr("option1_tab1", language())
        ),
        selected = "opt1"
      )
    }
    
  })
  
  
  # slider alpha
  output$slider_alpha_ui <- renderUI({
    sliderInput("alpha", tr("slider_alpha", language()),
                min = 0.001, max = 0.20, value = 0.05, step = 0.001)
  })
  
  ## tab 1-----------------------------------------------------------------
  # slider z
  output$slider_z_ui <- renderUI({
    sliderInput("z", tr("slider_z", language()),
                min = -4, max = 4, value = 1.5, step = 0.1)
  })
  
  # plot z
  output$plot_tab1 <- renderPlot({
    
    z_obs <- input$z
    alpha <- input$alpha
    tipo <- input$server_id_tab1
    
    x <- seq(-4, 4, length.out = 2000)
    df <- data.frame(x = x, y = dnorm(x))
    
    g <- ggplot(df, aes(x, y)) +
      geom_line(linewidth = 1.2) +
      theme_minimal(base_size = 16) +
      labs(x = "z", y = tr("plot_y_tab1", language()))
    
    # cola derecha
    if(tipo == "opt1"){
      
      # p-valor rojo
      shade_p <- subset(df, x >= z_obs)
      g <- g + geom_area(data = shade_p, fill = "red", alpha = 0.45)
      
      # región crítica azul
      zcrit <- qnorm(1 - alpha)
      shade_c <- subset(df, x >= zcrit)
      g <- g + geom_area(data = shade_c, fill = "blue", alpha = 0.25)
      
      g <- g +
        geom_vline(xintercept = z_obs, linetype="dashed", linewidth=1) +
        geom_vline(xintercept = zcrit, colour="blue", linewidth=1)
    }
    
    # cola izquierda
    if(tipo == "opt2"){
      
      shade_p <- subset(df, x <= z_obs)
      g <- g + geom_area(data = shade_p, fill = "red", alpha = 0.45)
      
      zcrit <- qnorm(alpha)
      shade_c <- subset(df, x <= zcrit)
      g <- g + geom_area(data = shade_c, fill = "blue", alpha = 0.25)
      
      g <- g +
        geom_vline(xintercept = z_obs, linetype="dashed", linewidth=1) +
        geom_vline(xintercept = zcrit, colour="blue", linewidth=1)
    }
    
    # bilateral
    if(tipo == "opt3"){
      
      z_abs <- abs(z_obs)
      
      shade_pr <- subset(df, x >= z_abs)
      shade_pl <- subset(df, x <= -z_abs)
      
      g <- g +
        geom_area(data = shade_pr, fill = "red", alpha = 0.45) +
        geom_area(data = shade_pl, fill = "red", alpha = 0.45)
      
      zcrit <- qnorm(1 - alpha/2)
      
      shade_cr <- subset(df, x >= zcrit)
      shade_cl <- subset(df, x <= -zcrit)
      
      g <- g +
        geom_area(data = shade_cr, fill = "blue", alpha = 0.25) +
        geom_area(data = shade_cl, fill = "blue", alpha = 0.25)
      
      g <- g +
        geom_vline(xintercept = z_abs, linetype="dashed", linewidth=1) +
        geom_vline(xintercept = -z_abs, linetype="dashed", linewidth=1) +
        geom_vline(xintercept = zcrit, colour="blue", linewidth=1) +
        geom_vline(xintercept = -zcrit, colour="blue", linewidth=1)
    }
    
    g
  })
  
  # textos
  output$subtitle_graph_tab1 <- renderText({ tr("subtitle_graph_tab1", language()) })
  output$subtitle_results_tab1 <- renderText({ tr("subtitle_results_tab1", language()) })
  output$subtitle_econometric_tab1 <- renderText({ tr("subtitle_econometric_tab1", language()) })
  output$interpretacion_text1 <- renderUI({HTML(tr("interpretacion", language()))})
  pvalor <- reactive({
    
    z <- input$z
    
    if(input$server_id_tab1 == "opt1"){
      1 - pnorm(z)
    } 
    else if(input$server_id_tab1 == "opt2"){
      pnorm(z)
    } 
    else{
      2 * (1 - pnorm(abs(z)))
    }
    
  })
  
  output$decision_text1 <- renderText({
    
    p <- pvalor()
    alpha <- input$alpha
    
    p_txt <- formatC(p, format = "f", digits = 4)
    a_txt <- formatC(alpha, format = "f", digits = 3)
    
    if(p < alpha){
      paste0("p-value = ", p_txt,
             " < α = ", a_txt,
             " → ", tr("reject", language()))
    } else {
      paste0("p-value = ", p_txt,
             " ≥ α = ", a_txt,
             " → ", tr("not_reject", language()))
    }
    
  })
  ## tab 2-------------------------------------------------------------------
  
  # slider t
  output$slider_t_ui <- renderUI({
    
    tagList(
      
      numericInput("beta",
                   label = tr("beta_tab2", language()),
                   value = 0.5, step = 0.01),
      
      numericInput("beta0",
                   label = tr("beta0_tab2", language()),
                   value = 0, step = 0.01),
      
      numericInput("se",
                   label = tr("se_tab2", language()),
                   value = 0.2, min = 0.0001, step = 0.01),
      
      numericInput("n",
                   label = tr("n_obs_tab2", language()),
                   value = 50, min = 5, step = 1),
      
      numericInput("k",
                   label = tr("k_var_tab2", language()),
                   value = 3, min = 1, step = 1)
    )
  })

  # plot t
  t_obs <- reactive({(input$beta - input$beta0) / input$se})
  
  gl_t <- reactive({input$n - input$k - 1})
  
  pvalor_t <- reactive({
    t <- t_obs()
    gl <- gl_t()
    
    if(input$server_id_tab2 == "opt1"){
      1 - pt(t, gl)
    } 
    else if(input$server_id_tab2 == "opt2"){
      pt(t, gl)
    } 
    else{
      2 * (1 - pt(abs(t), gl))
    }
    
  })
  
  output$plot_t <- renderPlot({
    
    t_val <- t_obs()
    gl <- gl_t()
    alpha <- input$alpha
    tipo <- input$server_id_tab2
    
    validate(
      need(gl > 0, "Los grados de libertad deben ser positivos")
    )
    
    x <- seq(-4, 4, length.out = 2000)
    df_plot <- data.frame(x = x, y = dt(x, gl))
    
    g <- ggplot(df_plot, aes(x, y)) +
      geom_line(linewidth = 1.2) +
      theme_minimal(base_size = 16) +
      labs(x = "t", y = tr("plot_y_tab1", language()))
    
    # cola derecha
    if(tipo == "opt1"){
      
      shade_p <- subset(df_plot, x >= t_val)
      g <- g + geom_area(data = shade_p, fill="red", alpha=.45)
      
      crit <- qt(1-alpha, gl)
      shade_c <- subset(df_plot, x >= crit)
      g <- g + geom_area(data = shade_c, fill="blue", alpha=.25)
      
      g <- g +
        geom_vline(xintercept=t_val, linetype="dashed") +
        geom_vline(xintercept=crit, colour="blue")
    }
    
    # Cola izquierda
    if(tipo == "opt2"){
      
      shade_p <- subset(df_plot, x <= t_val)
      g <- g + geom_area(data = shade_p, fill="red", alpha=.45)
      
      crit <- qt(alpha, gl)
      shade_c <- subset(df_plot, x <= crit)
      g <- g + geom_area(data = shade_c, fill="blue", alpha=.25)
      
      g <- g +
        geom_vline(xintercept=t_val, linetype="dashed") +
        geom_vline(xintercept=crit, colour="blue")
    }
    
    # Bilateral
    if(tipo == "opt3"){
      
      t_abs <- abs(t_val)
      
      shade_pr <- subset(df_plot, x >= t_abs)
      shade_pl <- subset(df_plot, x <= -t_abs)
      
      g <- g +
        geom_area(data = shade_pr, fill="red", alpha=.45) +
        geom_area(data = shade_pl, fill="red", alpha=.45)
      
      crit <- qt(1-alpha/2, gl)
      
      shade_cr <- subset(df_plot, x >= crit)
      shade_cl <- subset(df_plot, x <= -crit)
      
      g <- g +
        geom_area(data = shade_cr, fill="blue", alpha=.25) +
        geom_area(data = shade_cl, fill="blue", alpha=.25)
      
      g <- g +
        geom_vline(xintercept=t_abs, linetype="dashed") +
        geom_vline(xintercept=-t_abs, linetype="dashed") +
        geom_vline(xintercept=crit, colour="blue") +
        geom_vline(xintercept=-crit, colour="blue")
    }
    
    g
  })
  
  # textos
  output$subtitle_graph_tab2 <- renderText({ tr("subtitle_graph_tab2", language()) })
  output$subtitle_results_tab2 <- renderText({ tr("subtitle_results_tab2", language()) })
  output$subtitle_econometric_tab2 <- renderText({ tr("subtitle_econometric_tab2", language()) })
  output$interpretacion_text2 <- renderUI({HTML(tr("interpretacion", language()))})
  
  output$decision_t <- renderText({
    
    tval <- t_obs()
    gl <- gl_t()
    p <- pvalor_t()
    alpha <- input$alpha
    
    paste0(
      "t = ", round(tval,3),
      "   |   df = ", gl,
      "   |   p-value = ", round(p,4),
      " ",
      ifelse(p < alpha,
             tr("decision_rechazar_tab2", language()),
             tr("decision_no_rechazar_tab2", language()))
    )
  })
  
  output$ha_text <- renderUI({
    
    tipo <- input$server_id_tab2
    
    txt <- if(tipo == "opt1"){
      "H1: β > β₀"
    } else if(tipo == "opt2"){
      "H1: β < β₀"
    } else{
      "H1: β ≠ β₀"
    }
    
    HTML(paste0(
      "<div style='font-size:18px; text-align:center; margin-top:6px;'>",
      txt,
      "</div>"
    ))
  })
  
  output$texto_t <- renderUI({
    
    tipo <- input$server_id_tab2
    
    h1 <- if(tipo == "opt1"){
      "H1: β > 0"
    } else if(tipo == "opt2"){
      "H1: β < 0"
    } else{
      "H1: β ≠ 0"
    }
    
    HTML(paste0(
      tr("interpretation_tab2", language()),
      h1, "<br><br>",
      tr("significant_tab2", language())
    ))
  })
  
  ## tab 3-------------------------------------------------------------------
  # slider F
  output$slider_F_ui <- renderUI({
    
    tagList(
      numericInput("R2",
                   label = tr("r2_tab3", language()),
                   value = 0.40, min = 0.001, max = 0.99, step = 0.01),
      
      numericInput("n_F",
                   label = tr("n_obs_tab3", language()),
                   value = 50, min = 5, step = 1),
      
      numericInput("k_F",
                   label = tr("k_var_tab3", language()),
                   value = 3, min = 1, step = 1)
    )
  })
  
F_model <- reactive({
  R2 <- input$R2
  n <- input$n_F
  k <- input$k_F
  
  (R2/k)/((1-R2)/(n-k-1))
})

df1_model <- reactive({ input$k_F })
df2_model <- reactive({ input$n_F - input$k_F - 1 })

pvalor_F_model <- reactive({
  1 - pf(F_model(), df1_model(), df2_model())
})

  # plot F
output$plot_F <- renderPlot({
  
  Fval <- F_model()
  df1 <- df1_model()
  df2 <- df2_model()
  alpha <- input$alpha
  
  validate(
    need(df2 > 0, "Revisa n y k"),
    need(Fval >= 0, "")
  )
  
  x <- seq(0, max(5, Fval*1.5), length.out = 2000)
  df_plot <- data.frame(x = x, y = df(x, df1, df2))
  
  g <- ggplot(df_plot, aes(x,y)) +
    geom_line(linewidth=1.2) +
    theme_minimal(base_size=16) +
    labs(x="F", y=tr("plot_y_tab1", language()))
  
  shade_p <- subset(df_plot, x >= Fval)
  g <- g + geom_area(data=shade_p, fill="red", alpha=.45)
  
  crit <- qf(1-alpha, df1, df2)
  shade_c <- subset(df_plot, x >= crit)
  g <- g + geom_area(data=shade_c, fill="blue", alpha=.25)
  
  g +
    geom_vline(xintercept=Fval, linetype="dashed") +
    geom_vline(xintercept=crit, colour="blue")
})

  output$decision_F <- renderUI({
    
    Fval <- F_model()
    p <- pvalor_F_model()
    df1 <- df1_model()
    df2 <- df2_model()
    alpha <- input$alpha
    
    decision <- if(p < alpha){
      tr("reject_model", language())
    } else{
      tr("noreject_model", language())
    }
    
    HTML(paste0(
      "F = ", round(Fval,3),
      " &nbsp; | &nbsp; df<sub>1</sub> = ", df1,
      " &nbsp; | &nbsp; df<sub>2</sub> = ", df2,
      " &nbsp; | &nbsp; p-value = ", round(p,4),
      " ",
      decision
    ))
  })

  # textos
  output$subtitle_graph_tab3 <- renderText({ tr("subtitle_graph_tab3", language()) })
  output$subtitle_results_tab3 <- renderText({ tr("subtitle_results_tab3", language()) })
  output$subtitle_econometric_tab3 <- renderText({ tr("subtitle_econometric_tab3", language()) })
  output$interpretacion_text3 <- renderUI({HTML(tr("interpretacion", language()))})
  output$texto_F_model <- renderUI({HTML(tr("interpretation_tab3", language()))})
  
  ## tab 4-------------------------------------------------------------------
  # slider F
  output$slider_F_sub_ui <- renderUI({
    
    tagList(
      numericInput("R2_full",
                   label = tr("r2_full_tab4", language()),
                   value = 0.40, min = 0.001, max = 0.99, step = 0.01),
      
      numericInput("R2_restr",
                   label = tr("r2_restr_tab4", language()),
                   value = 0.30, min = 0.001, max = 0.99, step = 0.01),
      
      numericInput("n_F_sub",
                   label = tr("n_obs_tab4", language()),
                   value = 50, min = 5, step = 1),
      
      numericInput("k_F_sub",
                   label = tr("k_var_tab4", language()),
                   value = 5, min = 1, step = 1),
      
      numericInput("q_sub",
                   label = tr("q_tab4", language()),
                   value = 5, min = 1, step = 1)
    )
  })

  # plot F
  F_sub <- reactive({
    
    R2f <- input$R2_full
    R2r <- input$R2_restr
    n <- input$n_F_sub
    k <- input$k_F_sub
    q <- input$q_sub
    
    ((R2f - R2r)/q)/((1-R2f)/(n-k-1))
  })
  
  df1_sub <- reactive({ input$q_sub })
  df2_sub <- reactive({ input$n_F_sub - input$k_F_sub - 1 })
  
  pvalor_F_sub <- reactive({
    1 - pf(F_sub(), df1_sub(), df2_sub())
  })
  
  output$plot_F_sub <- renderPlot({
    
    Fval <- F_sub()
    df1 <- df1_sub()
    df2 <- df2_sub()
    alpha <- input$alpha
    
    validate(
      need(df2 > 0, "Revisa n,k,q"),
      need(Fval >= 0, "")
    )
    
    x <- seq(0, max(5, Fval*1.5), length.out = 2000)
    df_plot <- data.frame(x = x, y = df(x, df1, df2))
    
    g <- ggplot(df_plot, aes(x,y)) +
      geom_line(linewidth=1.2) +
      theme_minimal(base_size=16) +
      labs(x="F", y=tr("plot_y_tab1", language()))
    
    # pvalor
    shade_p <- subset(df_plot, x >= Fval)
    g <- g + geom_area(data=shade_p, fill="red", alpha=.45)
    
    # region critica
    crit <- qf(1-alpha, df1, df2)
    shade_c <- subset(df_plot, x >= crit)
    g <- g + geom_area(data=shade_c, fill="blue", alpha=.25)
    
    g +
      geom_vline(xintercept=Fval, linetype="dashed") +
      geom_vline(xintercept=crit, colour="blue")
  })
  # textos
  output$decision_F_sub <- renderUI({
    
    Fval <- F_sub()
    p <- pvalor_F_sub()
    df1 <- df1_sub()
    df2 <- df2_sub()
    alpha <- input$alpha
    
    decision <- if(p < alpha){
      tr("reject_subset", language())
    } else{
      tr("noreject_subset", language())
    }
    
    HTML(paste0(
      "F = ", round(Fval,3),
      " &nbsp; | &nbsp; df<sub>1</sub> = ", df1,
      " &nbsp; | &nbsp; df<sub>2</sub> = ", df2,
      " &nbsp; | &nbsp; p-value = ", round(p,4),
      " ",
      decision
    ))
  })
  
  
  output$subtitle_graph_tab4 <- renderText({ tr("subtitle_graph_tab4", language()) })
  output$subtitle_results_tab4 <- renderText({ tr("subtitle_results_tab4", language()) })
  output$subtitle_econometric_tab4 <- renderText({ tr("subtitle_econometric_tab4", language()) })
  output$interpretacion_text4 <- renderUI({HTML(tr("interpretacion", language()))})
  output$texto_F_sub <- renderUI({HTML(tr("interpretation_tab4", language()))})
}

# Create Shiny app ----------------------------------------------------------
shinyApp(ui, server)
