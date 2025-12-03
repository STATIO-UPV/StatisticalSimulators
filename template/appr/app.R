################ LIBRARIES IMPORT. DO NOT TOUCH THIS. #####################

library(shiny)
library(shinyjs)
library(tidyverse)
library(car)
library(agricolae)


if (FALSE) {
  library(munsell)
}


###################### This is the body of the application ##############################

showparams <- TRUE #change this to FALSE if no parameters are needed

texts <- list( 
  title = c( 
  ES = "Título de tu aplicación", 
  EN = "Application Title", 
  VAL = "Títol de la teua aplicació" ), 
  explanation = c( 
    ES = "Breve explicación de la aplicación.", 
    EN = "Short explanation of the application.", 
    VAL = "Breu explicació de l'aplicació." ), 
  button_parameters = c( 
    ES = "Parámetros", 
    EN = "Parameters", 
    VAL = "Paràmetres" ), 
  text_downmenu= c( 
    ES = "1. Tu menú desplegable", 
    EN= "2. Your dropdown menu", 
    VAL= "3. El teu menú desplegable" ), 
  dropdown_label = c( 
    ES = "Título del menú desplegable", 
    EN = "Title of drop-down menu", 
    VAL = "Títol del menú desplegable" ), 
  option1 = c( 
    ES = "OPCIÓN 1", 
    EN = "OPTION 1", 
    VAL = "OPCIÓ 1" ), 
  option2 = c( 
    ES = "OPCIÓN 2", 
    EN = "OPTION 2", 
    VAL = "OPCIÓ 2" ), 
  slider1= c( 
    ES = "BARRA DESLIZANTE 1", 
    EN= "SLIDER 1", 
    VAL= "Barra lliscant 1" ), 
  slider2= c( 
    ES = "BARRA DESLIZANTE 2", 
    EN= "SLIDER 2", 
    VAL= "Barra lliscant 2" ), 
  slider3= c( 
    ES = "BARRA DESLIZANTE 3", 
    EN= "SLIDER 3", 
    VAL= "Barra lliscant 3" ), 
  slider4= c( 
    ES = "BARRA DESLIZANTE 4", 
    EN= "SLIDER 4", 
    VAL= "Barra lliscant 4" ), 
  slider5= c( 
    ES = "BARRA DESLIZANTE 5", 
    EN= "SLIDER 5", 
    VAL= "Barra lliscant 5" ), 
  slider6= c( 
    ES = "BARRA DESLIZANTE 6", 
    EN= "SLIDER 6", 
    VAL= "Barra lliscant 6" ), 
  panel1 = c( 
    ES = "Panel 1", 
    EN = "Panel 1", 
    VAL = "Panell 1" ), 
  panel2 = c( 
    ES = "Panel 2", 
    EN = "Panel 2", 
    VAL = "Panell 2" ), 
  panel3 = c( 
    ES = "Panel 3", 
    EN = "Panel 3", 
    VAL = "Panell 3" ), 
  plot= c(
    ES= "Gráfico",
    EN= "Plot", 
    VAL= "Gràfic"),
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
    VAL= "Taula Exemple: Esta és una taula d'exemple (per a *ANOVA)" 
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
    ES= "Aplicación realizada para el proyecto docente de Javier Marín Morales. 
    Concurso a PPL C03/24, código de la plaza 7266.", 
    EN= "Application developed for Javier Marín Morales' teaching project. 
    PPL C03/24 competition, position code 7266. ENG", 
    VAL= "Aplicació realitzada per al projecte docent de Javier Marín Morales. 
    Concurs a *PPL C03/24, codi de la plaça 7266." )
  )

  tr <- function(id, lang) { texts[[id]][[lang]] }
ui <- fluidPage(
  
  useShinyjs(),
  
  absolutePanel(
  top = 10, right = 10, fixed = TRUE,
  actionButton("lang_es", "ES"),
  actionButton("lang_en", "EN"),
  actionButton("lang_va", "VAL")
),

  # --------------------------
  # 1. CSS. This is for style the template, do not touch. 
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

      
# Button that show parameters section. 

if(showparams) {
  actionButton("toggleSidebar", textOutput("button_parameters"))
},

div(id="sidebarWrapper", class= "closed",
    style="padding-top: 50px;",
    if(showparams) {
      div(
        
        #Here you can add as many parameters, menus, slides and titles as you may need. 
        h4(textOutput("text_downmenu")), uiOutput("dropdown_ui"),
        
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

  
############ This section is for the main body of your app #######################

div(id="contentWrapper",
    
   ############### This section is for the title and main explanation. #######################
   
   # Please, modify ONLY the title and main explanation. 
    
    div(
      style="padding-top: 70px;", 
      
      # TITLE
      
      fluidRow(
        column(
          width = 12,
          div(
            style="text-align: center; margin: 0;",
            h2(textOutput("title"), style="margin: 0 0 5px 0;")
          )
        )
      ),
      
      # Main explanation. 
      
      fluidRow(
        div(
          style="
          display: flex;
          justify-content: center;
          width: 100%;
          margin-top: 20px;       /* separación mínima */
        ",
          div(
            style="
            border: 2px solid #4a90e2;
            border-radius: 12px;
            padding: 10px 15px;
            background-color: white;
            box-shadow: 0px 2px 3px rgba(0,0,0,0.12);
            max-width: 600px;
            text-align: center;
            margin-left: 15px;
            margin-right: 15px;
          ",
            uiOutput("explanation")
          )
        )
      )
    ),
    
    
    ###################### This section is for all tabs. Add and modify as many tabs as needed. ######################
   
    div(style = "margin-top: 40px;",
        tabsetPanel(
          type = "tabs",
          tabPanel(textOutput("panel1_title"),
                   htmlOutput("plot_title"), 
                   plotOutput("Plot_ID"),
                   uiOutput("sampleStats"),
                   htmlOutput("table_name"), verbatimTextOutput("aov"),
                   htmlOutput("interpretation_text"), textOutput("conclusionText"), br(),
                   uiOutput("resultsMessage")
          ),
          tabPanel(textOutput("panel2_title"), 
    
          ),
          tabPanel(textOutput("panel3_title"),
  
          ),
          tabPanel("Data", tableOutput("data"))
        )
    ), 
   
   div(
     style="
    width: 100%;
    margin-top: 40px;
    padding: 25px 0;
    display: flex;
    flex-direction: column;
    align-items: center;     /* Centrado horizontal */
    justify-content: center; /* Centrado del contenido */
    text-align: center;
  ",
     
     div(
       class = "logo-row",
       style="display: flex; gap: 10px; justify-content: center;",
       tags$img(src='DEIOAC.png', class = "logo-img"),
       tags$img(src='UPV.png',    class = "logo-img")
     ),
     
     div(
       class = "text-box",
       style="max-width: 600px; margin-top: 10px; text-align: center;",
       htmlOutput("creditos")
     )
   )
   )
)
        

# Define server logic for random distribution app ----

server <- function(input, output) {
  
  #Do not touch this. This is for opening and closing slider.

  observeEvent(input$toggleSidebar, {
    shinyjs::toggleClass(id = "sidebarWrapper", class = "closed")
    shinyjs::toggleClass(id = "contentWrapper", class = "shifted")
  })
  
  language <- reactiveVal("ES") 
  observeEvent(input$lang_es, { language("ES") }) 
  observeEvent(input$lang_en, { language("EN") }) 
  observeEvent(input$lang_va, { language("VAL") })
  
  output$title <- renderText({ tr("title", language()) })
  output$explanation <- renderUI({ HTML(tr("explanation", language())) })
  output$panel1_title <- renderText({ tr("panel1", language()) })
  output$panel2_title <- renderText({ tr("panel2", language()) })
  output$panel3_title <- renderText({ tr("panel3", language()) })

  output$plot_title <- renderUI({
    HTML(paste0("<h3 style='font-size:22px; font-weight:bold'>",
                tr('plot', language()),
                "</h3>"))
  })
  
  output$table_name <- renderUI({
    HTML(paste0("<h3 style='font-size:22px; font-weight:bold'>",
                tr('tabla', language()),
                "</h3>"))
  })
  
  output$interpretation_text <- renderUI({
    HTML(paste0("<h3 style='font-size:22px; font-weight:bold'>",
                tr('interpretation', language()),
                "</h3>"))
  })
  
  output$creditos <- renderUI({ HTML(tr("credits", language())) })
  
  output$button_parameters <- renderText({tr("button_parameters", language())})
  output$text_downmenu <- renderText({ tr("text_downmenu", language()) })
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
  
  output$dropdown_ui <- renderUI({ 
    selectInput( "server_id", tr("dropdown_label", language()), 
                 choices = setNames( 
                   c("opt1", "opt2"), 
                   c(tr("option1", language()), tr("option2", language())) ) ) }
    )
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression

  ##################### DATASET CREATION ####################
  
  #Set as many parameters as selected. EXAMPLE OF HOW A DATASET COULD BE CALCULED. Set the parameters/values as you want. 
  #This generates a randomized dataset based on means and std set in sliders or fixed. Also, you can load your own dataset.
  # You can modify the way the dataset is created, this is an example. 
  
  d <- reactive({
    n <- 50 #Number of participants generated in the dataset.
    uA <- input$sliderId # This is how to link parameters to the dataset generation. 
    uB <- input$sliderId2
    uC <- input$sliderId3
    sdA <- 10
    sdB <- 10
    sdC <- 10
    
    set.seed(123)
    
    example1 <- data.frame(
      group = "a",
      value = rnorm(n, uA, sdA)
    )
    
    example2 <- data.frame(
      group = "b",
      value = rnorm(n, uB, sdB)
    )
    
    example3 <- data.frame(
      group = "c",
      value = rnorm(n, uC, sdC)
    )
    
    bind_rows(example1, example2, example3)
  })
  
  ##################### PLOTS  ##################### 
  
  #Set plots parameters. This is an example of a plot that can be created.
  # HERE, YOU WILL SHOW, IF NEEDED, ANY PLOT YOU WANT IN YOUR SIMULATION. 
  plot_x_label <- reactive({tr("plot_x" ,language())})
  plot_y_label <- reactive({tr("plot_y", language())})
  
  output$Plot_ID <- renderPlot({
    data <- d()
    ggplot(data, aes(x = group, y = value, fill = group)) +
      geom_violin(trim = FALSE) +
      geom_jitter(width = 0.1, size = 0.5) +
      labs(
        x = plot_x_label(),
        y = plot_y_label()
      ) +
      theme_minimal()
  })
  
  
  ######## DATA DESCRIPTION #################### 
  
  #In the variable sample stats we save: data, means per group, and a message formatted that shows data (means or std)
  
  output$sampleStats <- renderUI({
    data <- d()
    means <- data %>% group_by(group) %>% summarise(mean = mean(value))
    
    # Here, you can add as many equations or parameters as needed in your application. Maybe you would want to add ranges,
    # or max values, or medians. 
    
    withMathJax(HTML(paste0(
      "<h5>", tr("sampleStats_title", language()), "</h5><br>",
      "$$", tr("equation_label", language()), ":\\quad ",
      "Eq1 = ", round(means$mean[means$group == "A"], 2), ",\\quad ",
      "Eq2 = ", round(means$mean[means$group == "B"], 2), ",\\quad ",
      "Eq3 = ", round(means$mean[means$group == "C"], 2), "$$"
    )))
  })
  
  ##################### SIMULATION. TEST YOU WANT TO SHOW ##################### 
  
  # Reactive example (ANOVA, T-test, etc.). Reactive means that it depends on d() data. Is the way to vincule
  # data to a showed output. 
  
  # Change anova with the test you want to create. Change "aov" with "t.test" (test t), 
  # "wilcox.test" (wilcoxon), etc. 
  
  #ANOVA IS CREATED TO SHOW AN EXAMPLE.
  # HERE, YOU CAN ADD AS MANY RESULTS, CONCLUSSIONS OR EXPLANATIONS YOU WANT IN YOUR SIMULATION.
  # Also, you can remove whatever you want. 
  # ALSO, YOU CAN SEND ANY OF YOUR RESULTS TO A DIFFERENT TAB. 
  
  test_result <- reactive({
    data <- d() #Data used in your test. 
    aov(value ~ group, data = data)
  })
  
  # Get the p-value in ANOVA. Change this with your test (if p-val is not needed, 
  # remove this and change it into your results).
  
  p_value <- reactive({
    anova_summary <- summary(test_result())
    anova_summary[[1]][["Pr(>F)"]][1]
  })
  
  # Conclussion message based on pvalue. Maybe you want conclussions or results 
  # based on other parameter or only an explanation. 
  
  conclusion <- reactive({
    if (p_value() < 0.05) {
      tr("conclusion1", language())
    } else {
      tr("conclusion2", language())
    }
  })

  
  # This section is to set outputs, important to show what u want. 
  # Add as many outputs as you want to show (p-valor, confidences, conclusions ...)
  
  output$aov <- renderPrint({
    anova_result <- test_result()
    summary(anova_result)
  })
  
  output$pValueText <- renderText({
    paste("p-value or other parameter: ", format(p_value(), digits = 5))
  })
  
  output$conclusionText <- renderText({
    conclusion()
  })
  
    output$resultsMessage <- renderUI({
    HTML("<h3 style='color: gray; text-align: center;'>ADD AS MANY RESULTS AS YOU WANT</h3>")
  })
    
  output$data <- renderTable({
    d()
  })
    
}

# Create Shiny app ----
shinyApp(ui, server)
