# app.R — Simulador método gráfico (2 variables) + solver (lpSolve) + polígono exacto

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(boot)
library(munsell)
library(colorspace)

EPS <- 1e-9
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
num_or <- function(x, default = 0) {
  x <- x %||% default
  xnum <- suppressWarnings(as.numeric(x))
  if (length(xnum) == 0 || is.na(xnum)) default else xnum
}


ui <- page_sidebar(
  title = NULL,
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    # En pantallas pequeñas el sidebar pasa a modo "offcanvas" (responsive)
    width = 400,
    open = "desktop",
    withMathJax(),

    # --- Restricciones (dinámicas) ---
    tags$div(
      class = "d-flex justify-content-between align-items-center mb-2",
      tags$div(class = "fw-semibold", textOutput("lbl_restricciones", container = span)),
      uiOutput("restr_buttons")
    ),
    uiOutput("restricciones_ui"),

card(
      class = "mb-3 fo-card",
      card_header(div(class = "h6 mb-0", textOutput("lbl_funcion_objetivo", container = span))),
      div(
        class = "ineq-row",
        selectInput("sense", label = NULL, choices = c("Max", "Min"), selected = "Max", width = "84px", selectize = FALSE),
        span(class = "mx-var", HTML("\\(z =\\)")),
        numericInput("c1", label = NULL, value = 3, step = 0.5, width = "84px"),
        span(class = "mx-var", shiny::HTML("\\(x_1\\)")),
        span(class = "mx-1", "+"),
        numericInput("c2", label = NULL, value = 2, step = 0.5, width = "84px"),
        span(class = "mx-var", shiny::HTML("\\(x_2\\)"))
      )
    ),

    # --- No negatividad ---
    card(
      class = "mb-3",
      card_header(div(class = "h6 mb-0", textOutput("lbl_naturaleza_vars", container = span))),
      checkboxInput("nn", "\\(x_1\\ge 0,\\ x_2\\ge 0\\)", TRUE)
    )
  ),

  tags$style(HTML(
    ".r1-card { border-left: 6px solid #E74C3C; }\n",
    ".r2-card { border-left: 6px solid #3498DB; }\n",
    ".fo-card { border-left: 6px solid #7F8C8D; }\n",
    ".ineq-row { display:flex; align-items:center; gap:8px; flex-wrap:nowrap; }\n",
    ".ineq-row .form-control { padding: .25rem .5rem; }\n",
    ".ineq-row .selectize-control { min-width:76px; }\n",
    ".ineq-row .selectize-input { min-height: calc(1.5em + .5rem + 2px); padding:.25rem .5rem; }\n",
    ".card-header input.form-control { height: 2.0rem; padding: .2rem .5rem; }\n",
    ".mx-1 { margin: 0 .25rem; }\n",
    ".mx-var { font-size: 1.05rem; }\n",
    "
      .plot-controls { 
        display: flex; flex-wrap: wrap; gap: 18px;
        align-items: center;
        padding: 10px 12px;
        background: rgba(255,255,255,0.9);
        border: 1px solid #e5e7eb;
        border-radius: 10px;
      }
      .plot-controls .form-group { margin: 0 !important; }
      .plot-controls .ctrl-row { display:flex; align-items:center; gap:10px; }
      .plot-controls .iso-row { gap:12px; }
      .plot-controls .iso-slider { display:flex; align-items:center; gap:10px; }
      .plot-controls .iso-label { font-weight:600; color:#374151; }
\n.plot-controls .form-check { margin:0; }\n.plot-controls .shiny-input-container { margin:0 !important; }\n.plot-controls label { font-size:13px; font-weight:600; }\n.plot-controls .form-range { width:160px; }\n@media (max-width: 520px){ .ineq-row { flex-wrap:wrap; } }\n.plot-controls{display:flex; align-items:center; justify-content:space-between; gap:10px; flex-wrap:nowrap; white-space:nowrap; padding:10px 12px; background:#ffffff; border:1px solid rgba(0,0,0,0.08); border-radius:10px;}\n.controls-left{display:flex; align-items:center; gap:10px; flex-wrap:nowrap; white-space:nowrap;}\n.iso-inline{display:flex; align-items:center; gap:0px; flex-wrap:nowrap; white-space:nowrap;}\n.iso-slider-inline{display:flex; align-items:center; gap:4px; margin-left:4px; flex-wrap:nowrap; white-space:nowrap;}\n.iso-slider-inline .irs{height:22px;}
.iso-slider-inline .irs--shiny .irs-line{height:4px;}
.iso-slider-inline .irs--shiny .irs-bar{height:4px;}
.iso-slider-inline .irs--shiny .irs-handle{top:16px;}
.iso-slider-inline .irs--shiny .irs-single{font-size:11px;}
.iso-slider-inline .irs--shiny .irs-min, .iso-slider-inline .irs--shiny .irs-max{font-size:10px;}
.hover-chip{min-width:160px; text-align:center; padding:6px 10px; background:rgba(255,255,255,0.75); border:1px solid rgba(0,0,0,0.15); border-radius:6px; font-size:13px; font-weight:600; pointer-events:none;}\n.plot-controls .irs--shiny .irs-line, .plot-controls .irs--shiny .irs-bar{height:4px;}\n.plot-controls .irs--shiny .irs-handle{width:12px; height:12px; top:19px;}\n.model-line{display:flex; align-items:baseline; gap:8px; margin-top:6px;}\n",

".dark-mode body { background:#0b0f14; color:#e5e7eb; }\n",
".dark-mode .bslib-page, .dark-mode .bslib-page-sidebar { background:#0b0f14; }\n",
".dark-mode .card, .dark-mode .bslib-card { background:#111827; color:#e5e7eb; border-color:rgba(255,255,255,0.12); }\n",
".dark-mode .card-header { background:#0f172a; color:#e5e7eb; border-color:rgba(255,255,255,0.12); }\n",
".dark-mode .plot-controls { background:rgba(17,24,39,0.92); border-color:rgba(255,255,255,0.12); }\n",
".dark-mode .hover-chip { background:rgba(17,24,39,0.75); color:#e5e7eb; border-color:rgba(255,255,255,0.18); }\n",
"/* keep plot controls on one line */\n",
".plot-controls{display:flex; align-items:center; gap:12px; flex-wrap:nowrap;}\n",
".plot-controls .controls-left{display:flex; align-items:center; gap:14px; flex-wrap:nowrap; white-space:nowrap;}\n",
".plot-controls .controls-spacer{flex:1 1 auto;}\n",
".plot-controls .controls-right{display:flex; align-items:center; gap:10px; flex-wrap:nowrap; white-space:nowrap;}\n",
".iso-slider-inline{display:flex; align-items:center; gap:4px; margin-left:4px; flex-wrap:nowrap;}\n",
".iso-slider-inline .form-group{margin-bottom:0 !important;}\n",
".iso-slider-inline .irs{margin-top:-6px;}\n",
".iso-slider-inline .irs--shiny .irs-min, .iso-slider-inline .irs--shiny .irs-max, .iso-slider-inline .irs--shiny .irs-single, .iso-slider-inline .irs--shiny .irs-grid{display:none !important;}\n",
".iso-slider-inline .irs-line{height:3px;}\n",
".iso-slider-inline .irs-bar{height:3px;}\n",
".iso-slider-inline .irs-handle{top:18px;}\n",
".hover-chip{min-width:140px; padding:5px 8px; font-size:12.5px; white-space:nowrap;}\n",
".dark-mode .sidebar .fw-semibold, .dark-mode .bslib-sidebar .fw-semibold{color:#F9FAFB !important;}\n",
".dark-mode .iso-label{color:#F9FAFB !important;}\n",
".dark-mode .form-control, .dark-mode .selectize-input { background:#0f172a; color:#e5e7eb; border-color:rgba(255,255,255,0.18); }\n",
".dark-mode .selectize-dropdown { background:#0f172a; color:#e5e7eb; }\n",
".dark-mode .btn-outline-secondary { color:#e5e7eb; border-color:rgba(255,255,255,0.25); }\n",
".dark-mode .btn-outline-secondary:hover { background:rgba(255,255,255,0.08); }\n.dark-mode .pime-desc-box{background:#0f172a !important; border-color:#ffffff !important; color:#ffffff !important;}\n.dark-mode .pime-desc-box *{color:#ffffff !important;}\n.dark-mode .form-control::placeholder{color:rgba(229,231,235,0.75) !important;}\n.dark-mode input::placeholder{color:rgba(229,231,235,0.75) !important;}\n",
"/* === Responsive tweaks === */\n",
".bslib-sidebar-layout{--bslib-sidebar-width:400px;}\n",
"@media (max-width: 992px){\n  .bslib-sidebar-layout{--bslib-sidebar-width:100vw;}\n  /* En móvil permitimos que la barra de controles haga wrap */\n  .plot-controls{flex-wrap:wrap !important; white-space:normal !important;}\n  .plot-controls .controls-left, .plot-controls .controls-right{flex-wrap:wrap !important; white-space:normal !important;}\n  .hover-chip{min-width:0; font-size:12px;}\n}\n",
"#plt{min-height:420px;}\n",
"@media (max-width: 768px){#plt{min-height:320px; height:55vh !important;}}\n"
)),

  
  tags$script(HTML("
    Shiny.addCustomMessageHandler('toggle-dark', function(msg){
      try {
        document.documentElement.classList.toggle('dark-mode', !!msg.enable);
      } catch(e) {}
    });
  ")),
layout_column_wrap(
    width = 1,
    card(
      full_screen = TRUE,
      card_header(div(class = "h5 mb-0", uiOutput("solutions_header_ui"))),
      div(style="position:relative;",
          uiOutput("plot_controls"),
          # Altura responsive: se adapta a la altura disponible (con mínimos vía CSS)
          plotOutput("plt", height = "65vh", hover = hoverOpts("plt_hover", delay = 50, delayType = "throttle")),
      ),
      uiOutput("status"),
      uiOutput("formula")
    )
  )
)

server <- function(input, output, session) {
  # Toggle tema claro/oscuro (modo lectura) — sin bslib::bs_theme_update (evita crashes por versiones)
  is_dark <- reactiveVal(FALSE)
  observeEvent(input$toggle_theme, {
    is_dark(!is_dark())
    session$sendCustomMessage("toggle-dark", list(enable = is_dark()))
  }, ignoreInit = TRUE)



  # --- Número de restricciones (dinámico) ---
  n_restr <- reactiveVal(2)  # empieza con 2
  observeEvent(input$add_restr, {
    if (n_restr() < 5) n_restr(n_restr() + 1)
  })
  observeEvent(input$rem_restr, {
    if (n_restr() > 2) n_restr(n_restr() - 1)
  })

  output$restr_buttons <- renderUI({
    n <- n_restr()
    tags$div(
      class = "d-flex gap-2",
      actionButton(
        "add_restr", label = NULL, icon = icon("plus"),
        class = "btn btn-sm btn-outline-secondary",
        disabled = (n >= 5)
      ),
      actionButton(
        "rem_restr", label = NULL, icon = icon("minus"),
        class = "btn btn-sm btn-outline-secondary",
        disabled = (n <= 2)
      ),
      actionButton(
        "reset_restr", label = NULL, icon = icon("undo"),
        class = "btn btn-sm btn-outline-secondary"
      )
    )
  })

  observeEvent(input$reset_restr, {
    n_restr(2)

    # Resetear valores por defecto de R1 y R2 (títulos incluidos)
    updateTextInput(session, "r1title", value = "")
    updateNumericInput(session, "a11", value = 1)
    updateNumericInput(session, "a12", value = 1)
    updateSelectInput(session, "s1", selected = "≤")
    updateNumericInput(session, "b1", value = 8)

    updateTextInput(session, "r2title", value = "")
    updateNumericInput(session, "a21", value = 1)
    updateNumericInput(session, "a22", value = 1)
    updateSelectInput(session, "s2", selected = "≤")
    updateNumericInput(session, "b2", value = 10)
  })

  restr_colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")

  # --- Controles del gráfico (checkboxes + nivel isocoste) ---
  output$plot_controls <- renderUI({
    tagList(
      tags$div(
        class = "plot-controls",
        tags$div(
          class = "controls-left",
          checkboxInput("show_labels", label = textOutput("lbl_show_labels", container = span), value = TRUE),
          checkboxInput("freeze_axes", label = textOutput("lbl_freeze_axes", container = span), value = FALSE),
          tags$div(
            class = "iso-inline",
            checkboxInput("show_iso", label = textOutput("lbl_show_iso", container = span), value = FALSE),
            conditionalPanel(
              condition = "input.show_iso == true",
              tags$div(
                class = "iso-slider-inline",
                tags$span("Nivel z:", class = "iso-label"),
                sliderInput("z_level", label = NULL, min = 0, max = 50, value = 10, step = 0.5, width = "110px")
              )
            )
          )
        ),
        tags$div(
          class = "controls-right",
          uiOutput("hover_inline"),
          actionButton("toggle_theme", label = NULL, icon = icon("moon"), class = "btn btn-outline-secondary btn-sm theme-btn")
        )
      )
    )
  })



  # Congelar límites de ejes cuando se activa 'Fijar ejes'
  fixed_lim <- reactiveVal(NULL)
  observeEvent(input$freeze_axes, {
    if (isTRUE(input$freeze_axes)) fixed_lim(plot_limits())
    else fixed_lim(NULL)
  }, ignoreInit = TRUE)

  observe({
    if (isTRUE(input$show_iso)) {
      sol <- solve_lp()
      base <- if (isTRUE(sol$ok) && sol$status == 0 && is.finite(sol$z)) sol$z else 10
      # Mantener el valor actual del slider; solo ajustamos el rango alrededor de la FO "real"
      updateSliderInput(session, "z_level",
                        min = max(0, base - 30),
                        max = base + 30)
    }
  })
# Al activar Isocostes, el primer valor del slider debe coincidir con la recta FO
  # que ya estaba dibujada antes de marcar el checkbox (normalmente z* si existe óptimo).
  observeEvent(input$show_iso, {
    if (!isTRUE(input$show_iso)) return()
    sol <- solve_lp()
    base <- if (isTRUE(sol$ok) && sol$status == 0 && is.finite(sol$z)) sol$z else 10
    # Reiniciar SIEMPRE la FO y sus isocostes al activarlos: volvemos a la FO real (z* si existe)
    updateSliderInput(session, "z_level",
                      min = max(0, base - 30),
                      max = base + 30,
                      value = base)
  }, ignoreInit = TRUE)
output$restricciones_ui <- renderUI({
    n <- n_restr()
    l <- session$userData$lang()
    tx <- session$userData$texts[[l]]
    tagList(
      lapply(seq_len(n), function(i) {
      col <- restr_colors[i]
      card(
        class = "mb-3",
        style = paste0("border-left: 5px solid ", col, ";"),
        card_header(
          div(
            class = "d-flex align-items-center gap-2",
            div(class = "fw-semibold", paste0("R", i)),
            div(
              class = "flex-grow-1",
              textInput(
                paste0("r", i, "title"), label = NULL,
                value = isolate(input[[paste0("r", i, "title")]]) %||% "",
                placeholder = tx[["ph_restr_title"]],
                width = "100%"
              )
            )
          )
        ),
        div(
          class = "ineq-row",
          numericInput(paste0("a", i, "1"), label = NULL, value = num_or(isolate(input[[paste0("a", i, "1")]]), 1), step = 0.5, width = "84px"),
          span(class = "mx-var", shiny::HTML("\\(x_1\\)")),
          span(class = "mx-1", "+"),
          numericInput(paste0("a", i, "2"), label = NULL, value = num_or(isolate(input[[paste0("a", i, "2")]]), 1), step = 0.5, width = "84px"),
          span(class = "mx-var", shiny::HTML("\\(x_2\\)")),
          selectInput(paste0("s", i), label = NULL, choices = c("≤", "≥"),
                      selected = isolate(input[[paste0("s", i)]]) %||% "≤", width = "90px", selectize = FALSE),
          numericInput(paste0("b", i), label = NULL,
                       value = num_or(isolate(input[[paste0("b", i)]]), if (i == 1) 8 else if (i == 2) 10 else 10 + (i - 2)),
                       step = 0.5, width = "84px")
        )
      )
      }),
)
  })


    # --- Convertir a forma A x <= b ---
  ineqs <- reactive({
    n <- n_restr()

    A <- matrix(NA_real_, nrow = n, ncol = 2)
    b <- rep(NA_real_, n)
    s <- rep(NA_character_, n)

    for (i in seq_len(n)) {
      a1 <- num_or(input[[paste0("a", i, "1")]], 1)
      a2 <- num_or(input[[paste0("a", i, "2")]], 1)
      bi <- num_or(input[[paste0("b", i)]], if (i == 1) 8 else if (i == 2) 10 else 10 + (i - 2))
      si <- input[[paste0("s", i)]] %||% "≤"

      A[i, ] <- c(a1, a2)
      b[i] <- bi
      s[i] <- si
    }

    # Convertir ≥ a ≤ multiplicando por -1
    for (i in seq_len(n)) {
      if (s[i] == "≥") { A[i, ] <- -A[i, ]; b[i] <- -b[i] }
    }

    if (isTRUE(input$nn)) {
      A <- rbind(A,
                 c(-1, 0),
                 c(0, -1))
      b <- c(b, 0, 0)
    }

    list(A = A, b = b)
  })

# --- Solver (lpSolve) ---
  build_lp <- function(A, b, cvec, nn) {
    if (isTRUE(nn)) {
      list(obj = cvec,
           const_mat = A,
           const_rhs = b,
           map = function(sol) sol[1:2])
    } else {
      # variables libres: x = x+ - x-
      A2 <- cbind(A[, 1], -A[, 1], A[, 2], -A[, 2])
      obj2 <- c(cvec[1], -cvec[1], cvec[2], -cvec[2])
      map2 <- function(sol) c(sol[1] - sol[2], sol[3] - sol[4])
      list(obj = obj2, const_mat = A2, const_rhs = b, map = map2)
    }
  }

  solve_lp <- reactive({
    AB <- ineqs()
    cvec <- c(input$c1, input$c2)
    lpdat <- build_lp(AB$A, AB$b, cvec, nn = isTRUE(input$nn))
    
    # boot::simplex uses maxi = TRUE for Maximization
    is_max <- if (identical(input$sense, "Max")) TRUE else FALSE
    
    # boot::simplex parameters: a (objective), A1 (<= constraints matrix), b1 (<= rhs)
    res <- tryCatch(
      boot::simplex(a = lpdat$obj,
                    A1 = lpdat$const_mat,
                    b1 = lpdat$const_rhs,
                    maxi = is_max),
      error = function(e) e
    )
    
    if (inherits(res, "error")) {
      return(list(ok = FALSE, status = NA_integer_, status_txt = "ERROR", message = res$message,
                  x = NA_real_, y = NA_real_, z = NA_real_))
    }
    
    # Map boot::simplex status to your downstream rendering logic
    # boot::simplex returns: 1 (optimal), -1 (infeasible), 0 (maxiter/unbounded)
    st <- 0
    if (res$solved == 1) {
      st <- 0
    } else if (res$solved == -1) {
      st <- 2
    } else {
      st <- 3 
    }
    
    status_txt <- dplyr::case_when(
      st == 0 ~ "OPTIMAL",
      st == 2 ~ "INFEASIBLE",
      st == 3 ~ "UNBOUNDED",
      TRUE ~ "UNKNOWN"
    )
    
    xy <- if (st == 0) lpdat$map(res$soln) else c(NA_real_, NA_real_)
    z  <- if (st == 0) sum(cvec * xy) else NA_real_
    
    list(ok = TRUE, status = st, status_txt = status_txt,
         x = xy[1], y = xy[2], z = z, message = "")
  })

    # --- Geometría: rectas, candidatos, vértices factibles ---
  lines_eq <- reactive({
    n <- n_restr()
    L <- list()
    for (i in seq_len(n)) {
      a1 <- num_or(input[[paste0("a", i, "1")]], 1)
      a2 <- num_or(input[[paste0("a", i, "2")]], 1)
      bi <- num_or(input[[paste0("b", i)]], if (i == 1) 8 else if (i == 2) 10 else 10 + (i - 2))
      L[[paste0("R", i)]] <- c(a1, a2, bi)
    }
    L$X0 <- c(1, 0, 0)
    L$Y0 <- c(0, 1, 0)
    L
  })

intersect2 <- function(l1, l2) {
    A <- matrix(c(l1[1], l1[2], l2[1], l2[2]), 2, 2, byrow = TRUE)
    d <- det(A)
    if (is.na(d) || abs(d) < EPS) return(c(NA_real_, NA_real_))
    as.numeric(solve(A, c(l1[3], l2[3])))
  }

  candidates <- reactive({
    L <- lines_eq()
    nm <- names(L)
    P <- list()
    k <- 1

    for (i in seq_len(length(L) - 1)) {
      for (j in (i + 1):length(L)) {
        p <- intersect2(L[[i]], L[[j]])
        P[[k]] <- tibble(x = p[1], y = p[2], l1 = nm[i], l2 = nm[j])
        k <- k + 1
      }
    }

    bind_rows(P) %>%
      filter(is.finite(x), is.finite(y))
  })

  is_feasible_pt <- function(x, y, A, b, eps = EPS) {
    all((A %*% c(x, y)) <= (b + eps))
  }

  vertices <- reactive({
    AB <- ineqs()
    C <- candidates()
    if (nrow(C) == 0) return(tibble())

    C %>%
      rowwise() %>%
      mutate(ok = is_feasible_pt(x, y, AB$A, AB$b)) %>%
      ungroup() %>%
      filter(ok) %>%
      mutate(xr = round(x, 6), yr = round(y, 6)) %>%
      distinct(xr, yr, .keep_all = TRUE) %>%
      select(x, y)
  })

  # Orden y etiquetas para vértices (A, B, C...) de forma estable (por ángulo alrededor del centroide)
  label_vertices <- function(V) {
    if (is.null(V) || nrow(V) == 0) return(tibble())
    cx <- mean(V$x); cy <- mean(V$y)
    V2 <- V %>%
      mutate(.ang = atan2(y - cy, x - cx)) %>%
      arrange(.ang, x, y) %>%
      mutate(lbl = LETTERS[seq_len(n())]) %>%
      select(x, y, lbl)
    V2
  }

  vertices_labeled <- reactive({
    label_vertices(vertices())
  })

  # --- Límites del plot más robustos (interceptos + óptimo si existe) ---
  intercepts_nonneg <- function(a1, a2, b) {
    a1 <- num_or(a1, 0)
    a2 <- num_or(a2, 0)
    b  <- num_or(b,  0)
    pts <- list()
    if (abs(a1) > EPS) {
      xint <- b / a1
      if (is.finite(xint) && xint >= 0) pts[[length(pts) + 1]] <- c(xint, 0)
    }
    if (abs(a2) > EPS) {
      yint <- b / a2
      if (is.finite(yint) && yint >= 0) pts[[length(pts) + 1]] <- c(0, yint)
    }
    if (length(pts) == 0) return(matrix(numeric(0), ncol = 2))
    do.call(rbind, pts)
  }

  plot_limits <- reactive({
    pts <- matrix(c(0, 0), ncol = 2, byrow = TRUE)

        n <- n_restr()

    for (i in seq_len(n)) {
      pts <- rbind(pts, intercepts_nonneg(input[[paste0("a", i, "1")]], input[[paste0("a", i, "2")]], input[[paste0("b", i)]]))
    }

    sol <- solve_lp()
    if (isTRUE(sol$ok) && sol$status == 0 && is.finite(sol$x) && is.finite(sol$y)) {
      pts <- rbind(pts, c(sol$x, sol$y))
    }

    # Sanitizar
    pts <- pts[is.finite(pts[, 1]) & is.finite(pts[, 2]), , drop = FALSE]
    if (nrow(pts) == 0) pts <- matrix(c(0, 0), ncol = 2, byrow = TRUE)

    xmax <- max(pts[, 1], 0)
    ymax <- max(pts[, 2], 0)

    # Evitar rangos degenerados
    xmax <- max(10, xmax)
    ymax <- max(10, ymax)

    pad_x <- 0.15 * xmax
    pad_y <- 0.15 * ymax

    list(xmin = 0, xmax = xmax + pad_x, ymin = 0, ymax = ymax + pad_y)
  })

  # --- Polígono exacto (clipped al cuadro del plot) ---
  poly_feasible <- reactive({
    lim <- (fixed_lim() %||% plot_limits())
    AB <- ineqs()

    # Añadimos límites del cuadro como restricciones: x<=xmax, y<=ymax, -x<=0, -y<=0
    Aclip <- rbind(AB$A,
                   c(1, 0),
                   c(0, 1),
                   c(-1, 0),
                   c(0, -1))
    bclip <- c(AB$b, lim$xmax, lim$ymax, 0, 0)

    # Generar candidatos (intersecciones entre todas las rectas de Aclip x = bclip)
    m <- nrow(Aclip)
    P <- list()
    k <- 1

    for (i in seq_len(m - 1)) {
      for (j in (i + 1):m) {
        Ai <- Aclip[i, ]; Aj <- Aclip[j, ]
        M <- rbind(Ai, Aj)
        d <- det(M)
        if (!is.na(d) && abs(d) >= EPS) {
          p <- as.numeric(solve(M, c(bclip[i], bclip[j])))
          P[[k]] <- tibble(x = p[1], y = p[2])
          k <- k + 1
        }
      }
    }

    if (length(P) == 0) return(tibble())

    pts <- bind_rows(P) %>%
      filter(is.finite(x), is.finite(y))

    if (nrow(pts) == 0) return(tibble())

    # Filtrar factibles con clipping
    pts <- pts %>%
      rowwise() %>%
      mutate(ok = is_feasible_pt(x, y, Aclip, bclip)) %>%
      ungroup() %>%
      filter(ok) %>%
      mutate(xr = round(x, 6), yr = round(y, 6)) %>%
      distinct(xr, yr, .keep_all = TRUE) %>%
      select(x, y)

    if (nrow(pts) < 3) return(tibble())

    # Ordenar alrededor del centro
    cx <- mean(pts$x)
    cy <- mean(pts$y)
    pts %>%
      mutate(ang = atan2(y - cy, x - cx)) %>%
      arrange(ang) %>%
      select(x, y)
  })

  # --- Plot ---
  output$plt <- renderPlot({
    lim <- (fixed_lim() %||% plot_limits())
    poly <- poly_feasible()
    V <- vertices_labeled()
    sol <- solve_lp()

    # Datos para rectas (en el cuadro)
    make_line_df <- function(a1, a2, b, nm) {
      xs <- seq(lim$xmin, lim$xmax, length.out = 200)
      if (abs(a2) > EPS) {
        ys <- (b - a1 * xs) / a2
        tibble(x = xs, y = ys, name = nm)
      } else if (abs(a1) > EPS) {
        xconst <- b / a1
        tibble(x = rep(xconst, 200), y = seq(lim$ymin, lim$ymax, length.out = 200), name = nm)
      } else {
        tibble(x = numeric(0), y = numeric(0), name = nm)
      }
    }

        df_lines <- list()
    for (i in seq_len(n_restr())) {
      a1 <- num_or(input[[paste0("a", i, "1")]], 1)
      a2 <- num_or(input[[paste0("a", i, "2")]], 1)
      bi <- num_or(input[[paste0("b", i)]], if (i == 1) 8 else if (i == 2) 10 else 10 + (i - 2))
      df_lines[[i]] <- make_line_df(a1, a2, bi, paste0("R", i)) %>%
        mutate(col = restr_colors[i])
    }
    df_lines <- bind_rows(df_lines)

dark <- isTRUE(is_dark())
    # Colores dependientes del tema
    col_iso <- if (dark) "#F9FAFB" else "black"   # isocostes discontinuas
    col_fo  <- if (dark) "#F9FAFB" else "black"   # recta FO principal
    col_vz  <- if (dark) "#F9FAFB" else "black"

p <- ggplot() +
      coord_cartesian(xlim = c(lim$xmin, lim$xmax), ylim = c(lim$ymin, lim$ymax), expand = FALSE, clip = "off") +
      labs(x = expression(x[1]), y = expression(x[2])) +
      theme_minimal(base_size = 15) +
      theme(
        legend.position = "none",
        axis.title = element_text(size = 16, face = "bold", colour = if (dark) "#e5e7eb" else "#111827"),
        axis.text  = element_text(size = 13, face = "bold", colour = if (dark) "#e5e7eb" else "#111827"),
        panel.background = element_rect(fill = if (dark) "#0b0f14" else "white", colour = NA),
        plot.background  = element_rect(fill = if (dark) "#0b0f14" else "white", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = if (dark) "#1f2937" else "#e9ecef"),
        plot.margin = margin(14, 18, 10, 10)
      )

    if (nrow(poly) >= 3) {
      p <- p + geom_polygon(data = poly, aes(x = x, y = y), alpha = 0.25, fill = if (dark) "#94a3b8" else "grey60")
    }

        if (nrow(df_lines) > 0) {
      # cada restricción en su color, sin leyenda
      for (i in seq_len(n_restr())) {
        dfi <- df_lines %>% filter(name == paste0("R", i))
        if (nrow(dfi) > 0) {
          p <- p + geom_line(
            data = dfi, aes(x = x, y = y),
            linewidth = 1, color = restr_colors[i],
            linetype = "solid", show.legend = FALSE
          )
        }
      }
    }


    
    # Recta de la Función Objetivo (isocoste principal) — siempre visible
    c1 <- num_or(input$c1, 0)
    c2 <- num_or(input$c2, 0)
    # nivel para la recta principal: si hay óptimo, z*; si no, usamos el slider (o 10)
    z_base <- if (isTRUE(input$show_iso)) num_or(input$z_level, if (isTRUE(sol$ok) && sol$status == 0) sol$z else 10) else if (isTRUE(sol$ok) && sol$status == 0 && is.finite(sol$z)) sol$z else num_or(input$z_level, 10)

    if (abs(c1) + abs(c2) > EPS) {
      if (abs(c2) > EPS) {
        xs_fo <- seq(lim$xmin, lim$xmax, length.out = 260)
        p <- p + geom_line(
          data = tibble(x = xs_fo, y = (z_base - c1 * xs_fo) / c2),
          aes(x = x, y = y),
          inherit.aes = FALSE,
          linewidth = 1.6,
          color = col_fo
        )
      } else if (abs(c1) > EPS) {
        p <- p + geom_vline(xintercept = z_base / c1, linewidth = 1.6, color = col_fo)
      }
    }


# Puntos candidatos (SBNF en negro) y vértices factibles (SBF en verde)
    C_all <- candidates()
    if (nrow(C_all) > 0) {
      AB <- ineqs()
      C_all <- C_all %>%
        rowwise() %>%
        mutate(ok = is_feasible_pt(x, y, AB$A, AB$b)) %>%
        ungroup()

      C_nf <- C_all %>% filter(!ok)
      if (nrow(C_nf) > 0) {
        p <- p + geom_point(data = C_nf, aes(x = x, y = y), size = 3.2, color = "#800020")
      }
    }

    if (nrow(V) > 0) {
      p <- p + geom_point(data = V, aes(x = x, y = y), size = 4.6, color = "#6B8E23")
      if (isTRUE(input$show_labels)) {
        xr <- lim$xmax - lim$xmin
        yr <- lim$ymax - lim$ymin
        dx <- 0.02 * xr
        dy <- 0.04 * yr
        Vlab <- V %>% mutate(xl = x + dx, yl = y + dy)
        p <- p + geom_label(
          data = Vlab,
          aes(x = xl, y = yl, label = lbl),
          inherit.aes = FALSE,
          fill = "white",
          alpha = 0.7,
          label.size = 0,
          color = col_vz,
          fontface = "bold",
          size = 5
        )
      }
    }

    if (isTRUE(sol$ok) && sol$status == 0) {
      p <- p + geom_point(aes(x = sol$x, y = sol$y), size = 3.2, shape = 16, color = "black")

      # Círculo resaltando el vértice óptimo (círculo visual sin forzar coord_fixed)
      # Para que se vea como círculo aunque las escalas de ejes difieran, ajustamos el radio en Y
      # usando el tamaño real del plot en píxeles.
      w_px <- session$clientData$output_plt_width
      h_px <- session$clientData$output_plt_height
      if (is.null(w_px) || is.na(w_px) || w_px <= 0) w_px <- 900
      if (is.null(h_px) || is.na(h_px) || h_px <= 0) h_px <- 520

      xr <- lim$xmax - lim$xmin
      yr <- lim$ymax - lim$ymin

      rad_x <- 0.03 * max(xr, yr)
      # rad_y para que el radio en píxeles sea el mismo que en X
      rad_y <- rad_x * (yr * w_px) / (xr * h_px)

      t <- seq(0, 2*pi, length.out = 140)
      circ <- tibble(x = sol$x + rad_x * cos(t), y = sol$y + rad_y * sin(t))
      p <- p + geom_path(
        data = circ, aes(x = x, y = y),
        inherit.aes = FALSE,
        color = col_vz, linetype = "dashed", linewidth = 0.9
      )

      
      # Isocostes (familia + recta resaltada) y dirección de mejora (si está activado)
      if (isTRUE(input$show_iso)) {
        c1 <- num_or(input$c1, 0)
        c2 <- num_or(input$c2, 0)
        z0 <- z_base
        # si el óptimo existe, proponemos centrar el slider alrededor del óptimo
        # (el slider se actualiza fuera para no forzar re-render si no se toca)
        if (abs(c1) + abs(c2) > EPS) {
          # familia de isocostes alrededor de z0
          zs <- setdiff(seq(z0 - 5, z0 + 5, by = 1), z0)
          # dibujar líneas
          if (abs(c2) > EPS) {
            xs <- seq(lim$xmin, lim$xmax, length.out = 260)
            iso_df <- purrr::map_dfr(zs, function(zz) {
              tibble(x = xs, y = (zz - c1 * xs) / c2, z = zz)
            })
            p <- p + geom_line(
              data = iso_df, aes(x = x, y = y, group = z),
              inherit.aes = FALSE,
              linewidth = 0.6,
              linetype = "dashed",
              alpha = 0.35,
              color = col_iso
            )
          } else if (abs(c1) > EPS) {
            # verticales x = z/c1
            x_lines <- zs / c1
            p <- p + geom_vline(xintercept = x_lines, linetype = "dashed", alpha = 0.35, linewidth = 0.6, color = col_iso)
          }

          # Flecha dirección (max/min) basada en gradiente
          dir_mult <- if (identical(input$sense, "Max")) 1 else -1
          g <- c(c1, c2) * dir_mult
          gn <- sqrt(sum(g^2))
          if (is.finite(gn) && gn > EPS) {
            u <- g / gn
            xr <- lim$xmax - lim$xmin
            yr <- lim$ymax - lim$ymin
            L <- 0.10 * max(xr, yr)
            # Colocación dentro de la región factible (si existe), usando su centroide
            if (nrow(poly) >= 3) {
              x0 <- mean(poly$x); y0 <- mean(poly$y)
            } else if (nrow(V) > 0) {
              x0 <- mean(V$x); y0 <- mean(V$y)
            } else {
              x0 <- lim$xmin + 0.25 * xr
              y0 <- lim$ymin + 0.25 * yr
            }
            seg <- tibble(
              x = x0, y = y0,
              xend = x0 + L * u[1],
              yend = y0 + L * u[2],
              xlab = x0 + (L * 1.18) * u[1],
              ylab = y0 + (L * 1.18) * u[2],
              lab = if (identical(input$sense, "Max")) "△z" else "▽z"
            )
            p <- p + geom_segment(
              data = seg,
              aes(x = x, y = y, xend = xend, yend = yend),
              inherit.aes = FALSE,
              linewidth = 2.2,
              color = col_vz,
              arrow = grid::arrow(length = grid::unit(0.18, "cm"))
            ) +
              geom_text(
                data = seg,
                aes(x = xlab, y = ylab, label = lab),
                inherit.aes = FALSE,
                hjust = 0.5,
                vjust = 0.5,
                color = col_vz,
                fontface = "bold",
                size = 5.2
              )
          }
        }
      }

    }

    p
  })

# Coordenadas del cursor sobre el gráfico (overlay, no afecta a MathJax)
output$hover_inline <- renderUI({
  h <- input$plt_hover
  show <- !is.null(h) && is.finite(h$x) && is.finite(h$y)
  txt <- if (show) sprintf("x1 = %.2f  x2 = %.2f", h$x, h$y) else ""
  tags$span(
    class = "hover-chip",
    style = if (show) "display:inline-block;" else "display:none;",
    txt
  )
})


  # --- Status UI ---
  output$status <- renderUI({
    sol <- solve_lp()
    if (!isTRUE(sol$ok)) {
      return(HTML(sprintf("<b style='color:#b22222'>Error al resolver: %s</b>", sol$message)))
    }

    if (sol$status == 0) {
      l <- session$userData$lang()
      tx <- session$userData$texts[[l]]
      dir <- if (input$sense == "Max") tx[["dir_max"]] else tx[["dir_min"]]

      # Detectar óptimos múltiples (infinitas soluciones): varios vértices con el mismo valor óptimo
      Vlab <- vertices_labeled()
      multi_msg <- ""
      if (nrow(Vlab) >= 2 && is.finite(sol$z)) {
        c1 <- num_or(input$c1, 0)
        c2 <- num_or(input$c2, 0)
        vals <- c1 * Vlab$x + c2 * Vlab$y
        tol <- 1e-6 * max(1, abs(sol$z))
        idx <- which(abs(vals - sol$z) <= tol)
        if (length(idx) >= 2) {
          # Elegimos dos vértices "extremos" del conjunto óptimo
          Vopt <- Vlab[idx, , drop = FALSE]
          # tomar los dos más alejados entre sí
          dmat <- as.matrix(dist(Vopt[, c("x","y")]))
          ij <- which(dmat == max(dmat), arr.ind = TRUE)[1, ]
          A_lbl <- Vopt$lbl[ij[1]]
          B_lbl <- Vopt$lbl[ij[2]]
          multi_msg <- sprintf(tx[["inf_msg"]], A_lbl, B_lbl)
        }
      }

      return(tagList(
      withMathJax(HTML(sprintf(
        "<div class='mt-2'><b>%s</b> \\(x_1^* = %.3f,\\ x_2^* = %.3f\\), \\(z^* = %.3f\\).</div>%s",
        sprintf(tx[["opt_label"]], dir), sol$x, sol$y, sol$z, multi_msg
      ))),
      tags$script("if(window.MathJax) { if(MathJax.typesetPromise) { MathJax.typesetPromise(); } else if(MathJax.Hub) { MathJax.Hub.Queue(['Typeset', MathJax.Hub]); } }")
    ))
    }

    if (sol$status == 2) {
      return(HTML("<b style='color:#b22222'>Problema infactible (no hay región factible).</b>"))
    }

    if (sol$status == 3) {
      return(HTML("<b style='color:#b22222'>Problema no acotado (el óptimo no existe).</b>"))
    }

    HTML(sprintf("<b style='color:#b22222'>No se pudo determinar una solución óptima (status: %s).</b>", sol$status_txt))
  })

  # --- Fórmulas LaTeX ---
  output$formula <- renderUI({
    n <- n_restr()
    sn <- if (identical(input$sense, "Max")) "\\max" else "\\min"

    restr_lines <- lapply(seq_len(n), function(i) {
      si <- (input[[paste0("s", i)]] %||% "≤")
      s_ltx <- if (identical(si, "≤")) "\\le" else "\\ge"
      a1 <- num_or(input[[paste0("a", i, "1")]], 1)
      a2 <- num_or(input[[paste0("a", i, "2")]], 1)
      bi <- num_or(input[[paste0("b", i)]], 10 + max(0, i - 2))

      tags$div(
        class = "model-line",
        tags$span(class = "badge", style = paste0("background:", restr_colors[i]), paste0("R", i)),
        HTML(sprintf("&nbsp;\\(%.2f\\,x_1 + %.2f\\,x_2\\ %s\\ %.2f\\)", a1, a2, s_ltx, bi))
      )
    })

    withMathJax(tagList(
      tags$div(class = "mt-3",
        tags$div(
          class = "model-line",
          tags$span(class = "badge", style = "background:#7F8C8D", "FO"),
          HTML(sprintf("&nbsp;\\(%s\\, z = %.2f\\,x_1 + %.2f\\,x_2\\)", sn, num_or(input$c1, 0), num_or(input$c2, 0)))
        ),
        tags$div(class = "mt-1", tags$b("s.a:")),
        tags$div(restr_lines)
      )
    ))
  })



}


############################# PIME TEMPLATE WRAPPER (UI) #############################
# Añade título, explicación, créditos con logos y 3 botones de idioma (sin efecto).
# Importante: NO modifica ninguna funcionalidad de la app original.

.pime_header <- shiny::tagList(
  # Botones de idioma (por ahora sin efecto)
  shiny::absolutePanel(
    top = 10, right = 10, fixed = TRUE,
    shiny::actionButton("lang_es", "ES"),
    shiny::actionButton("lang_en", "EN"),
    shiny::actionButton("lang_va", "VAL")
  ),

  # Título + explicación (estilo plantilla)
  shiny::tags$div(
    style = "padding-top:50px; margin-bottom:30px;",
    shiny::uiOutput("pime_title_ui"),
    shiny::tags$div(
      style = "display:flex; justify-content:center;",
      shiny::tags$div(
        class = "pime-desc-box",
        style = "border:2px solid #000; border-radius:12px; padding:12px; width:95%; max-width:none; background:white; text-align:center; box-sizing:border-box;",
        shiny::uiOutput("pime_desc_ui")
      )
    )
  )
)

.pime_footer <- shiny::tags$div(
  style = "margin-top:10px; text-align:center; margin-bottom:20px;",
  shiny::tags$div(
    style = "display:flex; justify-content:center; align-items:center; gap:40px;",
    shiny::tags$img(src = "UPV.png", style = "height:85px; max-height:85px;"),
    shiny::tags$img(src = "DEIOAC.png", style = "height:65px; max-height:70px;")
  ),
  shiny::tags$div(
    style = "margin-top:8px;",
    shiny::uiOutput("pime_credits_ui")
  )
)

# Envolvemos la UI existente sin tocar su contenido
.ui_original <- ui
ui <- shiny::tagList(
shiny::tags$head(
  shiny::tags$script(shiny::HTML("
    if (window.Shiny) {
      Shiny.addCustomMessageHandler('typeset-math', function(x) {
        if (window.MathJax) {
          if (MathJax.typesetPromise) {
            MathJax.typesetPromise();
          } else if (MathJax.Hub && MathJax.Hub.Queue) {
            MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
          }
        }
      });
    }
  "))
),
  .pime_header,
  .ui_original,
  .pime_footer
)

# Hacemos wrapper del server para gestionar idioma (solo título + descripción)
.server_original <- server
server <- function(input, output, session) {

  # Estado de idioma (por defecto ES)
  lang <- shiny::reactiveVal("es")


# Re-render MathJax when language changes (supports MathJax v2 and v3)
observeEvent(lang(), {
  session$onFlushed(function() {
    session$sendCustomMessage("typeset-math", list())
  }, once = TRUE)
}, ignoreInit = TRUE)

  # Textos (solo cabecera PIME por ahora)
  texts <- list(
    es = list(
      title = "Método Gráfico - Programación Lineal (2 variables)",
      desc  = "Aplicación docente para resolver problemas de Programación Lineal con dos variables mediante el método gráfico, analizando restricciones, región factible y función objetivo.",
      credits = "STATIO es un Proyecto de Innovación y Mejora Educativa (PIME/25-26/562) desarrollado por el DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
      solutions_title = "Espacio de Soluciones",
      lbl_restricciones = "Restricciones",
      lbl_funcion_objetivo = "Función objetivo",
      lbl_naturaleza_vars  = "Naturaleza de las variables",
      lbl_show_labels = "Etiquetas",
      lbl_freeze_axes = "Fijar ejes",
      lbl_show_iso    = "Isocostes",
      ph_restr_title = "Título (p. ej., Capacidad del recurso ...)",
      opt_label = "Óptimo (%s) por solver:",
      dir_max   = "máximo",
      dir_min   = "mínimo",
      inf_msg   = "<div style='margin-top:6px;'><b>Infinitas soluciones:</b> todos los puntos del segmento \\(\\overline{%s%s}\\).</div>"

    ),
    en = list(
      title = "Graphical Method - Linear Programming (2 variables)",
      desc  = "Teaching app to solve two-variable Linear Programming problems using the graphical method, analyzing constraints, feasible region and objective function.",
      credits = "STATIO is an Educational Innovation and Improvement Project (PIME/25-26/562) developed by DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
      solutions_title = "Solution Space",
      lbl_restricciones = "Constraints",
      lbl_funcion_objetivo = "Objective function",
      lbl_naturaleza_vars  = "Variable nature",
      lbl_show_labels = "Labels",
      lbl_freeze_axes = "Lock axes",
      lbl_show_iso    = "Isocosts",
      ph_restr_title = "Title (e.g., Resource capacity ...)",
      opt_label = "Optimal (%s) by solver:",
      dir_max   = "maximum",
      dir_min   = "minimum",
      inf_msg   = "<div style='margin-top:6px;'><b>Infinite solutions:</b> all points on the segment \\(\\overline{%s%s}\\).</div>"

    ),
    va = list(
      title = "Mètode Gràfic - Programació Lineal (2 variables)",
      desc  = "Aplicació docent per a resoldre problemes de Programació Lineal amb dues variables mitjançant el mètode gràfic, analitzant restriccions, regió factible i funció objectiu.",
      credits = "STATIO és un Projecte d’Innovació i Millora Educativa (PIME/25-26/562) desenvolupat pel DEIOAC-<a href='https://upv.es' target='_blank'>UPV</a>.",
      solutions_title = "Espai de Solucions",
      lbl_restricciones = "Restriccions",
      lbl_funcion_objetivo = "Funció objectiu",
      lbl_naturaleza_vars  = "Naturalesa de les variables",
      lbl_show_labels = "Etiquetes",
      lbl_freeze_axes = "Fixar eixos",
      lbl_show_iso    = "Isocostos",
      ph_restr_title = "Títol (p. ex., Capacitat del recurs ...)",
      opt_label = "Òptim (%s) per solver:",
      dir_max   = "màxim",
      dir_min   = "mínim",
      inf_msg   = "<div style='margin-top:6px;'><b>Infinites solucions:</b> tots els punts del segment \\(\\overline{%s%s}\\).</div>"

    )
  )


  # Exponer idioma y diccionario al server original (para textos dinámicos sin romper scope)
  session$userData$lang  <- lang
  session$userData$texts <- texts

  # Cambiadores de idioma (afectan a textos UI seleccionados)
  shiny::observeEvent(input$lang_es, { lang("es") })
  shiny::observeEvent(input$lang_en, { lang("en") })
  shiny::observeEvent(input$lang_va, { lang("va") })

  # Render cabecera (título + descripción)
  output$pime_title_ui <- shiny::renderUI({
    shiny::tags$h2(texts[[lang()]][["title"]], align = "center")
  })

  output$pime_desc_ui <- shiny::renderUI({
    shiny::HTML(texts[[lang()]][["desc"]])
  })


  # Créditos (footer)
  output$pime_credits_ui <- shiny::renderUI({
    shiny::HTML(texts[[lang()]][["credits"]])
  })

  # Subtítulo "Espacio de Soluciones"
  output$solutions_header_ui <- shiny::renderUI({
    shiny::tags$span(texts[[lang()]][["solutions_title"]])
  })

  # Labels de controles del plot
  output$lbl_show_labels <- shiny::renderText({ texts[[lang()]][["lbl_show_labels"]] })
  output$lbl_freeze_axes <- shiny::renderText({ texts[[lang()]][["lbl_freeze_axes"]] })
  output$lbl_show_iso    <- shiny::renderText({ texts[[lang()]][["lbl_show_iso"]] })
  output$lbl_restricciones <- shiny::renderText({ texts[[lang()]][["lbl_restricciones"]] })
  output$lbl_funcion_objetivo <- shiny::renderText({ texts[[lang()]][["lbl_funcion_objetivo"]] })
  output$lbl_naturaleza_vars  <- shiny::renderText({ texts[[lang()]][["lbl_naturaleza_vars"]] })


  .server_original(input, output, session)
}
########################### FIN PIME TEMPLATE WRAPPER (UI) ###########################
shinyApp(ui, server)