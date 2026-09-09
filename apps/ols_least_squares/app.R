# ============================================================
#  Econometrics I 26-27 — Unit 2
#  "What least squares minimizes"
#
#  Unit 2's objectives frame promises that students will
#  "obtain the OLS estimates, and know exactly what 'least
#  squares' minimizes". The deck states the criterion; nothing
#  in it SHOWS the squares. This app does.
#
#  Move the line by hand. Every residual is drawn as a real
#  square, to scale, so its AREA is the residual squared. Their
#  total area is the SSR, shown as one number. Then press the
#  button: OLS is lower than anything you found by hand.
#
#  ONE TOPIC, ONE APP (Cristina, 9 Sep): each tool is its own
#  site with its own link and QR on the Campus, so students meet
#  them one at a time. Do NOT merge these into a tabbed app.
#
#  WORKS UNATTENDED (Cristina, 9 Sep): used both in class and by
#  students at home, so the panel says what to look at and the
#  defaults already pose the problem.
#
#  DATA: CAschools, the Unit 2-3 spine (PLAN Phase 02b). Seven
#  districts, spread across the income range, fixed seed -- the
#  picture is identical in class and at home. See prep_data.R for
#  why seven and why spread.
#
#  Run:     shiny::runApp("ols_least_squares")
#  Publish: shinylive::export("ols_least_squares", "_site_ols_least_squares")
# ============================================================

library(shiny)

## ---- house palette: econ1.sty, NOT the concurso app's ------
## The concurso app hardcodes navy = #1F4E79 (blue) and garnet =
## #9B2335 (red). econ1.sty redefines those SAME NAMES as forest
## green and terracotta. Copying the old hexes forward would give
## apps that do not match the decks.
navy   <- "#2F4A37"   # deep forest green (primary accent)
garnet <- "#B05A3C"   # terracotta        (secondary accent)
muted  <- "#6B6B66"
soft   <- "#F1F3F0"
steel  <- "#4F7088"

## ---- data -------------------------------------------------
data_path <- function(f) {
  cand <- c(f, file.path("ols_least_squares", f))
  hit  <- cand[file.exists(cand)]
  if (length(hit)) hit[1] else cand[1]
}
ca <- read.csv(data_path("caschools_teaching.csv"))
## as.logical: guard against the flag arriving as 1/0, which would turn this
## into numeric ROW INDEXING instead of a filter and silently return row 1
## seven times (that bug was live for one build on 2026-09-09).
dd <- ca[as.logical(ca$teach7), c("income", "math")]
dd <- dd[order(dd$income), ]
stopifnot(nrow(dd) == 7)

fit  <- lm(math ~ income, dd)
B0   <- unname(coef(fit)[1])
B1   <- unname(coef(fit)[2])
SSR0 <- sum(resid(fit)^2)

ssr <- function(b0, b1) sum((dd$math - (b0 + b1 * dd$income))^2)

## The best line the SLIDERS can reach. The button must land here, not on a
## naive rounding of the continuous optimum: rounding B0 to 630 gives an SSR
## 6.6 above the reachable minimum, and the "this is the OLS line" message
## would then never appear. Found once, at start-up.
grid  <- expand.grid(b0 = seq(580, 700, 1), b1 = seq(-1, 4, 0.05))
gv    <- mapply(ssr, grid$b0, grid$b1)
gbest <- grid[which.min(gv), ]
SSRg  <- min(gv)                       # 1736.45, vs 1735.87 unconstrained

## ====================  USER INTERFACE  ======================
ui <- fluidPage(
  tags$style(HTML(sprintf("
    body { background:%s; }
    .box  { background:white; border:1px solid #e0e0dd; border-radius:6px;
            padding:10px 14px; margin-bottom:8px; }
    .ssrnum { font-size:34px; font-weight:700; color:%s; line-height:1.15; }
    .note { color:%s; font-size:13px; }
    h2 { color:%s; }", soft, garnet, muted, navy))),

  titlePanel("What least squares minimizes"),
  p(class = "note",
    "Econometrics I · Unit 2 · the simple regression model. ",
    "California school districts: ", strong("math"),
    " is the district average maths score, ", strong("income"),
    " the district average income in thousands of dollars. ",
    "Seven districts, spread across the income range."),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      p(strong("Move the line by hand:")),
      sliderInput("b0", "Intercept  b₀", min = 580, max = 700, value = 660, step = 1),
      sliderInput("b1", "Slope  b₁",     min = -1,  max = 4,   value = 0,   step = 0.05),
      checkboxInput("sq", "Draw each residual as a square", TRUE),
      br(),
      actionButton("ols", "Show me the OLS line", class = "btn-primary"),
      actionButton("reset", "Start again"),
      hr(),
      div(class = "box",
          div(class = "note", "Smallest SSR you have reached by hand"),
          div(style = "font-size:21px;font-weight:600;",
              textOutput("best", inline = TRUE))),
      p(class = "note",
        strong("What to look at."), " The side of each square is the residual, so its ",
        em("area"), " is the residual squared. The SSR is the total shaded area. ",
        "Try to make it smaller than the OLS line does — you cannot. ",
        "That is all the words “least squares” mean.")
    ),

    mainPanel(
      width = 8,
      div(class = "box",
          fluidRow(
            column(5,
              div(class = "note", "Sum of squared residuals   SSR"),
              div(class = "ssrnum", textOutput("ssr", inline = TRUE))),
            column(7,
              div(class = "note", "Your line"),
              div(style = "font-size:18px;padding-top:8px;color:#2F4A37;",
                  textOutput("eq", inline = TRUE)),
              div(class = "note", style = "padding-top:6px;",
                  textOutput("verdict", inline = TRUE)))
          )),
      plotOutput("plot", height = "440px"),
      hr(),
      p(class = "note", style = "font-size:12px;",
        strong("Data."), " California Test Score Data — 420 school districts. ",
        "Online complements to Stock, J. H. and Watson, M. W. (2007), ",
        em("Introduction to Econometrics"), ", 2nd ed., Addison Wesley; ",
        "distributed in the R package ", code("AER"), " as ", code("CASchools"), ".")
    )
  )
)

## ====================  SERVER  ==============================
server <- function(input, output, session) {

  best    <- reactiveVal(Inf)
  current <- reactive(ssr(input$b0, input$b1))

  ## Only count lines the student set themselves, not the OLS button.
  cheated <- reactiveVal(FALSE)
  observeEvent(input$ols,   cheated(TRUE))
  observeEvent(input$reset, cheated(FALSE))
  observe({
    if (!isTRUE(cheated()) && current() < best()) best(current())
  })

  observeEvent(input$ols, {
    updateSliderInput(session, "b0", value = gbest$b0)
    updateSliderInput(session, "b1", value = gbest$b1)
  })

  observeEvent(input$reset, {
    updateSliderInput(session, "b0", value = 660)
    updateSliderInput(session, "b1", value = 0)
    best(Inf)
  })

  output$ssr  <- renderText(format(round(current()), big.mark = " "))
  output$best <- renderText(
    if (is.infinite(best())) "—" else format(round(best()), big.mark = " "))
  output$eq   <- renderText(
    sprintf("matĥ = %.0f %+.2f · income", input$b0, input$b1))

  output$verdict <- renderText({
    gap <- current() - SSRg
    if (gap < 0.5)      sprintf("This is the OLS line. Nothing reaches below %.0f.", SSR0)
    else if (gap < 300) sprintf("Close. OLS reaches %.0f, %.0f lower than this.", SSR0, gap)
    else                sprintf("OLS reaches %.0f, %.0f lower than this line.", SSR0, gap)
  })

  output$plot <- renderPlot({
    b0 <- input$b0; b1 <- input$b1
    x  <- dd$income; y <- dd$math
    yh <- b0 + b1 * x; e <- y - yh

    xr  <- range(x); pad <- diff(xr) * 0.18
    par(mar = c(4.4, 4.6, 1, 1), bg = "white")
    plot(NA, xlim = c(xr[1] - pad * 0.6, xr[2] + pad),
         ylim = range(c(y, yh)) + c(-15, 15), las = 1,
         xlab = "District income (thousands of dollars)",
         ylab = "Average maths score",
         col.lab = navy, cex.lab = 1.15, cex.axis = 1.05)

    ## Squares that LOOK square on screen. One user unit of y is not one user
    ## unit of x, so the width is converted through the physical plot size
    ## (par("pin"), inches). Verified 2026-09-09: side-in-inches matches
    ## height-in-inches to 3 decimals.
    if (isTRUE(input$sq)) {
      pin <- par("pin"); usr <- par("usr")
      per_x <- pin[1] / (usr[2] - usr[1])
      per_y <- pin[2] / (usr[4] - usr[3])
      w   <- abs(e) * per_y / per_x
      sgn <- ifelse(x > mean(xr), -1, 1)      # keep the squares inside the frame
      rect(x, pmin(y, yh), x + sgn * w, pmax(y, yh),
           col = adjustcolor(garnet, alpha.f = 0.20),
           border = adjustcolor(garnet, alpha.f = 0.60))
    }

    segments(x, y, x, yh, col = muted, lty = 3)
    abline(a = b0, b = b1, col = navy, lwd = 3)
    points(x, y, pch = 21, bg = steel, col = "white", cex = 1.7, lwd = 1.5)
  })
}

shinyApp(ui, server)
