# ============================================================
#  Econometrics I 26-27 — Unit 2
#  Two pages, one app:
#    1. "What least squares minimizes"  (CAschools, 7 districts)
#    2. "The true line and the sample line"  (simulated)
#
#  WHY TWO PAGES AND NOT TWO APPS (Cristina, 15 Sep). The 9 Sep
#  rule "one topic, one app -- do NOT merge these into a tabbed
#  app" was about NOT putting Unit 2's tool and Unit 3's tool in
#  one window. It still stands: partialling_out/ stays its own
#  site. These two pages are both UNIT 2, both about the same
#  fitted line, and they are meant to be met in order -- page 1
#  says what OLS minimizes, page 2 says what OLS is estimating.
#  Each page carries its own "How to use", so a student who opens
#  the link alone knows what to do.
#
#  PAGE 2, WHY SIMULATED DATA. With CAschools there is no true
#  line to draw: it does not exist as anything observable. The
#  whole point of page 2 is to show the one picture real data can
#  never give -- the population line next to the sample line --
#  and to separate the ERROR u (distance to the true line) from
#  the RESIDUAL e (distance to the fitted line). Students leave
#  this course believing those are the same object.
#
#  DELIBERATELY NOT HERE: the histogram of b1 over many samples
#  (Cristina, 15 Sep). Repeated sampling and the sampling
#  distribution are UNIT 5's, and the R scripts for them already
#  exist. Page 2 shows ONE sample at a time. Do not add it.
#
#  REPRODUCIBLE. Page 1 has a fixed seed. Page 2 draws its
#  population once from a fixed seed, and each new sample uses
#  the click counter as its seed, so reloading the page and
#  clicking the same number of times gives the same picture in
#  class and at home.
#
#  SIGMA IS FIXED AT 5, AND THERE IS NO SLIDER FOR IT (Cristina,
#  15 Sep: "too much info on the same page"). Moving sigma teaches
#  how the error variance drives the PRECISION of the estimator --
#  Var(b1) = sigma^2 / SST_x. That is the width of the sampling
#  distribution reached by a slider instead of by a histogram, so
#  it belongs with the histogram in UNIT 5, and it also needs
#  sigma^2, which students do not meet until Unit 4. Page 2 keeps
#  one message: the estimate is not the truth.
#  If it is ever wanted back: add the sliderInput and put
#  input$sigma where SIGMA is. The standard normal draws are fixed
#  once, so sigma only RESCALES them -- the cloud stretches and
#  the true line stays put, instead of the data reshuffling.
#
#  BASE R ONLY -- no package beyond shiny, so shinylive/webr
#  needs nothing extra.
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

## ============================================================
##  PAGE 1 DATA -- CAschools, seven districts
## ============================================================
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

## ============================================================
##  PAGE 2 POPULATION -- invented on purpose
## ============================================================
## An INVENTED population, not real data. That is the point: we
## can only compare the fitted line with the truth if we are the
## ones who chose the truth. x and the standard normal draws are
## fixed here, once, and SIGMA multiplies them.
NPOP  <- 2000
TB0   <- 10     # the true intercept
TB1   <- 2      # the true slope
SIGMA <- 5      # the standard deviation of the errors -- FIXED, see below
set.seed(20262027)
xpop <- round(runif(NPOP, 2, 18), 2)
zpop <- rnorm(NPOP)

## ====================  USER INTERFACE  ======================
css <- sprintf("
  body { background:%s; }
  .box  { background:white; border:1px solid #e0e0dd; border-radius:6px;
          padding:10px 14px; margin-bottom:8px; }
  .howto { background:white; border:1px solid %s; border-left:5px solid %s;
           border-radius:6px; padding:10px 14px 4px 14px; margin-bottom:12px; }
  .howto h4 { margin:0 0 6px 0; font-size:15px; font-weight:700; color:%s; }
  .howto ol { padding-left:18px; margin-bottom:6px; }
  .howto li { font-size:13px; margin-bottom:5px; line-height:1.35; }
  .howmade { background:%s; border-left:4px solid %s; border-radius:4px;
             padding:9px 12px; margin:4px 0 12px 0; font-size:12.5px;
             color:#3a3a36; line-height:1.4; }
  .ssrnum { font-size:34px; font-weight:700; color:%s; line-height:1.15; }
  .note { color:%s; font-size:13px; }
  .truebox { font-size:17px; font-weight:600; color:%s; }
  .estbox  { font-size:17px; font-weight:600; color:%s; }
  h2 { color:%s; }", soft, navy, navy, navy, soft, garnet, garnet, muted, garnet, navy, navy)

## ---------------- page 1 ----------------
page1 <- tabPanel(
  "1. What least squares minimizes",
  p(class = "note",
    "Econometrics I · Unit 2 · the simple regression model. ",
    "California school districts: ", strong("math"),
    " is the district average maths score, ", strong("income"),
    " the district average income in thousands of dollars. ",
    "Seven districts, spread across the income range."),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "howto",
        h4("How to use this page"),
        tags$ol(
          tags$li("Move the two sliders to put the line among the points."),
          tags$li("Each residual is drawn as a square. The ", em("area"),
                  " of a square is that residual, squared."),
          tags$li("Read the SSR above the graph: it is the total shaded area."),
          tags$li("Try to make the SSR as small as you can."),
          tags$li(HTML("Then press <strong>Show me the OLS line</strong>. No line you
                        set by hand has a smaller SSR."))
        )),
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
        HTML("<strong>Data.</strong> California Test Score Data — 420 school districts.
              Online complements to Stock, J. H. and Watson, M. W. (2007),
              <em>Introduction to Econometrics</em>, 2nd ed., Addison Wesley; distributed
              in the R package <code>AER</code> as <code>CASchools</code>."))
    )
  )
)

## ---------------- page 2 ----------------
page2 <- tabPanel(
  "2. The true line and the sample line",
  p(class = "note",
    "Econometrics I · Unit 2 · the simple regression model. ",
    strong("These data are invented."),
    HTML(sprintf("We chose the population ourselves, so for once we know the truth:
                  y = 10 + 2x + u. With real data you never see the line below in
                  <span style='color:%s;font-weight:600;'>orange</span>.", garnet))),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "howto",
        h4("How to use this page"),
        tags$ol(
          tags$li("The pale grey cloud is the whole ", strong("population"),
                  " (2000 individuals). The ",
                  span(style = sprintf("color:%s;font-weight:600;", garnet), "orange line"),
                  " is the ", strong("true"), " line."),
          tags$li(HTML("Press <strong>Draw a new sample</strong>: the app takes n
                        individuals at random. They are the dark points.")),
          tags$li(HTML(sprintf(
                  "The <span style='color:%s;font-weight:600;'>green line</span> is the
                   OLS line computed from that sample <em>only</em>.", navy))),
          tags$li("Compare the two lines, and the two pairs of numbers above the graph."),
          tags$li("Press the button again. The sample changes and the green line moves. ",
                  "The orange line never moves: it is not estimated, it is the truth.")
        )),
      actionButton("newsample", "Draw a new sample", class = "btn-primary"),
      br(), br(),
      sliderInput("n", "Sample size  n", min = 10, max = 200, value = 25, step = 5),
      checkboxInput("showpop", "Show the whole population", TRUE),
      hr(),
      radioButtons("resid_kind", "Lower graph shows:",
        choices = c("Residuals  e = y − ŷ" = "e",
                    "Errors  u = y − (β₀ + β₁x)" = "u"),
        selected = "e"),
      div(class = "howmade",
        strong("How the errors were produced."),
        " Every individual in the population received an error ", strong("u"),
        " drawn at random from a normal distribution with mean 0 and standard ",
        "deviation 5. The draw does not depend on x.",
        br(), br(),
        HTML("Mean 0 is a property of the <em>rule</em>, not of the numbers it produced:
              in any sample the errors do <strong>not</strong> average exactly zero.")),
      p(class = "note",
        strong("What to look at."),
        " The residual is the distance to the ",
        span(style = sprintf("color:%s;font-weight:600;", navy), "green"),
        " line; the error is the distance to the ",
        span(style = sprintf("color:%s;font-weight:600;", garnet), "orange"),
        " line. They are not the same thing. We can compute the residual from ",
        "the sample; we can see the error only because we invented the population.")
    ),

    mainPanel(
      width = 8,
      div(class = "box",
          fluidRow(
            column(6,
              div(class = "note", "The truth (we chose it)"),
              div(class = "truebox", textOutput("trueeq", inline = TRUE))),
            column(6,
              div(class = "note", "The OLS estimate from this sample"),
              div(class = "estbox", textOutput("esteq", inline = TRUE)))
          ),
          div(class = "note", style = "padding-top:8px;",
              textOutput("gapmsg", inline = TRUE))),
      plotOutput("plot2", height = "330px"),
      plotOutput("plot2r", height = "210px"),
      div(class = "box", textOutput("residnote")),
      hr(),
      p(class = "note", style = "font-size:12px;",
        strong("Invented data."), " x is drawn between 2 and 18 and the errors are normal ",
        "with mean zero and standard deviation 5. Nothing here is a real population.")
    )
  )
)

ui <- navbarPage(
  title = "Econometrics I · Unit 2 · the regression line",
  header = tags$style(HTML(css)),
  page1, page2
)

## ====================  SERVER  ==============================
server <- function(input, output, session) {

  ## ---------------- page 1 ----------------
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

  ## ---------------- page 2 ----------------
  ## The population never changes now that sigma is fixed. Kept as a reactive
  ## because samp() reads it, and because a sigma control is one line away if
  ## it is ever wanted back (see the note at the top).
  pop <- reactive({
    data.frame(x = xpop, y = TB0 + TB1 * xpop + SIGMA * zpop,
               u = SIGMA * zpop)
  })

  ## Seeded by the click counter, so the same number of clicks gives the same
  ## sample in class and at home.
  samp <- reactive({
    set.seed(1000 + input$newsample)
    p <- pop()
    p[sample.int(NPOP, input$n), ]
  })

  m2 <- reactive({
    s <- samp()
    lm(y ~ x, data = s)
  })

  output$trueeq <- renderText(sprintf("y = %d + %d x + u", TB0, TB1))
  output$esteq  <- renderText({
    cf <- coef(m2())
    sprintf("ŷ = %.2f %+.2f x", cf[1], cf[2])
  })
  output$gapmsg <- renderText({
    cf <- coef(m2())
    sprintf("The estimated slope misses the true slope by %.2f. Draw another sample and it will miss by a different amount, in a different direction. n = %d.",
            cf[2] - TB1, input$n)
  })

  output$plot2 <- renderPlot({
    p <- pop(); s <- samp(); cf <- coef(m2())
    par(mar = c(4.2, 4.6, 0.6, 1), bg = "white")
    plot(NA, xlim = range(xpop), ylim = range(p$y), las = 1,
         xlab = "x", ylab = "y", col.lab = navy, cex.lab = 1.15, cex.axis = 1.05)
    if (isTRUE(input$showpop))
      points(p$x, p$y, pch = 16, cex = 0.5, col = adjustcolor(muted, alpha.f = 0.20))
    segments(s$x, s$y, s$x, cf[1] + cf[2] * s$x, col = muted, lty = 3)
    abline(a = TB0, b = TB1, col = garnet, lwd = 3)
    abline(a = cf[1], b = cf[2], col = navy, lwd = 3)
    points(s$x, s$y, pch = 21, bg = steel, col = "white", cex = 1.4, lwd = 1.2)
    legend("topleft", bty = "n", lwd = 3, cex = 1.05,
           col = c(garnet, navy),
           legend = c("true line  y = 10 + 2x", "OLS line from this sample"))
  })

  ## The one number that separates a residual from an error: the residuals of
  ## an OLS fit with an intercept ALWAYS add up to zero, in every sample; the
  ## errors do not, and nothing makes them.
  output$residnote <- renderText({
    s <- samp(); cf <- coef(m2())
    if (input$resid_kind == "e") {
      ## floating point leaves a value like -3e-15, which sprintf prints as
      ## "-0.00" -- exactly the thing a student would stop and worry about.
      se <- sum(s$y - (cf[1] + cf[2] * s$x))
      if (abs(se) < 1e-8) se <- 0
      sprintf("The residuals add up to %.2f. They always add up to zero — that is a property of the OLS line, true in every sample.", se)
    } else {
      sprintf("The errors add up to %.2f, not to zero. Nothing makes them: they are what the population happened to give these %d individuals.",
              sum(s$u), input$n)
    }
  })

  output$plot2r <- renderPlot({
    s <- samp(); cf <- coef(m2())
    v   <- if (input$resid_kind == "e") s$y - (cf[1] + cf[2] * s$x) else s$u
    col <- if (input$resid_kind == "e") navy else garnet
    lab <- if (input$resid_kind == "e") "Residual  e" else "Error  u"
    par(mar = c(4.2, 4.6, 0.6, 1), bg = "white")
    plot(NA, xlim = range(xpop), ylim = max(abs(v)) * c(-1.15, 1.15), las = 1,
         xlab = "x", ylab = lab, col.lab = navy, cex.lab = 1.15, cex.axis = 1.05)
    abline(h = 0, col = muted, lwd = 2)
    segments(s$x, 0, s$x, v, col = adjustcolor(col, alpha.f = 0.55))
    points(s$x, v, pch = 21, bg = col, col = "white", cex = 1.3, lwd = 1.2)
  })
}

shinyApp(ui, server)
