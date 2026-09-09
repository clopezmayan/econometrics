# ============================================================
#  Econometrics I - Unit 5 (Qualitative variables)
#  Teaching app: interpreting dummy variables in regression
#
#  Two linked views (see DOC 3, section 7):
#   1. CONCEPT - sliders for the intercept, the slope and the
#      dummy coefficient redraw two parallel group lines, shade
#      the vertical gap, and let students SWITCH THE BASE GROUP
#      and watch the coefficients change while the lines stay put.
#   2. DATA - load the EES teaching data, tick controls on/off,
#      and see the coefficient table next to the group means
#      (with no controls, the dummy coef = difference in means).
#
#  Run:  shiny::runApp("leccion_materiales/shiny")
#  Needs: install.packages("shiny")
# ============================================================

library(shiny)

navy   <- "#1F4E79"   # base group  (men)
garnet <- "#9B2335"   # other group (women)

## ---- locate and load the teaching data ---------------------
data_path <- function(fname) {
  cand <- c(file.path("..", "data", fname),   # app run from shiny/
            file.path("data", fname),         # app run from leccion_materiales/ or bundled
            file.path("leccion_materiales", "data", fname),
            fname)
  hit <- cand[file.exists(cand)]
  if (length(hit)) hit[1] else cand[1]
}

ees <- read.csv(data_path("ees2022_teaching.csv"), stringsAsFactors = FALSE)

# Derived dummy / factor (the same the students build by hand)
ees$female   <- as.integer(ees$sex == "Woman")
ees$educ_lab <- relevel(factor(ees$educ_lab), ref = "Primary or less")

CONTROLS <- c("Tenure"               = "tenure",
              "Education (dummies)"   = "educ_lab",
              "Full-time"            = "fulltime")

## ---- helpers (also unit-tested outside the app) ------------
build_formula <- function(controls, logy) {
  y <- if (isTRUE(logy)) "log(hwage)" else "hwage"
  as.formula(paste(y, "~", paste(c("female", controls), collapse = " + ")))
}

group_means <- function(logy) {
  yv <- if (isTRUE(logy)) log(ees$hwage) else ees$hwage
  dv <- ees$female
  m1 <- mean(yv[dv == 1]); m0 <- mean(yv[dv == 0])
  list(m0 = m0, m1 = m1, diff = m1 - m0,
       n0 = sum(dv == 0), n1 = sum(dv == 1))
}

## ====================  USER INTERFACE  ======================
ui <- fluidPage(
  titlePanel("Dummy variables in regression — an interpretation tool"),
  helpText("Econometrics I · Unit 5 · qualitative variables. ",
           "A dummy shifts the intercept; its coefficient is a difference ",
           "relative to the base group."),
  tabsetPanel(

    ## ---------------- TAB 1: CONCEPT ----------------
    tabPanel("1. Concept (sliders)",
      sidebarLayout(
        sidebarPanel(
          p(strong("Model:"), "wage = β₀ + β₁·educ + β₂·female"),
          sliderInput("b0", "β₀  (intercept, base group = men)",
                      min = 0, max = 15, value = 10, step = 0.5),
          sliderInput("b1", "β₁  (slope: return to education)",
                      min = -1, max = 2, value = 0.5, step = 0.1),
          sliderInput("b2", "β₂  (dummy coefficient: women − men gap)",
                      min = -6, max = 6, value = -1.5, step = 0.5),
          radioButtons("base", "Base (reference) group",
                       c("Men (female = 1 for women)"   = "men",
                         "Women (male = 1 for men)"     = "women")),
          checkboxInput("showgap", "Shade the vertical gap (β₂)", TRUE),
          hr(),
          helpText("Switch the base group: the two lines DO NOT move, ",
                   "but the reported coefficients change.")
        ),
        mainPanel(
          plotOutput("conceptPlot", height = "420px"),
          h4("Fitted equations"),
          verbatimTextOutput("conceptEq"),
          h4("Coefficients (as reported with the chosen base group)"),
          tableOutput("conceptCoef")
        )
      )
    ),

    ## ---------------- TAB 2: DATA ----------------
    tabPanel("2. Data (real estimates)",
      sidebarLayout(
        sidebarPanel(
          p(strong("Data:"), "EES 2022 — gender wage gap in Spain ",
            "(teaching extract)."),
          checkboxGroupInput("controls", "Control variables", choices = CONTROLS),
          checkboxInput("logy", "Outcome in logs (percent gap)", FALSE),
          hr(),
          helpText("Tick controls on and off and watch the dummy ",
                   "coefficient move. With NO controls it equals the ",
                   "difference in group means shown on the right.")
        ),
        mainPanel(
          h4("OLS estimates"),
          tableOutput("dataCoef"),
          textOutput("dataN"),
          hr(),
          h4("Group means of the outcome"),
          tableOutput("dataMeans"),
          textOutput("dataIdentity")
        )
      )
    )
  )
)

## ====================  SERVER  ==============================
server <- function(input, output, session) {

  ## ----- Concept view -----
  output$conceptPlot <- renderPlot({
    b0 <- input$b0; b1 <- input$b1; b2 <- input$b2
    x  <- seq(0, 20, length.out = 100)
    yMen <- b0 + b1 * x; yWomen <- (b0 + b2) + b1 * x
    yr <- range(c(yMen, yWomen))
    par(mar = c(4, 4, 1, 1))
    plot(NA, xlim = c(0, 20), ylim = yr, xlab = "Years of education",
         ylab = "Hourly wage (EUR)", las = 1)
    lines(x, yMen,   col = navy,   lwd = 3)
    lines(x, yWomen, col = garnet, lwd = 3)
    if (isTRUE(input$showgap)) {
      x0 <- 10
      arrows(x0, b0 + b1 * x0, x0, (b0 + b2) + b1 * x0,
             code = 3, length = 0.08, col = "grey30")
      text(x0 + 0.4, b0 + b1 * x0 + b2 / 2,
           bquote(beta[2] == .(b2)), adj = 0, col = "grey20")
    }
    legend("topleft", c("Men", "Women"), col = c(navy, garnet),
           lwd = 3, bty = "n")
  })

  conceptParam <- reactive({
    if (input$base == "men")
      list(int = input$b0, slope = input$b1, dummy = input$b2,
           dname = "female", base = "men")
    else
      list(int = input$b0 + input$b2, slope = input$b1, dummy = -input$b2,
           dname = "male", base = "women")
  })

  output$conceptEq <- renderText({
    p <- conceptParam()
    other <- if (p$base == "men") "Women" else "Men"
    base  <- if (p$base == "men") "Men"   else "Women"
    sprintf(
      "%s (base):  wage = %.1f + %.1f*educ\n%s:         wage = (%.1f %+0.1f) + %.1f*educ\n\nThe %s line is the base; %s = %+.1f shifts the intercept for the other group.",
      base, p$int, p$slope,
      other, p$int, p$dummy, p$slope,
      base, p$dname, p$dummy)
  })

  output$conceptCoef <- renderTable({
    p <- conceptParam()
    data.frame(
      Coefficient = c("Intercept (β₀)", "Slope educ (β₁)",
                      paste0("Dummy ", p$dname, " (β₂)")),
      Value = c(p$int, p$slope, p$dummy))
  }, digits = 2)

  ## ----- Data view (EES only) -----
  fit <- reactive({
    lm(build_formula(input$controls, input$logy), data = ees)
  })

  output$dataCoef <- renderTable({
    s <- summary(fit())$coefficients
    data.frame(Term = rownames(s),
               Estimate = s[, 1], `Std. error` = s[, 2],
               `t value` = s[, 3], `p value` = s[, 4],
               check.names = FALSE)
  }, digits = 3)

  output$dataN <- renderText(sprintf("N = %d observations used.",
                                     length(fit()$residuals)))

  output$dataMeans <- renderTable({
    g <- group_means(input$logy)
    data.frame(
      Group = c("female = 0 (men)", "female = 1 (women)", "Difference (women - men)"),
      `Mean outcome` = c(g$m0, g$m1, g$diff),
      N = c(g$n0, g$n1, g$n0 + g$n1),
      check.names = FALSE)
  }, digits = 3)

  output$dataIdentity <- renderText({
    if (length(input$controls) == 0)
      "No controls: the dummy coefficient above equals this difference in means."
    else
      "With controls, the dummy coefficient is the difference holding those variables fixed (no longer the raw difference in means)."
  })
}

shinyApp(ui = ui, server = server)
