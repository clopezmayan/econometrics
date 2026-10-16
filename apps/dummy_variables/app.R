# ============================================================
#  Econometrics I 26-27 — Unit 6
#  "Dummy variables"
#
#  REBUILT 2026-10-16 (Cristina): Unit 2 layout and the minimum on
#  screen. The concurso version (9 Sep) is kept in
#  ../_deprecated/dummies_app_2026-09-09_concurso.R.
#
#  Two pages, each with its own "How to use":
#   1. CONCEPT. Sliders move two parallel lines (men, women). The
#      base-group switch changes the REPORTED equation (intercept and
#      dummy coefficient, which changes sign) while the lines stay put.
#      The sliders describe the lines, not the betas, so they mean the
#      same thing whatever the base group.
#   2. DATA (EES 2022). Only the female coefficient and the group
#      means. Controls: tenure and the four education dummies of the
#      EES practice (lowsec, uppsec, vet, univ; base primary or less).
#      With no controls the coefficient equals the difference in means.
#      Logs: approximate percentage only (100 x coefficient), as in
#      the practice -- no exp() formula (Cristina, 15 Oct).
#
#  Removed from the concurso version: the coefficient table of page 1,
#  the full lm() table of page 2, the full-time control, the N line.
#  No beta3 / interaction slider (Cristina, 16 Oct: keep it minimal).
#
#  Run:     shiny::runApp("dummies")
#  Publish: shinylive::export("dummies", "_site_dummies")  -- only on
#           Cristina's go; see README.
# ============================================================

library(shiny)

## ---- house palette: econ1.sty (see the Unit 2 app's note) ---
navy   <- "#2F4A37"   # forest green: men
garnet <- "#B05A3C"   # terracotta: women, key numbers
muted  <- "#6B6B66"
soft   <- "#F1F3F0"

data_path <- function(f) {
  cand <- c(file.path("data", f), file.path("dummies", "data", f), f)
  hit  <- cand[file.exists(cand)]
  if (length(hit)) hit[1] else cand[1]
}

ees <- read.csv(data_path("ees2022_teaching.csv"))

# The same dummies the students build in the EES practice
ees$female <- ifelse(ees$sex == "Woman", 1, 0)
ees$lowsec <- ifelse(ees$educ == 2, 1, 0)    # Lower secondary
ees$uppsec <- ifelse(ees$educ == 3, 1, 0)    # Upper secondary
ees$vet    <- ifelse(ees$educ == 4, 1, 0)    # Higher VET
ees$univ   <- ifelse(ees$educ == 5, 1, 0)    # University

CONTROLS <- c("Tenure (years with the employer)"            = "tenure",
              "Education (4 dummies, base: primary or less)" = "educ")

## ---- helpers -------------------------------------------------
female_coef <- function(controls, logy) {
  y  <- if (isTRUE(logy)) "log(hwage)" else "hwage"
  xs <- c("female",
          if ("tenure" %in% controls) "tenure",
          if ("educ"   %in% controls) c("lowsec", "uppsec", "vet", "univ"))
  unname(coef(lm(as.formula(paste(y, "~", paste(xs, collapse = " + "))),
                 data = ees))["female"])
}

group_means <- function(logy) {
  y <- if (isTRUE(logy)) log(ees$hwage) else ees$hwage
  m0 <- mean(y[ees$female == 0]); m1 <- mean(y[ees$female == 1])
  c(men = m0, women = m1, diff = m1 - m0)
}

## ====================  USER INTERFACE  ======================
## LAYOUT FOLLOWS THE UNIT 2 APP: navbar title, a one-line note, a sidebar
## of width 4 that opens with a "How to use" box. Same css classes.
css <- sprintf("
  body { background:%s; }
  .box  { background:white; border:1px solid #e0e0dd; border-radius:6px;
          padding:10px 14px; margin-bottom:8px; }
  .howto { background:white; border:1px solid %s; border-left:5px solid %s;
           border-radius:6px; padding:10px 14px 4px 14px; margin-bottom:12px; }
  .howto h4 { margin:0 0 6px 0; font-size:15px; font-weight:700; color:%s; }
  .howto ol { padding-left:18px; margin-bottom:6px; }
  .howto li { font-size:13px; margin-bottom:5px; line-height:1.35; }
  .note { color:%s; font-size:13px; }
  .big  { font-size:26px; font-weight:700; color:%s; line-height:1.15; }
  .lab  { font-size:12px; color:%s; }
  h2 { color:%s; }", soft, navy, navy, navy, muted, garnet, muted, navy)

## htmltools puts a space before a text node that starts with punctuation:
## such fragments are HTML() -- see the README.
page_concept <- tabPanel(
  "1. A dummy shifts the intercept",
  p(class = "note",
    HTML("Econometrics I · Unit 6 · qualitative explanatory variables.
          Model: <strong>wage</strong> = &beta;<sub>0</sub> + &beta;<sub>1</sub> educ +
          &beta;<sub>2</sub> female + u.")),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "howto",
        h4("How to use this page"),
        tags$ol(
          tags$li("Move the sliders. The two lines have the same slope: education has
                   the same effect for men and for women."),
          tags$li("The vertical distance between the lines is the coefficient of the
                   dummy: the wage difference between a woman and a man with the same
                   education."),
          tags$li("Change the base group. The lines do not move, but the intercept and
                   the dummy coefficient in the estimated equation change.")
        )),
      sliderInput("a_men", "Intercept for men",
                  min = 0, max = 15, value = 10, step = 0.5),
      sliderInput("slope", "Slope of education (both groups)",
                  min = -1, max = 2, value = 0.5, step = 0.1),
      sliderInput("gap", "Distance between the lines: women minus men",
                  min = -6, max = 6, value = -1.5, step = 0.5),
      radioButtons("base", "Base group",
                   c("Men (the dummy is female)" = "men",
                     "Women (the dummy is male)" = "women"))
    ),

    mainPanel(
      width = 8,
      plotOutput("conceptPlot", height = "380px"),
      div(class = "box", uiOutput("conceptEq"))
    )
  )
)

page_data <- tabPanel(
  "2. Real data: the gender wage gap",
  p(class = "note",
    HTML("Econometrics I · Unit 6 · qualitative explanatory variables.
          50,000 workers in Spain, 2022. Dependent variable: <strong>hourly
          wage</strong> in euros. <strong>female</strong> = 1 for women, 0 for men.")),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "howto",
        h4("How to use this page"),
        tags$ol(
          tags$li("With no control variables, compare the coefficient of female with
                   the difference in mean wages: they are equal."),
          tags$li("Add tenure and education. The coefficient now compares women and
                   men with the same tenure and education."),
          tags$li("Use the wage in logs: 100 × the coefficient is, approximately,
                   the wage gap in percent.")
        )),
      checkboxGroupInput("controls", "Control variables", choices = CONTROLS),
      checkboxInput("logy", "Hourly wage in logs", FALSE)
    ),

    mainPanel(
      width = 8,
      fluidRow(
        column(6, div(class = "box", uiOutput("coefBox"))),
        column(6, div(class = "box", uiOutput("meansBox")))
      ),
      div(class = "box", uiOutput("compare")),
      hr(),
      p(class = "note", style = "font-size:12px;",
        HTML("<strong>Data.</strong> INE, Encuesta de Estructura Salarial 2022 &mdash;
              teaching extract of 50,000 employees, drawn to be self-weighting. Source
              microdata freely available at www.ine.es."))
    )
  )
)

ui <- navbarPage(
  title = "Econometrics I · Unit 6 · dummy variables",
  header = tags$style(HTML(css)),
  page_concept,
  page_data
)

## ====================  SERVER  ==============================
server <- function(input, output, session) {

  ## ----- Page 1 -----
  output$conceptPlot <- renderPlot({
    a <- input$a_men; b <- input$slope; g <- input$gap
    x <- c(0, 20)
    yr <- range(c(a + b * x, a + g + b * x))
    par(mar = c(4.2, 4.2, 1, 1), bg = "white")
    plot(NA, xlim = x, ylim = yr, las = 1, xlab = "Years of education",
         ylab = "Hourly wage", col.lab = muted, cex.lab = 1.05)
    lines(x, a + b * x,     col = navy,   lwd = 3)
    lines(x, a + g + b * x, col = garnet, lwd = 3)
    if (g != 0) {
      arrows(10, a + b * 10, 10, a + g + b * 10, code = 3, length = 0.08, col = muted)
      text(10.4, a + b * 10 + g / 2, sprintf("%+.1f", g), adj = 0, col = muted, font = 2)
    }
    legend("topleft", c("Men", "Women"), col = c(navy, garnet), lwd = 3, bty = "n")
  })

  output$conceptEq <- renderUI({
    a <- input$a_men; b <- input$slope; g <- input$gap
    if (input$base == "men") { b0 <- a;     b2 <- g;  d <- "female" }
    else                     { b0 <- a + g; b2 <- -g; d <- "male"   }
    sg <- function(v) if (v < 0) "&minus;" else "+"
    hl <- function(v) sprintf("<span style='color:%s;font-weight:700;'>%s</span>",
                              garnet, v)
    HTML(sprintf(
      "<div class='lab'>Estimated equation (base group: %s)</div>
       <div style='font-size:18px;padding:2px 0 10px 0;'>
         wa&#285;e = %s %s %.1f educ %s %s %s</div>
       <div class='lab'>The two lines (they do not depend on the base group)</div>
       <div style='font-size:15px;padding-top:2px;'>
         Men: wa&#285;e = %.1f %s %.1f educ<br>
         Women: wa&#285;e = %.1f %s %.1f educ</div>",
      input$base,
      hl(sprintf("%.1f", b0)), sg(b), abs(b), hl(sg(b2)), hl(sprintf("%.1f", abs(b2))), d,
      a, sg(b), abs(b), a + g, sg(b), abs(b)))
  })

  ## ----- Page 2 -----
  output$coefBox <- renderUI({
    v <- female_coef(input$controls, input$logy)
    sub <- if (isTRUE(input$logy))
      sprintf("about %.1f%% %s for women", abs(100 * v), if (v < 0) "less" else "more")
    else
      sprintf("euros per hour %s for women", if (v < 0) "less" else "more")
    HTML(sprintf(
      "<div class='lab'>Coefficient of female</div>
       <div class='big'>%s%.3f</div>
       <div class='lab'>%s</div>", if (v < 0) "&minus;" else "", abs(v), sub))
  })

  output$meansBox <- renderUI({
    m <- group_means(input$logy)
    lab <- if (isTRUE(input$logy)) "Mean of log(hourly wage)" else "Mean hourly wage"
    HTML(sprintf(
      "<div class='lab'>%s</div>
       <div style='font-size:15px;padding-top:2px;'>
         Men: %.3f<br>Women: %.3f<br>
         <strong>Difference (women &minus; men): %s%.3f</strong></div>",
      lab, m["men"], m["women"], if (m["diff"] < 0) "&minus;" else "", abs(m["diff"])))
  })

  output$compare <- renderUI({
    if (length(input$controls) == 0)
      HTML("<span class='note'>No control variables: the coefficient of female
            <strong>is</strong> the difference in means.</span>")
    else
      HTML("<span class='note'>With control variables, the coefficient of female
            compares women and men with the same values of the controls. It is
            <strong>no longer</strong> the difference in means.</span>")
  })
}

shinyApp(ui = ui, server = server)
