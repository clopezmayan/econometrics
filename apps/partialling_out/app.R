# ============================================================
#  Econometrics I 26-27 — Unit 3
#  "Partialling out: what a control variable does"
#
#  Unit 3 section 3.2 is the conceptual heart of the unit:
#  ceteris paribus, the partial effect, and the Ballentine
#  diagrams added on 8 Sep. The annex unit3_annex_fwl.tex proves
#  the Frisch-Waugh-Lovell theorem behind it.
#
#  This app makes both visible ON REAL DATA:
#    A   math on stratio alone            -> the simple slope
#    B   stratio on the control           -> the residuals x1-tilde
#    C   math on x1-tilde                    -> EXACTLY the multiple
#                                               regression coefficient
#  with the Ballentine beside it, its overlap driven by the
#  correlation between stratio and the control the student picks.
#
#  THE POINT OF THE MENU (the thing worth discovering): the size of
#  the overlap is NOT what moves the coefficient. `expenditure`
#  overlaps most with stratio (r = -0.62) and barely moves it;
#  `income` overlaps least (r = -0.23) and moves it most. What
#  matters is the overlap TIMES how strongly the control explains
#  math -- which is the annex's equation 5.2, and Unit 7's omitted
#  variable bias, a unit early.
#
#  ONE TOPIC, ONE APP (Cristina, 9 Sep): its own site, its own link
#  and QR on the Campus. Do NOT merge with the Unit 2 tool.
#
#  Every number here was verified against lm() on 2026-09-09; the
#  identity in panel C holds to machine precision on all four
#  controls, and the 5.2 decomposition reproduces -1.939 for each.
#
#  Run:     shiny::runApp("partialling_out")
#  Publish: shinylive::export("partialling_out", "_site_partialling_out")
# ============================================================

library(shiny)

## ---- house palette: econ1.sty (see the Unit 2 app's note) ---
navy   <- "#2F4A37"   # forest green
garnet <- "#B05A3C"   # terracotta
muted  <- "#6B6B66"
soft   <- "#F1F3F0"
steel  <- "#4F7088"

data_path <- function(f) {
  cand <- c(f, file.path("partialling_out", f))
  hit  <- cand[file.exists(cand)]
  if (length(hit)) hit[1] else cand[1]
}
ca <- read.csv(data_path("caschools_teaching.csv"))

CONTROLS <- c("District income"          = "income",
              "% on subsidised lunch"    = "lunch",
              "% English learners"       = "english",
              "Spending per pupil"       = "expenditure")

SIMPLE <- unname(coef(lm(math ~ stratio, ca))[2])   # -1.939, never changes

## ====================  USER INTERFACE  ======================
ui <- fluidPage(
  tags$style(HTML(sprintf("
    body { background:%s; }
    .box { background:white; border:1px solid #e0e0dd; border-radius:6px;
           padding:10px 14px; margin-bottom:10px; }
    .note { color:%s; font-size:13px; }
    .big  { font-size:26px; font-weight:700; color:%s; }
    .lab  { font-size:12px; color:%s; }", soft, muted, garnet, muted))),

  titlePanel("Partialling out: what a control variable does"),
  p(class = "note",
    "Econometrics I · Unit 3 · sections 3.2 and 3.3. ",
    "420 California school districts. We want the effect of ", strong("stratio"),
    " (students per teacher) on ", strong("math"), ". Pick one control variable ",
    "and watch what happens."),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("c2", "Control variable", choices = CONTROLS, selected = "income"),
      checkboxInput("venn", "Show the Ballentine diagram", TRUE),
      hr(),
      div(class = "box",
          div(class = "lab", "Simple regression, no control"),
          div(class = "big", textOutput("bs", inline = TRUE)),
          div(class = "lab", style = "padding-top:8px;", "With the control added"),
          div(class = "big", textOutput("bm", inline = TRUE))),
      p(class = "note",
        strong("What to look at."), " Panel C regresses math on what is ",
        em("left"), " of stratio once the control is taken out of it. ",
        "Its slope is the multiple regression coefficient — not close to it, ",
        em("equal"), " to it. That is the Frisch–Waugh–Lovell theorem."),
      p(class = "note",
        strong("Then try all four controls."), " The one that overlaps most with ",
        "stratio is not the one that moves the coefficient most. Ask yourself why.")
    ),

    mainPanel(
      width = 9,
      plotOutput("panels", height = "300px"),
      div(class = "box", uiOutput("identity")),
      conditionalPanel("input.venn", plotOutput("venn", height = "300px")),
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

  bits <- reactive({
    c2 <- input$c2
    x1 <- ca$stratio; y <- ca$math; x2 <- ca[[c2]]
    mult <- lm(y ~ x1 + x2)
    aux  <- lm(x1 ~ x2)          # stratio on the control
    xt   <- resid(aux)           # x1-tilde: stratio with the control removed
    fwl  <- lm(y ~ xt)
    list(c2 = c2, x1 = x1, y = y, x2 = x2, xt = xt,
         b1 = unname(coef(mult)[2]),
         b2 = unname(coef(mult)[3]),
         d1 = unname(coef(lm(x2 ~ x1))[2]),     # slope of x2 on x1 (annex 5.2)
         fwl = unname(coef(fwl)[2]),
         r  = cor(x1, x2),
         r2 = summary(aux)$r.squared,
         lab = names(CONTROLS)[CONTROLS == c2])
  })

  output$bs <- renderText(sprintf("%+.3f", SIMPLE))
  output$bm <- renderText(sprintf("%+.3f", bits()$b1))

  output$identity <- renderUI({
    b <- bits()
    HTML(sprintf(
      "<div class='lab'>Panel C slope vs the multiple regression coefficient</div>
       <div style='font-size:16px;padding:4px 0 10px 0;'>
         slope on x&#771;<sub>1</sub> = <b>%.6f</b> &nbsp;&nbsp;
         &beta;&#770;<sub>1</sub> from math ~ stratio + %s = <b>%.6f</b>
         &nbsp; <span style='color:%s'>identical</span>
       </div>
       <div class='lab'>Where the difference between the two slopes comes from
         (annex &sect;5.2)</div>
       <div style='font-size:16px;padding-top:4px;'>
         &beta;&#770;<sub>1</sub><sup>s</sup> = &beta;&#770;<sub>1</sub> +
         &beta;&#770;<sub>2</sub>&middot;&delta;&#770;<sub>1</sub> &nbsp;=&nbsp;
         %.3f + (%.3f)(%.4f) = <b>%.3f</b>
       </div>
       <div class='lab' style='padding-top:6px;'>
         correlation between stratio and %s = %.3f &nbsp;·&nbsp;
         it explains %.1f%% of the variation in stratio</div>",
      b$fwl, b$c2, b$b1, navy,
      b$b1, b$b2, b$d1, b$b1 + b$b2 * b$d1,
      b$c2, b$r, 100 * b$r2))
  })

  output$panels <- renderPlot({
    b <- bits()
    par(mfrow = c(1, 3), mar = c(4.2, 4.2, 2.6, 1), bg = "white")

    sc <- function(x, y, xlab, ylab, ttl, slope_lab) {
      plot(x, y, pch = 16, col = adjustcolor(steel, 0.45), cex = 0.7, las = 1,
           xlab = xlab, ylab = ylab, main = ttl,
           col.main = navy, col.lab = muted, cex.main = 1.25, cex.lab = 1.05)
      abline(lm(y ~ x), col = garnet, lwd = 3)
      mtext(slope_lab, side = 3, line = -1.4, adj = 0.96, col = navy, font = 2, cex = 0.95)
    }

    sc(b$x1, b$y, "stratio", "math",
       "A. math on stratio", sprintf("slope %+.3f", SIMPLE))
    plot(b$x2, b$x1, pch = 16, col = adjustcolor(steel, 0.45), cex = 0.7, las = 1,
         xlab = b$c2, ylab = "stratio",
         main = sprintf("B. stratio on %s", b$c2),
         col.main = navy, col.lab = muted, cex.main = 1.25, cex.lab = 1.05)
    abline(lm(b$x1 ~ b$x2), col = muted, lwd = 3)
    mtext("residuals = x̃₁", side = 3, line = -1.4, adj = 0.96,
          col = navy, font = 2, cex = 0.95)
    sc(b$xt, b$y, "x̃₁  (stratio, control removed)", "math",
       "C. math on x̃₁", sprintf("slope %+.3f", b$fwl))
  })

  output$venn <- renderPlot({
    b <- bits(); r <- 0.62
    ## Distance between the x1 and x2 circles falls as their correlation rises,
    ## so the overlap the student sees IS the correlation reported below.
    d  <- 2 * r * (1 - abs(b$r)) * 0.92
    cx1 <- -d / 2; cx2 <- d / 2; cy <- -0.34
    yx <- 0; yy <- 0.46

    circ <- function(cx, cy, col) {
      th <- seq(0, 2 * pi, length.out = 200)
      polygon(cx + r * cos(th), cy + r * sin(th),
              col = adjustcolor(col, 0.30), border = adjustcolor(col, 0.8), lwd = 2)
    }
    par(mar = c(0, 0, 2.2, 0), bg = "white")
    plot(NA, xlim = c(-1.5, 1.5), ylim = c(-1.15, 1.25), axes = FALSE,
         xlab = "", ylab = "",
         main = sprintf("The Ballentine:  overlap of stratio and %s  (r = %.2f)",
                        b$c2, b$r),
         col.main = navy, cex.main = 1.2)
    circ(yx, yy, navy)          # y
    circ(cx1, cy, garnet)       # x1 = stratio
    circ(cx2, cy, steel)        # x2 = the control
    text(yx, yy + 0.42, "math", col = navy, font = 2, cex = 1.1)
    text(cx1 - 0.30, cy - 0.46, "stratio", col = garnet, font = 2, cex = 1.1)
    text(cx2 + 0.30, cy - 0.46, b$c2, col = steel, font = 2, cex = 1.1)
    ## Label positions follow the annex, section 5.1: A is the part of
    ## stratio inside math but NOT shared with the control, B the part
    ## inside neither, D+E what stratio shares with the control. So A must
    ## sit LEFT of the control circle's left edge (cx2 - r) and still inside
    ## the math circle -- hence the clamp. Both move as the overlap changes.
    ax <- max(-0.42, cx2 - r - 0.16)
    text(ax, 0.02, "A", col = navy, font = 2, cex = 1.2)
    text(cx1 - 0.30, cy - 0.05, "B", col = navy, font = 2, cex = 1.2)
    text((cx1 + cx2) / 2, cy - 0.30, "D + E", col = navy, font = 2, cex = 1.05)
    mtext(expression(hat(beta)[1] == A / (A + B)), side = 1, line = -1.2,
          col = navy, cex = 1.1)
  })
}

shinyApp(ui, server)
