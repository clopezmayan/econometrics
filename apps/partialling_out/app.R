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
#  The Ballentine diagram that sat beside them was REMOVED (Cristina,
#  7 Oct: too much information); it lives in the deck. Its code is in
#  _deprecated/partialling_out_app_2026-10-07_20102bb.R.
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
              "% on subsidized lunch"    = "lunch",
              "% English learners"       = "english",
              "Spending per pupil"       = "expenditure")

SIMPLE   <- unname(coef(lm(math ~ stratio, ca))[2])   # -1.939, never changes
SIMPLE_A <- unname(coef(lm(math ~ stratio, ca))[1])   # 691.42

## ====================  USER INTERFACE  ======================
## LAYOUT FOLLOWS THE UNIT 2 APP (Cristina, 7 Oct): navbar title, a one-line
## note under it, a sidebar of width 4 that opens with a "How to use" box,
## and the key numbers in a box ABOVE the graphs. Same css classes, so the
## two apps look like one family even though they are separate sites.
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

## htmltools puts a space before a text node that starts with punctuation
## (strong("math"), ". Pick" renders as "math . Pick"). Every such fragment
## below is HTML() -- see the README.
page <- tabPanel(
  "Partialling out: what a control variable does",
  p(class = "note",
    HTML("Econometrics I · Unit 3 · the multiple regression model.
          420 California school districts. We want the effect of <strong>stratio</strong>
          (students per teacher) on <strong>math</strong>. Pick one control variable
          and watch what happens.")),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "howto",
        h4("How to use this page"),
        tags$ol(
          tags$li("Pick a control variable in the menu below."),
          tags$li(HTML("<strong>Panel A</strong> regresses math on stratio alone. Its slope
                        is the simple regression coefficient, and it does not change
                        when you change the control.")),
          tags$li(HTML("<strong>Panel B</strong> regresses stratio on the control. The
                        residuals are stratio with the control removed. We call them
                        x&#771;<sub>1</sub>.")),
          tags$li(HTML("<strong>Panel C</strong> regresses math on x&#771;<sub>1</sub>.
                        Compare its slope with the coefficient of stratio in the
                        <em>multiple regression</em>, below the graphs: they are equal."))
        )),
      selectInput("c2", "Control variable", choices = CONTROLS, selected = "income"),
      hr(),
      p(class = "note",
        HTML("<strong>What to look at.</strong> Panel C regresses math on what is
              <em>left</em> of stratio once the control is taken out of it.
              Its slope is the multiple regression coefficient &mdash; not close to it,
              <em>equal</em> to it. That is the Frisch&ndash;Waugh&ndash;Lovell theorem."))
    ),

    mainPanel(
      width = 8,
      plotOutput("panels", height = "300px"),
      ## LESS ON SCREEN (Cristina, 7 Oct): the coefficient box above the graphs
      ## and the FWL identity / decomposition box below them are gone. What is
      ## left is the two regressions themselves; the stratio coefficient of the
      ## multiple one is the slope printed on panel C.
      div(class = "box", uiOutput("regs")),
      hr(),
      p(class = "note", style = "font-size:12px;",
        HTML("<strong>Data.</strong> California Test Score Data &mdash; 420 school districts.
              Online complements to Stock, J. H. and Watson, M. W. (2007),
              <em>Introduction to Econometrics</em>, 2nd ed., Addison Wesley; distributed
              in the R package <code>AER</code> as <code>CASchools</code>."))
    )
  )
)

ui <- navbarPage(
  title = "Econometrics I · Unit 3 · partialling out",
  header = tags$style(HTML(css)),
  page
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
         a  = unname(coef(mult)[1]),
         b2 = unname(coef(mult)[3]),
         fwl = unname(coef(fwl)[2]),
         r  = cor(x1, x2),
         lab = names(CONTROLS)[CONTROLS == c2])
  })

  ## The two fitted equations. The stratio coefficient gets 3 decimals, as on
  ## the panels; the control's coefficient 3 significant digits, because
  ## expenditure (in dollars) has a coefficient of 0.00162.
  output$regs <- renderUI({
    b <- bits()
    sg  <- function(v) if (v < 0) "&minus;" else "+"
    hl  <- function(v) sprintf("<span style='color:%s;font-weight:700;'>%s %.3f</span>",
                               garnet, sg(v), abs(v))
    HTML(sprintf(
      "<div class='lab'>Simple regression (no control)</div>
       <div style='font-size:17px;padding:2px 0 10px 0;'>
         mat&#293; = %.2f %s stratio</div>
       <div class='lab'>Multiple regression (with %s)</div>
       <div style='font-size:17px;padding-top:2px;'>
         mat&#293; = %.2f %s stratio %s %s %s</div>",
      SIMPLE_A, hl(SIMPLE),
      b$c2, b$a, hl(b$b1), sg(b$b2), formatC(abs(b$b2), digits = 3, format = "fg"), b$c2))
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

}

shinyApp(ui, server)
