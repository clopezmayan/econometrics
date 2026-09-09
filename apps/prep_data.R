# ============================================================
#  Econometrics I 26-27 — teaching extract for the Shiny tools
#
#  Builds  caschools_teaching.csv  (one copy per app) from
#      Applications/Rscripts/caschools.RData
#  which is the course's own CAschools file. Verified 2026-09-09
#  to be byte-identical (same md5) to the original in
#  Materials/ppts_finalversion/Rscripts/, so Applications/ is
#  self-contained and nothing here reaches outside it.
#
#  CAschools is the data spine for Units 2 and 3 (PLAN Phase 02b):
#  students meet it in Unit 2 with one explanatory variable and
#  again in Unit 3 with several.
#
#  Run from this folder:  Rscript prep_data.R
#  Created 2026-09-09; moved into Applications/ the same day.
# ============================================================

src <- file.path("..", "Rscripts", "caschools.RData")
stopifnot(file.exists(src))

e <- new.env(); load(src, envir = e)
d <- as.data.frame(get("caschools", envir = e))

keep <- c("math", "studteachr", "income", "lunch", "english", "expenditure")
d <- d[stats::complete.cases(d[, keep]), keep]

# ---- the 7 districts drawn in the least-squares app -------------------
# WHY SEVEN, AND WHY SPREAD OUT. The squares are drawn to scale, so their
# side is the residual. With 20 districts packed into a narrow income band
# the squares are wider than the gaps between the points and the picture
# turns to mush (tried on 2026-09-09). Seven districts, one drawn AT RANDOM
# from each seventh of the income range, keeps every square separate.
#
# Equal-WIDTH bins, not quantiles: income is right-skewed, so decile bins
# still cluster the points at the bottom.
#
# HONEST SAMPLING. The draw within each bin is random -- nothing is selected
# on its residual. An early attempt picked the smallest-residual district in
# each bin and produced R2 = 0.98, a fit that is not in the data. That was
# discarded. Note that spreading x out does raise R2 above the full-sample
# value by construction; the app is about the MECHANICS of least squares,
# and its caption says the seven districts are spread across the range.
set.seed(26)
brk <- seq(min(d$income), max(d$income), length.out = 8)
bin <- cut(d$income, breaks = brk, include.lowest = TRUE)
parts <- split(seq_len(nrow(d)), bin)
parts <- parts[lengths(parts) > 0]
d$teach7 <- FALSE
d$teach7[vapply(parts, function(i) i[sample(length(i), 1)], integer(1))] <- TRUE

# One copy per app: each app must be self-contained for shinylive::export(),
# and (Cristina, 9 Sep) each topic is its own site with its own link and QR.
out <- file.path("ols_least_squares", "caschools_teaching.csv")
# NB: round() the numeric columns ONLY. Rounding the data frame as a whole
# coerces the logical flag to 1/0, and then ca[ca$teach20, ] in the app is
# NUMERIC ROW INDEXING, not a filter -- it silently returns row 1 twenty
# times. Caught 2026-09-09.
d[keep] <- round(d[keep], 4)
utils::write.csv(d[, c(keep, "teach7")], out, row.names = FALSE)

cat("wrote", out, "-", nrow(d), "districts,", sum(d$teach7), "flagged for the least-squares app\n\n")

## ---- the numbers the app asserts, printed so they can be checked ----
s <- lm(math ~ studteachr, d)
cat(sprintf("simple:    math = %.2f %+.3f studteachr     (n = %d)\n",
            coef(s)[1], coef(s)[2], nrow(d)))
d7 <- d[d$teach7, ]
s7 <- lm(math ~ income, d7)
cat(sprintf("app data:  math = %.2f %+.3f income          (n = %d, SSR = %.0f, R2 = %.3f)\n",
            coef(s7)[1], coef(s7)[2], nrow(d7), sum(resid(s7)^2), summary(s7)$r.squared))
cat(sprintf("           full-sample math ~ income: slope %+.3f, R2 %.3f\n\n",
            coef(lm(math ~ income, d))[2], summary(lm(math ~ income, d))$r.squared))

cat("Annex 5.2:  b1_simple = b1_multiple + b2 * delta1\n")
for (c2 in setdiff(keep, c("math", "studteachr"))) {
  m  <- lm(as.formula(paste("math ~ studteachr +", c2)), d)
  dl <- coef(lm(as.formula(paste(c2, "~ studteachr")), d))[2]
  cat(sprintf("  %-12s b1=%7.3f  b2=%8.3f  delta1=%10.4f  ->  %7.3f\n",
              c2, coef(m)[2], coef(m)[3], dl, coef(m)[2] + coef(m)[3] * dl))
}

# Second copy, for the Unit 3 app.
file.copy(out, file.path("partialling_out", "caschools_teaching.csv"), overwrite = TRUE)
cat("copied to partialling_out/\n")
