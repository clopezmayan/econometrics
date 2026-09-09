# Econometrics I — interactive teaching apps

Visual tools for **Econometrics I**, Grau d'Economia (English group), Universitat de
Barcelona.

Each app runs entirely in your browser. There is nothing to install: no R, no account.
They work on a phone or tablet as well as on a laptop. The first time you open one it takes
a few seconds to start.

| App | Link |
|---|---|
| **What least squares minimizes** | https://clopezmayan.github.io/econometrics/least-squares/ |
| **Partialling out** | https://clopezmayan.github.io/econometrics/partialling-out/ |
| **Dummy variables in regression** | https://clopezmayan.github.io/econometrics/dummy-variables/ |

---

### What least squares minimizes

Move a line over seven California school districts with two sliders. Every residual is
drawn as a real square, to scale, so the *area* of each square is the residual squared, and
the sum of squared residuals is the total shaded area.

Try to make that total as small as you can by hand. The app keeps your best score. Then
press *"Show me the OLS line"* — no line you can set will do better. That is all the words
“least squares” mean.

### Partialling out

We want the effect of class size on maths scores, and we add a control variable.

Three panels: `math` on `studteachr`; `studteachr` on the control you choose; then `math`
on **what is left of `studteachr`** once the control has been taken out of it. The slope of
the third panel is the multiple regression coefficient — not close to it, equal to it.
A Ballentine diagram beside them shows the two explanatory variables overlapping, and the
overlap grows or shrinks with the control you pick.

Try all four controls. The one that overlaps most with class size is *not* the one that
changes the coefficient most — it is worth working out why.

### Dummy variables in regression

Sliders redraw two parallel group lines and shade the vertical gap between them. Switch the
base group and watch the lines stay exactly where they are while the reported coefficients
change.

A second tab estimates the gender wage gap on real Spanish earnings data, with control
variables that can be switched on and off, next to the group means.
