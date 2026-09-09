# Econometrics I — interactive teaching apps

Visual tools for **Econometrics I**, Grau d'Economia (English group), Universitat de
Barcelona. Each one runs entirely in the browser — no R, no install, no account — and works
on a phone.

| App | Link | Where it is used |
|---|---|---|
| **What least squares minimizes** | https://clopezmayan.github.io/econometrics/least-squares/ | Unit 2 — the simple regression model |
| **Partialling out** | https://clopezmayan.github.io/econometrics/partialling-out/ | Unit 3 — *ceteris paribus*, Frisch–Waugh–Lovell |
| **Dummy variables in regression** | https://clopezmayan.github.io/econometrics/dummy-variables/ | Unit 6 — qualitative explanatory variables |

**One topic, one link.** The apps are deliberately separate rather than tabs in a single
page, so they can be given to students one at a time as the course reaches each idea.

Paths are named by **topic, never by unit number**: the course renumbers between years (what
was Unit 5 is Unit 6 in 26-27), and a link or QR code already handed out cannot be reissued.

---

## What each app does

**Least squares.** Two sliders move a line over seven California school districts. Every
residual is drawn as a real square, to scale, so its *area* is the residual squared; the SSR
is their total area, shown as one number. A counter records the best the student reached by
hand, and then the OLS line undercuts it. A grid search over the whole slider range confirms
no setting beats OLS.

**Partialling out.** Three panels: `math` on `studteachr`; `studteachr` on a control the
student chooses; then `math` on what is left of `studteachr`. The slope of the third panel
equals the multiple-regression coefficient to six decimals — that is Frisch–Waugh–Lovell,
on real data. A Ballentine diagram beside it grows and shrinks with the actual correlation.
The menu is the lesson: `expenditure` overlaps most with `studteachr` and moves the
coefficient *least*; `income` overlaps least and moves it *most*.

**Dummy variables.** Sliders redraw two parallel group lines and shade the vertical gap, and
the base group can be switched to show that the lines stay put while the coefficients change.
A second tab estimates the gender wage gap on real Spanish data with controls toggled on and
off.

---

## Data, and why it is safe to publish

| App | Data |
|---|---|
| Least squares, Partialling out | **CAschools** — 420 California school districts, a standard public teaching dataset |
| Dummy variables | **EES 2022 teaching extract** — 50,000 employees drawn from INE's *Encuesta de Estructura Salarial* |

Everything here is public teaching data. The EES extract carries **no geography, no employer
identifier, and age only in bands** — seven coarse variables in total — on top of the
anonymisation INE itself applies (the five highest and five lowest wages per autonomous
community are masked). The source microdata is freely downloadable by anyone from INE.

**Source and attribution.** INE, *Encuesta de Estructura Salarial* 2022 (wave published
September 2024). Microdata: <https://www.ine.es/ftp/microdatos/salarial/datos_2022.zip> ·
Methodology: <https://www.ine.es/metodologia/t22/meto_ees22.pdf>. The full provenance of the
extract — the hourly-wage recipe, the sampling, and the self-weighting draw — is documented
in the course materials.

> ⚠️ **This repository is public.** Nothing from `Quizzes/`, `Final_exams/`, solutions, or
> any student data belongs here. Note that shinylive **bundles each app's data into the
> published site in the clear**: the CSV is readable at `<app>/app.json`. Only publish data
> you are content to hand out.

---

## Layout

```
docs/                     what GitHub Pages serves (Settings → Pages: main, /docs)
  dummy-variables/        built site
  least-squares/          built site
  partialling-out/        built site
apps/                     the app sources, a MIRROR of the course folder
  prep_data.R             builds the CAschools teaching extract
publish.R                 rebuild every app into docs/
```

`apps/` is a one-way mirror. The canonical sources live in the course folder
(`Applications/Visual_tools/` and `materiales_concurso/leccion_materiales/shiny_web/`);
edit them there, then re-sync. Editing `apps/` directly makes the two copies drift.

## Rebuilding and publishing

```sh
Rscript publish.R --sync     # refresh apps/ from the course folder, then export
git add -A && git commit -m "rebuild apps" && git push
```

Pages redeploys in a minute or two. The URLs never change.

⚠️ **Do not upgrade `shinylive` mid-course.** Each app embeds a ~66 MB WebAssembly R
runtime, but they are byte-identical across apps, so git stores it **once** and each further
app costs about 30 KB. A version bump writes a whole new 66 MB runtime into history,
permanently. If you must upgrade, rebuild and push all apps in one commit.
