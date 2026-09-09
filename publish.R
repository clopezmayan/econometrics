# ============================================================
#  Econometrics I — rebuild the published apps
#
#  Rscript publish.R --sync     refresh apps/ from the course folder,
#                               then export every app into docs/
#  Rscript publish.R            export what is already in apps/
#
#  Then: git add -A && git commit -m "rebuild apps" && git push
#
#  apps/ is a one-way mirror; the canonical sources are in the course
#  folder. Edit them there, not here.
#
#  Notes on the build, the data and what must not go in this public
#  repo are kept OUT of it, in the course folder:
#      Applications/Visual_tools/README.md
#  Read that before changing anything here.
# ============================================================

APPS <- c(
  # source folder in apps/   ->   published path under docs/
  # Topic names, never unit numbers -- see the internal notes.
  "dummy_variables"   = "dummy-variables",
  "ols_least_squares" = "least-squares",
  "partialling_out"   = "partialling-out"
)

COURSE <- file.path(
  "~", "Documents", "Docencia", "UB", "Econometrics I_GEco",
  "Materials", "revised_materials")

## Refresh apps/ from the course folder.
sync_from_course <- function() {
  vt <- path.expand(file.path(COURSE, "Applications", "Visual_tools"))
  dv <- path.expand(file.path(COURSE, "materiales_concurso",
                              "leccion_materiales", "shiny_web"))
  if (dir.exists(vt)) {
    for (a in c("ols_least_squares", "partialling_out")) {
      unlink(file.path("apps", a), recursive = TRUE)
      file.copy(file.path(vt, a), "apps", recursive = TRUE)
    }
    file.copy(file.path(vt, "prep_data.R"), "apps", overwrite = TRUE)
    message("synced least squares + partialling out from the course folder")
  } else {
    message("course folder not found -- publishing apps/ as it stands")
  }
  if (dir.exists(dv)) {
    unlink(file.path("apps", "dummy_variables"), recursive = TRUE)
    file.copy(dv, "apps", recursive = TRUE)
    file.rename(file.path("apps", "shiny_web"), file.path("apps", "dummy_variables"))
    message("synced dummy variables from the concurso folder")
  }
}

if (!interactive() && "--sync" %in% commandArgs(TRUE)) sync_from_course()

for (i in seq_along(APPS)) {
  src <- file.path("apps", names(APPS)[i])
  out <- file.path("docs", APPS[[i]])
  if (!dir.exists(src)) { message("skipping ", src, " (not present)"); next }
  message("exporting ", src, " -> ", out)
  unlink(out, recursive = TRUE)
  shinylive::export(src, out)
}

## .nojekyll: stops Pages running the files through Jekyll.
file.create(file.path("docs", ".nojekyll"), showWarnings = FALSE)

message("\nDone. The three published paths are:")
for (p in APPS) message("  https://clopezmayan.github.io/econometrics/", p, "/")
message("\nNow: git add -A && git commit -m 'rebuild apps' && git push")
