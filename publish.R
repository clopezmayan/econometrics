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

## Refresh apps/ from the course folder. All three apps live in
## Applications/Visual_tools/ (the dummy-variables app since 16 Oct 2026;
## before that it was read from the concurso folder).
sync_from_course <- function() {
  vt <- path.expand(file.path(COURSE, "Applications", "Visual_tools"))
  if (!dir.exists(vt)) {
    message("course folder not found -- publishing apps/ as it stands")
    return(invisible())
  }
  # course folder name -> name in apps/
  src <- c("ols_least_squares" = "ols_least_squares",
           "partialling_out"   = "partialling_out",
           "dummies"           = "dummy_variables")
  for (i in seq_along(src)) {
    to <- file.path("apps", src[[i]])
    unlink(to, recursive = TRUE)
    dir.create(to)
    file.copy(list.files(file.path(vt, names(src)[i]), full.names = TRUE),
              to, recursive = TRUE)
  }
  file.copy(file.path(vt, "prep_data.R"), "apps", overwrite = TRUE)
  message("synced the three apps from the course folder")
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
