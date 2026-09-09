# ============================================================
#  Econometrics I — rebuild the published apps
#
#  Run from the repository root:   Rscript publish.R
#  then:                           git add -A && git commit && git push
#
#  Each app in apps/ is exported to docs/<path>/ , which is what
#  GitHub Pages serves. The path names are the STUDENT-FACING URLs
#  and are deliberately topic-named, never unit-numbered: the course
#  renumbers between years (old Unit 5 became Unit 6 in 26-27) and a
#  link or QR code already handed out cannot be reissued.
#
#  ONE-WAY SYNC. The canonical sources live in the course folder:
#      Applications/Visual_tools/     (least squares, partialling out)
#      materiales_concurso/leccion_materiales/shiny_web/  (dummy variables)
#  apps/ here is a MIRROR, refreshed by sync_from_course() below.
#  Never edit apps/ directly -- edit the course folder and re-sync,
#  or the two copies drift.
#
#  ⚠️ DO NOT UPGRADE shinylive MID-COURSE. Every app embeds a copy of
#  the WebAssembly R runtime (~66 MB). They are byte-identical across
#  apps, so git stores the runtime ONCE and a new app costs ~30 KB.
#  A shinylive version bump writes a whole new 66 MB runtime into
#  history, permanently. If you must upgrade, rebuild and push ALL
#  apps in the same commit so only one new runtime enters history.
# ============================================================

APPS <- c(
  # source folder in apps/   ->   published path under docs/
  "dummy_variables"   = "dummy-variables",
  "ols_least_squares" = "least-squares",
  "partialling_out"   = "partialling-out"
)

COURSE <- file.path(
  "~", "Documents", "Docencia", "UB", "Econometrics I_GEco",
  "Materials", "revised_materials")

## Refresh apps/ from the course folder. Optional: skip it if you are
## only republishing what is already mirrored here.
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

## .nojekyll at the docs root: stops GitHub Pages running the files
## through Jekyll, which ignores paths beginning with an underscore.
file.create(file.path("docs", ".nojekyll"), showWarnings = FALSE)

message("\nDone. The three published paths are:")
for (p in APPS) message("  https://clopezmayan.github.io/econometrics/", p, "/")
message("\nNow: git add -A && git commit -m 'rebuild apps' && git push")
