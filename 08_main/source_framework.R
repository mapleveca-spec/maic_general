# source_framework() -----------------------------------------------------------
#
# Sources every module file in dependency order. The one place that knows the
# module layout; tests, sandbox scripts, and production scripts all call it.
#
#   root  framework root directory (the one containing 01_inputs/ etc.)

FRAMEWORK_MODULES <- c(
  "01_inputs", "02_summaries", "03_balance", "04_weighting",
  "05_models", "06_scenarios", "07_reporting", "08_main"
)

source_framework <- function(root = ".", envir = globalenv()) {
  for (module in FRAMEWORK_MODULES) {
    files <- sort(list.files(file.path(root, module), pattern = "\\.R$", full.names = TRUE))
    for (f in files) sys.source(f, envir = envir)
  }
  invisible(root)
}
