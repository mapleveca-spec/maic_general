# Sources the framework so tests run against the current source tree.
# testthat loads helper-*.R files automatically before any test file.

framework_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

source(file.path(framework_root, "08_main", "source_framework.R"))
source_framework(framework_root)

source(file.path(framework_root, "sandbox", "make_toy_data.R"))
source(file.path(framework_root, "sandbox", "toy_analysis_inputs.R"))

# Shared fixture. Tests mutate one field at a time to isolate the rule under test.
toy <- make_toy_data()
