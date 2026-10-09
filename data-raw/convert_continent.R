# Convert the continent layer shipped with the package from .qs to .rds, so that
# the package no longer needs {qs} to draw maps. Run once from the package root,
# with {qs} installed, then commit the result.

continent <- qs::qread("inst/extdata/continent.qs")
saveRDS(continent, "inst/extdata/continent.rds", compress = "xz")
file.remove("inst/extdata/continent.qs")
