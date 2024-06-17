#cleanup
rm(list = setdiff(ls(), "odf"))  # Remove all objects except 'odf'
lapply(setdiff(loadedNamespaces(), base::loadedNamespaces()), function(pkg) try(detach(paste("package", pkg, sep = ":"), character.only = TRUE, unload = TRUE), silent = TRUE))  # Unload all non-base packages
