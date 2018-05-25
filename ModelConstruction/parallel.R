# Fri May 25 10:06:17 PDT 2018
#
# Generate parallel versions from the serial scripts

library(autoparallel)

fname = "run_model.v2.R"

oldcode = parse(fname)

newcode = preprocess(oldcode)

writeLines(as.character(newcode), paste0("gen_", fname))
