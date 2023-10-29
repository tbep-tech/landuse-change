library(tidyverse)
library(here)

# pull data from hmpu-workflow repo -----------------------------------------------------------

load(url("https://github.com/tbep-tech/hmpu-workflow/raw/master/data/acres.RData"))
load(url("https://github.com/tbep-tech/hmpu-workflow/raw/master/data/subtacres.RData"))
load(url("https://github.com/tbep-tech/hmpu-workflow/raw/master/data/chgdat.RData"))
load(url("https://github.com/tbep-tech/hmpu-workflow/raw/master/data/subtchgdat.RData"))
fluccs <- read.csv('https://github.com/tbep-tech/hmpu-workflow/raw/master/data/FLUCCShabsclass.csv', stringsAsFactors = F)

save(acres, file = here('data/acres.RData'))
save(subtacres, file = here('data/subtacres.RData'))
save(chgdat, file = here('data/chgdat.RData'))
save(subtchgdat, file = here('data/subtchgdat.RData'))
save(fluccs, file = here('data/fluccs.RData'))
