##----required-packages----

getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")

pckg = try(require(knitr))
if(!pckg) {
  cat("Installing 'knitr' from CRAN\n")
  getPckg("knitr")
  require(knitr)
}

pckg = try(require(magrittr))
if(!pckg) {
  cat("Installing 'magrittr' from CRAN\n")
  getPckg("magrittr")
  require(magrittr)
}

pckg = try(require(dplyr))
if(!pckg) {
  cat("Installing 'dplyr' from CRAN\n")
  getPckg("dplyr")
  require(dplyr)
}


pckg = try(require(stats))
if(!pckg) {
  cat("Installing 'stats' from CRAN\n")
  getPckg("stats")
  require(stats)
}

pckg = try(require(ggplot2))
if(!pckg) {
  cat("Installing 'ggplot2' from CRAN\n")
  getPckg("ggplot2")
  require(ggplot2)
}

pckg = try(require(xtable))
if(!pckg) {
  cat("Installing 'xtable' from CRAN\n")
  getPckg("xtable")
  require(xtable)
}

pckg = try(require(texreg))
if(!pckg) {
  cat("Installing 'texreg' from CRAN\n")
  getPckg("texreg")
  require(texreg)
}

pckg = try(require(CDM))
if(!pckg) {
  cat("Installing 'CDM' from CRAN\n")
  getPckg("CDM")
  require(CDM)
}

pckg = try(require(sna))
if(!pckg) {
  cat("Installing 'sna' from CRAN\n")
  getPckg("sna")
  require(sna)
}

pckg = try(require(igraph))
if(!pckg) {
  cat("Installing 'igraph' from CRAN\n")
  getPckg("igraph")
  require(igraph)
}