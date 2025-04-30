pkgname <- "xvm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "xvm-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('xvm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("export_xvg")
### * export_xvg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: export_xvg
### Title: export xvg data
### Aliases: export_xvg

### ** Examples

## Not run: 
##D xvg <- read_xvg(system.file("extdata/rmsd.xvg", package = "xvm"))
##D # Export as TSV
##D export_xvg(xvg, "rmsd.tsv", sep = "\t")
##D # Export as CSV
##D export_xvg(xvg, "rmsd.csv", sep = ",")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("export_xvg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_xpm")
### * plot_xpm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_xpm
### Title: plot xpm data
### Aliases: plot_xpm

### ** Examples

## No test: 
library(xvm)
xpm_file_path <- system.file("extdata/gibbs.xpm", package = "xvm")
xpm_data <- read_xpm(xpm_file_path)
plot_xpm(xpm_data) # plot the xpm data using plot_xpm() function
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_xpm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_xpm_3d")
### * plot_xpm_3d

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_xpm_3d
### Title: generate 3d scatter plot from xpm Data
### Aliases: plot_xpm_3d

### ** Examples

library(xvm)
xpm_file_path <- system.file("extdata/gibbs.xpm", package = "xvm")
xpm_data <- read_xpm(xpm_file_path)
plot_xpm_3d(xpm_data) # plot 3D scatter plot from xpm file



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_xpm_3d", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_xpm_facet")
### * plot_xpm_facet

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_xpm_facet
### Title: generate faceted plots from xpm Data
### Aliases: plot_xpm_facet

### ** Examples

## No test: 
library(xvm)
xpm_file_path <- system.file("extdata/gibbs.xpm", package = "xvm")
xpm_data <- read_xpm(xpm_file_path)
plot_xpm_facet(xpm_data) # plot pseudo-3D from xpm file
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_xpm_facet", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_xvg")
### * plot_xvg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_xvg
### Title: plot xvg data
### Aliases: plot_xvg

### ** Examples

## No test: 
library(xvm)
rmsd_file_path <- system.file("extdata/rmsd.xvg", package = "xvm")
rmsd_data <- read_xvg(rmsd_file_path)
plot_xvg(rmsd_data) # plot the xvg data using plot_xvg() function
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_xvg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_xpm")
### * read_xpm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_xpm
### Title: read xpm files
### Aliases: read_xpm

### ** Examples

## No test: 
library(xvm)
# Retrieve the path to the example file included in the package
xpm_file_path <- system.file("extdata/gibbs.xpm", package = "xvm")
xpm_data <- read_xpm(xpm_file_path) # read the xpm file using read_xpm() function
names(xpm_data)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_xpm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_xvg")
### * read_xvg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_xvg
### Title: read xvg files
### Aliases: read_xvg

### ** Examples

## No test: 
library(xvm)
# Retrieve the path to the example file included in the package:
rmsd_file_path <- system.file("extdata/rmsd.xvg", package = "xvm")
rmsd_data <- read_xvg(rmsd_file_path) # read the xvg file using read_xvg() function
names(rmsd_data)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_xvg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary_xvg")
### * summary_xvg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary_xvg
### Title: summarize xvg Data
### Aliases: summary_xvg

### ** Examples

path <- system.file("extdata/rmsd.xvg", package = "xvm")
xvg <- read_xvg(path)
summary_xvg(xvg)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary_xvg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
