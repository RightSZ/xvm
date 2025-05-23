## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = FALSE
)

## ----load_xvm-----------------------------------------------------------------
# Load the xvm package
library(xvm)

## ----dependency_packages, message=FALSE, error=FALSE--------------------------
# Load dependency packages
library(ggplot2)

## ----get_file_path------------------------------------------------------------
# This example file is an xvg file containing RMSD data generated by GROMACS
rmsd_file_path <- system.file("extdata/rmsd.xvg", package = "xvm")

## ----read_single_xvg_file-----------------------------------------------------
# Read the xvg file using read_xvg() function
rmsd_data <- read_xvg(rmsd_file_path)

# The imported xvg file is stored as a list, with the list name corresponding to the file name.
names(rmsd_data)

## ----show_xvg_content---------------------------------------------------------
str(rmsd_data[[1]])

## ----plot_single_xvg_file, dpi = 90, fig.width=5, fig.height=5----------------
# Plot the xvg data using plot_xvg() function
plot_xvg(rmsd_data)

## ----read_multiple_xvg_files--------------------------------------------------
# Similarly, you can also read multiple types of xvg files. 
multi_file_path <- dir(system.file("extdata", package = "xvm"))

# Filter out xvg files using stringr package
library(stringr)
multi_file_path <- multi_file_path[str_detect(multi_file_path, ".xvg")]
print(multi_file_path)

# Set the full xvg file paths
multi_file_path <- file.path(system.file("extdata", package = "xvm"),
                             multi_file_path
                             )

## ----batch_read---------------------------------------------------------------
# Read multiple xvg files at once in batch:
multi_data <- read_xvg(multi_file_path)
names(multi_data)

## ----inspect_read-------------------------------------------------------------
# Check the first xvg file info via indexing
str(multi_data[[1]])

## ----plot_a_single_xvg_file, dpi = 90, fig.width=5, fig.height=5--------------
# Plot one of the xvg files using the plot_xvg() function:
plot_xvg(multi_data[[1]])

## ----plot_multiple_xvg_file---------------------------------------------------
# Use lapply() to plot all the xvg files in batch
mutli_xvg_plots <- lapply(multi_data, plot_xvg)

## ----arrange_xvg_plots, dpi = 90, fig.width=8, fig.height=5, message=FALSE----
# Arrange the plots using ggpubr
library(ggpubr)
ggarrange(plotlist = mutli_xvg_plots)

