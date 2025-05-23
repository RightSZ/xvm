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
# This example file is an xpm file containing (free energy landscape, FEL) data generated by GROMACS
gibbs_file_path <- system.file("extdata/gibbs.xpm", package = "xvm")

## ----read_single_xpm_file-----------------------------------------------------
# Read the xpm file using read_xpm() function
gibbs_data <- read_xpm(gibbs_file_path)

# The imported xpm file is stored as a list, with the list name corresponding to the file name.
names(gibbs_data)

## ----show_xvg_content---------------------------------------------------------
str(gibbs_data[[1]],max.level = 1)

## ----plot_single_xpm_file, dpi = 90, fig.width=5, fig.height=5----------------
# Plot the xpm data using plot_xpm() function
plot_xpm(gibbs_data)

## ----read_multiple_xvg_files--------------------------------------------------
# Similarly, you can also read multiple types of xpm files. 
multi_file_path <- dir(system.file("extdata", package = "xvm"))

# Filter out xpm files using stringr package
library(stringr)
multi_file_path <- multi_file_path[str_detect(multi_file_path, ".xpm")]
print(multi_file_path)

# Set the full xvg file paths
multi_file_path <- file.path(system.file("extdata", package = "xvm"),
                             multi_file_path
                             )

## ----batch_read---------------------------------------------------------------
# Read multiple xpm files at once in batch:
multi_data <- read_xpm(multi_file_path)
names(multi_data)

## ----inspect_read-------------------------------------------------------------
# Check the first xpm file info via indexing
str(multi_data[[1]],max.level = 1)

## ----plot_multiple_xpm_File---------------------------------------------------
# Use lapply() to plot all the xpm files in batch
mutli_xpm_plots <- lapply(multi_data, plot_xpm)

## ----arrange_xpm_plots, dpi = 90, fig.width=8, fig.height=5, message=FALSE----
# Arrange the plots using ggpubr
library(ggpubr)
ggarrange(plotlist = mutli_xpm_plots)

## ----plot_pseudo_3D, dpi = 90, fig.width=5, fig.height=5----------------------
# Usage is similar to the plot_xpm() function
plot_xpm_facet(gibbs_data)

## ----plot_3D_scatter, dpi = 90, fig.width=5, fig.height=5---------------------
# Usage is similar to the plot_xpm() function
plot_xpm_3d(gibbs_data)

