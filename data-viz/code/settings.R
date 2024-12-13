## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Qatar Report 2024 - Settings
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     December 10th, 2024
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages & viz functions                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## VERY IMPORTANT !!
## YOU NEED TO INSTALL THE FOLLOWING PACKAGES: 
## (1) remotes::install_github("yjunechoe/ggtrace") The CRAN ggtrace package is from another author
## (2) remotes::install_github("ctoruno/WJPr")

library(pacman)
p_load(char = c(
  # Visualizations
  "showtext", "ggtext", "patchwork", "cowplot", "ggh4x",
  "ggrepel", "ggtrace",
  
  # Data Loading
  "haven", "openxlsx",
  
  # Utilities
  "kableExtra", "glue",
  
  # Good 'ol Tidyverse
  "tidyverse"
  
))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (Sys.info()["user"] == "carlostoruno") {
  path2SP <- "/Users/carlostoruno/OneDrive - World Justice Project"

}
path2main <- file.path(
  path2SP, "Data Analytics/6. Country Reports/Qatar-Report",
  fsep = "/"
)

# Viz functions
source("https://raw.githubusercontent.com/ctoruno/WJPr/refs/heads/main/R/barsChart.R")
source("https://raw.githubusercontent.com/ctoruno/WJPr/refs/heads/main/R/divbarsChart.R")
source("https://raw.githubusercontent.com/ctoruno/WJPr/refs/heads/main/R/edgebarsChart.R")
source("https://raw.githubusercontent.com/ctoruno/WJPr/refs/heads/main/R/radarChart.R")
source("https://raw.githubusercontent.com/ctoruno/WJPr/refs/heads/main/R/lollipopChart.R")
source("https://raw.githubusercontent.com/ctoruno/WJPr/refs/heads/main/R/roseChart.R")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Loading Fonts                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

path2fonts<- file.path(
  path2SP, "Data Analytics/6. Country Reports/0. Fonts",
  fsep = "/"
)
font_add(family     = "Lato Full",
         regular    = file.path(path2fonts, "Lato-Regular.ttf", fsep = "/"),
         italic     = file.path(path2fonts, "Lato-LightItalic.ttf", fsep = "/"),
         bold       = file.path(path2fonts, "Lato-Bold.ttf", fsep = "/"),
         bolditalic = file.path(path2fonts, "Lato-BoldItalic.ttf", fsep = "/"))
font_add(family  = "Lato Light",
         regular = file.path(path2fonts, "Lato-Light.ttf", fsep = "/"))
font_add(family  = "Lato Black",
         regular = file.path(path2fonts, "Lato-Black.ttf", fsep = "/"))
font_add(family  = "Lato Black Italic",
         regular = file.path(path2fonts, "Lato-BlackItalic.ttf", fsep = "/"))
font_add(family  = "Lato Medium",
         regular = file.path(path2fonts, "Lato-Medium.ttf", fsep = "/"))
showtext_auto()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  WJP theme                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

WJP_theme <- function() {
  theme(panel.background   = element_blank(),
        plot.background    = element_blank(),
        panel.grid.major   = element_line(linewidth = 0.25,
                                          colour    = "#5e5c5a",
                                          linetype  = "dashed"),
        panel.grid.minor   = element_blank(),
        axis.title.y       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C"),
        axis.text.x        = element_text(family = "Lato Full",
                                          face   = "plain",
                                          size   = 3.514598*.pt,
                                          color  = "#524F4C"),
        axis.ticks         = element_blank(),
        plot.margin        = unit(c(0, 0, 0, 0), "points")
  ) 
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5.  General utilities                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

outputsReset <- function(figure_map){
  
  # Listing previous outputs
  prevOutputs <- list.files(
    file.path(
      path2main,
      "data-viz/outputs"
    ), 
    include.dirs = F, 
    full.names   = T, 
    recursive    = T
  )
  
  # Deleting previous outputs
  file.remove(prevOutputs)
  
  # Creating folders for each chart
  figure_list <- figure_map %>% 
    distinct(id) %>% 
    pull(id)
  for (figure in figure_list) {
    dir.create(file.path(path2main,
                         "data-viz/outputs",
                         figure,
                         fsep = "/"), 
               showWarnings = FALSE)
  }
}



