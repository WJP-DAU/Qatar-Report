## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Qatar Report 2024 - RunMe File
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
## 0.  Presetting                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading settings and functions
source("code/settings.R")
source("code/data-wrangling.R")
# source("code/data-viz.R")

# Report outline version
outline_version <- "v0"

# Loading data
master_data <- read_dta(
  file.path(path2main,
            "data-viz/data/Qatar_final_2024.dta",
            fsep = "/")
) %>%
  mutate(
    latestYear = 2024,
    nationality = case_when(
      nation == 1 ~ "Qatari",
      nation == 2 ~ "Foreigner"
    )
  )

master_data <- add_specialVariables(master_data)

# Loading figure map & outline
figure_map <- read.xlsx(glue("../report_outline_{outline_version}.xlsx"), sheet = "figure_map") %>%
  mutate(
    panelID = paste(id, panel, sep = "_")
  ) %>%
  relocate(panelID)
outline <- read.xlsx(glue("../report_outline_{outline_version}.xlsx"), sheet = "outline")

# Study Variables
study_variables <- unlist(
  str_split(
    figure_map %>%
      filter(var_id != "TBD") %>%
      pull(var_id),
    ", "
  )
)

# Cleaning outputs
outputsReset(figure_map)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Data Wrangling                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Producing a data bank
data_bank <- DataBank(master_data)
