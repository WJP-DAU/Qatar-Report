## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Qatar Report - Special Data Viz Functions
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     December 13th, 2024
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Waffle                                                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wjp_waffle <- function(){
  
  # Wrangling data
  data2plot <- master_data %>%
    select(discrimination1) %>%
    summarise(
      discrimination1 = mean(discrimination1, na.rm = T)
    ) %>%
    pivot_longer(
      everything(),
      names_to  = "category",
      values_to = "Yes"
    ) %>%
    mutate(
      Yes = round(Yes*100,0),
      label = paste0(format(round(Yes, 0),
                            nsmall = 0),
                     "%"),
      No = 100 - Yes
    ) %>%
    pivot_longer(
      cols      = !c(category, label), 
      names_to  = "group", 
      values_to = "value"
    ) %>%
    mutate(
      label = if_else(group %in% "empty_value", NA_character_, label),
      x_pos = if_else(group == "Yes", 1, 0)
    ) %>%
    mutate(group = case_when(
      group == "Yes" ~ "41% Yes",
      group == "No"  ~ "59% No"
    ))
  
  parts = c(
    "Yes" = (data2plot$value[[1]]), 
    "No"  = (data2plot$value[[2]])
  )
  names(parts) = paste0(parts,"%", " ", names(parts))
  
  # Creating plot
  plot <- waffle(
    parts, 
    size = 1,
    rows = 5, 
    colors = c("#2a2a94", "#B9B9B9"),
    legend_pos = "left",
    # use_glyph  = "child", 
    glyph_size = 2.5
  ) +
    theme_enhance_waffle() +
    theme(
      panel.spacing    = unit(0.25, "cm"),
      strip.background = element_blank(),
      strip.text       = element_text(size = 11, 
                                      hjust = 0,
                                      family = "Lato Full",
                                      face   = "italic"),
      legend.position    = "left",
      plot.background    = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(), 
      panel.border       = element_blank(), 
      legend.text        = element_text(hjust  = 0,
                                        family = "Lato Full",
                                        face   = "plain",
                                        size   = 11),
      legend.key.width      = unit(0.5,"cm"), 
      legend.key.height     = unit(0.5,"cm"), 
      legend.background     = element_blank(), 
      legend.box.background = element_blank(), 
      plot.margin           = margin(-50,0,0,0), 
      legend.box.just       = "center"
    )
  
  return(plot)
}

wafflem <- function(data){
  
  parts <- round(data$perc)
  names(parts) <- paste0(
    data$answer, " (",
    format(
      round(parts, 0),
      nsmall = 0
    ),
    "%)"
  )
  
  # Creating plot
  plot <- waffle(
    parts, 
    size = 1,
    rows = 5, 
    colors = c("#18538E", "#46B5FF", "#FFC818", "#FF7900", "#CCCCCC"),
    legend_pos = "left",
    # use_glyph  = "child", 
    glyph_size = 2.5
  ) +
    theme_enhance_waffle() +
    theme(
      panel.spacing    = unit(0.25, "cm"),
      strip.background = element_blank(),
      strip.text       = element_text(size = 11, 
                                      hjust = 0,
                                      family = "Lato Full",
                                      face   = "italic"),
      legend.position    = "left",
      plot.background    = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(), 
      panel.border       = element_blank(), 
      legend.text        = element_text(hjust  = 0,
                                        family = "Lato Full",
                                        face   = "plain",
                                        size   = 11),
      legend.key.width      = unit(0.5,"cm"), 
      legend.key.height     = unit(0.5,"cm"), 
      legend.background     = element_blank(), 
      legend.box.background = element_blank(), 
      plot.margin           = margin(10,0,0,0), 
      legend.box.just       = "center"
    )
  
  return(plot)
}

