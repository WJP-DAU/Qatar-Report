## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Qatar Report - Data Viz
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     December 12th, 2024
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Utilities                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Extract Viz Parameters function
extractParameters <- function(pid, figure_map, outline){
  
  # Extracting general parameters
  parameters <- lapply(
    c("legend_labels" = "legend_text", 
      "color_codes"   = "legend_color",
      "sample"        = "sample",
      "chart_id"      = "id",
      "variables"     = "var_id",
      "plot_function" = "type",
      "unique_id"     = "unique_id"),
    
    function(item){
      
      unlist(
        str_split(
          figure_map %>%
            filter(panelID %in% pid) %>%
            select(v = all_of(item)) %>%
            pull(v),
          pattern = ", "
        )
      )
    }
  )
  
  # Defining color palette
  parameters[["color_palette"]] <- parameters[["color_codes"]]
  
  if (all(parameters[["legend_labels"]] != "None")) {
    names(parameters[["color_palette"]]) <- parameters[["legend_labels"]]
  }
  
  # Extracting macro type
  parameters[["HTML_macro"]] <- outline %>%
    separate_wider_delim(
      charts, 
      names = c("charts1", "charts2"), 
      delim = ", ", 
      too_few = "align_start"
    ) %>%
    filter(
      charts1 %in% parameters[["chart_id"]] | charts2 %in% parameters[["chart_id"]]
    ) %>%
    distinct(macro, .keep_all = T) %>%
    pull(macro)
    
  return(parameters)
}

# Saving function
saveIT.fn <- function(chart, fig, pid, w, h) {
  ggsave(plot   = chart,
         file   = file.path(path2main, 
                            "data-viz/outputs",
                            fig,
                            paste0(pid, ".svg"),
                            fsep = "/"), 
         width  = w, 
         height = h,
         units  = "mm",
         dpi    = 72,
         device = "svg")
} 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Visualizer                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

callVisualizer <- function(pid, figure_map, outline){
  
  print(paste("Generating ", pid))
  
  # Defining data & parameters
  parameters <- extractParameters(pid, figure_map, outline)
  if (parameters[["unique_id"]] %in% c("crv1", "pos2", "dis1", "dis2")){
    data       <- NULL
  } else {
    data       <- data_points[[pid]]
  }
  
  # Calling visualizer
  if(parameters[["plot_function"]] == "Diverging bars"){
    viz <- wjp_divbars(
      data         = data,             
      target       = "perc",       
      grouping     = "labels",         
      diverging    = "value",     
      negative     = "Yes",   
      cvec         = parameters$color_palette,
      labels       = "value_labs",       
      custom_order = F, 
      order        = NULL,  
      ptheme       = WJP_theme()
    )
  }
  if(parameters[["plot_function"]] == "Edgebars"){
    viz <- wjp_edgebars(
      data         = data,
      y_value      = "perc",
      x_var        = "variable",
      # x_lab_pos    = "order",
      label_var    = "labels",
      color_var    = "sample",
      nudge_lab    = 4.5,
      bar_colors   = parameters[["color_palette"]],
      margin_top   = 15,
      bar_width    = 0.5,
      ptheme       = WJP_theme()
    )
  }
  if(parameters[["plot_function"]] == "Horizontal bars"){
    if(parameters[["unique_id"]] %in% c("acc1", "pos1")){
      viz <- wjp_bars(
        data       = data,     
        labels     = "value_labs",
        lab_pos    = "label_pos", 
        target     = "perc",        
        grouping   = "sample",      
        colors     = "color_category",        
        cvec       = parameters[["color_palette"]],            
        direction  = "horizontal",         
        ptheme     = WJP_theme()
      )
    }
    if (parameters[["unique_id"]] %in% c("cja1","cja2","cja3", "dis3")){
      viz <- wjp_bars(
        data       = data,     
        labels     = "value_labs",
        lab_pos    = "label_pos", 
        target     = "perc",        
        grouping   = "labels",      
        colors     = "color_category",        
        cvec       = parameters[["color_palette"]],            
        direction  = "horizontal",         
        ptheme     = WJP_theme()
      )
    }
    
  }
  if(parameters[["plot_function"]] == "Lollipops"){
    viz <- wjp_lollipops(
      data     = data,
      target   = "perc",
      grouping = "labels"
    )
  }
  if(parameters[["plot_function"]] == "Radar"){
    viz <- wjp_radar(
      data       = data,
      axis_var   = "variable",
      target_var = "perc",
      label_var  = "labels",
      order_var  = "order",
      color_var  = "sample",
      colors     = parameters[["color_palette"]],
      maincat    = "Total"
    )
  }
  if(parameters[["plot_function"]] == "Rose"){
    viz <- wjp_rose(
    data         = data,
    target_var   = "perc",
    grouping_var = "variable",
    alabels_var  = "labels",
    plabels_var  = "value_labs",
    colors       = parameters[["color_palette"]],
    order_var    = "order_no"
    )
  }
  if(parameters[["plot_function"]] == "Stacked bars"){
    viz <- wjp_bars(
      data       = data,     
      labels     = "value_labs",
      lab_pos    = "label_pos", 
      target     = "perc",        
      grouping   = "labels",      
      colors     = "value",        
      cvec       = parameters[["color_palette"]],            
      direction  = "horizontal",  
      stacked    = TRUE,
      ptheme     = WJP_theme()
    )
  }
  if(parameters[["plot_function"]] == "Waffle"){
    viz <- wjp_waffle()
  }
  if(parameters[["plot_function"]] == "Gauge"){
    viz <- wjp_gauge(
      data   = data, 
      target = "perc", 
      colors = "value", 
      labels = "labels",
      cvec   = parameters[["color_palette"]]
    )
  }
  
  
  # Defining plot dimensions
  if (parameters[["HTML_macro"]] == "singlepanel"){
    h = 183.1106
    w = 189.7883
  }
  if (parameters[["HTML_macro"]] == "bipanel"){
    h = 68.88612
    w = 189.7883
    if (parameters[["unique_id"]] %in% c("dis1")){
      h = 38.88612 
    }
    if (parameters[["unique_id"]] %in% c("dis3")){
      h = 110.88612 
    }
  }
  if (parameters[["HTML_macro"]] == "tripanel"){
    h = 49.90729
    w = 189.7883
    
    if (parameters[["unique_id"]] %in% c("trt1")){
      h = 21.90729 
    }
    if (parameters[["unique_id"]] %in% c("trt2", "cor1")){
      h = 38.90729 
    }
  }
  if (parameters[["HTML_macro"]] == "quadpanel"){
    h = 76.9697
    w = 91.37955
  }
  if (parameters[["HTML_macro"]] == "pentapanel"){
    h = 40.26276
    w = 63.26276
  }
  if (parameters[["HTML_macro"]] == "hexpanel"){
    h = 45.68977
    w = 90.67663
  }
  
  # Saving plot as SVG
  saveIT.fn(chart = viz, 
            fig   = parameters[["chart_id"]], 
            pid   = pid, 
            w     = w, 
            h     = h)
  
  return(viz)
  
}
