## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            USA Report - Data Wrangling
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 12th, 2024
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Adding Special Variables                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

add_specialVariables <- function(data){
  data %>%
    mutate(
      cr_corr = case_when(
        q8b_11 == 1 ~ 1,
        q8b_12 == 1 ~ 1,
        q8b_13 == 1 ~ 1,
        q8b_99 == 1 ~ NA_real_,
        q8a    == 1 ~ NA_real_,
        TRUE ~ 0
      ),
      cr_prop = case_when(
        q8b_1  == 1 ~ 1,
        q8b_2  == 1 ~ 1,
        q8b_3  == 1 ~ 1,
        q8b_4  == 1 ~ 1,
        q8b_5  == 1 ~ 1,
        q8b_6  == 1 ~ 1,
        q8b_7  == 1 ~ 1,
        q8b_8  == 1 ~ 1,
        q8b_9  == 1 ~ 1,
        q8b_10 == 1 ~ 1,
        q8b_99 == 1 ~ NA_real_,
        q8a    == 1 ~ NA_real_,
        TRUE ~ 0
      ),
      cr_int  = case_when(
        q8b_14 == 1 ~ 1,
        q8b_15 == 1 ~ 1,
        q8b_16 == 1 ~ 1,
        q8b_17 == 1 ~ 1,
        q8b_99 == 1 ~ NA_real_,
        q8a    == 1 ~ NA_real_,
        TRUE ~ 0
      ),
      discrimination1 = case_when(
        q16a == 99 & q16b == 99 & q16c == 99 & q16d == 99 & q16e == 99 ~ NA_real_,
        q16a <= 4  ~ 1,
        q16b <= 4  ~ 1,
        q16c <= 4  ~ 1,
        q16d <= 4  ~ 1,
        q16e <= 4  ~ 1,
        TRUE ~ 0
      )
    )
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Data Bank                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

DataBank <- function(data) {
  
  map_dfr(
    study_variables,
    function(target){
      
      bind_rows(
        
        # Total sample
        data %>%
          select(year, value = all_of(target)) %>%
          group_by(year, value) %>%
          summarise(
            count = n(),
            .groups = "keep"
          ) %>%
          filter(
            !is.na(value) & !(value %in% c(98, 99))
          ) %>%
          mutate(
            variable = target
          ) %>%
          group_by(year, variable) %>%
          mutate(
            total  = sum(count, na.rm = T),
            perc   = count/total,
            sample = "Total"
          ) %>%
          relocate(
            all_of(c("variable", "sample")),
            .after = year
          ),
        
        # Nationality Sample
        data %>%
          select(year, nationality, value = all_of(target)) %>%
          group_by(year, sample = nationality, value) %>%
          summarise(
            count = n(),
            .groups = "keep"
          ) %>%
          filter(
            !is.na(value) & !is.na(sample) & !(value %in% c(98, 99))
          ) %>%
          mutate(
            variable = target
          ) %>%
          group_by(year, sample, variable) %>%
          mutate(
            total  = sum(count, na.rm = T),
            perc   = count/total,
          ) %>%
          relocate(
            all_of(c("variable", "sample")),
            .after = year
          )
      ) %>%
        mutate(
          perc = perc*100
        )
    }
  )
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Labellers                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

labelVars <- function(input){
  
  output <- case_when(
    
    # Trust in Institutions
    input == "q1a"          ~ paste("People living in Qatar"),
    input == "q1b"          ~ paste("Municipal Officers"),
    input == "q1c"          ~ paste("Government Officers"),
    input == "q1d"          ~ paste("Police Officers"),
    input == "q1e"          ~ paste("Prosecutors"),
    input == "q1f"          ~ paste("Public Defense Attorneys"),
    input == "q1g"          ~ paste("Judges and Magistrates"),
    input == "q1h"          ~ paste("Civil Servants"),
    
    # Corruption
    input == "q2a"          ~ paste("Consultative Council"),
    input == "q2b"          ~ paste("Municipal Officers"),
    input == "q2c"          ~ paste("Government Officers"),
    input == "q2d"          ~ paste("Police Officers"),
    input == "q2e"          ~ paste("Prosecutors"),
    input == "q2f"          ~ paste("Public Defense Attorneys"),
    input == "q2g"          ~ paste("Judges and Magistrates"),

    # Fundamental Freedoms
    input == "q46c_G2"      ~ paste("**People** can <br> express opinions<br>against the government"),
    input == "q46f_G2"      ~ paste("**Civil society** <br>organizations can <br> express opinions",
                                    "against<br>the government"),
    input == "q46g_G2"      ~ paste("**Political parties**<br>can express opinions<br>",
                                    "against the<br>government"),
    input == "q46c_G1"      ~ paste("**The media**<br>can express opinions<br>",
                                    "against the<br>government"),
    input == "q46e_G2"      ~ paste("The media<br>can **expose cases<br>of corruption**"),
    input == "q46d_G2"      ~ paste("People can<br>**attend community<br>meetings**"),
    input == "q46f_G1"      ~ paste("People can<br>**join any political<br>organization**"),
    input == "q46a_G2"      ~ paste("People can<br>**organize around an<br>issue or petition**"),
    input == "q46d_G1"      ~ paste("Local government<br>officials **are elected<br>through a clean<br>process**"),
    input == "q46e_G1"      ~ paste("People can<br>**vote freely** without<br>feeling harassed<br>or pressured"),
    input == "q46h_G2"      ~ paste("Religious minorities<br>can **observe their<br>holy days**"),
    
    # Police - Panel A: Trust and Safety
    input == "q1d"         ~ "Trust the police",
    input == "q48a_G2"     ~ "Resolve security problems in  the community",
    input == "q48b_G2"     ~ "Help them feel safe",
    input == "q48c_G2"     ~ "Are available to help when needed",
    input == "q48d_G2"     ~ "Treat all people with respect",
    
    # Police - Panel B: Accountability and Due Process
    input == "q48a_G1"     ~ "Act lawfully",
    input == "q48b_G1"     ~ "Perform effective and lawful investigations",
    input == "q48c_G1"     ~ "Respect the rights of suspects",
    input == "q48d_G1"     ~ "Are held accountable for violating laws",
    
    # Police - Panel C: Discrimination
    input == "q18a" ~ "Socioeconomic status",
    input == "q18b" ~ "Gender",
    input == "q18c" ~ "Ethnicity",
    input == "q18d" ~ "Religion",
    input == "q18e" ~ "Citizenship status",
    input == "q18f" ~ "Sexual orientation",
    
  )
  
  return(output)
}

labelVals <- function(input){
  output <- paste0(format(round(input, 0),
                          nsmall = 0),
                   "%")
  
  return(output)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Data Points Extraction                                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

getDataPoints <- function(pid, figure_map){
  
  # Extracting relevant parameters
  parameters <- lapply(
    c("target_vars"  = "var_id", 
      "reportValues" = "reportValues", 
      "grouping"     = "sample",
      "type"         = "type",
      "label_var"    = "label_at",
      "time_frame"   = "years"),
    function(target){
      unlist(
        str_split(
          figure_map %>%
            filter(panelID %in% pid) %>%
            select(v = all_of(target)) %>%
            pull(v),
          pattern = ", "
        )
      )
    }
  )
  
  # if (parameters[["time_frame"]][1] == "All"){
  #   parameters[["time_frame"]] <- c(2013, 2014, 2016, 2017, 2018, 2021, 2024)
  # }
  
  # Filtering data
  if (parameters["reportValues"] == "All"){
    data2plot <- data_bank %>%
      filter(
        variable %in% parameters[["target_vars"]] & 
          sample %in% parameters[["grouping"]] &
          year   %in% parameters[["time_frame"]]
      )
    
  } else {
    data2plot <- data_bank %>%
      filter(
        variable %in% parameters[["target_vars"]] & 
          value  %in% parameters[["reportValues"]] & 
          sample %in% parameters[["grouping"]] &
          year   %in% parameters[["time_frame"]]
      )
  }
  
  
  if (parameters[["type"]] == "Horizontal bars"){
    data2plot <- data2plot %>%
      mutate(
        label_position = values2plot + 3
      )
  }
  
  # Calling labelers
  if (parameters["label_var"] == "Variables"){
    data2plot <- data2plot %>% 
      mutate(
        labels = labelVars(variable)
      )
    
    # Special labeling for Radars
    if (parameters["type"] == "Radar"){
      data2plot <- data2plot %>%
        mutate(
          democrat_value   = if_else(sample == "Democrats", values2plot, NA_real_),
          republican_value = if_else(sample == "Republicans", values2plot, NA_real_)
        ) %>%
        group_by(variable) %>%
        mutate(
          democrat_value   = first(democrat_value, na_rm = T),
          republican_value = first(republican_value, na_rm = T),
          across(
            ends_with("_value"),
            ~paste0(
              format(
                round(.x, 0),
                nsmall = 0
              ),
              "%"
            )
          ),
          across(labels,
                 ~paste0(
                   "<span style='color:#003b8a;font-size:4.217518mm'>",democrat_value,"</span>",
                   "<span style='color:#524F4C;font-size:4.217518mm'> | </span>",
                   "<span style='color:#fa4d57;font-size:4.217518mm'>",republican_value,"</span><br>",
                   "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",
                   labels,
                   "</span>"
                 )
          )
        ) %>% 
        group_by(year, sample) %>%
        mutate(
          order = row_number()
        )
    }
  }
  
  if (parameters["label_var"] == "Values"){
    data2plot <- data2plot %>%
      mutate(
        labels = labelVals(values2plot)
      )
  }
  
  if (parameters["type"] == "Edgebars"){
    
    if (pid %in% c("Figure_11_A")){
      data2plot <- data2plot %>%
        mutate(
          order = case_when(
            variable == "USA_q1k"     ~ 5,
            variable == "USA_q21g_G2" ~ 4,
            variable == "USA_q21f_G1" ~ 3,
            variable == "USA_q21e_G1" ~ 2,
            variable == "USA_q2h"     ~ 1
          )
        )
    }
  }
  
  return(data2plot)
  
}
