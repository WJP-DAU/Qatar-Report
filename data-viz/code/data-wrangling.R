## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Qatar Report - Data Wrangling
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     December 11th, 2024
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
        q8b_99 == 1 ~ 0,
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
        q8b_99 == 1 ~ 0,
        q8a    == 1 ~ NA_real_,
        TRUE ~ 0
      ),
      cr_int  = case_when(
        q8b_14 == 1 ~ 1,
        q8b_15 == 1 ~ 1,
        q8b_16 == 1 ~ 1,
        q8b_17 == 1 ~ 1,
        q8b_99 == 1 ~ 0,
        q8a    == 1 ~ NA_real_,
        TRUE ~ 0
      ),
      discrimination1 = case_when(
        # q16a == 99 & q16b == 99 & q16c == 99 & q16d == 99 & q16e == 99 ~ NA_real_,
        q16a <= 4  ~ 1,
        q16b <= 4  ~ 1,
        q16c <= 4  ~ 1,
        q16d <= 4  ~ 1,
        q16e <= 4  ~ 1,
        TRUE ~ 0
      ),
      
      EXP_q22e_G1_inv = case_when(
        EXP_q22e_G1 == 1  ~ 4,
        EXP_q22e_G1 == 2  ~ 3,
        EXP_q22e_G1 == 3  ~ 2,
        EXP_q22e_G1 == 4  ~ 1,
        EXP_q22e_G1 == 99 ~ 99
      ),
      EXP_q22k_G2_inv = case_when(
        EXP_q22k_G2 == 1  ~ 4,
        EXP_q22k_G2 == 2  ~ 3,
        EXP_q22k_G2 == 3  ~ 2,
        EXP_q22k_G2 == 4  ~ 1,
        EXP_q22k_G2 == 99 ~ 99
      ),
      EXP_q22j_G2_inv = case_when(
        EXP_q22j_G2 == 1  ~ 4,
        EXP_q22j_G2 == 2  ~ 3,
        EXP_q22j_G2 == 3  ~ 2,
        EXP_q22j_G2 == 4  ~ 1,
        EXP_q22j_G2 == 99 ~ 99
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
            !is.na(value) #& !(value %in% c(98, 99))
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
        
        # Gender Sample
        data %>%
          select(year, gender, value = all_of(target)) %>%
          group_by(year, sample = gender, value) %>%
          summarise(
            count = n(),
            .groups = "keep"
          ) %>%
          filter(
            !is.na(value) & !is.na(sample) #& !(value %in% c(98, 99))
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
    input == "q1a" ~ paste("People living in Qatar     "),
    input == "q1b" ~ paste("Municipal officers"),
    input == "q1c" ~ paste("Government officers      "),
    input == "q1d" ~ paste("Police officers"),
    input == "q1e" ~ paste("Prosecutors"),
    input == "q1f" ~ paste("Public defense attorneys"),
    input == "q1g" ~ paste("Judges and magistrates"),
    input == "q1h" ~ paste("Civil servants"),
    
    # Corruption
    input == "q2a" ~ paste("Consultative council     "),
    input == "q2b" ~ paste("Municipal officers"),
    input == "q2c" ~ paste("Government officers      "),
    input == "q2d" ~ paste("Police officers"),
    input == "q2e" ~ paste("Prosecutors"),
    input == "q2f" ~ paste("Public defense attorneys"),
    input == "q2g" ~ paste("Judges and magistrates  "),
    
    # Bribery Victimization
    input == "q4a" ~ paste("Request a government permit or document"),
    input == "q4b" ~ paste("Request public benefits or assistance"),
    input == "q4c" ~ paste("Obtain a birth certificate or ID"),
    input == "q4d" ~ paste("Secure a place at a public school"),
    input == "q4e" ~ paste("Use a public health service"),
    
    # Criminal Justice System
    input == "q49a"        ~ paste("Is **effective** in bringing<br>people who commit<br>crimes to justice."),
    input == "q49b_G2"     ~ paste("Ensures **equal treatment<br>of victims** by allowing all<br>victims to seek justice<br>regardless of who they are."),
    input == "q49e_G2"     ~ paste("Safeguards the<br>**presumption of<br>innocence** by treating<br>those accused of<br>crimes as innocent<br>until proven guilty."),
    input == "q49c_G2"     ~ paste("Ensures **the equal treatment of<br>the accused** by giving all a<br>fair trial regardless of who<br>they are."),
    input == "q49e_G1"     ~ paste("Gives **appropriate<br>punishments** that fit<br>the crime."),
    input == "EXP_q23d_G1" ~ paste("Ensures **uniform quality** by<br>providing equal service<brregardless of where<br>they live."),
    input == "q49c_G1"     ~ paste("Ensures everyone<br>has **access** to the<br>justice system."),
    input == "q49b_G1"     ~ paste("Ensures **timeliness**<br>by dealing with<br>cases prompty and<br>efficiently."),
    
    # Criminal Justice Actors
    input == "q48f_G2" ~ "Prosecutors",
    input == "q48g_G2" ~ "Judges and magistrates",
    input == "q48h_G1" ~ "Public defense attorneys",
    
    # Police
    input == "q48c_G2"     ~ "Are available to help when needed",
    input == "EXP_q22i_G2" ~ "Serve the interest of the community",
    input == "EXP_q22h_G2" ~ "Serve the interest of regular citizens",
    input == "q48a_G2"     ~ "Resolve security problems in  the community",
    input == "q48b_G2"     ~ "Help them feel safe",
    
    input == "q48a_G1"     ~ "Act lawfully",
    input == "q48b_G1"     ~ "Perform effective and lawful investigations",
    input == "EXP_q22e_G1_inv" ~ "Do not use excessive force",
    input == "q48c_G1"     ~ "Respect the rights of suspects",
    input == "q48d_G2"     ~ "Treat all people with respect",
    
    input == "q48d_G1"     ~ "Are held accountable for violating laws",
    input == "EXP_q22f_G1" ~ "Are held accountable for seeking bribes",
    input == "EXP_q22h_G1" ~ "Are investigated for misconduct",
    input == "EXP_q22k_G2_inv" ~ "Do not serve the interests of gangs",
    input == "EXP_q22j_G2_inv" ~ "Do not serve the interests of politicians",

    input == "q18a" ~ "Socioeconomic status",
    input == "q18b" ~ "Gender",
    input == "q18c" ~ "Ethnicity",
    input == "q18d" ~ "Religion",
    input == "q18e" ~ "Citizenship status",
    input == "q18f" ~ "Sexual orientation",
    
    # Victim Support
    input == "EXP_q24c_G1" ~ "Receive effective and<br>timely **medical and<br>psychological care**.",
    input == "EXP_q24d_G1" ~ "Receive **information<br>and legal advice**<br>when going to the<br>authorities.",
    input == "EXP_q24d_G2" ~ "Receive adequate<br>care and protection<br>as **victims of<br>domestic violence**.",
    input == "EXP_q24a_G1" ~ "Receive **prompt and<br>courteous attention**<br>when they report a<br>crime.",
    input == "EXP_q24b_G1" ~ "Are **believed** when<br>they report a crime.",
    input == "EXP_q23f_G1" ~ "Are **guaranteed<br>their rights** in<br>criminal justice<br>proceedings.",
    input == "EXP_q24c_G2" ~ "Receive adequate<br>care and protection<br>as **victims of sexual<br>crimes**.",
    input == "EXP_q24b_G2" ~ "Receive protection<br>during criminal<br>proceedings to<br>**prevent repeat<br>victimization**.",
    input == "EXP_q24f_G2" ~ "Receive a **clear<br>explanation** of<br>the process when<br>reporting  a crime to<br>the police.",
    input == "EXP_q24a_G2" ~ "Receive **protection**<br>from the police if<br>their safety is in<br>danger.",
    input == "EXP_q24g_G2" ~ "Are addressed by<br>the police using<br>**accessible language**.",
    
    # Fundamental Freedoms
    input == "q46c_G2"     ~ paste("**People** can<br>express opinions<br>against the government."),
    input == "q46f_G2"     ~ paste("**Civil society**<br>organizations can<br>express opinions",
                                   "against<br>the government."),
    input == "q46g_G2"     ~ paste("**Political parties**<br>can express opinions<br>",
                                   "against the<br>government."),
    input == "q46c_G1"     ~ paste("**The media**<br>can express opinions<br>",
                                   "against the<br>government."),
    input == "q46e_G2"     ~ paste("The media<br>can **expose cases<br>of corruption**."),
    input == "q46d_G2"     ~ paste("People can<br>**attend community<br>meetings**."),
    input == "q46f_G1"     ~ paste("People can<br>**join any political<br>organization**."),
    input == "q46a_G2"     ~ paste("People can<br>**organize around an<br>issue or petition**."),
    input == "q46d_G1"     ~ paste("Local government<br>officials **are elected<br>through a clean<br>process**."),
    input == "q46e_G1"     ~ paste("People can<br>**vote freely** without<br>feeling harassed<br>or pressured."),
    input == "q46h_G2"     ~ paste("Religious minorities<br>can **observe their<br>holy days**."),
    input == "q46b_G2"     ~ paste("Workers can freely<br>bargain for their<br>**labor rights**."),
    
    # Crime
    input == "cr_corr" ~ "Corruption, financial,\nand commercial crimes",
    input == "cr_prop" ~ "Property crimes",
    input == "cr_int"  ~ "Crimes against life and integrity\nof individuals",
    
    # Discrimination reasons
    input == "q17_1"  ~ "Ancestry or national\norigin",
    input == "q17_2"  ~ "Gender",
    input == "q17_3"  ~ "Race",
    input == "q17_4"  ~ "Age",
    input == "q17_5"  ~ "Religion",
    input == "q17_6"  ~ "Height",
    input == "q17_7"  ~ "Weight",
    input == "q17_8"  ~ "Physical\nappearence",
    input == "q17_9"  ~ "Physical or mental\ndisability",
    input == "q17_10" ~ "Sexual orientation",
    input == "q17_11" ~ "Education or\nincome level",
    input == "q17_12" ~ "Nationality or inmigration\nstatus",
    input == "q17_13" ~ "Shade of\nskin color",
    input == "q17_14" ~ "Tribe",
    input == "q17_15" ~ "Clothing or\nhairstyle",  
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
      "time_frame"   = "years",
      "unique_id"    = "unique_id"),
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
  
  if (!parameters[["type"]] %in% c("Diverging bars", "Stacked bars", "Gauge")){
    data2plot <- data2plot %>%
      group_by(year, variable, sample) %>%
      summarise(
        count = sum(count, na.rm = T),
        total = mean(total, na.rm = T),
        perc  = sum(perc, na.rm = T),
        .groups = "keep"
      )
  }
  
  # Special wrangling per chart type
  if (parameters[["type"]] %in% c("Diverging bars", "Edgebars", "Rose", "Stacked bars")){
    data2plot <- data2plot %>%
      mutate(
        value_labs = if_else(
          abs(perc) <= 5, "", paste0(round(abs(perc),0), "%")
        )
      )
  }
  if (parameters[["type"]] %in% c("Lollipops")){
    data2plot <- data2plot %>%
      mutate(
        order_no = row_number()
      )
  }
  if (parameters[["type"]] == "Diverging bars"){
    data2plot <- data2plot %>%
      mutate(
        value = case_when(
          value == 0 ~ "No",
          value == 1 ~ "Yes"
        ),
        direction = case_when(
          value == "No"  ~ "Positive",
          value == "Yes" ~ "Negative"
        ),
        perc = if_else(
          direction == "Negative",
          perc*-1,
          perc
        )
      )
  }
  if (parameters[["type"]] == "Horizontal bars"){
    data2plot <- data2plot %>%
      mutate(
        label_pos = perc + 5,
        color_category = sample,
        value_labs = paste0(round(abs(perc),0), "%")
      )
  }
  if (parameters[["type"]] == "Rose"){
    data2plot <- data2plot %>%
      arrange(desc(perc)) %>%
      ungroup() %>%
      mutate(
        order_no = row_number()
      )
  }
  if (parameters[["type"]] == "Stacked bars"){
    if (parameters[["unique_id"]] %in% c("trt1","trt2","trt3")){
      data2plot <- data2plot %>%
        mutate(
          value = case_when(
            value == 1  ~ "A lot",
            value == 2  ~ "Some",
            value == 3  ~ "Little",
            value == 4  ~ "No trust",
            value == 99 ~ "No answer"
          ),
          value = factor(
            value,
            levels = rev(c("A lot", "Some", "Little", "No trust", "No answer"))
          ),
          label_pos = cumsum(perc)-(perc/2)
        )
    }
    if (parameters[["unique_id"]] %in% c("cor1","cor2")){
      data2plot <- data2plot %>%
        mutate(
          value = case_when(
            value == 4  ~ "All of them",
            value == 3  ~ "Most of them",
            value == 2  ~ "Some of them",
            value == 1  ~ "None of them",
            value == 99 ~ "No answer"
          ),
          value = factor(
            value,
            levels = c("No answer", "All of them", "Most of them", "Some of them", "None of them")
          ),
          label_pos = cumsum(perc)-(perc/2)
        )
    }
    if (parameters[["unique_id"]] %in% c("pol1","pol2", "pol3")){
      data2plot <- data2plot %>%
        mutate(
          value = case_when(
            value == 1  ~ "Very well",
            value == 2  ~ "Fairly well",
            value == 3  ~ "Fairly badly",
            value == 4  ~ "Very badly",
            value == 99 ~ "No answer"
          ),
          value = factor(
            value,
            levels = rev(c("Very well", "Fairly well", "Fairly badly", "Very badly", "No answer"))
          ),
          label_pos = cumsum(perc)-(perc/2)
        )
    }
  }
  if (parameters[["type"]] == "Gauge"){
    data2plot <- data2plot %>%
      mutate(
        value = case_when(
          value == 1  ~ "Very confident",
          value == 2  ~ "Fairly confident",
          value == 3  ~ "Not very confident",
          value == 4  ~ "Not at all",
          value == 99 ~ "No answer"
        ),
        value = factor(
          value,
          levels = c("Very confident", "Fairly confident", "Not very confident", "Not at all", "No answer")
        ),
        label_pos = cumsum(perc)-(perc/2)
      )
  }
  
  # Calling labellers
  if (parameters["label_var"] == "Variables"){
    data2plot <- data2plot %>% 
      mutate(
        labels = labelVars(variable)
      )
    
    # Special labeling for Radars
    if (parameters["type"] == "Radar"){
      data2plot <- data2plot %>%
        # mutate(
        #   male_value   = if_else(sample == "Male", perc, NA_real_),
        #   female_value = if_else(sample == "Female", perc, NA_real_)
        # ) %>%
        group_by(variable) %>%
        mutate(
          # male_value   = first(male_value, na_rm = T),
          # female_value = first(female_value, na_rm = T),
          # across(
          #   ends_with("_value"),
          #   ~paste0(
          #     format(
          #       round(.x, 0),
          #       nsmall = 0
          #     ),
          #     "%"
          #   )
          # ),
          value = paste0(
            format(
              round(perc, 0),
              nsmall = 0
            ),
            "%"
          ),
          across(labels,
                 ~paste0(
                   # "<span style='color:#49178e;font-size:4.217518mm'>",male_value,"</span>",
                   # "<span style='color:#524F4C;font-size:4.217518mm'> | </span>",
                   # "<span style='color:#dd58b1;font-size:4.217518mm'>",female_value,"</span><br>",
                   # "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",
                   "<span style='color:#2a2a94;font-size:4.217518mm'>",value,"</span><br>",
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
        labels = labelVals(abs(perc))
      )
  }
  if (parameters["type"] == "Gauge"){
    data2plot <- data2plot %>%
      mutate(
        labels = if_else(
          perc < 5, "", labels
        )
      )
  }
  
  # Additional modifications
  if (parameters[["unique_id"]] %in% c("cja1","cja2","cja3")){
    data2plot <- data2plot %>%
      mutate(
        label_pos = perc + 3,
        color_category = labels
      )
  }
  
  return(data2plot)
  
}
