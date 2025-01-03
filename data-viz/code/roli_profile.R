subfactors <- list(
  "F1" = c(         "f_1_2", "f_1_3", "f_1_4", "f_1_5", "f_1_6", "f_1_7"),
  "F2" = c("f_2_1", "f_2_2", "f_2_3", "f_2_4"),
  "F3" = c("f_3_1", "f_3_2", "f_3_3", "f_3_4"),
  "F4" = c("f_4_1", "f_4_2", "f_4_3", "f_4_4", "f_4_5", "f_4_6", "f_4_7", "f_4_8"),
  "F5" = c("f_5_1", "f_5_2", "f_5_3"),
  "F6" = c("f_6_1", "f_6_2", "f_6_3", "f_6_4", "f_6_5"),
  "F7" = c("f_7_1", "f_7_2", "f_7_3", "f_7_4", "f_7_5", "f_7_6", "f_7_7"),
  "F8" = c("f_8_1", "f_8_2", "f_8_3", "f_8_4", "f_8_5", "f_8_6", "f_8_7")
)

itervalues <- list("region"       = "MENA",
                   "income_group" = "H",
                   "country"      = "Qatar")

regional_averages <- imap_dfr(
  itervalues,
  function(target, grouping){
    
    roli_data %>%
      select(
        group = all_of(grouping),
        starts_with("f_")
      ) %>%
      filter(
        group %in% c(target)
      ) %>%
      summarise(
        across(
          starts_with( "f_"),
          \(x) mean(x)
        )
      ) %>%
      mutate(
        group = target
      ) %>%
      relocate(group)
    
  })

country      = "Qatar"
bar_color    = "#2a2a94"
y_lab_pos    = 0
nudge_lab    = 2.5
margin_top   = 20
bar_width    = 0.35

subfactors_hs <- list(
  "F1" = 84.53466,
  "F2" = 56.68977,
  "F3" = 58.68977,
  "F4" = 121.3795,
  "F5" = 44.76733,
  "F6" = 75.61221,
  "F7" = 101.4571,
  "F8" = 101.4571
)

results <- imap(subfactors, 
     function(subf, subf_name){
       
       fmaster_data <- regional_averages %>% 
         select(group, all_of(subf)) %>%
         pivot_longer(
           !group,
           names_to  = "subfactor",
           values_to = "score" 
         ) %>%
         group_by(group) %>%
         mutate(
           order = row_number(),
           label = case_when(
             subfactor == "f_1_2" ~ "1.1 Limits by legislature",
             subfactor == "f_1_3" ~ "1.2 Limits by judiciary",
             subfactor == "f_1_4" ~ "1.3 Independent auditing",
             subfactor == "f_1_5" ~ "1.4 Sanctions for officil misconduct",
             subfactor == "f_1_6" ~ "1.5 Non-govermental checks",
             subfactor == "f_1_7" ~ "1.6 Lawful transition of power",
             
             subfactor == "f_2_1" ~ "2.1 In the executive branch",
             subfactor == "f_2_2" ~ "2.2 In the judiciary",
             subfactor == "f_2_3" ~ "2.3 In the police/military",
             subfactor == "f_2_4" ~ "2.4 In the legislature",
             
             subfactor == "f_3_1" ~ "3.1 Publicized laws and gov't data",
             subfactor == "f_3_2" ~ "3.2 Right to information",
             subfactor == "f_3_3" ~ "3.3 Civi participation",
             subfactor == "f_3_4" ~ "3.4 Complaint mechanisms",
             
             subfactor == "f_4_1" ~ "4.1 No discrimination",
             subfactor == "f_4_2" ~ "4.2 Right to life and security",
             subfactor == "f_4_3" ~ "4.3 Due process of law",
             subfactor == "f_4_4" ~ "4.4 Freedom of expression",
             subfactor == "f_4_5" ~ "4.5 Freedom of religion",
             subfactor == "f_4_6" ~ "4.6 Right to privacy",
             subfactor == "f_4_7" ~ "4.7 Freedom of association",
             subfactor == "f_4_8" ~ "4.8 Labor rights",
             
             subfactor == "f_5_1" ~ "5.1 Absence of crime",
             subfactor == "f_5_2" ~ "5.2 Absence of civil conflict",
             subfactor == "f_5_3" ~ "5.3 Absence of violent redress",
             
             subfactor == "f_6_1" ~ "6.1 Effective regulatory enforcement",
             subfactor == "f_6_2" ~ "6.2 No improper influence",
             subfactor == "f_6_3" ~ "6.3 No unreasonable delay",
             subfactor == "f_6_4" ~ "6.4 Resect for due process",
             subfactor == "f_6_5" ~ "6.5 No expropriation w/out adequate compensation",
             
             subfactor == "f_7_1" ~ "7.1 Accessibility and affordability",
             subfactor == "f_7_2" ~ "7.2 No discrimination",
             subfactor == "f_7_3" ~ "7.3 No corruption",
             subfactor == "f_7_4" ~ "7.4 No improper gov't influence",
             subfactor == "f_7_5" ~ "7.5 No unreasonable delay",
             subfactor == "f_7_6" ~ "7.6 Effective enforcement",
             subfactor == "f_7_7" ~ "7.7 Impartial and efective ADRs",
             
             subfactor == "f_8_1" ~ "8.1 Effective investigations",
             subfactor == "f_8_2" ~ "8.2 Timely an effective adjudication",
             subfactor == "f_8_3" ~ "8.3 Effective correctional system",
             subfactor == "f_8_4" ~ "8.4 No discrimination",
             subfactor == "f_8_5" ~ "8.5 No corruption",
             subfactor == "f_8_6" ~ "8.6 No improper gov't influence",
             subfactor == "f_8_7" ~ "8.7 Due process of law"
           )
         )
       
       data4bars <- fmaster_data %>%
         mutate(
           value = "main",
         ) %>%
         bind_rows(
           fmaster_data %>%
             mutate(
               score = 1 - score,
               value = "fill"
             )
         ) %>% 
         filter(group %in% country) %>%
         mutate(
           value_label = if_else(
             value == "main",
             format(round(score, 2),
                    nsmall = 0),
             ""
           )
         )
       
       cvec <- c("main" = bar_color,
                 "fill" = "#D6D6D6")
       
       plt <- ggplot(
         data = data4bars, 
         aes(
           x    = reorder(subfactor,-order),
           y    = score, 
           fill = value
         )
       ) +
         geom_bar(
           # position = "dodge", 
           stat     = "identity",
           width    = bar_width, 
           position = "stack",
           show.legend = F,
         ) +
         geom_richtext(
           aes(
             x        = reorder(subfactor,-order), 
             y        = y_lab_pos,
             label    = label, 
             family   = "Lato Full", 
             fontface = "plain"
           ),
           fill  = NA, 
           hjust = 0, 
           vjust = 0, 
           size  = 3.514598,
           label.color = NA,
           label.padding = unit(c(0, 0, nudge_lab, 0), "mm"),
         ) +
         geom_text(
           data = data4bars %>% filter(value == "main"),
           aes(
             x = reorder(subfactor,-order),
             y = 1.025,
             label = value_label
           ),
           color    = "#4a4a49",
           position = position_dodge(width = bar_width),
           family   = "Lato Full",
           fontface = "bold", 
           size     = 3.514598, 
           hjust    = 0
         ) +
         # geom_segment(
         #   data = fmaster_data %>% 
         #     filter(group %in% itervalues$region),
         #   aes(
         #     x = reorder(subfactor,-order),
         #     y = score
         #   ),
         #   color = "#3C887E",
         #   linewidth = 2
         # ) +
         scale_fill_manual(
           values = cvec
         ) +
         scale_y_continuous(
           expand = expansion(mult = c(0, 0.1))
         ) +
         coord_flip(clip = "off") +
         WJP_theme() +
         theme(
           panel.grid.major.x = element_blank(),
           panel.grid.major.y = element_blank(),
           axis.text.y        = element_blank(),
           axis.title.x       = element_blank(),
           axis.title.y       = element_blank(),
           axis.text.x        = element_blank(),
           plot.margin        = margin(margin_top, 10, 2, 0),
           plot.background    = element_blank()
         )
       
       pid <- paste0("Figure_1_", subf_name)
       
       saveIT.fn(
         plt,
         fig =  "Figure_1",
         pid = pid,
         w   = 90.67663,
         h   = subfactors_hs[[subf_name]]
       )  
       
       return(plt)
       
     })















