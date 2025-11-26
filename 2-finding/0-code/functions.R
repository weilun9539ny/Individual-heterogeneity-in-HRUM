# Tidy up results after unnesting conjoint models
results_tidy <- function(results) {
  results %>% 
    filter(term != "(Intercept)") %>% 
    
    mutate(attribute = substr(term, 1, regexpr("([A-Z]|[0-9])", term) - 1),
           attribute = str_to_sentence(attribute),
           attribute = ifelse(str_starts(term,"age_category"),"Age Category",attribute)) %>% 
    
    mutate(term = tidy_coefs(term))
}


# Tidy coefficients in tables and figures
tidy_coefs <- function(col) {
  
  tidy_col <- col %>% 
    gsub("vulnerability","",.) %>% 
    gsub("transmission","",.) %>% 
    gsub("occupation","",.) %>% 
    sub("income","",.) %>% 
    gsub("age_category","",.) %>% 
    gsub("(Twice the average risk of catching and transmitting the COVID-19 virus)",
         "of transmission",
         .) %>% 
    gsub("(Twice the average risk of COVID-19 death)",
         "risk of death",
         .) %>% 
    gsub("(Five times the average risk of catching and transmitting the COVID-19 virus)",
         "of transmission",
         .) %>% 
    gsub("(Five times the average risk of COVID-19 death)",
         "risk of death",
         .) %>% 
    gsub("Average risk of catching and transmitting the COVID-19 virus",
         "Average risk (of transmission)",
         .) %>% 
    gsub("Average risk of COVID-19 death",
         "Average (risk of death)",
         .) %>%  
    
    factor(.,levels = rev(c("(Intercept)",
                            "Average (risk of death)",
                            "Moderate (risk of death)",
                            "High (risk of death)",
                            "Average risk (of transmission)",
                            "Moderate risk (of transmission)",
                            "High risk (of transmission)",
                            "Lowest 20% income level",
                            "Average income level",
                            "Highest 20% income level",
                            "Not working",
                            "Non-Key worker: Can work at home",
                            "Non-Key worker: Cannot work at home",
                            "Key worker: Education and childcare",
                            "Key worker: Factory worker",
                            "Key worker: Water and electricity service",
                            "Key worker: Police and fire-fighting",
                            "Key worker: Health and social care",
                            "25 years old",
                            "40 years old",
                            "65 years old",
                            "79 years old",
                            "countryAustralia",
                            "countryBrazil",                                                                                    
                            "countryCanada",                                                                                      
                            "countryChile",                                                                                      
                            "countryChina",                                                                                       
                            "countryColombia",                                                                                    
                            "countryFrance",                                                                                      
                            "countryItaly",                                                                                       
                            "countrySpain",                                                                                       
                            "countryUK",                                                                                          
                            "countryUS")))
  
  return(tidy_col)
  
}


# Plot Model Fitting Result
plot_result <- function(res.df) {
  ggplot(res.df, aes(x = estimate, y = term)) +
    geom_point(
      position = position_dodge(width = 0.7),
      alpha = .7
    ) +
    geom_errorbarh(
      aes(
        xmin = estimate - 1.96*std.error,
        xmax = estimate + 1.96*std.error
      ),
      position = position_dodge(width = 0.7),
      linewidth = 0.5, alpha = .7
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme(strip.text = element_text(face = "bold.italic")) +
    # facet_grid(attribute ~ ., space = "free", scales = "free_y",) +
    labs(
      x = "Effect Estimation", y = "", color = ""
    ) +
    scale_y_discrete(limits = rev(levels("term"))) +
    theme(
      legend.position = "none",
      text = element_text(size = 12),
      strip.text = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.text = element_text(size = 12)
    ) +
    theme_light()
}


