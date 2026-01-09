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


fit_model <- function(formula, dat, model = "ols", is_cluster = F, cluster = "ClusterColName", design = "design") {
  # detect type of model
  if (model %in% c("ols", "linear regression")) {
    mod <- 1
  } else if (model %in% c("logit", "logistic", "logistic regression")) {
    mod <- 2
  } else if (model %in% c("cl", "clogit", "conditional logistic", "conditional logistic regression")) {
    mod <- 3
  } else if (model == "amce") {
    mod <- 4
  } else {
    warning("Model not defined! Fitting OLS!")
    mod <- 1
  }
  
  # initialize output
  res <- list(
    "model" = NA,
    "result" = NA
  )
  
  if (cluster != "ClusterColName") is_cluster <- T
  # model fitting
  if (mod >= 3) {  # clogit or amce
    if (is_cluster) {  # check if the cluster-robust standard error should be controlled
      if (mod == 3) mod.res <- clogit(formula, data = dat, model = T, method = "efron", cluster = subject)
      else mod.res <- amce(formula, data = dat, cluster = T, respondent.id = cluster, design = design)
    } else {  # no clustering
      if (mod == 3) mod.res <- clogit(formula, data = dat)
      else mod.res <- amce(formula, data = dat, cluster = F, design = design)
    }
    res[[1]] <- mod.res
  } else {  # OLS or logit
    if (mod == 1) {
      mod.res <- glm(formula, family = gaussian, data = dat)
    } else if (mod == 2) {
      mod.res <- glm(formula, family = binomial(link = 'logit'), data=dat)
    }
    
    # apply cluster-robust standard error or not
    if (is_cluster) {
      mod.vcovCL <- vcovCL(mod.res, dat[[cluster]])
      mod.res <- coeftest(mod.res, mod.vcovCL)
    }
    res[[1]] <- mod.res
  }
  
  # output the result data frame
  if (mod == 4) {
    res.df <- summary(mod.res)$amce %>% 
      rename_with(~ tolower(.)) %>% 
      rename(
        term = level,
        std.error = "std. err",
        statistic = "z value",
        p.value = "pr(>|z|)"
      ) %>% 
      select(1:6) %>% 
      mutate(attribute = str_replace_all(attribute, "[.]", ""))
  } else {
    res.df <- mod.res %>% 
      tidy() %>% 
      filter(term != "(Intercept)") %>%
      mutate(
        attribute = str_remove_all(term, "\\.[^:]*"),
        term = str_remove_all(term, "(?<=^|:)[^:.]+\\."),
        .before = 1
      )
  }
  res[[2]] <- res.df
  
  return(res)
}


# Plot Model Fitting Result
plot_result <- function(res.df, n.mod) {
  if (n.mod <= 1) {
    base.plot <- ggplot(res.df, aes(x = estimate, y = term))
  } else {
    base.plot <- ggplot(res.df, aes(x = estimate, y = term, color = model))
  }
  base.plot +
    geom_point(
      position = position_dodge(width = 0.7),
      alpha = .7
    ) +
    geom_errorbar(
      aes(
        xmin = CI.lower,
        xmax = CI.upper
      ),
      position = position_dodge(width = 0.7),
      linewidth = 0.5, alpha = .7
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme(strip.text = element_text(face = "bold.italic")) +
    labs(y = "") +
    theme(
      text = element_text(size = 12),
      strip.text = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.text = element_text(size = 12)
    ) +
    theme_light()
}


