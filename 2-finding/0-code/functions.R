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
    guides(color = guide_legend(reverse = TRUE)) +
    theme(
      text = element_text(size = 12),
      strip.text = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.text = element_text(size = 12)
    ) +
    theme_light()
}


compute_immigrant_AME <- function(mod, file, is_cluster = T) {
  # `mod` is the target model
  # `file` is the path to the AME object
  # `is_cluster` indicates should the AME clustered by subject
  
  # initialization
  if (is_cluster) cluster <- vcovCL(mod, cluster = immigrant$subject)
  else cluster <- T
  # start
  if (file.exists(file)) {
    cat("File Found! \nReading file... \n\n")
    res <- read_rds(file)
  } else {
    cat("File not found! \nComputing AME... \n\n")
    # prepare for filtering
    high_edu <- paste0("Education.", 5:7)
    high_prof <- unique(immigrant$Profession.)[8:11]
    danger_ctry <- unique(immigrant$Origin.)[c(1, 8:10)]
    # w/o clustering
    AME.indep <- avg_comparisons(
      model = mod, vcov = cluster,
      variables = c("Gender.", "JobExperience.", "JobPlans.", "PriorTripstoUS.", "Language.")
    )
    AME.prof_full <- avg_comparisons(  # AME of profession of all education
      model = mod, vcov = cluster, variables = "Profession."
    )
    AME.prof_sub <- avg_comparisons(  # AME of profession w/ only high education
      model = mod, vcov = cluster, variables = "Profession.",
      newdata = subset(immigrant, Education. %in% high_edu)
    )
    AME.prof <- bind_rows(
      AME.prof_full %>% filter(!contrast %in% paste(high_prof, "- Janitor")),
      AME.prof_sub %>% filter(contrast %in% paste(high_prof, "- Janitor"))
    )
    AME.educ <- avg_comparisons(  # AME of education state
      model = mod, vcov = cluster, variables = "Education.",
      newdata = subset(immigrant, !Profession. %in% high_prof)  # compute w/o special professions
    )
    AME.orig <- avg_comparisons(  # AME of origin
      model = mod, vcov = cluster, variables = "Origin.",
      newdata = subset(immigrant, ApplicationReason. != "ApplicationReason.3")  # compute w/o escape persecution
    )
    AME.appl_full <- avg_comparisons(  # AME of application reason for all countrys
      model = mod, vcov = cluster, variables = "ApplicationReason."
    )
    AME.appl_sub <- avg_comparisons(  # AME of application reason w/ only from dangerous country
      model = mod, vcov = cluster, variables = "ApplicationReason.",
      newdata = subset(immigrant, Origin. %in% danger_ctry)
    )
    AME.appl <- bind_rows(
      AME.appl_full %>% filter(contrast == "ApplicationReason.2 - ApplicationReason.1"),
      AME.appl_sub %>% filter(contrast == "ApplicationReason.3 - ApplicationReason.1")
    )
    
    # combine AMEs
    AME <- bind_rows(
      AME.indep, 
      AME.prof,
      AME.educ,
      AME.orig,
      AME.appl
    )
    write_rds(AME, file)
  }
}


compute_immigrant_AME.cl <- function(mod, dat, file, V, VCL) {
  # `mod` is the target model
  # `file` is the path to the AME object
  # `V` is the covariance matrix
  f <- formula(mod)
  f.clean <- update(f, . ~ . - strata(trial_id))
  beta.all <- coef(mod)
  X.comp.all <- model.matrix(f.clean, data = dat[dat$alt_id, ])
  beta.clean <- beta.all
  beta.clean[is.na(beta.clean)] <- 0
  est.id <- !is.na(beta.all)
  
  # Define functions for this function
  get_avg_pred <- function(beta_vec, target_var, target_level, subset_id = NULL) {
  # compute average prediction for delta method
    b.full <- beta.all
    b.full[est.id] <- beta_vec
    b.full[is.na(b.full)] <- 0
    
    # counter-factual data
    df_cf <- dat
    df_cf[[target_var]] <- factor(target_level, levels = levels(df[[target_var]]))
    X.target <- model.matrix(f.clean, data = df_cf)[, names(b.full)]
    X.comp <- X.comp.all[, names(b.full)]
    
    V.target <- X.target %*% b.full
    V.comp <- X.comp %*% b.full
    probs <- 1 / (1 + exp(V.comp - V.target))
    
    if (!is.null(subset_id)) {
      return(mean(probs[subset_id], na.rm = TRUE))
    } else {
      return(mean(probs, na.rm = TRUE))
    }
  }
  
  calc_AME_se <- function(variable, lvl, subset_dat = NULL) {
    s_id <- if(!is.null(subset_dat)) which(dat$id %in% subset_dat$id) else NULL
    
    lvl_ref <- levels(dat[[variable]])[1]
    lvl_target <- lvl
    cat("  Computing [", lvl_target, "] vs [", lvl_ref, "]\n", sep="")
    
    # 1. Estimate AME
    b.est <- beta.all[est.id]
    mu_ref <- get_avg_pred(b.est, variable, lvl_ref, s_id)
    mu_target <- get_avg_pred(b.est, variable, lvl_target, s_id)
    ame <- mu_target - mu_ref
    
    # 2. Compute gradient
    grad <- jacobian(
      function(b) {
        m_t <- get_avg_pred(b, variable, lvl_target, s_id)
        m_r <- get_avg_pred(b, variable, lvl_ref, s_id)
        return(m_t - m_r)
      },
      x = b.est, method = "simple"
    )
    
    # 3. Delta Method : SE = sqrt( grad %*% V %*% t(grad) )
    # make sure there is no NAs in matrice
    V.sub <- V[est.id, est.id]
    VCL.sub <- VCL[est.id, est.id]
    # grad[is.na(grad)] <- 0
    se <- sqrt(as.numeric(grad %*% V.sub %*% t(grad)))
    robust_se <- sqrt(as.numeric(grad %*% VCL.sub %*% t(grad)))
    
    # 4. output
    return(data.frame(
      attribute = variable,
      term = lvl_target,
      estimate = ame,
      std.error = se,
      robust.se = robust_se
    ))
  }
  
  # start
  if (file.exists(file)) {
    cat("File Found! \nReading file... \n\n")
    res <- read_rds(file)
  } else {
    cat("File not found! \nComputing AME... \n\n")
    # initialization
    AME <- data.frame()
    
    # prepare for filtering
    high_edu <- paste0("Education.", 5:7)
    high_prof <- unique(dat$Profession.)[8:11]
    low_prof <- setdiff(levels(dat$Profession.), high_prof)[-1]
    danger_ctry <- unique(dat$Origin.)[c(1, 8:10)]
    
    # calculate AME of all the contrasts
    # independent variables
    for (var in c("Gender.", "JobExperience.", "JobPlans.", "PriorTripstoUS.", "Language.")) {
      cat("Variable:", var, "\n")
      for (lvl in levels(dat[[var]])[-1]) {
        AME <- AME %>%
          bind_rows(calc_AME_se(var, lvl))
      }
    }
    # unrestricted professions
    cat("Variable: Profession.\n")
    for (lvl in low_prof) {
    AME <- AME %>%
    bind_rows(calc_AME_se("Profession.", lvl))
    }
    # restricted professions
    for (lvl in high_prof) {
      sub <- dat %>% filter(Education. %in% high_edu)
      AME <- AME %>%
        bind_rows(calc_AME_se("Profession.", lvl, sub))
    }
    # education
    cat("Variable: Education.\n")
    for (lvl in levels(dat$Education.)[-1]) {
      sub <- dat %>% filter(!Profession. %in% high_prof)
      AME <- AME %>%  # compute w/o special professions
        bind_rows(calc_AME_se("Education.", lvl, sub))
    }
    # origin
    cat("Variable: Origin.\n")
    for (lvl in levels(dat$Origin.)[-1]) {
      sub <- dat %>% filter(ApplicationReason. != "ApplicationReason.3")
      AME <- AME %>%  # compute w/o escaping persecution
        bind_rows(calc_AME_se("Origin.", lvl, sub))
    }
    # unrestricted application reason
    cat("Variable: ApplicationReason.\n")
    AME <- AME %>%  # AME of application reason for all countrys
      bind_rows(calc_AME_se("ApplicationReason.", "ApplicationReason.2"))
    # restricted application reason
    sub <- dat %>% filter(Origin. %in% danger_ctry)
    AME <- AME %>%  # AME of application reason w/ only from dangerous country
      bind_rows(calc_AME_se("ApplicationReason.", "ApplicationReason.3", sub))
    
    write_rds(AME, file)
    return(AME)
  }
}
