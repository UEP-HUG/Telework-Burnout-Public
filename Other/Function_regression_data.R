# Function to generate the regression model data for the forestplots
# For logistic regression, outcomes should be either factors or binary
UEP_regression_data = function(data,outcomes, outcomes_clean_name = NULL, outcomes_family, main_variable, main_variable_clean_name = NULL, covariates = NULL, estimate_rounding = 2){
  require(tidyverse)
  require(broom.helpers)
  # If a clean name is not provided, then default to the outcome name
  if(is.null(outcomes_clean_name)){
    outcomes_clean_name = outcomes
  }
  # If a clean name is not provided, then default to the variable name
  if(is.null(main_variable_clean_name)){
    main_variable_clean_name = main_variable
  }
  # If no covariates are specified, only univariable is calculated, otherwise both are
  if(!is.null(covariates)){
    covariates <- list(NULL, covariates)
  }
  # Initialize an empty dataframe that will be filled out
  a <- tibble()
  
  # Start the triple loop
  for (i in 1:length(outcomes)) {
    for (j in 1:length(main_variable)) {
      for (k in 1:length(covariates)) {
        # Create model
        ## vector of main_variable and covariate column names
        r_model <- c(main_variable[j], covariates[[k]]) %>%
          str_c(collapse = "+") %>% # combine variables separated by a plus
          ## combine the variables with outcome in formula style
          str_c(outcomes[i]," ~ ", .) %>%
          glm(family = outcomes_family[i], data = dat)
        
        # Get number of observations
        N_obs <- broom.helpers::model_get_n(r_model) %>% 
          mutate(term = str_remove(term,paste0(main_variable[j]))) %>% 
          rename(model_levels = term) %>%
          mutate(model_levels = case_when(model_levels == "(Intercept)" ~ main_variable_clean_name[j], 
                                          .default = model_levels))
        
        # Get exponentiated coefficients depending on family
        if(outcomes_family[i] == "binomial"){
          model_data <- cbind(
            estimate = exp(coef(r_model)),
            exp(confint(r_model)),
            pvalue = coef(summary(r_model))[,'Pr(>|z|)']) %>%
            as_tibble(rownames = "model_levels")} else {
            model_data <- cbind(
              estimate = coef(r_model),
              confint(r_model),
              pvalue = coef(summary(r_model))[,'Pr(>|t|)']
            )%>% as_tibble(rownames = "model_levels")}
        
        # Add empty n_event column for UV estimates (see add_column below)
        cols <- c(n_event = NA)
        if(outcomes_family[i] == "gaussian"){
          model_data <- model_data %>%
            add_column(!!!cols[!names(cols) %in% names(.)])
        }
        
        # Clean the output
        model_data_clean <- model_data %>%
          mutate(variable = main_variable_clean_name[j], .before = 1) %>%
          # Keep only main_variable estimates
          filter(str_detect(model_levels, paste0(main_variable[j]))|str_detect(model_levels,"(Intercept)")) %>%
          rename(conf.low = `2.5 %`, conf.high = `97.5 %`) %>%
          add_row(variable = main_variable_clean_name[j],
                  model_levels = main_variable_clean_name[j],
                  .before = 1) %>% 
          mutate(
            label = if_else(model_levels == main_variable_clean_name[j], TRUE, FALSE),
            family = if_else(outcomes_family[i] == "gaussian", "Beta", "OR"),
            model = case_when(is.null(covariates[[k]])~"Univariable", .default = "Multivariable"),
            outcome = outcomes_clean_name[i],
            reference = if_else(model_levels == "(Intercept)", TRUE, FALSE),
            model_levels = c(main_variable_clean_name[j], r_model$xlevels[[1]]),
            estimate = case_when(family == "Beta" & reference ~ 0,
                                 family == "OR" & reference ~ 1,
                                 .default = round(estimate,estimate_rounding)),
            conf.low = case_when(family == "Beta" & reference ~ 0,
                                 family == "OR" & reference ~ 1,
                                 .default = round(conf.low,estimate_rounding)),
            conf.high = case_when(family == "Beta" & reference ~ 0,
                                  family == "OR" & reference ~ 1,
                                  .default = round(conf.high,estimate_rounding)),
            full_estimate = case_when(label ~ NA,
                                      reference ~ paste0("(ref)") ,
                                      .default = paste0(
                                        format(estimate, nsmall = estimate_rounding), 
                                        " (", 
                                        format(conf.low, nsmall = estimate_rounding), 
                                        ", ", 
                                        format(conf.high, nsmall = estimate_rounding), 
                                        ")")),
            pvalue = if_else(reference, NA, pvalue),
            p.value = case_when(label | reference ~ NA,
                                pvalue<0.001 ~ "<0.001", .default = format(round(pvalue, 3), nsmall = 3)),
          ) %>% 
          left_join(N_obs) %>% relocate(n_event, .after = last_col())
        # iteratively bind each model_data output
        a <- rbind(a,model_data_clean)
      }
    }
  }
  # Print out the final dataframe
  a
}
