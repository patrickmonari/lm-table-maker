generate_lm_table <- function(data, dependent_var, independent_vars){
  # Fit lm model
  fit <- lm(paste0(dependent_var, " ~ ", paste0(independent_vars, collapse = " * ")), data = data)
  
  # Extract model summary table
  lm_summary <- summary(fit)
  
  # Extract coefficients and standard errors
  coef_table <- coef(fit)
  se_table <- sqrt(diag(vcov(fit)))
  pval_table <- summary(fit)$coefficients[, "Pr(>|t|)"]
  ci_table <- confint(fit)
  
  # Create significance stars column
  p_value_signif <- ifelse(pval_table < 0.001, "***",
                           ifelse(pval_table < 0.01, "**",
                                  ifelse(pval_table < 0.05, "*", "")))
  
  # Rename the terms
  term_names <- c("(Intercept)" = "Intercept")
  for (i in 1:length(independent_vars)){
    term_names[paste0(independent_vars[i], "_C")] <- independent_vars[i]
  }
  for (i in 1:length(independent_vars)){
    for (j in i:length(independent_vars)){
      term_names[paste0(independent_vars[i], "_C:", independent_vars[j], "_C")] <- paste0(independent_vars[i], " * ", independent_vars[j])
    }
  }
  
  # Combine coefficients, standard errors, and p-values into a table with renamed terms
  lm_table <- data.frame(Coefficients = format(coef_table,scientific = TRUE, digits = 3),
                         "Std.Error" = format(se_table, scientific = TRUE, digits = 3),
                         "CI" = paste0("[", format(ci_table[,1], scientific = TRUE, digits = 3), ", ", format(ci_table[,2], scientific = TRUE, digits = 3), "]"),
                         "p-value" = ifelse(pval_table < 0.0001, "<0.0001", sprintf("%.4f", pval_table)),
                         "Significance" = p_value_signif) %>%
    rownames_to_column(var = "Term") %>%
    mutate(Term = term_names[Term])
  
  # Export the table for a scientific publication
  kable(lm_table, align = c("l", "c", "c", "c", "c"), caption = paste0("Linear regression model predicting ", dependent_var)) %>%
    kable_classic(full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    column_spec(2:3, width = "2cm") %>%
    column_spec(4, width = "3.8cm") %>%
    column_spec(5, width = "2cm") %>%
    row_spec(0, bold = TRUE) %>%
    save_kable(file = paste0("lm_table_", dependent_var, ".html"))
}