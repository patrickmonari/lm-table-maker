library(tidyverse)
library(knitr)
library(kableExtra)

# Function to create a table of coefficients, standard errors, p-values, and confidence intervals
create_lm_table <- function(lm_model, term_names, caption, file_name) {
  
  # Extract model summary table
  lm_summary <- summary(lm_model)
  
  # Extract coefficients and standard errors
  coef_table <- coef(lm_model)
  se_table <- sqrt(diag(vcov(lm_model)))
  pval_table <- lm_summary$coefficients[, "Pr(>|t|)"]
  ci_table <- confint(lm_model)
  
  # Create significance stars column
  p_value_signif <- ifelse(pval_table < 0.001, "***",
                           ifelse(pval_table < 0.01, "**",
                                  ifelse(pval_table < 0.05, "*", "")))
  
  # Rename the terms
  term_names <- setNames(term_names, names(coef_table))
  
  # Combine coefficients, standard errors, and p-values into a table with renamed terms
  lm_table <- data.frame(Coefficients = format(coef_table,scientific = TRUE, digits = 3),
                         "Std.Error" = format(se_table, scientific = TRUE, digits = 3),
                         "CI" = paste0("[", format(ci_table[,1], scientific = TRUE, digits = 3), ", ", format(ci_table[,2], scientific = TRUE, digits = 3), "]"),
                         "p-value" = ifelse(pval_table < 0.0001, "<0.0001", sprintf("%.4f", pval_table)),
                         "Significance" = p_value_signif) %>%
    rownames_to_column(var = "Term") %>%
    mutate(Term = term_names[Term])
  
  # Export the table for a scientific publication
  kable(lm_table, align = c("l", "c", "c", "c", "c"), caption = caption) %>%
    kable_classic(full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    column_spec(2:3, width = "2cm") %>%
    column_spec(4, width = "3.8cm") %>%
    column_spec(5, width = "2cm") %>%
    row_spec(0, bold = TRUE) %>%
    save_kable(file = file_name)
}
