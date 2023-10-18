generate_lmer_table <- function(data, formula_str, output_label = "lmer_table", caption_label = "Linear mixed effects model") {
  
  # Ensure required packages are loaded
  # You can comment the library lines if needed
  library(lme4)
  library(effectsize)
  library(kableExtra)
  library(webshot)
  
  # Parse formula from string
  formula_obj <- as.formula(formula_str)
  
  # Fit a linear mixed-effects model
  fit <- lmer(formula_obj, data = data)
  
  # Calculate effect sizes for the model - partial omega squared
  effect_size <- effectsize(fit)
  
  # Obtain ANOVA table for the model
  lmer_summary <- Anova(fit, type = 3, test = "F")
  
  # Create a vector to store custom term names
  term_names <- character()
  
  # Get terms from the model and ask the user for custom labels
  model_terms <- rownames(lmer_summary)
  for (term in model_terms) {
    cat(sprintf("Current term: %s\n", term))
    term_names[term] <- readline(prompt = "Enter custom label or press enter to keep current name: ")
    if (term_names[term] == "") {
      term_names[term] <- term
    }
  }
  
  # Extract model parameters
  fixef_table <- fixef(fit)
  se_table <- sqrt(diag(vcov(fit)))
  f_table <- lmer_summary[,"F"]
  df_table <- lmer_summary[,"Df"]
  df_res_table <- lmer_summary[,"Df.res"]
  pval_table <- lmer_summary[,"Pr(>F)"]
  
  # Extract effect sizes and associated confidence intervals
  es <- effect_size$Std_Coefficient[1:nrow(effect_size)]
  ci_low <- effect_size$CI_low[1:nrow(effect_size)]
  ci_high <- effect_size$CI_high[1:nrow(effect_size)]
  
  # Create a significance notation based on p-value thresholds
  p_value_signif <- ifelse(pval_table < 0.001, "***",
                           ifelse(pval_table < 0.01, "**",
                                  ifelse(pval_table < 0.05, "*",
                                         ifelse(pval_table < 0.06, ".", ""))))

  # Create a summary table
  lm_table <- data.frame(Coefficients = format(fixef_table, scientific = TRUE, digits = 3),
                         EffectSize = ifelse(es < -0.010, sprintf("%.3f", es),
                                             ifelse(es < 0.010, "<0.010", sprintf("%.3f", es))),
                         EffectSizeCI = sprintf("[%s, %s]", format(ci_low, digits = 2), format(ci_high, digits = 2)),
                         FValue = format(f_table, digits = 3),
                         DF = sprintf("(%s, %s)", format(df_table, digits = 1), format(df_res_table, digits = 1)),
                         pValue = ifelse(pval_table < 0.0001, "<0.0001", sprintf("%.3f", pval_table)),
                         Significance = p_value_signif) %>%
    rownames_to_column(var = "Term") %>%
    mutate(Term = term_names[Term])
  
  # Export the table for a scientific publication
  html_file_name <- paste0(output_label, ".html")
  png_file_name <- paste0(output_label, ".png")
  
  kable(lm_table, align = c("l", "c", "c", "c", "c", "c", "c", "l"), 
        caption = caption_label) %>%
    kable_classic(full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    column_spec(2:3, width = "2cm") %>%
    column_spec(4, width = "3cm") %>%
    column_spec(5, width = "1.3cm") %>%
    column_spec(6, width = "1.3cm") %>%
    column_spec(7, width = "2cm") %>%
    row_spec(0, bold = TRUE) %>%
    save_kable(file = html_file_name)
  
  webshot(url = html_file_name, file = png_file_name, selector = "table", zoom = 2)
  
  cat("HTML and PNG generated successfully!\n")
  
  return(png_file_name)
}

# Usage
# generate_lmer_table(data = d, formula_str = "y ~ x*z + (1|id)", output_label = "lmer_table", caption_label = "Linear mixed effects model")
