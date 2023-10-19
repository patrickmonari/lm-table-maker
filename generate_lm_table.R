# Ensure required packages are loaded
library(car)
library(effectsize)
library(kableExtra)
library(webshot)

generate_lm_table <- function(data, formula_str, output_label, caption_label) {
  
  # Silly workaround to make sure formula_str updates in the global environment
  formula_str <<- formula_str
  
  # Parse formula from string
  formula_obj <<- as.formula(formula_str)
  formula_obj <- as.formula(formula_str)
  
  # Fit a linear model
  fit <<- lm(formula_obj, data = data)
  fit <- lm(formula_obj, data = data)

  # Calculate effect sizes for the model - partial eta squared
  effect_size <<- effectsize(fit, data = data)
  effect_size <- effectsize(fit, data = data)
  
  # Obtain summary for the model
  lm_summary <- Anova(fit, type = 3, test = "F")
  
  # Create a vector to store custom term names
  term_names <- character()
  
  # Get terms from the model and ask the user for custom labels
  common_terms <- intersect(names(fit$coefficients), rownames(lm_summary))
  for (term in common_terms) {
    cat(sprintf("Current term: %s\n", term))
    term_names[term] <- readline(prompt = "Enter custom label or press enter to keep current name: ")
    if (term_names[term] == "") {
      term_names[term] <- term
    }
  }
  
  # Remove residuals from lm_summary
  lm_summary <- lm_summary[!(rownames(lm_summary) %in% "Residuals"), ]
  
  # Extract model parameters
  coefficients <- coef(fit)
  se_table <- sqrt(diag(vcov(fit)))
  f_table <- lm_summary[,"F value"]
  pval_table <- lm_summary[,"Pr(>F)"]
  
  # Calculate degrees of freedom
  n <- nrow(data)
  k <- length(coef(fit))
  df1 <- 1
  df2 <- n - k
  
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
  lm_table <- data.frame(Coefficients = format(coefficients, scientific = TRUE, digits = 3),
                         EffectSize = ifelse(es < -0.010, sprintf("%.3f", es),
                                             ifelse(es < 0.010, "<0.010", sprintf("%.3f", es))),
                         EffectSizeCI = sprintf("[%s, %s]", format(ci_low, digits = 2), format(ci_high, digits = 2)),
                         FValue = format(f_table, digits = 3),
                         DF = sprintf("(%d, %d)", df1, df2),
                         pValue = ifelse(pval_table < 0.0001, "<0.0001", sprintf("%.3f", pval_table)),
                         Significance = p_value_signif) %>%
    rownames_to_column(var = "Term") %>%
    mutate(Term = term_names[Term])
  
  # Export the table for a scientific publication
  html_file_name <- paste0(output_label, ".html")
  png_file_name <- paste0(output_label, ".png")
  
  kable(lm_table, align = c("l", "c", "c", "c", "c", "l"), 
        caption = caption_label) %>%
    kable_classic(full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    column_spec(2:5, width = "3cm") %>%
    column_spec(6, width = "2cm") %>%
    row_spec(0, bold = TRUE) %>%
    save_kable(file = html_file_name)
  
  webshot(url = html_file_name, file = png_file_name, selector = "table", zoom = 2)
  
  cat("HTML and PNG generated successfully!\n")
  
  return(png_file_name)
}

# Usage (ensure you have a dataset 'd' and the necessary packages installed)
# generate_lm_table(data = d, formula_str = "y ~ x*z", output_label = "lm_table", caption_label = "Linear model")
