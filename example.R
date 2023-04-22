# load data, function
d = read.csv("simulated_data_example.csv")
source("lm-table-maker-function.R")

# center-code vars
d$iv_1_C = as.numeric(varRecode(d$iv_1,c("a","b"),c("-.5",".5")))
d$iv_2_C = as.numeric(varRecode(d$iv_2,c("y","z"),c("-.5",".5")))
d$iv_3_C = d$iv_3 - mean(d$iv_3)

# linear model structure: interaction between iv_1 and iv_2 with iv_3 as a covariate
fit = lm(dv ~ iv_1_C * iv_2_C + iv_3_C, data = d)
summary(fit)


# Define the term names
# Note that you need to define the term names as a named character vector with the same names as the coefficients in the model. 
# TERMS MUST BE LABELED IN THE CORRECT ORDER - will read out in same order as summary(fit)
term_names <- c("(Intercept)" = "Intercept",
                "iv_1_C" = "IV 1",
                "iv_2_C" = "IV 2",
                "iv_3_C" = "Covariate",
                "iv_1_C * iv_2_C" = "IV 1 x IV 2")

caption <- "Linear regression predicting DV from IV 1 x IV 2 with IV 3 as a covariate"

# Call the function
create_lm_table(lm_model = fit, term_names = term_names, caption = caption, file_name = "lm_table.html")


















