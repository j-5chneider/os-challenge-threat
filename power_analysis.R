library(lavaan)
library(tidyverse)


os_sim_model <- "
  appraisal_challe ~ .27*resource_perso + .27*resource_infra
  appraisal_threat ~ -.27*resource_perso + -.27*resource_infra

  engagement ~ .4*appraisal_challe + -.3*appraisal_threat + .3*resource_perso + .3*resource_infra"


os_model <- "
  appraisal_challe ~ lamb1*resource_perso + lamb2*resource_infra
  appraisal_threat ~ lamb3*resource_perso + lamb4*resource_infra
 
  engagement ~ lamb5*appraisal_challe + lamb6*appraisal_threat + lamb7*resource_perso + lamb8*resource_infra

  appraisal_challe ~~ cov1*appraisal_challe
  appraisal_threat ~~ cov2*appraisal_threat
"

### SET FEATURES OF POWER ANALYSIS #############################################
# number of simulations
n_sim <- 1000

# lowest sample size
n_min <- 90

# highest sample size
n_max <- 130
  
# steps to be taken between sample sizes
steps <- 5



### EMPTY DATA FRAME FOR RESULTS ###############################################
# make empty data frame to save results for all sample sizes
sim_results <- data.frame(n = as.numeric(),
                          p_lamb1_pow = as.numeric(),
                          p_lamb2_pow = as.numeric(),
                          p_lamb3_pow = as.numeric(),
                          p_lamb4_pow = as.numeric(),
                          p_lamb5_pow = as.numeric(),
                          p_lamb6_pow = as.numeric(),
                          p_var_ch_pow = as.numeric(),
                          p_var_th_pow = as.numeric())

### LOOP OVER DIFFERENT SAMPLE SIZES ###########################################
for (n in seq(from = n_min, to = n_max, by = steps)) {
  # make empty data frame to save results within one sample size
  sim_results_n <- data.frame(p_lamb1 = as.numeric(),
                              p_lamb2 = as.numeric(),
                              p_lamb3 = as.numeric(),
                              p_lamb4 = as.numeric(),
                              p_lamb5 = as.numeric(),
                              p_lamb6 = as.numeric(),
                              p_var_ch = as.numeric(),
                              p_var_th = as.numeric())

  ### LOOP OVER n_sim SIMULATIONS ##############################################
  for (i in 1:n_sim) {
    set.seed(123 + i + n)
    sim_data <- lavaan::simulateData(os_sim_model,
                                     sample.nobs = n)  
    fit <- lavaan::sem(os_model, sim_data)
    fit_stand <- summary(fit, standardized = TRUE)
    
    sim_results_n <- sim_results_n |>
      dplyr::add_row(p_lamb1 = fit_stand$pe$pvalue[fit_stand$pe$label == "lamb1"],
                     p_lamb2 = fit_stand$pe$pvalue[fit_stand$pe$label == "lamb2"],
                     p_lamb3 = fit_stand$pe$pvalue[fit_stand$pe$label == "lamb3"],
                     p_lamb4 = fit_stand$pe$pvalue[fit_stand$pe$label == "lamb4"],
                     p_lamb5 = fit_stand$pe$pvalue[fit_stand$pe$label == "lamb5"],
                     p_lamb6 = fit_stand$pe$pvalue[fit_stand$pe$label == "lamb6"],
                     p_var_ch = fit_stand$pe$pvalue[fit_stand$pe$label == "cov1"],
                     p_var_th = fit_stand$pe$pvalue[fit_stand$pe$label == "cov2"])
  }

sim_results_n <- sim_results_n |>
  dplyr::mutate(p_lamb1_sig = ifelse(p_lamb1 < .05, 1, 0),
                p_lamb2_sig = ifelse(p_lamb2 < .05, 1, 0),
                p_lamb3_sig = ifelse(p_lamb3 < .05, 1, 0),
                p_lamb4_sig = ifelse(p_lamb4 < .05, 1, 0),
                p_lamb5_sig = ifelse(p_lamb5 < .05, 1, 0),
                p_lamb6_sig = ifelse(p_lamb6 < .05, 1, 0),
                p_var_ch_sig = ifelse(p_var_ch < .05, 1, 0),
                p_var_th_sig = ifelse(p_var_th < .05, 1, 0)) |>
  dplyr::summarize(across(p_lamb1_sig:p_var_th_sig, mean))

sim_results <- sim_results |>
  dplyr::add_row(n = n,
                 p_lamb1_pow = sim_results_n$p_lamb1_sig,
                 p_lamb2_pow = sim_results_n$p_lamb2_sig,
                 p_lamb3_pow = sim_results_n$p_lamb3_sig,
                 p_lamb4_pow = sim_results_n$p_lamb4_sig,
                 p_lamb5_pow = sim_results_n$p_lamb5_sig,
                 p_lamb6_pow = sim_results_n$p_lamb6_sig,
                 p_var_ch_pow = sim_results_n$p_var_ch_sig,
                 p_var_th_pow = sim_results_n$p_var_th_sig)
}

sim_results_long <- sim_results |>
  pivot_longer(p_lamb1_pow:p_var_th_pow, names_to = "variables", values_to = "values") |>
  dplyr::mutate(parameter = str_sub(variables, 1, -5),
                variables = "power") |>
  pivot_wider(id_cols = c(n, parameter), names_from = "variables", values_from = "values")


ggplot(sim_results_long, aes(x=n, y=power, color=parameter)) +
  geom_hline(yintercept = .8, color = "darkgrey", linetype = 'dashed', linewidth = 2, alpha = .5) +
  geom_line(linewidth = 1, alpha = .6) +
  # scale_y_continuous(limits = c(0,1)) +
  theme_minimal()


# library(semPlot)
# 
# layout_m <- matrix(c("resource_perso",   NA,  "resource_infra",
#                      "appraisal_challe", NA, "appraisal_threat",
#                      NA,  "engagement",   NA), byrow = TRUE, 3, 3)
# 
# semPaths(fit, 
#          whatLabels = "est",
#          sizeMan = 10,
#          edge.label.cex = 1.15,
#          style = "ram",
#          nCharNodes = 0, 
#          nCharEdges = 0,
#          layout = layout_m)


