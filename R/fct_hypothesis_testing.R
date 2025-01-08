## # CHAT GPT CONVERSION OF THE MATLAB SCRIPT FROM:

## # This R script is associated with the following manuscript:

## # Barrick, S.K., S.R. Clippinger, L. Greenberg, M.J. Greenberg.

## # 2019. Computational tool to study perturbations in muscle regulation and its application to heart disease.


## # This is a script for hypothesis testing. The input is two matrices, each

## # of which contain the best-fit parameters from the real data (best_fit_params)

## # and from the accompanying bootstrapping simulations (bootstrap_params) for

## # one of the two datasets to be compared (e.g., WT and mutant).


## library(ggplot2)

## library(dplyr)


## # Change the names on the right side to the names of the variables in the workspace.

## A_best <- best_fit_params_A

## A_boot <- bootstrap_params_A

## B_best <- best_fit_params_B

## B_boot <- bootstrap_params_B


## tag <- 'KW_WTvsdE160'  # When saving the data, this will be the name of the output.

## ind <- 1  # This is the index of the parameter to be compared.


## alpha <- 0.05  # This is the threshold value for significance.

## null <- 0  # This is the value of the null hypothesis.


## tail <- 2  # This defines whether the p-value is for a 1- or 2-tailed test.

## # Use 2 by default.


## A <- A_boot[, ind]  # Select the parameters to be examined.

## B <- B_boot[, ind]


## A <- A[!is.na(A)]  # Remove blanks.

## B <- B[!is.na(B)]


## # Extract the true values of A and B and calculate the test statistic.

## out_data <- c(A_best[ind], B_best[ind])  # Values obtained from raw data.

## out_stat_data <- A_best[ind] - B_best[ind]  # Test statistic value.


## # Calculate the cumulative distributions and plot them.

## cumulative_A <- sort(A)

## cumulative_B <- sort(B)

## A_index <- seq_along(cumulative_A) / length(cumulative_A)

## B_index <- seq_along(cumulative_B) / length(cumulative_B)


## ggplot() +

##   geom_line(aes(x = cumulative_A, y = A_index), color = "black") +

##   geom_line(aes(x = cumulative_B, y = B_index), color = "red") +

##   ggtitle('Cumulative Distribution') +

##   theme_minimal()

## ggsave(paste0(tag, '_cumulative.pdf'))  # Save cumulative distributions.


## # Calculate the test statistic for each bootstrapping simulation.

## out_sim <- cbind(A, B)

## out_stat_sim <- out_sim[,1] - out_sim[,2]


## if (median(out_stat_sim) <= 0) {

##   out_stat_sim <- -out_stat_sim

##   out_stat_data <- -out_stat_data

## }


## # Calculate the confidence intervals for both the data and the test statistic.

## CI_stat <- quantile(out_stat_sim, probs = c(alpha / tail, 1 - alpha / tail))

## CI_data <- apply(out_sim, 2, quantile, probs = c(alpha / tail, 1 - alpha / tail))


## # Plot the bootstrapped test statistic.

## ggplot() +

##   geom_histogram(aes(x = out_stat_sim, y = ..density..), bins = 30) +

##   geom_vline(aes(xintercept = out_stat_data), color = "yellow", linetype = "dashed", size = 1) +

##   geom_vline(aes(xintercept = CI_stat[1]), color = "red", linetype = "dashed", size = 1) +

##   geom_vline(aes(xintercept = CI_stat[2]), color = "red", linetype = "dashed", size = 1) +

##   geom_vline(aes(xintercept = null), color = "blue", linetype = "dashed", size = 1) +

##   xlab('Difference between means') +

##   ggtitle('Bootstrapped Test Statistic')

## ggsave(paste0(tag, '_hist.pdf'))  # Save histogram.


## # Get the p-value from the cumulative distribution of the test statistic.

## out_cumulative_mean_ind <- seq_along(out_stat_sim) / length(out_stat_sim)

## cumulative_mean <- sort(out_stat_sim)


## ggplot() +

##   geom_line(aes(x = cumulative_mean, y = out_cumulative_mean_ind), color = "black") +

##   geom_vline(aes(xintercept = out_stat_data), color = "yellow", linetype = "dashed", size = 1) +

##   geom_vline(aes(xintercept = CI_stat[1]), color = "red", linetype = "dashed", size = 1) +

##   geom_vline(aes(xintercept = CI_stat[2]), color = "red", linetype = "dashed", size = 1) +

##   geom_vline(aes(xintercept = null), color = "blue", linetype = "dashed", size = 1) +

##   xlab('Difference between means') +

##   ggtitle('Cumulative Distribution of Test Statistic')

## ggsave(paste0(tag, '_pval.pdf'))  # Save cumulative distributions.


## # Calculate the p-value.

## p_ind <- which(cumulative_mean >= 0)[1]

## p_value <- out_cumulative_mean_ind[p_ind] * tail


## # Display a table with the values.

## output <- data.frame(

##   true_values = out_data,

##   lower_CI = out_data - CI_data[1,],

##   upper_CI = CI_data[2,] - out_data

## )

## print(output)


## # Print p_value

## print(paste("P-value:", p_value))
