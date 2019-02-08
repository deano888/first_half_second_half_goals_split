# Bootstrap 95% CI for R-Squared
library(boot)

# function to obtain R-Squared from the data 
calc.1st.half.perc <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample 
  # print(d)
  result <- sum(d$goals1) / sum(d$TG)
  return(result)
} 
# bootstrapping with 1000 replications 
results <- boot(data=ger2, statistic=calc.1st.half.perc, 
                R=10000)
mean.boot <- mean(results$t)

# view results
results 
plot(results)

# get 95% confidence interval 
conf.ints <- boot.ci(results, type="bca")

conf.ints$t0
lower.confint <- conf.ints$bca[4]
upper.conf.int <- conf.ints$bca[5]

country <- 'Germany 2'

boot.summary <- data.table(country, mean.boot, lower.confint, upper.conf.int)
boot.summary
