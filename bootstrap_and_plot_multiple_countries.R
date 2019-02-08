# https://www.statmethods.net/advstats/bootstrapping.html

# bootstrap for multiple countries
library(data.table)
library(infer)
library(dplyr)

dt <- fread('data_3.5_seasons.csv')

# function to obtain 1st half % from the data 
calc.1st.half.perc <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample 
  # print(d)
  result <- sum(d$goals1) / sum(d$TG)
  return(result)
} 

countries <- unique(dt$League)
# where to store list of dts
data.list <- list()

for (i in 1:length(countries)) {
  next_country <- countries[i]
  #print(next_country)
  
  next.dt <- dt[League==next_country,]
  # bootstrapping with 10000 replications 
  boot.results <- boot(data=next.dt, statistic=calc.1st.half.perc, 
                  R=10000)
  # extract stats
  mean.boot <- mean(boot.results$t)
  # get 95% confidence interval 
  conf.ints <- boot.ci(boot.results, type="bca")
  lower.confint <- conf.ints$bca[4]
  upper.conf.int <- conf.ints$bca[5]
  
  boot.summary <- data.table(country = next_country, mean.boot, lower.confint, upper.conf.int)
  print(boot.summary)
  data.list[[i]] <- boot.summary
  
}


z <- data.table::rbindlist(data.list)

z <- z[order(mean.boot)]

z
setnames(z, c('League', 'First.half.Goals.Percent', 'lower.confint', 'upper.conf.int'))

write.csv(z, 'bootstrap_conf_ints.csv', row.names = F)

# plot confidence intervals

# https://www.quora.com/How-do-you-graph-a-confidence-interval-in-ggplot2-R
library(ggplot2)
library(plyr)

# re order 
x <- z$League
z$League <- factor(z$League, levels = x)

actual.mean.goals <- 0.434	

### 
ggplot(z, aes(y = League, x = First.half.Goals.Percent, color = League)) +
  geom_point(size = 2)  +
  geom_vline(xintercept = actual.mean.goals, linetype = 3, color = "black") + 
  geom_errorbarh(aes(xmin = lower.confint, xmax = upper.conf.int),
                 height = 0, lwd = 0) +
  geom_errorbarh(aes(xmin = lower.confint, 
                     xmax = upper.conf.int), height = 0, lwd = 1) +
  ggtitle(expression(atop("First Half Goal Percent By League With Confidence Levels 95%", atop(italic("Mean All Games = 0.434"), "")))) +
  theme(plot.title = element_text(hjust=0.5))
xlab("1st Half Goals %") 
# geom_point(size = 3)  
#geom_pointrange()



