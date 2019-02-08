# 

library(data.table)
library(highcharter)
rm(list=ls())

convertFD <- function(leagueFD, year, league.name){
  # paste together link
  fdweb <- "http://www.football-data.co.uk/mmz4281/"
  fileName <- paste0(fdweb, year, "/", leagueFD, ".csv")
  data <- fread(fileName)
  data$Date <- as.Date(data$Date, format = "%d/%m/%y")
  print(head(data$Date))
  # add league and year and Month
  data$League <- league.name
  data$Year <- year
  data$Month <- month(data$Date)
  # take only what we need
  data <- data[, .(Year, League, Date,Month, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTHG, HTAG, HTR)]
  return(data)
}

#z <- convertFD('D1', '1718', 'Germany1')

leagues.name <- c('Germany1',	'Germany2',	'England Prem',	'England Champ',	'England League1',	'England League2',	'France1',	'Italy1',	'Italy2',	
                  'Netherlands1',	'Portugal1',	'Scotland Prem',	'Spain1',	'Spain2',	'Turkey1')
# which leagues do we want
leaguesFD <- c('D1',	'D2',	'E0',	'E1',	'E2',	'E3',	'F1',	'I1',	'I2',	
             'N1',	'P1',	'SC0',	'SP1',	'SP2',	'T1')
# which year do we want
years <- c("1516", "1617", "1718", "1819")
#league<- c('D1')

# initialise empty list to store dts
datalist = list()

# loop through year
for (year in years) {
  print(year)
  
  # loop through leagues
  for (i in 1:length(leagues.name)) {
    league <- leagues.name[i]
    print(league)
    leagueFD <- leaguesFD[i]
    print(leagueFD)
    # get next dt
    dt <- convertFD(leagueFD, year, league)
    # store dt in datalist
    dt.id <- paste0(year, i)
    datalist[[dt.id]] <- dt 
    
  }
}

dt <- data.table::rbindlist(datalist)

dt <- na.omit(dt)

dt$goals1 <- dt$HTHG + dt$HTAG
dt$goals2 <- (dt$FTHG + dt$FTAG) - (dt$HTHG + dt$HTAG)
dt$TG <- dt$goals1 +dt$goals2
dt

write.csv(dt, 'data_3.5_seasons.csv')


# overall 
sum(dt$goals1) / (sum(dt$goals1) + sum(dt$goals2))

dt[, .(.N, half1 = sum(goals1) / (sum(goals1) + sum(goals2)), 
       half2 = sum(goals2) / (sum(goals1) + sum(goals2)), 
       ave_TG = mean(TG), 
       sd_TG = sd(TG)), by = c('League')]

# add All dt
dt[, .(.N, half1 = sum(goals1) / (sum(goals1) + sum(goals2)), 
       half2 = sum(goals2) / (sum(goals1) + sum(goals2)), 
       ave_TG = mean(TG), 
       sd_TG = sd(TG))]


half1 <- dt[, .(.N, percent = sum(goals1) / (sum(goals1) + sum(goals2))), by = c('Year')]
half1$half <- 1
half2 <- dt[, .(.N, percent = sum(goals2) / (sum(goals1) + sum(goals2))), by = c('Year')]
half2$half <- 2
all <- rbind(half1, half2)
all$percent <- round(all$percent, 3)
prem <- all[League=='England Prem',]
prem


########### to graph

get_league_split <- function(league, dt){
  # get 1st half
  half1 <- dt[, .(.N, percent = sum(goals1) / (sum(goals1) + sum(goals2))), by = c('Year', 'League')]
  half1$half <- 1
  # get 2nd half
  half2 <- dt[, .(.N, percent = sum(goals2) / (sum(goals1) + sum(goals2))), by = c('Year', 'League')]
  half2$half <- 2
  # bind
  all <- rbind(half1, half2)
  all$percent <- round(all$percent, 3)
  result <- all[League==league,]
  return(result)
  
}
unique(dt$League)

z <- get_league_split('Spain1', dt)
z$Year <- as.factor(z$Year)

hchart(z, 
       "column", 
       hcaes(x = Year,
             y = percent, 
             group = half))







      








