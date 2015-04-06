library(Jmisc)
library(plyr)

#load the datasets:
hitting <- read.csv("hitting.csv", header=TRUE)
stadiums <- read.csv("stadiums_v3.csv")

#change the Factor variables to string to make them easier to
#work with:
stadiums$Team = as.character(stadiums$Team)
stadiums$Stadium = as.character(stadiums$Stadium)
stadiums$City = as.character(stadiums$City)

#convert instances of NA in the data to 0. This makes sense, as if a player
#has NA hits or stolen bases, that means they have 0 hits or stolen bases
hits = as.character(hitting$HR)
hits[hits == "\\N"] = 0
hits = as.numeric(hits)

hitting$HR = hits

stolen = as.character(hitting$SB)
stolen[stolen == "\\N"] = 0
stolen = as.numeric(stolen)

hitting$SB = stolen

#calculate our performance metric:
hitting$pmetric = (hitting$HR + hitting$SB)

#partition into 16 datasets, 1 for each year from 1995-2010:
ddat <- as.list(rep("", 16))
for (i in 1995:2010 ) {
  ddat[[i-1994]]  = data.frame(hitting[hitting$yearID==i,])
}

#now filter these datasets to contain only the top 10% of players
#based on our metric:
for (i in 1:16) {
  ddat[[i]]  = ddat[[i]][ddat[[i]]$pmetric >=quantile(ddat[[i]]$pmetric, 0.90),]
  ddat[[i]]$teamID = as.character(ddat[[i]]$teamID)
}

#for each year 1995-2010, sum up how many "star players" each team had:
teams <- as.list(rep("", 16))
for (i in 1:16 ) {
  x =table(ddat[[i]]$teamID)
  teams[[i]]  = data.frame(names(x))
  teams[[i]]$starplayers=x
  teams[[i]] = addCol(teams[[i]], i+1994)
}

#combine the 16 datasets above into a single dataframe:
dfinal <- ldply(teams, data.frame)

#perform some minor formating to the dataframe:
#############################
x = dfinal[3]
y = x[,1]
y = as.numeric(y)
dfinal$year = y
dfinal = dfinal[,-c(3)]
#############################

#normalize CAL, ANA, and LAA to be the same team:
dfinal$names.x.[dfinal$names.x. %in% c('CAL','ANA','LAA')] <- 'ANA'
#normalize ML4 and MIL to be the same team:
dfinal$names.x.[dfinal$names.x. == 'ML4'] <-'MIL'

#minor formatting and renaming column headers:
dfinal$names.x. = as.character(dfinal$names.x.)
colnames(dfinal) = c("Team", "star_players", "yearID")

#normalize CAL, ANA, and LAA to be the same team in the stadiums dataset:
stadiums$Team[stadiums$Team %in% c('CAL','ANA','LAA')] <- 'ANA'
#normalize ML4 and MIL to be the same team in the stadiums dataset:
stadiums$Team[stadiums$Team == 'ML4'] <-'MIL'

#perform an outer join of the stadiums dataset and our dataset of how many "star players"
#each team had for every year:
test = merge( stadiums,dfinal, all.x=TRUE, by=c("Team","yearID"))

#the outer join generates some NAs in the instance that a team had no "star players"
#for a particular year, we set these to 0:
test$star_players[is.na(test$star_players)] = 0

attach(test)

#this is the model we get from running regression of the full dataset, which
#contains every team:
mod = lm(Avg.Attendance~star_players + City.Population + Capacity+yearID)
summary(mod)


##############
#Isolated Regressions
##############

teamIDs = as.data.frame(table(test$Team))$Var1

#define a blank dataframe:
result <- data.frame(TeamID= character(0), 
                      Adj_R2= numeric(0), 
                      p_value = numeric(0), 
                      sig = integer(0), stringsAsFactors=FALSE)

#define a function to check whether a p value is significant:
check_p <- function(p){
  if(p<=0.05)
  return(1)
  else
    return(0)
}

#for each team, perform an isolated regression using only that team's data points:
teams <- as.list(rep("", 31))
for (i in 1:31 ) {
teams[[i]] = test[Team == teamIDs[i],]
mod = lm(teams[[i]]$Avg.Attendance~teams[[i]]$star_players + 
           teams[[i]]$City.Population + teams[[i]]$Capacity + teams[[i]]$yearID)

#extract the adjusted R^2 and p-value of the coefficient of "Star Players":
adj_r2 = summary(mod)$adj.r.squared
p_value = summary(mod)$coefficients[,4][2]

#append the row containing the adjusted R^2 and p value to the dataframe:
result[i,] = c(as.character(teamIDs[i]), adj_r2, p_value, check_p(p_value))
}
