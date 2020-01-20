year <- 2017
path_offense <- paste("~/Documents/Work/New_College/Research/GLM_for_Improved_Ranking_and_Predictive_Features_CFB/Game_Logs/",year, "/Offense/", sep="")
path_defense <- paste("~/Documents/Work/New_College/Research/GLM_for_Improved_Ranking_and_Predictive_Features_CFB/Game_Logs/",year, "/Defense/", sep="")
Data <- read.csv(paste(path_offense,"Texas-San Antonio.csv", sep=""))
head(Data)

stats <- colnames(Data)[c(9:11,13:15,17:18,25:27)]
stats

FBS_Team_names <- sapply(list.files(path=path_offense), function(x) substring(x, 1,nchar(x)-4))
names(FBS_Team_names) <- NULL


n.st <- 1
stat <- stats[n.st]

## First, try using ALL GAMES. 
## Later we'll go with "up until week ..".
full.df <- NULL

library(stringr)
n.teams <- length(FBS_Team_names) + 1

## There's "Central Florida" in "Opponent" column, instead of "UCF"...
#  and a bunch of other examples...
for (team in FBS_Team_names){
  Data_offense <- read.csv(paste(path_offense, team, ".csv", sep=""))
  Data_defense <- read.csv(paste(path_defense, team, ".csv", sep=""))
  full.df <- rbind(full.df,cbind(team, Data_offense[,c("Opponent", "X.1", stat)]))
  if (nrow(subset(Data_defense, !Opponent %in% FBS_Team_names)) != 0){
      full.df <- rbind(full.df,cbind(team="Non-Major", 
                                     subset(Data_defense, !Opponent %in% FBS_Team_names)[,c("Opponent", "X.1", stat)]))
  }
  
}
length(sort(unique(full.df$Opponent)))



full.df$Opponent <- sapply(full.df$Opponent, function(x) str_remove(x, "\\*"))
full.df$Opponent <- ifelse(full.df$Opponent %in% FBS_Team_names, full.df$Opponent, "Non-Major")

length(unique(full.df$Opponent))

## "NON-MAJOR" MESSES IT UP... there's NO "NON-MAJOR" in "team" variable...
# Gotta get the DEFENSE side for NON-MAJOR TEAMS??

cbind(sort(unique(full.df$Opponent)),
      sort(unique(as.character(full.df$team))))

options(contrasts = rep("contr.sum", 2))
contr.sum(n.teams)
contr.sum(n.teams)


lm.obj <- lm(full.df[,stat] ~ team + Opponent,
             data=full.df)
lm.obj


## Adjusted averages
offensive.worth <- c(coef(lm.obj)[1] + coef(lm.obj)[2:n.teams],
                     coef(lm.obj)[1] - sum(coef(lm.obj)[2:n.teams]))
defensive.worth <- c(coef(lm.obj)[1] + coef(lm.obj)[(n.teams+1):(2*n.teams-1)],
                     coef(lm.obj)[1] - sum(coef(lm.obj)[(n.teams+1):(2*n.teams-1)]))

dim(full.df)

library(plotrix)
dim(model.matrix(lm.obj))
color2D.matplot(model.matrix(lm.obj))
