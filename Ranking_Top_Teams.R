library(tidyverse)
library(gridExtra)

year <- 2017
week <- 20
side <- "Offense"                         # Offense/Defense
GLM_type <- "Poisson"                     # Gaussian/Poisson
stat <- "Fum"
  
common_path <- paste("~/Documents/Work/New_College/Research/GLM_for_Improved_Ranking_and_Predictive_Features_CFB/Rankings/",
                     year, "/Week=",
                     week,"/",
                     side,"/",
                     sep="")
classic.rank.table <- read.csv(paste(common_path,"Classic/", stat, ".csv", sep=""))
glm_adj.rank.table <- read.csv(paste(common_path, "GLM_Adj/",GLM_type, "/",stat,".csv", sep=""))
glm_adj_w_hf.rank.table <- read.csv(paste(common_path, "GLM_Adj_w_HomeField/",GLM_type, "/",stat,".csv", sep=""))


## Classic vs GLM_Adj
grid.arrange(
  tableGrob(classic.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% head(10)),
  tableGrob(glm_adj.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% head(10)),
  nrow=1)

## GLM_Adj vs GLM_Adj_w_Homefield
grid.arrange(
  tableGrob(glm_adj.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% head(10)),
  tableGrob(glm_adj_w_hf.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% head(10)),
  nrow=1)


## ISSUE: after adjusting for HOME-FIELD, all the values CHANGE IN ONE DIRECTION
#  It's supposed to project onto a NEUTRAL FIELD, and 
#          be BENEFICIAL TO ROAD-HEAVY TEAMS,
#          while harmful to HOME-HEAVY TEAMS..

mean(glm_adj.rank.table$Value)
mean(glm_adj_w_hf.rank.table$Value)
summary(glm_adj.rank.table$Value - glm_adj_w_hf.rank.table$Value)


