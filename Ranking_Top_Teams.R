library(tidyverse)
library(gridExtra)

year <- 2017
week <- 21
side <- "Offense"                         # Offense/Defense
GLM_type <- "Gaussian"                     # Gaussian/Poisson
stat <- "Points"
  
common_path <- paste("~/Documents/Work/New_College/Research/GLM_for_Improved_Ranking_and_Predictive_Features_CFB/Rankings/",
                     year, "/Week=",
                     week,"/",
                     side,"/",
                     sep="")
classic.rank.table <- read.csv(paste(common_path,"Classic/", stat, ".csv", sep=""))
glm_adj.rank.table <- read.csv(paste(common_path, "GLM_Adj/",GLM_type, "/",stat,".csv", sep=""))
glm_adj_w_hf.rank.table <- read.csv(paste(common_path, "GLM_Adj_w_HomeField/",GLM_type, "/",stat,".csv", sep=""))
corr_glm_adj.rank.table <- read.csv(paste(common_path, "CORR_GLM_Adj/",GLM_type, "/",stat,".csv", sep=""))
corr_glm_adj_w_hf.rank.table <- read.csv(paste(common_path, "CORR_GLM_Adj_w_HomeField/",GLM_type, "/",stat,".csv", sep=""))


## Classic vs GLM_Adj
grid.arrange(
  tableGrob(classic.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% mutate(Value = round(Value,2)) %>% head(10)),
  tableGrob(glm_adj.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% mutate(Value = round(Value,2)) %>% head(10)),
  nrow=1)

## GLM_Adj vs GLM_Adj_w_Homefield
grid.arrange(
  tableGrob(glm_adj.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% head(10)),
  tableGrob(glm_adj_w_hf.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% head(10)),
  nrow=1)

## CORR_GLM_Adj vs CORR_GLM_Adj_w_Homefield
grid.arrange(
  tableGrob(corr_glm_adj.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% head(10)),
  tableGrob(corr_glm_adj_w_hf.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% head(10)),
  nrow=1)


## CORR_GLM_Adj vs GLM_Adj
grid.arrange(
  tableGrob(glm_adj.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% head(10)),
  tableGrob(corr_glm_adj.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% head(10)),
  nrow=1)

## CORR_GLM_Adj_w_Homefield vs GLM_Adj_w_Homefield
grid.arrange(
  tableGrob(glm_adj_w_hf.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% head(10)),
  tableGrob(corr_glm_adj_w_hf.rank.table %>% arrange(Rank) %>% select(Team, Value) %>% head(10)),
  nrow=1)
