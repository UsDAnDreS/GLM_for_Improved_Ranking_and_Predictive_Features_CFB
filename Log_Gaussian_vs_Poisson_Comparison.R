# !!! https://happygitwithr.com/rstudio-git-github.html !!!

## !!! HAS A MENTION of CHI-SQUARED ERROR !!!
# https://stats.stackexchange.com/questions/49473/is-it-possible-to-compare-model-fit-for-a-gaussian-vs-binomial-glm
# https://stats.stackexchange.com/questions/48811/cost-function-for-validating-poisson-regression-models


## Try the GAUSSIAN with LOG-LINK...

## Compare AIC & DEVIANCES of FITTED MODELS...
## A_bivariate_Weibull_count_model_for_fore.pdf


## For classic MSE:
#         Gaussian  vs   Poisson
# Pct:    132.2272  vs  134.248
# Yds:    7805.008  vs  7712.741
# TD:     1.789326  vs  1.823455
# Yds.1:  6749.547  vs  6640.393
# Avg:    2.739842  vs  2.725134
# TD.1:   1.907407  vs  1.950399
# Yds.2:  11048.64  vs  10996.79
# Avg.1:  1.85948   vs  1.854077
# Fum:    0.8605853 vs  0.8655569
# Int:    1.086233  vs  1.072876
# TO:     1.927359  vs  1.892901
# TD.2:   2.868265  vs  2.908623
# Points: 150.5808  vs  151.0563



## For Chi-Squared:
#         Gaussian  vs   Poisson
# Pct:    2.618527  vs  2.639474
# Yds:    36.8723   vs  36.15417
# TD:     2.094856  vs  1.364321
# Yds.1:  40.05549  vs  39.08046
# Avg:    0.657629  vs  0.6506385
# TD.1:   1.686238  vs  1.278422
# Yds.2:  28.67358  vs  28.57013
# Avg.1:  0.3324229 vs  0.3304226
# Fum:    1.874291  vs  !!??!? 65620.85 !!?!?!
# Int:    !?!? -5.417302 !?!?!  vs  1.61218
# TO:     1.460569  vs  1.47141
# TD.2:   0.9897562  vs  0.9633495
# Points: 5.842696  vs  5.838357


## For Log:
#         Gaussian  vs   Poisson
# Pct:    0.07840364  vs  0.09919698
# Yds:    0.2781077   vs  0.3047034
# TD:     0.1353672  vs  0.2930806
# Yds.1:  0.4006057  vs  0.4350452
# Avg:    0.07691797  vs  0.1191871
# TD.1:   0.1297262  vs  0.2708729
# Yds.2:  0.0958814  vs  0.09569329
# Avg.1:  0.0333477 vs  0.04570202
# Fum:    0.1039563  vs  0.262192
# Int:    0.1147111  vs  0.2746425
# TO:     0.1534951  vs  0.3312943
# TD.2:   0.1162545  vs  0.2089876
# Points: 0.313129  vs  0.4017946




library(boot)

year <- 2017
#week <- 10
GLM_type <- "Gaussian"
cost <- "Log"   # Can be one of "MSE", "Chi-Sq", "Log"

if (cost == "MSE"){
  cost.Gauss <- cost.Pois <- function(x,y) mean((x-y)^2)
}

if (cost == "Chi-Sq"){
  cost.Gauss <- function(x,y) mean((x-y)^2/(y-1))
  cost.Pois <- function(x,y) mean((x-y)^2/y)
}

if (cost == "Log"){
  cost.Gauss <- cost.Pois <- function(x,y) mean((log(x+1)-log(y+1))^2)
}


corr.team.vs.opponent.stats <- T  # Whether to print out correlation between Team's & Opponent's stats in the same category
# Because, in many cases, the lower the point count for the opponent, the lower the point count for the team itself..


path <- paste("~/Documents/Work/New_College/Research/GLM_for_Improved_Ranking_and_Predictive_Features_CFB/Game_Logs/",year, "/", sep="")
path_offense <- paste(path,"/Offense/", sep="")
path_defense <- paste(path, "/Defense/", sep="")
Data <- read.csv(paste(path_offense,"Texas-San Antonio.csv", sep=""))

head(Data)

latest.date <- function(week, year){
  data <- read.csv(paste("Game_Logs/", year, "/All_DISTINCT_Condensed_Game_Logs_NO_NEUTRAL_with_WEEKS.csv", sep=""))
  return(max(as.Date(subset(data, Wk == week)$Date, "%b %d, %Y")))
}

latest.week <- function(year){
  data <- read.csv(paste("Game_Logs/", year, "/All_DISTINCT_Condensed_Game_Logs_NO_NEUTRAL_with_WEEKS.csv", sep=""))
  return(max(data$Wk))
}


max.week <- latest.week(year)


for (week in max.week){
  
  print(week)
  
  max.date <- latest.date(week, year)
  
  # "Pct" - Completion %
  
  # "Yds" - Passing Yards per game
  # "Yds.1" - Rushing yards per game
  # "Yds.2" - Total yards per game
  
  # "Avg" - Yards per carry
  # "Avg.1" - Yards per play
  # "Avg.2" - Yards per passing play
  
  # "TD"  - Passing TD per game  
  # "TD.1" - Rushing TD per game
  # "TD.2"  - Total TD per game
  
  # "Fum"  
  # "Int"  
  # "TO"
  # "Points"
  
  
  stats <- colnames(Data)[c(9:11,13:15,17:18,25:27)]
  stats <- c(stats, "TD.2", "Points", "Avg.2")
  if (GLM_type == "Poisson") stats <- c("TD", "TD.1", "Fum", "Int", "TO", "TD.2", "Points")
  
  FBS_Team_names <- sapply(list.files(path=path_offense), function(x) substring(x, 1,nchar(x)-4))
  names(FBS_Team_names) <- NULL
  
  
  ## Picking a stat, creating a mini-dataframe
  n.st <- 1
  stat <- stats[n.st]
  
  ## Scrolling through all relevant stats, doing the adjustment.
  
  for (stat in stats){
    print(stat)
    dec <- 1  
    if (stat %in% c("Fum", "Int", "TO")) dec <- -1   # For turnovers, the rankings are "1. Smallest, ..."
    
    full.df <- NULL
    
    library(stringr)
    n.teams <- length(FBS_Team_names) + 1
    
    Data_homefield_full <- subset(read.csv(paste(path, "All_DISTINCT_Condensed_Game_Logs.csv", sep="")), as.Date(Date) <= max.date)
    
    for (team in FBS_Team_names){
      #print(team)
      Data_offense <- subset(read.csv(paste(path_offense, team, ".csv", sep="")), as.Date(Date) <= max.date)
      Data_defense <- subset(read.csv(paste(path_defense, team, ".csv", sep="")), as.Date(Date) <= max.date)
      
      if (stat == "TD.2") {Data_offense$TD.2 <- Data_offense$TD + Data_offense$TD.1; Data_defense$TD.2 <- Data_defense$TD + Data_defense$TD.1}
      if (stat == "Avg.2") {Data_offense$Avg.2 <- ifelse(Data_offense$Att !=0, Data_offense$Yds/Data_offense$Att,0); 
      Data_defense$Avg.2 <- ifelse(Data_offense$Att !=0, Data_defense$Yds/Data_defense$Att,0)}
      
      if (stat == "Points"){
        Data_offense$Points <-   sapply(Data_offense$X.2, function(x){
          y <- str_extract(x, "\\(.*?\\-");
          return(as.numeric(substring(y,2,(nchar(y)-1))))})
        Data_defense$Points <-   sapply(Data_defense$X.2, function(x){
          y <- str_extract(x, "\\-.*?\\)");
          return(as.numeric(substring(y,2,(nchar(y)-1))))})
      }
      
      # https://stackoverflow.com/questions/1454913/regular-expression-to-find-a-string-included-between-two-characters-while-exclud
      
      
      
      full.df <- rbind(full.df,data.frame(Team=factor(team), 
                                          Data_offense[,c("Opponent", stat,"X.1","Date")]))
      #Homefield = Data_homefield$X))
      
      Data_defense$Opponent <- sapply(Data_defense$Opponent, function(x) str_remove(x, "\\*"))
      interm.df <- subset(Data_defense, !Opponent %in% FBS_Team_names)
      
      if (nrow(interm.df) != 0){
        full.df <- rbind(full.df,data.frame(Team=factor("Non-Major"), 
                                            Opponent = team,
                                            interm.df[,c(stat,"X.1","Date")]))
        #Homefield=subset(Data_homefield, Opponent == "Non-Major")$X)[,-4])
      }
    }
    
    
    #length(sort(unique(full.df$Opponent)))
    #dim(full.df)
    
    
    # Getting rid of "*" due to bowl games, and creating a "Non-Major" team
    full.df$Opponent <- sapply(full.df$Opponent, function(x) str_remove(x, "\\*"))
    full.df$Opponent <- ifelse(full.df$Opponent %in% FBS_Team_names, full.df$Opponent, "Non-Major")
    
    ## Making sure factor level ORDERS correspond between Team & Opponent
    full.df$Team <- factor(full.df$Team, levels = sort(levels(full.df$Team)))
    full.df$Opponent <- factor(full.df$Opponent)
    
    ## Clean up the NAs
    full.df[,3] <- ifelse(!is.na(full.df[,3]), full.df[,3], 0)
    
    
    ####
    ## Getting a NEUTRAL FIELD sign ("N")
    ## A REALLY CLUNKY CHUNK OF CODE...
    ####
    
    merged.df <- merge(full.df, Data_homefield_full, by=c("Team","Opponent"),
                       all.x=T,
                       sort=F)
    
    #head(merged.df$Homefield)
    merged.df$Homefield <- factor(ifelse(is.na(merged.df$X), 
                                         as.character(merged.df$X.1.x), 
                                         as.character(merged.df$X)))
    
    merged.df[,c(6:9)] <- NULL
    
    ## For all the "Neutral" games, make sure it also shows "Neutral" the other way around
    # (because, otherwise, the "All_DISTINCT" file only contains ONE REPLICATE of one game)
    
    merged.df.neutral <- subset(merged.df, Homefield == "N")
    for (j in 1:nrow(merged.df.neutral)){
      merged.df[
        merged.df$Team == merged.df.neutral$Opponent[j] & 
          merged.df$Opponent == merged.df.neutral$Team[j] &
          merged.df$Date.x == merged.df.neutral$Date.x[j], ]$Homefield <- "N"
    }
    
    dim(merged.df)
    # nrow(subset(merged.df, Homefield == "N"))
    
    merged.df$X.1.x <- NULL
    
    full.df <- merged.df   # Re-assigning the NEW FULL DATA FRAME...
    # full.df$Homefield <- relevel(full.df$Homefield, ref = "")  # Making "N" the reference group
    full.df$Homefield <- factor(full.df$Homefield, levels=c("","N","@"))  # Making it ordered Home -> Neutral -> Away
    
    cbind(sort(unique(as.character(full.df$Opponent))),
          sort(unique(as.character(full.df$Team))))
    
    
    
    ########
    ### Classic averages
    ########
    offensive.worth.classic <- tapply(full.df[, stat], full.df$Team, mean) 
    defensive.worth.classic <- tapply(full.df[, stat], factor(full.df$Opponent), mean) 
    offensive.worth.classic.df <- data.frame(Team=levels(full.df$Team), 
                                             Value=offensive.worth.classic,
                                             Rank=rank(-offensive.worth.classic*dec))
    defensive.worth.classic.df <- data.frame(Team=levels(factor(full.df$Opponent)), 
                                             Value=defensive.worth.classic,
                                             Rank=rank(defensive.worth.classic*dec))
    rownames(offensive.worth.classic.df) <- rownames(defensive.worth.classic.df) <- NULL
    
    
    ####################
    ####################
    ## GLM ADJUSTMENT ##
    ####################
    ####################
    
    # Making the contrasts for sum alpha_i =0,  sum beta_j = 0.
    options(contrasts = rep("contr.sum", 2))
    contr.sum(n.teams)
    contr.sum(n.teams)
    
    
    
    
    
    ###########
    ## Gaussian GLM
    ###########
    
    #####
    ## WITH HOME-FIELD effect
    #####
    
    ## Setting "Homefield" to be 0-0 if "N", 1-0 if "Home" (or " "), 0-1 if "Away" (or "@")
    contrasts(full.df$Homefield) <- "contr.treatment"
    
    contrasts(full.df$Team)
    contrasts(full.df$Opponent)
    contrasts(full.df$Homefield)
    
    
    # Relevel:
    # relevel(full.df$Homefield, "")
    
    colnames(full.df)[3] <- "Stat"
    full.df$Stat <- ifelse(full.df$Stat>=0, full.df$Stat, 0)
    ## Making Homefield a NUMERIC VARIABLE to be modeled with SINGLE PARAMETER:
    ##  0 - Home, 1- Neutral, 2 - Away
    full.df$Homefield012 <- as.numeric(full.df$Homefield)-1
    full.df$Stat1 <- full.df$Stat + 1
    lm.obj.hfield.Gauss <- glm(Stat1 ~ Team + Opponent, #+ Homefield012,
                               family = gaussian("log"),
                               data=full.df)
    lm.obj.hfield.Gauss
    summary(predict(lm.obj.hfield.Gauss, type="response")-1)
    
    plot(lm.obj.hfield.Gauss, which=1)
    # sum.obj.Gauss <- summary(lm.obj.hfield.Gauss)
    # sum.obj.Gauss$deviance
    # sum.obj.Gauss$aic
    
    
    ## Heteroscedasticity issue doesn't look fixable in that
    ##  FACTOR-ONLY predictor type of space, with each factor level
    ##  only having ~12 observations.
    
    library(MASS)
    bc <- boxcox(Stat1 ~ Team + Opponent,
                 data=full.df)
    lambda <- bc$x[which.max(bc$y)]
    lm.bc <- lm(((Stat1^lambda-1)/lambda) ~ Team + Opponent,
                data = full.df)
    plot(lm.bc, which=1)
    plot(lm.bc, which=2)
    
    
    #  print("Gaussian:")
    #  print(sqrt(mean((resid(lm.obj.hfield.Gauss))^2)))
    print("Gaussian CV:")
    print(cv.glm(full.df, 
                 lm.obj.hfield.Gauss, 
                 K=100,
                 cost = cost.Gauss)$delta[1])
                 #cost = function(x,y) mean((log(y+1)-log(x+1))^2))$delta[1])
    #cost = function(x,y) mean(sum(ifelse(y>=0, y,0)+1)-log(x+1))^2))$delta[1])
    # print(mean((resid(lm.obj.hfield.Gauss)/predict(lm.obj.hfield.Gauss))^2))
    
    plot(lm.obj.hfield.Gauss, which=1)
    
    library(tidyverse)
    res.df.Gauss <- data.frame(Res=abs(resid(lm.obj.hfield.Gauss)),
                               True=full.df[,"Stat"],
                               Pred=predict(lm.obj.hfield.Gauss)) %>% arrange(desc(Res))
    
    # print("INTERCEPT for GLM ADJ")
    # print(coef(lm.obj)[1])
    # print("INTERCEPT for HOME-AWAY ADJ  -  GAMMA (for AVG OPPONENT ON NEUTRAL)")
    # print(coef(lm.obj.hfield)[1] - tail(coef(lm.obj.hfield),1))
    # print("HOME-AWAY COEFFICIENTS:")
    # print(tail(coef(lm.obj.hfield),1))
    
    ## Adjusted averages, with HOMEFIELD 
    offensive.worth.adjusted.hfield <- c(coef(lm.obj.hfield.Gauss)[1] + coef(lm.obj.hfield.Gauss)[2:n.teams],
                                         coef(lm.obj.hfield.Gauss)[1] - sum(coef(lm.obj.hfield.Gauss)[2:n.teams])) + tail(coef(lm.obj.hfield.Gauss),1)
    defensive.worth.adjusted.hfield <- c(coef(lm.obj.hfield.Gauss)[1] + coef(lm.obj.hfield.Gauss)[(n.teams+1):(2*n.teams-1)],
                                         coef(lm.obj.hfield.Gauss)[1] - sum(coef(lm.obj.hfield.Gauss)[(n.teams+1):(2*n.teams-1)])) + tail(coef(lm.obj.hfield.Gauss),1)
    
    offensive.worth.adjusted.hfield.df <- data.frame(Team=levels(full.df$Team), 
                                                     Value=offensive.worth.adjusted.hfield,
                                                     Rank=rank(-offensive.worth.adjusted.hfield*dec))
    defensive.worth.adjusted.hfield.df <- data.frame(Team=levels(factor(full.df$Opponent)), 
                                                     Value=offensive.worth.adjusted.hfield,
                                                     Rank=rank(defensive.worth.adjusted.hfield*dec))
    rownames(offensive.worth.adjusted.hfield.df) <- rownames(defensive.worth.adjusted.hfield.df) <- NULL
    
    # BUT "HOMEFIELD" is for GAMES AT HOME... hence, JUST ADD a GAMMA to get a NEUTRAL FIELD..
    # print(summary(offensive.worth.adjusted.hfield - offensive.worth.adjusted)) + tail(coef(lm.obj.hfield),1)
    
    
    
    ####################
    ### POISSONG GLM ###
    ####################
    
    ###########
    ## WITH HOME-FIELD effect
    ###########
    
    # Relevel:
    # relevel(full.df$Homefield, "")
    
    contrasts(full.df$Homefield) <- "contr.treatment"
    
    contrasts(full.df$Team)
    contrasts(full.df$Opponent)
    contrasts(full.df$Homefield)
    
    full.df$Homefield012 <- as.numeric(full.df$Homefield)-1
    lm.obj.hfield.Poiss <- glm(Stat  ~ Team + Opponent + Homefield012,
                               family="poisson",
                               data=full.df)
    lm.obj.hfield.Poiss
    levels(full.df$Team)
    
    sum.obj.Poiss <- summary(lm.obj.hfield.Poiss)
    sum.obj.Poiss$deviance
    sum.obj.Poiss$aic
    
    
    ## Adjusted averages, with HOMEFIELD
    offensive.worth.adjusted.hfield <- c(exp(coef(lm.obj.hfield.Poiss)[1] + coef(lm.obj.hfield.Poiss)[2:n.teams] + tail(coef(lm.obj.hfield.Poiss),1)),
                                         exp(coef(lm.obj.hfield.Poiss)[1] - sum(coef(lm.obj.hfield.Poiss)[2:n.teams]) + tail(coef(lm.obj.hfield.Poiss),1)))
    defensive.worth.adjusted.hfield <- c(exp(coef(lm.obj.hfield.Poiss)[1] + coef(lm.obj.hfield.Poiss)[(n.teams+1):(2*n.teams-1)] + tail(coef(lm.obj.hfield.Poiss),1)),
                                         exp(coef(lm.obj.hfield.Poiss)[1] - sum(coef(lm.obj.hfield.Poiss)[(n.teams+1):(2*n.teams-1)]) + tail(coef(lm.obj.hfield.Poiss),1)))
    
    
    # print("Poisson:")
    # print(sqrt(mean((resid(lm.obj.hfield.Poiss, type="response"))^2)))
    print("Poisson CV:")
    set.seed(1)
    print(cv.glm(full.df, 
                 lm.obj.hfield.Poiss, 
                 K=100,
                 cost=cost.Pois
                 #cost = function(x,y) mean((log(y+1)-log(x+1))^2))$delta[1])
    )$delta[1])
    
    
    
    
    res.df.Poiss <- data.frame(Res=abs(resid(lm.obj.hfield.Poiss, type="response")),
                               True=full.df[,"Stat"],
                               Pred=predict(lm.obj.hfield.Poiss,type="response")) %>% arrange(desc(Res))
    
    
    plot(lm.obj.hfield.Poiss, which=1)
    
    # print("INTERCEPT for GLM ADJ")
    # print(exp(coef(lm.obj)[1]))
    # print("INTERCEPT for HOME-AWAY ADJ  -  GAMMA (for AVG OPPONENT ON NEUTRAL)")
    # print(exp(coef(lm.obj.hfield.Poiss)[1] - tail(coef(lm.obj.hfield.Poiss),1)))
    # print("HOME-AWAY COEFFICIENTS:")
    # print(tail(coef(lm.obj.hfield.Poiss),1))
    
    offensive.worth.adjusted.hfield.df <- data.frame(Team=levels(full.df$Team), 
                                                     Value=offensive.worth.adjusted.hfield,
                                                     Rank=rank(-offensive.worth.adjusted.hfield*dec))
    defensive.worth.adjusted.hfield.df <- data.frame(Team=levels(factor(full.df$Opponent)), 
                                                     Value=offensive.worth.adjusted.hfield,
                                                     Rank=rank(defensive.worth.adjusted.hfield*dec))
    rownames(offensive.worth.adjusted.hfield.df) <- rownames(defensive.worth.adjusted.hfield.df) <- NULL
    
    # BUT "HOMEFIELD" is for GAMES AT HOME... hence, JUST ADD a GAMMA to get a NEUTRAL FIELD..
    # print(summary(offensive.worth.adjusted.hfield - offensive.worth.adjusted)) + tail(coef(lm.obj.hfield),1)
    
  }
  
}


