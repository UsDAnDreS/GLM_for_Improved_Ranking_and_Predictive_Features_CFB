# !!! https://happygitwithr.com/rstudio-git-github.html !!!


## There just doesn't appear to be a gigantic benefit from using Poisson vs Gaussian
## even in cases of low-count stats...
## Regardless of the metric (be it MSE, log-MSE, or Chi-Sq)

## Try the GAUSSIAN with LOG-LINK...




# WHICH GLM FAMILY TO USE??
# https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use?noredirect=1&lq=1

# https://stats.stackexchange.com/questions/223160/should-i-use-poisson-distribution-for-non-integer-count-like-data

# https://stats.stackexchange.com/questions/48811/cost-function-for-validating-poisson-regression-models


library(boot)

year <- 2017
#week <- 10
GLM_type <- "Gaussian"

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


for (week in 12){
  
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
      lm.obj.hfield.Gauss <- glm(Stat ~ Team + Opponent + Homefield012,
                                 # family = gaussian("log"),
                                 data=full.df)
      lm.obj.hfield.Gauss
      predict(lm.obj.hfield.Gauss, type="response")
      
    #  print("Gaussian:")
    #  print(sqrt(mean((resid(lm.obj.hfield.Gauss))^2)))
      print("Gaussian CV:")
      print(cv.glm(full.df, 
                   lm.obj.hfield.Gauss, 
                   K=100,
                   #)$delta[1])
                   cost = function(x,y) mean((log(y+1)-log(x+1))^2))$delta[1])
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
                   cost = function(x,y) mean((log(y+1)-log(x+1))^2))$delta[1])
      #)$delta[1])
      
      
      
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


