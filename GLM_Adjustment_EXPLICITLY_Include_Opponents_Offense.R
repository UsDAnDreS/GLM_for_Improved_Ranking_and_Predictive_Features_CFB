# !!! https://happygitwithr.com/rstudio-git-github.html !!!

## !!! DPLYR's "SELECT()" clashes with MASS's "SELECT()" !!!
library(nlme)
library(tidyverse)

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
  
  
  Data$X.3 <- NULL
  stats <- colnames(Data)[c(9:11,13:15,17:18,25:27)]
  stats <- c(stats, "Tot.TD", "Pass.Avg", "Points")
  if (GLM_type == "Poisson") stats <- c("Pass.TD", "Rush.TD", "Fum", "Int", "TO", "Tot.TD", "Points")
  
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
      
      Data_offense$X <- Data_defense$X <- Data_offense$X.3 <- Data_defense$X.3 <- NULL
      
      Data_offense$Tot.TD <- Data_offense$Rush.TD + Data_offense$Pass.TD; Data_defense$Tot.TD <- Data_defense$Rush.TD + Data_defense$Pass.TD
      Data_offense$Pass.Avg <- ifelse(Data_offense$Pass.Att !=0, Data_offense$Pass.Yds/Data_offense$Pass.Att,0); 
      Data_defense$Pass.Avg <- ifelse(Data_offense$Pass.Att !=0, Data_defense$Pass.Yds/Data_defense$Pass.Att,0)
      
      Data_offense$Points <-   sapply(Data_offense$X.2, function(x){
        y <- str_extract(x, "\\(.*?\\-");
        return(as.numeric(substring(y,2,(nchar(y)-1))))})
      
      Data_defense$Points <-   sapply(Data_defense$X.2, function(x){
        y <- str_extract(x, "\\-.*?\\)");
        return(as.numeric(substring(y,2,(nchar(y)-1))))})
      
      
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
    
    
    
    # Getting rid of "*" due to bowl games, and creating a "Non-Major" team
    full.df$Opponent <- sapply(full.df$Opponent, function(x) str_remove(x, "\\*"))
    full.df$Opponent <- ifelse(full.df$Opponent %in% FBS_Team_names, full.df$Opponent, "Non-Major")
    
    ## Making sure factor level ORDERS correspond between Team & Opponent
    full.df$Team <- factor(full.df$Team, levels = sort(levels(full.df$Team)))
    full.df$Opponent <- factor(full.df$Opponent)
    
    ## Clean up the NAs
    full.df[,3] <- ifelse(!is.na(full.df[,3]), full.df[,3], 0)
    
    full.df.flipped <- full.df
    full.df.flipped$Team <- full.df$Opponent
    full.df.flipped$Opponent <- full.df$Team
    full.df.joined <- inner_join(full.df %>% select(Team, Opponent, 3, Date),
                                 full.df.flipped %>% select(Team, Opponent, 3, Date),
                                 by=c("Team","Opponent", "Date"))
    dim(full.df.joined)
    dim(full.df)
    
    
    ## Getting the x=MAX stat in a game, y=MIN.. 
    ## getting rid of duplicates
    dim(t(apply(full.df.joined %>% select(3,5), 1, sort)))
    sorted.stats <- unique(t(apply(full.df.joined %>% select(3,5), 1, sort)))
    print(sorted.stats %>% cor() %>% .[1,2])
    sorted.stats %>% plot(main= colnames(full.df)[3])
    
    
    ## The HETEROSCEDASTICITY is tough to fix again:
    ## We have more data on games with losing teams having 0-14 points
    ## than on games with losing teams scoring 17-28 points, 28+ etc...
    ## Hence there's MORE VARIABILITY as to HOW MUCH A WINNER SCORES
    ## for situations where LOSING TEAMS HAVE 0-14 points
    
    summary(apply(sorted.stats,1,diff))
    lm.obj <- lm(X1 ~ X2, 
                 data = data.frame(sorted.stats))
    summary(lm.obj)
    abline(lm.obj)
    plot(lm.obj, which=1)
   # plot(lm.obj, which=2)
    
    
    ####
    ## Getting a NEUTRAL FIELD sign ("N")
    ## A REALLY CLUNKY CHUNK OF CODE...
    ####
    
    merged.df <- merge(full.df, Data_homefield_full, by=c("Team","Opponent", "Date"),
                       all.x=T,
                       sort=F)
    
    #head(merged.df$Homefield)
    merged.df$Homefield <- factor(ifelse(is.na(merged.df$X), 
                                         as.character(merged.df$X.1.x), 
                                         as.character(merged.df$X)))
    
    merged.df <- merged.df[,c("Team","Opponent", "Date", stat, "Homefield")]
    
    ## For all the "Neutral" games, make sure it also shows "Neutral" the other way around
    # (because, otherwise, the "All_DISTINCT" file only contains ONE REPLICATE of one game)
    
    merged.df.neutral <- subset(merged.df, Homefield == "N")
    for (j in 1:nrow(merged.df.neutral)){
      merged.df[
        merged.df$Team == merged.df.neutral$Opponent[j] & 
          merged.df$Opponent == merged.df.neutral$Team[j] &
          merged.df$Date == merged.df.neutral$Date[j], ]$Homefield <- "N"
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
    
    # Needed to implement "gls()" function across all stats
    colnames(full.df)[4] <- "Stat"
      
      #####
      ## NO HOME-FIELD effect
      #####

      full.df$group.assignments <- 0
      
      ## Creating "group assignments" for PAIRS OF OBSERVATIONS that correspond to THE SAME GAME
      group.no <- 1
      group.assignments <- numeric(nrow(full.df))
      for (i in 1:nrow(full.df)){
        # print(i)
        #interm.res <- apply(full.df[1:(i-1),c("Team","Opponent")], 1, function(x) all(x == full.df[i,c("Opponent","Team")]))
        interm.res <- subset(full.df[1:(i-1),], Team == full.df[i,c("Opponent")] & 
                               Opponent == full.df[i,c("Team")] &
                               Date == full.df[i,"Date"])
        
        if (nrow(interm.res) != 0) full.df$group.assignments[i] <- interm.res$group.assignments
        if (nrow(interm.res) == 0) {full.df$group.assignments[i] <- group.no; group.no <- group.no + 1}
      }
      
      # print("Group assignments (should be 2 all around):")
      # print(table(table(full.df$group.assignments)))
      # full.df[table(full.df$group.assignments) == 1,]
      
      
      
      ### Calculating Opponents OFFENSE in the SAME GAME...
      full.df$Stat.Opp <- sapply(1:nrow(full.df), 
                                 function(x){
                                   interm.df <- 
                                     full.df %>% select(Stat, group.assignments,Team) %>%
                                     filter(group.assignments == full.df[x,]$group.assignments & 
                                              #Stat != full.df[x,]$Stat)
                                              Team == full.df[x,]$Opponent) %>% 
                                     select(Stat)
                                   interm.df$Stat[1]}
      )
      
      
      
  if (GLM_type == "Gaussian"){
    
      ## Trying out the comparison of:
          # - EXPLICIT model:
          #     y1 = mu + beta*X1 + eps_1,
          #     y2 = mu + beta*X2 + eps_2,
 
    lm.obj <- lm(Stat ~ Team + Opponent + Stat.Opp,
                 data=full.df)
    lm.obj
    summary(lm.obj)
    tail(coef(lm.obj))[1]
    head(coef(lm.obj))[1]
    coef(lm.obj)[1]
    tail(coef(lm.obj))
    
    
          # - IMPLICIT model:
          #     y1 = mu + eps_1,  cov(eps_1, eps_2) = rho
          #     y2 = mu + eps_2,
    
    ## YES, the APPROXIMATELY MATCH via:
    ##     mu1 = (mu2 - rho*mu2) 
    ##     beta = rho
    ##     RSE1 = sqrt((1-rho^2)*RSE2^2)
              
      ## Implementing the correlation structure between BOTH TEAMS' OUTPUTS IN A GAME...
      cor.mat <- Initialize(corCompSymm(form = ~ 1 | factor(group.assignments)),
                            data = full.df)
     # corMatrix(cor.mat)
      
      full.df$Points
      
      ## Compound symmetry, making it
      ## (1 rho)
      ## (rho 1)
      ## for observations corresponding to the same game.
      ## rho is shared across all games.
      # http://staff.pubhealth.ku.dk/~jufo/courses/rm2017/rPackageLME.pdf
      
      gls.obj <- gls(Stat ~ Team + Opponent,
                     data=full.df,
                     correlation = corCompSymm(form = ~ 1 | group.assignments))
      print("Correlation of Offenses")
      print(intervals(gls.obj)$corStruct[1:3])
      
      gls.obj$coefficients[1]
      gls.obj
      tail(gls.obj$coefficients)
      
      # !!! MARCH 16TH !!!!
      # http://www.maths.qmul.ac.uk/~ig/MTH5118/Notes11-09.pdf
      # https://stats.stackexchange.com/questions/340973/express-multivariate-normal-as-a-univariate-normals
      # E[y1|y2] FORMULA IS THE KEY !!!
      # E(X|Y) = µ1 +ρ σ1/σ2(Y −µ2).
      
      # Intercept = 57.973986306
      # rho = 0.02820537
      # RSE = 10.56872
    
      
      ## Adjusted averages
      ## Making sure  to project onto an AVERAGE OFFENSE of the OPPONENT...
      offensive.worth.adjusted <- c(coef(lm.obj)[1] + coef(lm.obj)[2:n.teams],
                                    coef(lm.obj)[1] - sum(coef(lm.obj)[2:n.teams])) + mean(full.df$Stat)*tail(coef(lm.obj),1)
      defensive.worth.adjusted <- c(coef(lm.obj)[1] + coef(lm.obj)[(n.teams+1):(2*n.teams-1)] + mean(full.df$Stat)*tail(coef(lm.obj),1),
                                    coef(lm.obj)[1] - sum(coef(lm.obj)[(n.teams+1):(2*n.teams-1)])) + mean(full.df$Stat)*tail(coef(lm.obj),1)
      
      offensive.worth.adjusted.df <- data.frame(Team=levels(full.df$Team), 
                                                Value=offensive.worth.adjusted,
                                                Rank=rank(-offensive.worth.adjusted*dec))
      defensive.worth.adjusted.df <- data.frame(Team=levels(factor(full.df$Opponent)), 
                                                Value=defensive.worth.adjusted,
                                                Rank=rank(defensive.worth.adjusted*dec))
      rownames(offensive.worth.adjusted.df) <- rownames(defensive.worth.adjusted.df) <- NULL
      
      
      
      #####
      ## WITH HOME-FIELD effect
      #####
      
      ## Trying out the comparison of:
      # - EXPLICIT model:
      #     y1 = mu + beta*X1 + eps_1, eps_1 ~ N(0,sigma^2)
      #     y2 = mu + beta*X2 + eps_2, eps_2 ~ N(0,sigma^2)
      
      
      # - IMPLICIT model:
      #     y1 = mu + eps_1, eps_1 ~ N(0,sigma^2), 
      #     y2 = mu + eps_2, eps_2 ~ N(0,sigma^2),
      #     cov(eps_1, eps_2) = rho
      
      # E[y1|y2] FORMULA IS THE KEY !!!
      # E(X|Y) = µ1 +ρ σ1/σ2(Y −µ2).
      
      ## YES, they APPROXIMATELY MATCH via:
      ##     mu1 = (mu2 - rho*mu2) 
      ##     beta = rho
      ##     RSE1 = sqrt((1-rho^2)*RSE2^2)
      
      # With the homefield included:
      # 58.50282* (1 - 0.02974459) = 56.76268   vs  56.78868 
      # 0.02983669                              vs  0.02974459 
      # 10.55966*sqrt(1-0.02974459^2)=10.55499  vs  10.56
      
      ## Setting "Homefield" to be 0-0 if "N", 1-0 if "Home" (or " "), 0-1 if "Away" (or "@")
      contrasts(full.df$Homefield) <- "contr.treatment"
      
      contrasts(full.df$Team)
      contrasts(full.df$Opponent)
      contrasts(full.df$Homefield)
      
      # Relevel:
      # relevel(full.df$Homefield, "")
      
      ## Making Homefield a NUMERIC VARIABLE to be modeled with SINGLE PARAMETER:
      ##  0 - Home, 1- Neutral, 2 - Away
      
      ## Explicit regression
      full.df$Homefield012 <- as.numeric(full.df$Homefield)-1
      lm.obj.hfield <- lm(#Stat ~ Team + Opponent + Stat.Opp + Homefield012,
                          Stat ~ Stat.Opp + Homefield012,
                          data=full.df)
      lm.obj.hfield
      summary(lm.obj.hfield)
      tail(coef(lm.obj.hfield),2)[1]
      head(coef(lm.obj.hfield),1)
      
      
      ## Bivariate model with correlation
      gls.obj.hfield <- gls(# Stat ~ Team + Opponent + Homefield012,
                            Stat ~ Homefield012,
                            data=full.df,
                            correlation = corCompSymm(form = ~ 1 | group.assignments))
      print("Correlation of Offenses")
      print(intervals(gls.obj.hfield)$corStruct[2])
      gls.obj.hfield$coefficients[1]
      # gls.obj.hfield
      
      
      
      # print("INTERCEPT for GLM ADJ")
      # print(coef(lm.obj)[1])
      # print("INTERCEPT for HOME-AWAY ADJ  -  GAMMA (for AVG OPPONENT ON NEUTRAL)")
      # print(coef(lm.obj.hfield)[1] - tail(coef(lm.obj.hfield),1))
      # print("HOME-AWAY COEFFICIENTS:")
      # print(tail(coef(lm.obj.hfield),1))
      
      ## Adjusted averages, with HOMEFIELD 
      offensive.worth.adjusted.hfield <- c(coef(lm.obj.hfield)[1] + coef(lm.obj.hfield)[2:n.teams],
                                           coef(lm.obj.hfield)[1] - sum(coef(lm.obj.hfield)[2:n.teams])) + mean(full.df$Stat)*tail(coef(lm.obj.hfield),2)[1] + tail(coef(lm.obj.hfield),1)
      defensive.worth.adjusted.hfield <- c(coef(lm.obj.hfield)[1] + coef(lm.obj.hfield)[(n.teams+1):(2*n.teams-1)],
                                           coef(lm.obj.hfield)[1] - sum(coef(lm.obj.hfield)[(n.teams+1):(2*n.teams-1)])) + mean(full.df$Stat)*tail(coef(lm.obj.hfield),2)[1] + tail(coef(lm.obj.hfield),1)
      
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
    
    
    if (GLM_type == "Poisson"){
      
      ###########
      ## NO HOME-FIELD effect
      ###########
      
      lm.obj <- glm(ifelse(full.df[,"Stat"]>=0,full.df[,"Stat"],0) ~ Team + Opponent + Stat.Opp,
                    family="poisson",
                    data=full.df)
      lm.obj
      levels(full.df$Team)
      
      sum.obj <- summary(lm.obj)
      print(tail(sum.obj$coefficients,1))
      
      plot(lm.obj, which=1)
      
      
      ## Adjusted averages
      offensive.worth.adjusted <- c(exp(coef(lm.obj)[1] + coef(lm.obj)[2:n.teams] + mean(full.df$Stat)*tail(coef(lm.obj),1)),
                                    exp(coef(lm.obj)[1] - sum(coef(lm.obj)[2:n.teams]) + mean(full.df$Stat)*tail(coef(lm.obj),1)))
      defensive.worth.adjusted <- c(exp(coef(lm.obj)[1] + coef(lm.obj)[(n.teams+1):(2*n.teams-1)] + mean(full.df$Stat)*tail(coef(lm.obj),1)),
                                    exp(coef(lm.obj)[1] - sum(coef(lm.obj)[(n.teams+1):(2*n.teams-1)]) + mean(full.df$Stat)*tail(coef(lm.obj),1)))
      
      offensive.worth.adjusted.df <- data.frame(Team=levels(full.df$Team), 
                                                Value=offensive.worth.adjusted,
                                                Rank=rank(-offensive.worth.adjusted*dec))
      defensive.worth.adjusted.df <- data.frame(Team=levels(factor(full.df$Opponent)), 
                                                Value=defensive.worth.adjusted,
                                                Rank=rank(defensive.worth.adjusted*dec))
      rownames(offensive.worth.adjusted.df) <- rownames(defensive.worth.adjusted.df) <- NULL
      
      
      
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
      lm.obj.hfield <- glm(ifelse(full.df[,"Stat"]>=0,full.df[,"Stat"],0)  ~ Team + Opponent + Stat.Opp + Homefield012,
                           family="poisson",
                           data=full.df)
      lm.obj.hfield
      levels(full.df$Team)
      
      lm.obj.hfield
      sum.obj.hfield <- summary(lm.obj.hfield)
      print(tail(sum.obj.hfield$coefficients,2))
      
      
      
      ## Adjusted averages, with HOMEFIELD
      offensive.worth.adjusted.hfield <- c(exp(coef(lm.obj.hfield)[1] + coef(lm.obj.hfield)[2:n.teams] + mean(full.df$Stat)*tail(coef(lm.obj.hfield),2)[1] + tail(coef(lm.obj.hfield),1)),
                                           exp(coef(lm.obj.hfield)[1] - sum(coef(lm.obj.hfield)[2:n.teams]) + mean(full.df$Stat)*tail(coef(lm.obj.hfield),2)[1] + tail(coef(lm.obj.hfield),1)))
      defensive.worth.adjusted.hfield <- c(exp(coef(lm.obj.hfield)[1] + coef(lm.obj.hfield)[(n.teams+1):(2*n.teams-1)] + mean(full.df$Stat)*tail(coef(lm.obj.hfield),2)[1] + tail(coef(lm.obj.hfield),1)),
                                           exp(coef(lm.obj.hfield)[1] - sum(coef(lm.obj.hfield)[(n.teams+1):(2*n.teams-1)]) + mean(full.df$Stat)*tail(coef(lm.obj.hfield),2)[1] + tail(coef(lm.obj.hfield),1)))
      
      
      # print("INTERCEPT for GLM ADJ")
      # print(exp(coef(lm.obj)[1]))
      # print("INTERCEPT for HOME-AWAY ADJ  -  GAMMA (for AVG OPPONENT ON NEUTRAL)")
      # print(exp(coef(lm.obj.hfield)[1] - tail(coef(lm.obj.hfield),1)))
      # print("HOME-AWAY COEFFICIENTS:")
      # print(tail(coef(lm.obj.hfield),1))
      
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
    
    

    #dir.create("/Rankings")
    dir.create(paste(getwd(),"/Rankings/", year,"/", sep=""))
    # dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/", sep=""))
    #
    dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/Defense/", sep=""))
    dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/Defense/Classic/", sep=""))
    dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/Defense/EXPLICIT_GLM_Adj/", sep=""))
    dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/Defense/EXPLICIT_GLM_Adj_w_HomeField/", sep=""))
    dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/Defense/EXPLICIT_GLM_Adj/",GLM_type,"/", sep=""))
    dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/Defense/EXPLICIT_GLM_Adj_w_HomeField/",GLM_type,"/", sep=""))

    dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/Offense/", sep=""))
    dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/Offense/Classic/", sep=""))
    dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/Offense/EXPLICIT_GLM_Adj/", sep=""))
    dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/Offense/EXPLICIT_GLM_Adj_w_HomeField/", sep=""))
    dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/Offense/EXPLICIT_GLM_Adj/",GLM_type,"/", sep=""))
    dir.create(paste(getwd(),"/Rankings/", year,"/Week=",week,"/Offense/EXPLICIT_GLM_Adj_w_HomeField/",GLM_type,"/", sep=""))


    write.csv(defensive.worth.adjusted.df,
              paste(getwd(),"/Rankings/", year,"/Week=",week,"/Defense/EXPLICIT_GLM_Adj/",GLM_type,"/",stat,".csv", sep=""))
    write.csv(defensive.worth.adjusted.hfield.df,
              paste(getwd(),"/Rankings/", year,"/Week=",week,"/Defense/EXPLICIT_GLM_Adj_w_HomeField/",GLM_type,"/",stat,".csv", sep=""))

    write.csv(offensive.worth.adjusted.df,
              paste(getwd(),"/Rankings/", year,"/Week=",week,"/Offense/EXPLICIT_GLM_Adj/",GLM_type,"/",stat,".csv", sep=""))
    write.csv(offensive.worth.adjusted.hfield.df,
              paste(getwd(),"/Rankings/", year,"/Week=",week,"/Offense/EXPLICIT_GLM_Adj_w_HomeField/",GLM_type,"/",stat,".csv", sep=""))

  }
}


