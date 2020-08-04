library(tidyverse)
library(tidyr)
library(Matrix)
#the data was scraped from the nhl website using api https://github.com/HarryShomer/Hockey-Scraper
#becasue the data is very memory consuming and wrangling data to the format from the article you need a lot of memory
# to ensure you don't run out of memory you can divide the data to the events that you need for the analysis and then run the code. 
nhl_pbp20162017 <- read_csv("D:/Hockey Project/nhl_pbp20182019.csv")
shots = which(nhl_pbp20162017[,"Event"]=="SHOT")
goals = which(nhl_pbp20162017[,"Event"]=="GOAL")
block = which(nhl_pbp20162017[,"Event"]=="BLOCK")
missshot= which(nhl_pbp20162017[,"Event"]=="MISS")


data_sample <- nhl_pbp20162017[c(shots,goals,block,missshot),]


#get the lsit of players
players_name_1 <- as.matrix(data_sample[,"awayPlayer1"])

players_name_2 <- as.matrix(data_sample[,"awayPlayer2"])

players_name_3 <- as.matrix(data_sample[,"awayPlayer3"])

players_name_4 <- as.matrix(data_sample[,"awayPlayer4"])

players_name_5 <- as.matrix(data_sample[,"awayPlayer5"])

players_name_6 <- as.matrix(data_sample[,"homePlayer1"])

players_name_7 <- as.matrix(data_sample[,"homePlayer2"])

players_name_8 <- as.matrix(data_sample[,"homePlayer3"])

players_name_9 <- as.matrix(data_sample[,"homePlayer4"])

players_name_10 <- as.matrix(data_sample[,"homePlayer5"])




#get the id of players
players_ids_1 <- as.matrix(data_sample[,"awayPlayer1_id"])

players_ids_2 <- as.matrix(data_sample[,"awayPlayer2_id"])

players_ids_3 <- as.matrix(data_sample[,"awayPlayer3_id"])                        

players_ids_4 <- as.matrix(data_sample[,"awayPlayer4_id"])

players_ids_5 <- as.matrix(data_sample[,"awayPlayer5_id"])



players_ids_6 <- as.matrix(data_sample[,"homePlayer1_id"])

players_ids_7 <- as.matrix(data_sample[,"homePlayer2_id"])

players_ids_8 <- as.matrix(data_sample[,"homePlayer3_id"])

players_ids_9 <- as.matrix(data_sample[,"homePlayer4_id"])

players_ids_10 <- as.matrix(data_sample[,"homePlayer5_id"])







# make a list of the names and ids

players_name_list_1 <- rbind(players_name_1,players_name_2,players_name_3,players_name_4,players_name_5,players_name_6
                             
                             ,players_name_7,players_name_8,players_name_9,players_name_10)



players_ids_list_1 <- rbind(players_ids_1,players_ids_2,players_ids_3,players_ids_4,players_ids_5,players_ids_6,
                            
                            players_ids_7,players_ids_8,players_ids_9,players_ids_10)



players_list <- cbind(players_name_list_1,players_ids_list_1)


#remove the duplicates
players_list_uq <- unique(players_list)

players_list_uq = na.omit(players_list_uq)



#events happening for home or away team

home_eve <- which(data_sample[,"Ev_Team"]==data_sample[,"Home_Team"])

away_eve <- which(data_sample[,"Ev_Team"]==data_sample[,"Away_Team"])


#defining the initial data
event_data <- matrix(data = NA , nrow = nrow(data_sample), ncol = nrow(players_list_uq)+7)






#fix the column names
colnames(event_data) <- c("home_ev","event","time","strength","Type","home_team", "away_team",na.omit(players_list_uq[,2]))
a11 = as.data.frame(colnames(event_data))
event_data_1 <- event_data #offense data
event_data_2 <- event_data #defense data
ha_eve = sort(c(away_eve,home_eve), decreasing = FALSE)

#as the articl suggests, the matrix is divided in four parts, if they are home player and they are on the def side (Call this D), if they are home players and they are on the
#attacking side(Call this X), and the same for the away players. The matrix looks like this 
#DX
#XD

# in this part we are looking for the players that were in the specific event and based on the event_data that is built above, we are putting 1 or 0s below the players ID (column names)
# this is simple searching for loop to find the players that were in the event. 
for (i in home_eve){
  print(i)
  j=c("homePlayer1_id","homePlayer2_id","homePlayer3_id","homePlayer4_id","homePlayer5_id")
  test.vec=c(as.numeric(data_sample[i,j]),as.numeric(colnames(event_data)[8:ncol(event_data)])) #Combine the "present" players and existing player dictionary
  test=which(duplicated(na.omit(test.vec))) #Find the "present" players in dictionary
  # test.vec[test] #Ensuring the code is working
  num_col=which(duplicated(c(na.omit(players_list_uq[,2]),test.vec[test]),fromLast = TRUE)) #Find the location of "present" players in dictionary
  event_data_1[i,num_col+7]=1 #Assign 1 if the player was involved in the event
}

#testing the result
which(event_data_1[157208,] ==1)
#event_data_3 <- Matrix(event_data_1)
for (i in away_eve){
  print(i)
  j=c("awayPlayer1_id","awayPlayer2_id","awayPlayer3_id","awayPlayer4_id","awayPlayer5_id")
  test.vec=c(as.numeric(colnames(event_data)[8:ncol(event_data)]),as.numeric(data_sample[i,j])) #Combine the "present" players and existing player dictionary
  test=which(duplicated(na.omit(test.vec))) #Find the "present" players in dictionary
  # test.vec[test] #Ensuring the code is working
  num_col=which(duplicated(c(players_list_uq[,2],test.vec[test]),fromLast = TRUE)) #Find the location of "present" players in dictionary
  event_data_1[i,num_col+7]=1 #Assign 1 if the player was involved in the event
}
#event_data_3 <- Matrix(event_data_1)

for (i in home_eve){
  print(i)
  j=c("awayPlayer1_id","awayPlayer2_id","awayPlayer3_id","awayPlayer4_id","awayPlayer5_id")
  test.vec=c(as.numeric(data_sample[i,j]),as.numeric(colnames(event_data)[8:ncol(event_data)])) #Combine the "present" players and existing player dictionary
  test=which(duplicated(na.omit(test.vec))) #Find the "present" players in dictionary
  # test.vec[test] #Ensuring the code is working
  num_col=which(duplicated(c(na.omit(players_list_uq[,2]),test.vec[test]),fromLast = TRUE)) #Find the location of "present" players in dictionary
  event_data_2[i,num_col+7]=1 #Assign 1 if the player was involved in the event
}

for (i in away_eve){
  print(i)
  j=c("homePlayer1_id","homePlayer2_id","homePlayer3_id","homePlayer4_id","homePlayer5_id")
  test.vec=c(as.numeric(data_sample[i,j]),as.numeric(colnames(event_data)[8:ncol(event_data)])) #Combine the "present" players and existing player dictionary
  test=which(duplicated(na.omit(test.vec))) #Find the "present" players in dictionary
  # test.vec[test] #Ensuring the code is working
  num_col=which(duplicated(c(na.omit(players_list_uq[,2]),test.vec[test]),fromLast = TRUE)) #Find the location of "present" players in dictionary
  event_data_2[i,num_col+7]=1 #Assign 1 if the player was involved in the event
}
a11 = as.data.frame(colnames(evnt_data))
y1 = as.vector(1/(data_sample$Seconds_Elapsed))*3600

y0 = as.vector(rep(0,length.out=nrow(data_sample)))

y = c(y1,y0)

zoff_1 = which(data_sample$Event=="FAC" & data_sample$Ev_Zone == "Off")
zdef_1 = which(data_sample$Event=="FAC" & data_sample$Ev_Zone == "Def")

zoff = as.vector(rep(1, length.out=nrow(data_sample)))
zoff[-zoff_1] = 0
zdef = as.vector(rep(1, length.out=nrow(data_sample)))
zdef[-zdef_1] = 0

z_off = as.vector(rbind(zoff,zoff))
z_def = as.vector(rbind(zdef,zdef))

# event_data_1= Matrix(event_data_1[,-c(1:7)])
# event_data_2= Matrix(event_data_2[,-c(1:7)])

event_off <- event_data_1[,-c(1:7)]
event_def = event_data_2[,-c(1:7)]
a11 = as.data.frame(colnames(evnt_data))
rm(event_data_1,event_data_2)
rm(event_data,event_data_3,events_frame)
even_offense_1 <- rbind(event_off,event_def)

even_defe <- rbind(event_def,event_off)
# 
# write.csv(even_offense_1,'event_offense.csv')
# write.csv(even_defe,'event_defense.csv')
# write.csv(y,'y.csv')
# write.csv(y0,'y0.csv')
# write.csv(z_off,'zoff.csv')
# write.csv(z_def,'zdef.csv')
beta =  as.vector(rep(1,length.out=2*nrow(data_sample)))
evnt_data = cbind(beta,even_offense_1,even_defe,z_off,z_def)



goal = which(data_sample[,5]=="GOAL")
goal_eve = evnt_data[c(goal,nrow(data_sample)+goal),]
goal_eve[is.na(goal_eve)] <- 0
require(glmnet)

set.seed(999)
cv.ridge <- cv.glmnet(evnt_data , y, alpha=0, parallel=TRUE, standardize=TRUE, type.measure='auc')

# Results
plot(cv.ridge)
cv.ridge$lambda.min
cv.ridge$lambda.1se
coef(cv.ridge, s=cv.ridge$lambda.min)

head(evnt_data)


#############################################################

even_offense_1 <-  read_csv("D:/Hockey Project/event_offense.csv")
even_offense_1[is.na(even_offense_1)]= 0 
save(even_offense_1,file="even_offense_1.Rda")
even_defe <- read_csv("D:/Hockey Project/event_defense.csv")
even_defe[is.na(even_defe)] = 0
save(even_defe,file="even_defe.Rda")
even_offense_1 <- even_offense_1[,-1]


load(even_defe, even_offense_1)

#############################################################

y1 = as.vector(1/(data_sample$Seconds_Elapsed))*3600

y0 = as.vector(rep(0,length.out=nrow(data_sample)))

y = c(y1,y0)
yinf = which(y==Inf)
y = y[-yinf ]
#y= ifelse(y==Inf, 0,y)
zoff_1 = which(data_sample$Event=="FAC" & data_sample$Ev_Zone == "Off")
zdef_1 = which(data_sample$Event=="FAC" & data_sample$Ev_Zone == "Def")

zoff = as.vector(rep(1, length.out=nrow(data_sample)))
zoff[-zoff_1] = 0
zdef = as.vector(rep(1, length.out=nrow(data_sample)))
zdef[-zdef_1] = 0

z_off = as.vector(rbind(zoff,zoff))
z_def = as.vector(rbind(zdef,zdef))
event_off <- event_data_1
event_def = event_data_2

# even_offense_2 <- rbind(even_offense_1,even_defe)
# even_defe_1 <- rbind(event_defe,event_offense_1)
# event_data_1= event_data_1[,-c(1:7)]
# event_data_2= event_data_2[,-c(1:7)]
# write.csv(even_offense_1,'event_offense.csv')
# write.csv(even_defe,'event_defense.csv')
# write.csv(y,'y.csv')
# write.csv(y0,'y0.csv')
# write.csv(z_off,'zoff.csv')
# write.csv(z_def,'zdef.csv')
beta = y0 = as.vector(rep(1,length.out=2*nrow(data_sample)))
evnt_data = cbind(beta,even_offense_1,even_defe,z_off,z_def)
head(evnt_data)

goal = which(data_sample[,5]=="GOAL")
goal_eve = evnt_data[c(goal,nrow(data_sample)+goal),]
evnt_data[is.na(evnt_data)] <- 0
goal_eve <- goal_eve[,-2]
goal_eve <- lapply(goal_eve, function(x) as.numeric(as.character(x)))
goal_eve = as.matrix(goal_eve)
y11 = as.matrix(y[c(goal,nrow(data_sample)+goal)])
str(goal_eve)
require(glmnet)

set.seed(999)
#cv.ridge <- cv.glmnet(goal_eve, y[c(goal,nrow(data_sample)+goal)], alpha=0, parallel=TRUE, standardize=TRUE, type.measure='mse')
lambdas <- 10^seq(3, -2, by = -.1)

fit <- glmnet(goal_eve, y11, alpha = 0, lambda = lambdas)
summary(fit)
# Results
plot(cv.ridge)
cv.ridge$lambda.min
cv.ridge$lambda.1se
coef(cv.ridge, s=cv.ridge$lambda.min)


####
#goal = which(data_sample[,5]=="GOAL")
#goal_eve = evnt_data[c(goal,nrow(data_sample)+goal),]
rm(events,event_off,event_def,madata)
rm(even_defe,even_offense_1)
evnt_data[is.na(evnt_data)] <- 0
######evnt_data <- evnt_data[,-2]
#evnt_data <- lapply(evnt_data, function(x) as.numeric(as.character(x)))
#evnt_data = as.matrix(evnt_data)
y11 = as.matrix(y)
y11[is.na(y11)] <- 0
str(goal_eve)
require(glmnet)

set.seed(999)
na_index <- which(is.na(evnt_data))
y12 <- which(y11== Inf)
y11 = y11 
y11 = as.numeric(y11)
str(y11)
rm(even_defe, even_offense_1)
evnt_data = evnt_data[-yinf,]
cv.ridge <- cv.glmnet(evnt_data, y11, alpha=0, parallel=TRUE, type.measure='mae')
lambdas <- 10^seq(3, -2, by = -.1)

fit <- glmnet(evnt_data, y11, alpha = 0, lambda = lambdas)
summary(fit)
# Results
plot(cv.ridge)
cv.ridge$lambda.min
cv.ridge$lambda.1se
coeff = as.matrix(coef(cv.ridge, s=cv.ridge$lambda.min))
write.csv(coeff,'coeff11.csv')
colnames(players_list_uq) = c("name", "id")

last_coef = merge(coefid, players_list, by = "id", all.x = TRUE)
write.csv(players_list,'players_list.csv')
last_coef1 = unique(last_coef)
write.csv(last_coef1,'last_coef1.csv')
