#a basic function that returns the mean, median and standard deviation of a variable
explore<-function(x){
  data<-c("Mean"=mean(x, na.rm=TRUE),
          "Median"=median(x, na.rm =T), 
          "Standard Deviation" = sd(x, na.rm =T),
          "Length" = length(x))
  return(data)
}
library(RColorBrewer)
#read datasets
playing_time <- read.csv("player_playingtime.csv")
shooting <- read.csv("player_shooting.csv")
possession <- read.csv("player_possession.csv")

#clean data
playing_time <- na.omit(playing_time)
player_possession <- na.omit(player_possession)

#assign continents to each dataset
player_shooting$area <- ifelse(player_shooting$team=='United States'|player_shooting$team=='Canada'|player_shooting$team=='Mexico'|player_shooting$team=='Costa Rica', 'North America',
                     ifelse(player_shooting$team=="Brazil"|player_shooting$team=="Uruguary"|player_shooting$team=="Argentina"|player_shooting$team=="Ecuador"|player_shooting$team=="Uruguay","South America",
                     ifelse(player_shooting$team=="Senegal"|player_shooting$team=="Tunisia"|player_shooting$team=="Morocco"|player_shooting$team=="Cameroon"|player_shooting$team=="Ghana","Africa",
                     ifelse(player_shooting$team=="Korea Republic"|player_shooting$team=="Japan","East Asia",
                     ifelse(player_shooting$team=="Qatar"|player_shooting$team=="Saudi Arabia"|player_shooting$team=="IR Iran","Middle East",
                     ifelse(player_shooting$team=="Netherlands"|player_shooting$team=="England"|player_shooting$team=="Wales"|player_shooting$team=="Poland"|player_shooting$team=="France"|player_shooting$team=="Denmark"
                     |player_shooting$team=="Spain"|player_shooting$team=="Germany"|player_shooting$team=="Belgium"|player_shooting$team=="Croatia"|player_shooting$team=="Serbia"|player_shooting$team=="Switzerland"
                     |player_shooting$team=="Portugal", "Europe",
                     ifelse(player_shooting$team=="Australia","Oceania",NA)))))))  

player_playingtime$area <- ifelse(player_playingtime$team=='United States'|player_playingtime$team=='Canada'|player_playingtime$team=='Mexico'|player_playingtime$team=='Costa Rica', 'North America',
                        ifelse(player_playingtime$team=="Brazil"|player_playingtime$team=="Uruguary"|player_playingtime$team=="Argentina"|player_playingtime$team=="Ecuador"|player_playingtime$team=="Uruguay","South America",
                        ifelse(player_playingtime$team=="Senegal"|player_playingtime$team=="Tunisia"|player_playingtime$team=="Morocco"|player_playingtime$team=="Cameroon"|player_playingtime$team=="Ghana","Africa",
                        ifelse(player_playingtime$team=="Korea Republic"|player_playingtime$team=="Japan","East Asia",
                        ifelse(player_playingtime$team=="Qatar"|player_playingtime$team=="Saudi Arabia"|player_playingtime$team=="IR Iran","Middle East",
                        ifelse(player_playingtime$team=="Netherlands"|player_playingtime$team=="England"|player_playingtime$team=="Wales"|player_playingtime$team=="Poland"|player_playingtime$team=="France"|player_playingtime$team=="Denmark"
                        |player_playingtime$team=="Spain"|player_playingtime$team=="Germany"|player_playingtime$team=="Belgium"|player_playingtime$team=="Croatia"|player_playingtime$team=="Serbia"|player_playingtime$team=="Switzerland"
                        |player_playingtime$team=="Portugal", "Europe",
                        ifelse(player_playingtime$team=="Australia","Oceania",NA)))))))

player_possession$area <- ifelse(player_possession$team=='United States'|player_possession$team=='Canada'|player_possession$team=='Mexico'|player_possession$team=='Costa Rica', 'North America',
                          ifelse(player_possession$team=="Brazil"|player_possession$team=="Uruguary"|player_possession$team=="Argentina"|player_possession$team=="Ecuador"|player_possession$team=="Uruguay","South America",
                          ifelse(player_possession$team=="Senegal"|player_possession$team=="Tunisia"|player_possession$team=="Morocco"|player_possession$team=="Cameroon"|player_possession$team=="Ghana","Africa",
                          ifelse(player_possession$team=="Korea Republic"|player_possession$team=="Japan","East Asia",
                          ifelse(player_possession$team=="Qatar"|player_possession$team=="Saudi Arabia"|player_possession$team=="IR Iran","Middle East",
                          ifelse(player_possession$team=="Netherlands"|player_possession$team=="England"|player_possession$team=="Wales"|player_possession$team=="Poland"|player_possession$team=="France"|player_possession$team=="Denmark"
                          |player_possession$team=="Spain"|player_possession$team=="Germany"|player_possession$team=="Belgium"|player_possession$team=="Croatia"|player_possession$team=="Serbia"|player_possession$team=="Switzerland"
                          |player_possession$team=="Portugal", "Europe",
                          ifelse(player_possession$team=="Australia","Oceania",NA)))))))

#TESTING 1:
#null hypothesis: Players from the Middle East and Africa have similar minutes_per_game as players from North America, South America, Europe, East Asia, Oceania
#alt hypothesis: Players from the Middle East and Africa have higher minutes_per_game than players from North America, South America, Europe, East Asia, Oceania
         
#step - 1 subset the data
ame_playtime <-subset(player_playingtime,player_playingtime$area=="Middle East" | player_playingtime$area=="Africa")
non_ame_playtime <-subset(player_playingtime,player_playingtime$area!="Middle East" && player_playingtime$area!="Africa")

#step - 2 calculate z scores
ame_playtime_explore <- explore(ame_playtime$minutes_per_game)
ame_playtime_explore
non_ame_playtime_explore <- explore(non_ame_playtime$minutes_per_game)
sd_ame_playtime <- sqrt(ame_playtime_explore[3]^2/ame_playtime_explore[4] +non_ame_playtime_explore[3]^2/non_ame_playtime_explore[4])

z_score <- (non_ame_playtime_explore[1]-ame_playtime_explore[1])/sd_ame_playtime
z_score

#step - 3 calculate p value

1-pnorm(z_score) 
#the p-value is 0.4673842 
#since it is greater than 0.05, we cannot reject the null hypothesis
#CONCLUSION: the area of the world the players come from does not seem to have impacted their playtime/ability to play under the 
#hot temperatures of Qatar

#plot avg number of unused subs per geo region
range1 <- tapply(player_playingtime$unused_subs, player_playingtime$area, mean)
barplot(range1, xlab = "Geographic Region of Teams", ylab = "Avg Number of Unused Subs", main = "Barplot of Unused Subs Based on Geographic Regions of Teams",col=brewer.pal(7,"PRGn"))

#plot sum of goals per geo region
range2 <- tapply(player_shooting$goals, player_shooting$area, mean)
barplot(range2, xlab = "Geographic Region of Teams", ylab = "Average of Number of Goals", main="Barplot of Goals Based on Geographic Regions of Teams",col=brewer.pal(7,"PRGn"))

#plot sum of touches per geo region
range3 <- tapply(player_possession$touches, player_possession$area, mean)
barplot(range3, xlab = "Geographic Region of Teams", ylab = "Average of Number Touches", main="Barplot of Touches Based on Geographic Regions of Teams",col=brewer.pal(7,"PRGn"))

