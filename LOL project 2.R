#League of Legends Win Predictor

rm(list=ls()) #removes all previously stored variables

#load ggplot
library(ggplot2)

#read in data
game_data <- read.csv("~/Desktop/R datasets/high_diamond_ranked_10min.csv")

#remove columns that have multicolinearity from dataset
game_data2 <- subset(game_data, select = -c(gameId, redKills, redTotalGold, redAvgLevel, redTotalExperience, 
                                           redTotalMinionsKilled, redTotalJungleMinionsKilled, redGoldDiff, redExperienceDiff, 
                                           redCSPerMin, redGoldPerMin, blueTotalGold, blueAvgLevel, blueTotalExperience, 
                                           blueTotalMinionsKilled, blueTotalJungleMinionsKilled, blueGoldDiff, blueExperienceDiff,
                                           blueCSPerMin, blueGoldPerMin, redFirstBlood, blueFirstBlood,
                                           redEliteMonsters, blueEliteMonsters, redDeaths))

#creating correlation matrix so data is prepared to plot
cormat <- round(cor(game_data2), 2)
head(cormat)

#utilizing reshape2 to melt the correlation matrix
install.packages("reshape2")
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

#plotting the matrix into a messy initial form
ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile()

#get lower triangle of correlation matrix
get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# calling previously made function
upper_tri <- get_upper_tri(cormat)
upper_tri


melted_cormat <- melt(upper_tri, na.rm = TRUE)

#plotting the triangle
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)
  coord_fixed()

# bar plot to see if the number of wins is equal for blue and red
ggplot(game_data, aes(x = blueWins)) +
  geom_bar(position = position_dodge(), fill = c('lightblue', 'pink'),
           width = 0.5)+
  geom_text(stat = 'count', 
            aes(label = stat(count)),
            position = position_dodge(width=1), vjust=-0.5)

#distribution of blue total experience
ggplot(game_data, aes(x = blueTotalExperience))+
  geom_density(fill = 'coral')


# create training and test set
training_test_split <- function(data, fraction = 0.8, train){
  total_rows = nrow(game_data2)
  train_rows = total_rows * fraction
  sample = 1:train_rows
  if (train == TRUE){
    return(data[sample, ])
  } else {
    return (data[-sample, ])
  }
    
}

training_set <- training_test_split(game_data2, 0.8, TRUE)
test_set <- training_test_split(game_data2, 0.8, FALSE)




# logistic regression model - has an error rate of 29.95%
logistic <- glm(blueWins ~ ., data = game_data2, family = "binomial")
prediction <- predict(logistic, newdata = test_set, type = "response")
prediction <- round(prediction, digits = 0)
t = table(paste("estim_", as.factor(prediction), sep = ""), paste("true_", as.factor(test_set$blueWins), sep = ""))
err = 100 - 100*(t[1,1] / (t[1,1] + t[1,2]) + t[1,1] / (t[2,1] + t[2,2])) / 2
print(paste("Linear Model error rate:", paste(round(err, digit = 2), "%")))

# make t (a table) into a data frame so we can use ggplot
t <- as.data.frame(t)
levels(t$Var1) <- c('predicted red win', 'predicted blue win')
levels(t$Var2) <- c('true red win', 'true blue win')

# double bar graph to show the model working
ggplot(t, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity")+
  scale_fill_discrete(name = "Team", labels = c('Red win', 'Blue win')) +
  labs(y = "Frequency", x = 'Estimated Team Wins')


#----------------------------------------------------------------------------------------


# random forest model - has an error rate of 32.49%
library(randomForest)
rf_classifier = randomForest(as.factor(blueWins) ~ ., 
                             data=training_set, ntree=500, mtry=7, importance=TRUE)
varImpPlot(rf_classifier)
res1 = predict(rf_classifier, test_set)
t = table(paste("estim_", as.factor(res1), sep = ""), paste("true_", as.factor(test_set$blueWins), sep = ""))
err = 100 - 100*(t[1,1] / (t[1,1] + t[1,2]) + t[1,1] / (t[2,1] + t[2,2])) / 2
print(paste("Random Forest error rate:", paste(round(err, digit = 2), "%")))















