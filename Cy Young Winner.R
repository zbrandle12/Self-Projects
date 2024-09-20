library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(car)
library(caret)
library(randomForest)
set.seed(123)

data = read_csv("/Users/zacharybrandle/Desktop/Self Projects/razzball.csv")
attach(data)
glimpse(data)

## I feel ERA is the first variable we will look at to predict the 2024 cy young
data$`Y!` = ifelse(data$GS > 0, "SP", "RP")

sp_data = data %>%
  filter(`Y!` == "SP" & IP >= 80)
train_index = createDataPartition(sp_data$ERA, p = 0.8, list = FALSE)
train_data = sp_data[train_index, ]
test_data = sp_data[-train_index, ]
model_train = lm(ERA ~ H + ER + K + BB + WHIP, data = train_data)
test_data$predicted_ERA = predict(model_train, test_data)
test_data = test_data %>% arrange(predicted_ERA) %>% mutate(rank = row_number())
head(test_data %>% select(Name, Team, predicted_ERA, rank))
train_predicted = predict(model1, train_data)
train_actual = train_data$ERA
test_predicted = predict(model1, test_data)
test_actual = test_data$ERA
summary(model1)$r.squared
summary(model1)$adj.r.squared
(rmse_train = sqrt(mean((train_actual - train_predicted)^2)))
(rmse_test = sqrt(mean((test_actual - test_predicted)^2)))

------------------------------------------------------=-----------
model = lm(ERA ~ G + GS + W + L + SV + HLD + IP + H + ER + HR + K + BB + FIP + WHIP + BABIP, data = sp_data)
model1 = lm(ERA~ H + ER + HR + K + BB + FIP + WHIP, data = sp_data)
summary(model)
summary(model1)
summary(model2)
plot(model1)
sp_data$predicted_ERA <- predict(model1, sp_data)
sp_data = sp_data %>%
  arrange(predicted_ERA) %>%
  mutate(rank = row_number())
head(sp_data %>% select(Name, Team, predicted_ERA, rank))
-------------------------------------------------------------------
  
model_rf = randomForest(ERA ~ H + ER + K + BB + WHIP, data = train_data, ntree = 500)
test_data$predicted_ERA_rf = predict(model_rf, test_data)
test_data = test_data %>%
  arrange(predicted_ERA_rf) %>%
  mutate(rank_rf = row_number())
         head(test_data %>% select(Name, Team, predicted_ERA_rf, rank_rf))
         rmse_rf_test = sqrt(mean((test_data$ERA - test_data$predicted_ERA_rf)^2))
         r_squared_rf_test = 1 - (sum((test_data$ERA - test_data$predicted_ERA_rf)^2) / 
                                     sum((test_data$ERA - mean(test_data$ERA))^2))
         
         rmse_test
         r_squared_rf_test         
         importance(model_rf)
         varImpPlot(model_rf)         
         test_data <- test_data %>%
           arrange(predicted_ERA_rf) %>%
           mutate(rank_rf = row_number())
         head(test_data %>% select(Name, Team, predicted_ERA_rf, rank_rf))
## we will now move onto the next variable which will be WHIP!
---------------------------------------------------------------------------
modelwhip = lm(WHIP~ G + GS + ERA + L + SV + HLD + IP + H + ER + HR + K + BB + FIP + W + BABIP, data = train_data)
summary(modelwhip)
modelwhip1 = lm(WHIP ~ ERA + IP + H + ER + BB + BABIP, data=train_data)
summary(modelwhip1)
rfwhip = randomForest(WHIP ~ ERA + IP + H + ER + BB + BABIP, data=train_data)
test_data$predicted_WHIP_rf = predict(rfwhip, test_data)
test_data = test_data %>%
  arrange(predicted_WHIP_rf) %>%
  mutate(rank_whip_rf = row_number())
head(test_data %>% select(Name, Team, predicted_WHIP_rf, rank_whip_rf))

head(test_data %>% select(Name, Team, rank_rf, rank_whip_rf))
test_data = test_data %>%
  mutate(total_rank = rank_rf  + rank_whip_rf)
test_data = test_data %>%
  arrange(total_rank)
head(test_data %>% select(Name, Team, total_rank, rank_rf, rank_whip_rf))
--------------------------------------------------------------------------------
##FIP is next!
modelfip = lm(FIP~ G + GS + ERA + L + SV + HLD + IP + H + ER + HR + K + BB + WHIP + W + BABIP, data = train_data)
summary(modelfip)
modelfip1 = lm(FIP~  GS + ERA + IP + ER + HR + K + BB , data = train_data)
summary(modelfip1)
rffip = randomForest(FIP~  GS + ERA + IP + ER + HR + K + BB , data = train_data)
test_data$predicted_FIP_rf = predict(rffip, test_data)
test_data = test_data %>%
  arrange(predicted_FIP_rf) %>%
  mutate(rank_fip_rf = row_number())
head(test_data %>% select(Name, Team, predicted_FIP_rf,rank_fip_rf))
test_data = test_data %>%
  mutate(total_rank = rank_rf  + rank_whip_rf+rank_fip_rf)
test_data = test_data %>%
  arrange(total_rank)
head(test_data %>% select(Name, Team, total_rank, rank_rf, rank_whip_rf,rank_fip_rf))
--------------------------------------------------------------------------------------------
## Lets do BABIP
modelbabip = lm(BABIP~ G + GS + ERA + L + SV + HLD + IP + H + ER + HR + K + BB + WHIP + W + FIP, data = train_data)
summary(modelbabip)
modelbabip1 = lm(BABIP~ IP + H  + HR + K + BB + WHIP, data = train_data)
rfbabip = randomForest(BABIP~ IP + H  + HR + K + BB + WHIP, data = train_data)
test_data$predicted_BABIP_rf = predict(rfbabip, test_data)
test_data = test_data %>%
  arrange(predicted_BABIP_rf) %>%
  mutate(rank_babip_rf = row_number())
head(test_data %>% select(Name, Team, predicted_BABIP_rf,rank_babip_rf))
test_data = test_data %>%
  mutate(total_rank = rank_rf  + rank_whip_rf+rank_fip_rf+rank_babip_rf)
test_data = test_data %>%
  arrange(total_rank)
head(test_data %>% select(Name, Team, total_rank, rank_rf, rank_whip_rf,rank_fip_rf, rank_babip_rf))
--------------------------------------
  train_data = train_data %>%
  mutate(K_per_9 = (K / IP) * 9)

test_data = test_data %>%
  mutate(K_per_9 = (K / IP) * 9)
modelk9 = lm(K_per_9~ G + GS + ERA + L + SV + HLD + IP + H + ER + HR + BABIP + BB + WHIP + W + FIP, data = train_data)
summary(modelk9)
modelk91 = lm(K_per_9 ~ ERA +ER + HR + BABIP + BB + WHIP +  FIP, data = train_data)
summary(modelk91)
rfbk9 = randomForest(K_per_9~ IP + H  + HR + K + BB + WHIP, data = train_data)
test_data$predicted_K_per_9_rf = predict(rfbk9, test_data)
test_data = test_data %>%
  arrange(desc(predicted_K_per_9_rf)) %>%
  mutate(rank_k9_rf = row_number())
head(test_data %>% select(Name, Team, predicted_K_per_9_rf,rank_k9_rf))
test_data = test_data %>%
  mutate(total_rank = rank_rf  + rank_whip_rf+rank_fip_rf+rank_babip_rf+rank_k9_rf)
test_data = test_data %>%
  arrange(total_rank)
head(test_data %>% select(Name, Team, total_rank, rank_rf, rank_whip_rf,rank_fip_rf, rank_babip_rf,rank_k9_rf))
----------------------------------------------
  train_data = train_data %>%
  mutate(BB_per_9 = (BB / IP) * 9)
  test_data = test_data %>%
  mutate(BB_per_9 = (BB / IP) * 9)
modelbb9 = lm(BB_per_9~ G + GS + ERA + L + SV + HLD + IP + H + ER + HR + BABIP + K + WHIP + W + FIP, data = train_data)
summary(modelbb9)
modelbb91 = lm(BB_per_9 ~  ERA + H + ER + HR + BABIP + K + WHIP +  FIP, data = train_data)
summary(modelbb91)
rfbb9 = randomForest(BB_per_9 ~  ERA + H + ER + HR + BABIP + K + WHIP +  FIP, data = train_data)
test_data$predicted_BB_per_9_rf = predict(rfbb9, test_data)
test_data = test_data %>%
  arrange((predicted_BB_per_9_rf)) %>%
  mutate(rank_bb9_rf = row_number())
head(test_data %>% select(Name, Team, predicted_BB_per_9_rf,rank_bb9_rf))
test_data = test_data %>%
  mutate(total_rank = rank_rf  + rank_whip_rf+rank_fip_rf+rank_babip_rf+rank_k9_rf+rank_bb9_rf)
test_data = test_data %>%
  arrange(total_rank)

head(test_data %>% select(Name, Team, total_rank, rank_rf, rank_whip_rf,rank_fip_rf, rank_babip_rf,rank_k9_rf,rank_bb9_rf))
# Now that my model is finished. THe updated rankings are as follows: Tarik Skubal is the favorite to win the AL cy young and Paul Skenes is the favorite for NL Cy young
# Thank you very much!