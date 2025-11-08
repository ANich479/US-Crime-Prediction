#데마 플젝 범죄율 회귀

rm(list=ls())

data <- read.csv("communities.data", header = FALSE, na.strings = "?")
colnames(data) <- c(
  "state", "county", "community", "communityname", "fold",
  "population", "householdsize", "racepctblack", "racePctWhite", "racePctAsian",
  "racePctHisp", "agePct12t21", "agePct12t29", "agePct16t24", "agePct65up",
  "numbUrban", "pctUrban", "medIncome", "pctWWage", "pctWFarmSelf",
  "pctWInvInc", "pctWSocSec", "pctWPubAsst", "pctWRetire", "medFamInc",
  "perCapInc", "whitePerCap", "blackPerCap", "indianPerCap", "AsianPerCap",
  "OtherPerCap", "HispPerCap", "NumUnderPov", "PctPopUnderPov", "PctLess9thGrade",
  "PctNotHSGrad", "PctBSorMore", "PctUnemployed", "PctEmploy", "PctEmplManu",
  "PctEmplProfServ", "PctOccupManu", "PctOccupMgmtProf", "MalePctDivorce", "MalePctNevMarr",
  "FemalePctDiv", "TotalPctDiv", "PersPerFam", "PctFam2Par", "PctKids2Par",
  "PctYoungKids2Par", "PctTeen2Par", "PctWorkMomYoungKids", "PctWorkMom", "NumIlleg",
  "PctIlleg", "NumImmig", "PctImmigRecent", "PctImmigRec5", "PctImmigRec8",
  "PctImmigRec10", "PctRecentImmig", "PctRecImmig5", "PctRecImmig8", "PctRecImmig10",
  "PctSpeakEnglOnly", "PctNotSpeakEnglWell", "PctLargHouseFam", "PctLargHouseOccup", "PersPerOccupHous",
  "PersPerOwnOccHous", "PersPerRentOccHous", "PctPersOwnOccup", "PctPersDenseHous", "PctHousLess3BR",
  "MedNumBR", "HousVacant", "PctHousOccup", "PctHousOwnOcc", "PctVacantBoarded",
  "PctVacMore6Mos", "MedYrHousBuilt", "PctHousNoPhone", "PctWOFullPlumb", "OwnOccLowQuart",
  "OwnOccMedVal", "OwnOccHiQuart", "RentLowQ", "RentMedian", "RentHighQ",
  "MedRent", "MedRentPctHousInc", "MedOwnCostPctInc", "MedOwnCostPctIncNoMtg", "NumInShelters",
  "NumStreet", "PctForeignBorn", "PctBornSameState", "PctSameHouse85", "PctSameCity85",
  "PctSameState85", "LemasSwornFT", "LemasSwFTPerPop", "LemasSwFTFieldOps", "LemasSwFTFieldPerPop",
  "LemasTotalReq", "LemasTotReqPerPop", "PolicReqPerOffic", "PolicPerPop", "RacialMatchCommPol",
  "PctPolicWhite", "PctPolicBlack", "PctPolicHisp", "PctPolicAsian", "PctPolicMinor",
  "OfficAssgnDrugUnits", "NumKindsDrugsSeiz", "PolicAveOTWorked", "LandArea", "PopDens",
  "PctUsePubTrans", "PolicCars", "PolicOperBudg", "LemasPctPolicOnPatr", "LemasGangUnitDeploy",
  "LemasPctOfficDrugUn", "PolicBudgPerPop", "ViolentCrimesPerPop"
)
head(data)
dim(data) #1994 128

colSums(is.na(data))

missing_vars <- colSums(is.na(data))
missing_vars[missing_vars>0]
#일부 도시에서는 아예 치안 리소스 관련 정보가 없어서 통째로 결측이 됨

#state, community name, county code, community code 예측에 의미 없음

# 1. 변수별 결측률 계산
missing_pct_vars <- colSums(is.na(data)) / nrow(data)

# 2. 결측률 20% 이상인 변수 추출
vars_to_remove <- names(missing_pct_vars[missing_pct_vars >= 0.2])

# 3. 데이터에서 해당 변수 제거
data_cleaned <- data[, !(names(data) %in% vars_to_remove)]

# OtherPerCap이 NA인 행 제거
data_cleaned <- data_cleaned[!is.na(data_cleaned$OtherPerCap), ]

# 제거된 변수 확인
vars_to_remove

# 정제된 데이터 구조 확인
str(data_cleaned)

# 정제 후 결측 변수 다시 확인
colSums(is.na(data_cleaned))[colSums(is.na(data_cleaned)) > 0]

dim(data_cleaned) #1993 104

#lasso로 변수선택 및 회귀
library(glmnet)

# x: 독립변수 (matrix), y: 종속변수 (벡터)
x <- model.matrix(ViolentCrimesPerPop ~ ., data = data_cleaned)[, -1]  # 첫 번째 열 제거 (intercept)
y <- data_cleaned$ViolentCrimesPerPop

set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1, standardize = TRUE)

#plot(cv.lasso)
cv.lasso$lambda.min   # 최소 MSE를 주는 lambda: 0.00728
cv.lasso$lambda.1se   # 1-SE 기준 간단한 모델을 위한 lambda: 0.0153

# 최소 MSE 기준으로 선택된 변수
coef_min <- coef(cv.lasso, s = "lambda.min")
selected_vars <- rownames(coef_min)[which(coef_min != 0)][-1]  # (Intercept 제외)
selected_vars
length(selected_vars)  # 선택된 변수 수: 89개


# 1-SE 기준으로 선택된 변수
coef_1se <- coef(cv.lasso, s = "lambda.1se")
selected_vars_1se <- rownames(coef_1se)[which(coef_1se != 0)][-1]  # (Intercept 제외)
selected_vars_1se
length(selected_vars_1se) #15개

cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.min]     # MSE at lambda.min
cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se]     # MSE at lambda.1se


vars_to_remove_additional <- c(
  "state",
  "communitynameLaCanadaFlintridgecity",
  "communitynameVernoncity",
  "communitynameWestHollywoodcity"
)

final_vars_clean <- setdiff(selected_vars_1se, vars_to_remove_additional)

# 이 변수들과 타겟만 추출
data_final <- data_cleaned[, c(final_vars_clean, "ViolentCrimesPerPop")]

# 확인
str(data_final)
length(final_vars_clean)  # → 최종 변수 수: 11
final_vars_clean
dim(data_final) #1993 12
summary(data_final) #이미 정규화 되어있음

# 데이터 분할
set.seed(123)
train_idx <- sample(nrow(data_final), 0.7 * nrow(data_final))
train <- data_final[train_idx, ]
test <- data_final[-train_idx, ]

#------------------------------------------------
#KNN
#-------------------------------------------------
#library(caret)
library(FNN)
set.seed(123)
# 독립변수와 종속변수 분리
x_train <- train[, -which(names(train) == "ViolentCrimesPerPop")]
y_train <- train$ViolentCrimesPerPop

x_test <- test[, -which(names(test) == "ViolentCrimesPerPop")]
y_test <- test$ViolentCrimesPerPop

#k값 튜닝
# k 후보값들
k_values <- 1:50
rmse_list <- numeric(length(k_values))

# 각 k에 대해 RMSE 계산
for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_fit <- knn.reg(train = x_train, test = x_test, y = y_train, k = k)
  y_pred <- knn_fit$pred
  rmse_list[i] <- sqrt(mean((y_test - y_pred)^2))
}

# 최적 k 출력
best_k <- k_values[which.min(rmse_list)]
cat("최적 k:", best_k, "→ 최소 RMSE:", round(min(rmse_list), 4), "\n")
#k=21, 0.1296

# KNN 회귀 모델 학습 및 예측
knn_fit <- knn.reg(train = x_train, test = x_test, y = y_train, k = best_k)

# 예측값
y_pred <- knn_fit$pred

# 성능 평가
rmse <- sqrt(mean((y_test - y_pred)^2))
r2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

#결과 요약
cat("최적 k =", best_k, 
    "\nRMSE =", round(rmse, 4), 
    "\nR² =", round(r2, 4), "\n")
#RMSE = 0.1296
#R^2 = 0.6782

# 결과 시각화
plot(k_values, rmse_list, type = "b", pch = 19,
     xlab = "k (이웃 수)", ylab = "RMSE", ylim = c(0.13, 0.20), main = "k값에 따른 RMSE 변화")

best_rmse <- min(rmse_list)

# ▶ 최적 k 지점 강조 표시
points(best_k, best_rmse, col = "red", pch = 19, cex = 1.2)  # 붉은 점 추가
text(best_k, best_rmse, labels = paste0("k = ", best_k),
     pos = 3, offset = 0.8, col = "red", cex = 1.1)  # 텍스트 레이블 (위쪽)

abline(v = best_k, col = "red", lty = 2)  # 최적 k에 세로선

#caret 패키지 이용
#knn_model <- train(
#  ViolentCrimesPerPop ~ .,
#  data = train,
#  method = "knn",
#  trControl = trainControl(method = "cv", number = 10),
#  tuneLength = 10
#) #number에 따라 최적값 바뀔 수 있음

# 최적 k 확인
#knn_model$bestTune #23

# 예측 성능 확인
#knn_model$results

# 결과 확인
#print(knn_model)

# 예측
#knn_pred <- predict(knn_model, newdata = test)

#knn_rmse <- sqrt(mean((test$ViolentCrimesPerPop - knn_pred)^2))
#knn_r2 <- 1 - sum((test$ViolentCrimesPerPop - knn_pred)^2) / sum((test$ViolentCrimesPerPop - mean(test$ViolentCrimesPerPop))^2)

#cat("KNN 회귀 - RMSE:", knn_rmse, ", R²:", knn_r2, "\n")

#---------------------------------
#linear regression
#---------------------------------
# 선형 회귀 모델 학습
lm_model <- lm(ViolentCrimesPerPop ~ ., data = train)

# 예측
lm_pred <- predict(lm_model, newdata = test)

# 성능 평가
lm_rmse <- sqrt(mean((test$ViolentCrimesPerPop - lm_pred)^2))
lm_r2 <- 1 - sum((test$ViolentCrimesPerPop - lm_pred)^2) / sum((test$ViolentCrimesPerPop - mean(test$ViolentCrimesPerPop))^2)

cat("선형 회귀 - RMSE:", round(lm_rmse,4), ", R²:", round(lm_r2,4), "\n")
#RMSE: 0.1267, R^2: 0.6925

#선형회귀 계수 해석
summary(lm_model) #pctvacantboarded 만 p값 유의하지 않음

#회귀계수 요약
library(broom)
library(dplyr)
tidy(lm_model) %>% arrange(p.value) %>% head(12)

#결과 시각화
plot(lm_pred, test$ViolentCrimesPerPop,
     xlab = "예측값 (Predicted)", ylab = "실제값 (Observed)",
     main = "선형 회귀 예측 vs 실제")
abline(0, 1, col = "red", lty = 2)


#plot(lm_model)

# 예측값과 잔차 산점도 (등분산성 문제 없음)
plot(lm_model$fitted.values, lm_model$residuals)
abline(h = 0, col = "red")

library(ggplot2)
# 계수 추출 & 정렬
top_terms_abs <- tidy(lm_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(abs_estimate = abs(estimate)) %>%
  arrange(desc(abs_estimate)) %>%
  slice(1:5)

# 시각화
ggplot(top_terms_abs, aes(x = reorder(term, abs_estimate), y = estimate)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "절댓값 기준 영향력 큰 변수 Top 5",
       x = "변수", y = "회귀계수")

#---------------------------
#regression Tree
#---------------------------
library(rpart)
library(rpart.plot)

set.seed(123)
#train <- train[order(rownames(train)), ]
#도대체 왜 계속 결과가 바뀔까요.. 시드 값도 정렬도 다 해줘도 실행할 때마다 바뀌네..

# 회귀 트리 모델 학습 (cp기본 값으로 적합)
tree_model_base <- rpart(ViolentCrimesPerPop ~ ., data = train, method = "anova")
tree_model <- rpart(ViolentCrimesPerPop ~ ., data = train, method = "anova",control = rpart.control(cp = 0.001))

#cp 테이블 확인
printcp(tree_model)

#최적 cp 값 선택 
opt_cp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"]
cat("선택된 최적 cp:", opt_cp, "\n") #0.0065

#1se 기준 cp
cp_1se <- tree_model$cptable[which(tree_model$cptable[,"xerror"] <= min(tree_model$cptable[,"xerror"]) + tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "xstd"]), "CP"][1]
cat("선택된 1se cp:", cp_1se, "\n") #0.0263

#가지치기
pruned_tree <- prune(tree_model, cp = opt_cp)
pruned_tree_simple <- prune(tree_model, cp = cp_1se)

#트리 시각화
rpart.plot(tree_model)
rpart.plot(pruned_tree)
rpart.plot(pruned_tree_simple, cex = 0.6)

# 예측 및 성능 평가
#기본 모델
tree_pred <- predict(tree_model_base, newdata = test)
tree_rmse <- sqrt(mean((test$ViolentCrimesPerPop - tree_pred)^2))
tree_r2 <- 1 - sum((test$ViolentCrimesPerPop - tree_pred)^2) / sum((test$ViolentCrimesPerPop - mean(test$ViolentCrimesPerPop))^2)

cat("기본 모델 - RMSE:", round(tree_rmse,4), ", R²:", round(tree_r2,4), "\n")
#RMSE: 0.1453, R^2: 0.5957

# --- 최적 cp로 가지치기한 모델 ---
tree_pred_opt <- predict(pruned_tree, newdata = test)
tree_rmse_opt <- sqrt(mean((test$ViolentCrimesPerPop - tree_pred_opt)^2))
tree_r2_opt <- 1 - sum((test$ViolentCrimesPerPop - tree_pred_opt)^2) / sum((test$ViolentCrimesPerPop - mean(test$ViolentCrimesPerPop))^2)

cat("최적 cp 모델 - RMSE:", round(tree_rmse_opt,4), ", R²:", round(tree_r2_opt,4), "\n")
#RMSE: 0.1438, R^2: 0.6039

# --- 1SE 기준 가지치기한 모델 ---
tree_pred_1se <- predict(pruned_tree_simple, newdata = test)
tree_rmse_1se <- sqrt(mean((test$ViolentCrimesPerPop - tree_pred_1se)^2))
tree_r2_1se <- 1 - sum((test$ViolentCrimesPerPop - tree_pred_1se)^2) / sum((test$ViolentCrimesPerPop - mean(test$ViolentCrimesPerPop))^2)

cat("1SE cp 모델 - RMSE:", round(tree_rmse_1se,4), ", R²:", round(tree_r2_1se,4), "\n")
#RMSE: 0.1519, R^2: 0.5581


plotcp(tree_model)  # cp vs xerror 그래프

# 1SE 기준선 그리기 (점선, 빨간색)
abline(
  h = min(tree_model$cptable[,"xerror"]) + tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "xstd"],
  col = "red", lty = 2
)

# 선택된 1SE cp 위치 찾기
cp_1se <- tree_model$cptable[
  which(tree_model$cptable[,"xerror"] <= min(tree_model$cptable[,"xerror"]) + tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "xstd"]),
  "CP"
][1]

# 그 cp의 위치(인덱스)를 찾아서 수직선 추가 (파란 점선)
cp_index <- which(tree_model$cptable[,"CP"] == cp_1se)
abline(v = cp_index, col = "blue", lty = 2)

# 최적 cp 계산 (xerror가 가장 낮은 지점)
opt_cp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"]

# 해당 cp의 인덱스 (몇 번째 줄인지)
opt_index <- which(tree_model$cptable[,"CP"] == opt_cp)

# 수직선 추가 (파란 점선은 이미 1SE니까 다른 색으로 예: 초록)
abline(v = opt_index, col = "darkgreen", lty = 2)

# 변수 중요도 시각화
# 변수 중요도 추출 (train() 함수 없이 직접 추출)
imp_df <- data.frame(
  Variable = names(pruned_tree_simple$variable.importance),
  Importance = pruned_tree_simple$variable.importance
)

# 내림차순 정렬
imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]

# 시각화
ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance (Tree Model)", x = "Variable", y = "Importance")


#-----------------------------
#random forest
#-----------------------------
library(randomForest)

#하이퍼파라미터 튜닝
#mtry
tuneRF(
  x = train[ , setdiff(names(train), "ViolentCrimesPerPop")],
  y = train$ViolentCrimesPerPop,
  ntreeTry = 500,
  stepFactor = 1.5,
  improve = 0.01
)

# 학습
set.seed(123)
rf_model <- randomForest(
  ViolentCrimesPerPop ~ ., 
  data = train,
  importance = TRUE,   # 변수 중요도 저장
  ntree = 500,         # 기본값이지만 명시적으로 500그루 사용
  mtry = 2,
  nodesize = 3         #(회귀)기본값 5부터 1까지 실험 결과 3에서 RMSE 최소
)

# 예측
rf_pred <- predict(rf_model, newdata = test)

# 성능 평가
rf_rmse <- sqrt(mean((test$ViolentCrimesPerPop - rf_pred)^2))
rf_r2 <- 1 - sum((test$ViolentCrimesPerPop - rf_pred)^2) / sum((test$ViolentCrimesPerPop - mean(test$ViolentCrimesPerPop))^2)

cat("Random Forest - RMSE:", rf_rmse, ", R²:", round(rf_r2,4), "\n")
#rmse: 0.1265 R^2: 0.6936


varImpPlot(rf_model)

#전체 성능 정리
results <- data.frame(
  Model = c("KNN Regression", "Linear Regression", "Regression Tree", "Random Forest"),
  RMSE = c(rmse, lm_rmse, tree_rmse, rf_rmse),
  R2 = c(r2, lm_r2, tree_r2, rf_r2)
)
print(results)

# 랜덤 포레스트 예측 vs 실제 비교
plot(rf_pred, y_test,
     xlab = "예측값 (Predicted)",
     ylab = "실제값 (Observed)",
     main = "랜덤 포레스트 예측 VS 실제",
     xlim = c(0,1),
     pch = 1, col = "darkgreen")
abline(0, 1, lty = 2, col = "red")  # 45도 기준선


#모델의 과소예측 이유?
library(ggplot2)

y_df <- data.frame(ViolentCrimesPerPop = y)

# 히스토그램 + 밀도곡선
ggplot(y_df, aes(x = ViolentCrimesPerPop)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "steelblue", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "폭력 범죄율 분포", x = "ViolentCrimesPerPop", y = "밀도") +
  theme_minimal()
#분포 불균형
