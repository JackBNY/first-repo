#March Madness 2026 XGBoost Model

#Step One: Insert My Data
df <- AllNewData %>%
  select(-season, -alookup, -blookup, -team_a, -team_b)
View(df)

#Step Two: Turn my categorical variables into factors.
all_confs <- unique(c(df$ConfA, df$ConfB))

df$ConfA <- factor(df$ConfA, levels = all_confs)
df$ConfB <- factor(df$ConfB, levels = all_confs)

df$Round <- factor(df$round, 
                   levels = c("FF","R64", "R32", "R16", "R8", "R4", "C"))


#Step Three: Add a upset column for further analysis later on.
df$upset <- ifelse(
  (df$seed_a > df$seed_b & df$teamA_win == 1) |
    (df$seed_b > df$seed_a & df$teamA_win == 0),
  1,
  0
)

library(dplyr)

#I want my model to be matchup based, so each team is entered into the model, and 
#it looks at the difference between my predictors.
model_df <- df %>%
  mutate(
    seed_diff      = seed_a - seed_b,
    wins_diff = WinsA - WinsB,
    confW_diff = ConfWA - ConfWB,
    Krating_diff = KratingA - KratingB,
    tempo_diff = tempo_a - tempo_b,
    luck_diff = luck_a -  luck_b,
    Heff_diff = HeffA - HeffB,
    Experience_diff = ExperienceA - ExperienceB,
    Continuity_diff = ContinuityA - ContinuityB,
    Bench_diff = BenchA - BenchB,
    efgO_diff = efg_oA - efg_ob,
    efgD_diff = efg_dA - efg_dB,
    TO_O_diff = TO_pctOA - TO_pctOB,
    TO_D_diff = TO_pctDA - TO_pctDB,
    OR_O_diff = OR_pctOA - OR_pctOB,
    OR_D_diff = OR_pctDA - OR_pctDB,
    FT_Orate_diff = FT_rateOA - FT_rateOB,
    FT_Drate_diff = FT_rateDA - FT_rateDB,
    FT_Odist_diff = FTdistOA - FTdistOB,
    FT_Ddist_diff = FTdistDA - FTdistDB,
    TwodistO_diff = TwodistOA - TwodistOB,
    TwodistD_diff = TwodistDA - TwodistDB,
    ThreedistO_diff = ThreedistOA -ThreedistOB,
    ThreedistD_diff = ThreedistDA - ThreedistDB,
    
  ) %>%
  select(round, ends_with("_diff"), teamA_win, ConfA, ConfB)

#Step Four: Flip my data frame so that there is no team A or team B bias.
flipped_df <- model_df %>%
  mutate(
    across(ends_with("_diff"), ~ -.),
    
    # temp copies
    ConferenceA_tmp = ConfA,
    
    # swap
    ConfA = ConfB,
    ConfB = ConferenceA_tmp,
    
    # drop temp
    ConferenceA_tmp = NULL,
    
    # flip target
    teamA_win = 1 - teamA_win,
  )

final_df <- bind_rows(model_df, flipped_df)


#Step Five: Turn my predictors into a matrix, which the X has to be in for XGBoost models.
X <- model.matrix(
  ~ round + ConfA + ConfB + . - teamA_win - 1,
  data = final_df
)


y_win <- final_df$teamA_win

library(xgboost)

#Step Six: Split my data into training and test data.

set.seed(123)

n <- nrow(X)
train_idx <- sample(1:n, size = 0.8 * n)

X_train <- X[train_idx, ]
X_test  <- X[-train_idx, ]

y_train <- y_win[train_idx]
y_test  <- y_win[-train_idx]

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test, label = y_test)


y_train <- factor(y_train, levels = c(0, 1), labels = c("Loss", "Win"))




library(caret)

#Step Seven: This is parameter tuning. Looking for the most efficient parameters
#to use for my model by first creating a grid and then using the validation metric 
#ROC to make sure that the model is using the parameter value that is best.
tune_grid <- expand.grid(
  nrounds          = c(100, 200, 500),
  max_depth        = c(3, 5, 7),
  eta              = c(0.01, 0.05, 0.1),
  gamma            = c(0, 1),
  colsample_bytree = c(0.6, 0.8, 1.0),
  min_child_weight = c(1, 5),
  subsample        = c(0.7, 1.0)
)

control <- trainControl(
  method    = "cv",       # k-fold cross validation
  number    = 5,          # 5 folds
  verboseIter = TRUE,
  classProbs  = TRUE,
  summaryFunction = twoClassSummary  # for binary classification
)

xgb_tuned <- train(
  x         = X_train,
  y         = y_train,
  method    = "xgbTree",
  trControl = control,
  tuneGrid  = tune_grid,
  metric    = "ROC"
)



# Best parameters
xgb_tuned$bestTune



nrow(X_train)

library(xgboost)

#Step Eight: Set the best parameters
best_params <- list(
  objective        = "binary:logistic",
  max_depth        = 3,
  eta              = 0.01,
  gamma            = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample        = 0.7
)

# Convert to DMatrix
dtrain <- xgb.DMatrix(data = X_train, label = as.numeric(y_train == "Win"))

#Step Nine: Train final model
final_model <- xgb.train(
  params  = best_params,
  data    = dtrain,
  nrounds = 200,
  verbose = 1
)

# Check feature importance
importance <- xgb.importance(model = final_model)
xgb.plot.importance(importance, top_n = 15)

# Evaluate on test set
dtest <- xgb.DMatrix(data = X_test, label = as.numeric(y_test == "Win"))
test_preds <- predict(final_model, dtest)


library(pROC)

# Create ROC curve
roc_obj <- roc(y_test, test_preds)

# Plot it
plot(roc_obj, 
     print.thres = "best",    # marks the best threshold on the curve
     print.auc   = TRUE,      # shows AUC on plot
     main        = "ROC Curve - March Madness Model")

# Get the optimal threshold mathematically
best_thresh <- coords(roc_obj, "best", best.method = "youden")
print(best_thresh)

test_labels <- ifelse(test_preds > 0.4795446, "Win", "Loss")

# Re-run confusion matrix to confirm improvement
confusionMatrix(
  factor(test_labels,   levels = c("Loss", "Win")),
  factor(y_test_labels, levels = c("Loss", "Win")),
  positive = "Win"
)


#Test Model With New Data
create_matchup <- function(teamA, teamB, team_data, round_name) {
  
  # Get team rows
  A <- team_data[team_data$TeamName == teamA, ]
  B <- team_data[team_data$TeamName == teamB, ]
  
  # Safety check
  if (nrow(A) == 0 | nrow(B) == 0) {
    stop("One of the teams not found in team_data")
  }
  
  # Build matchup row
  matchup <- data.frame(
    
    TeamA = teamA,
    TeamB = teamB,
    
    # Differences
    seed_diff = A$seed - B$seed,
    wins_diff = A$W - B$W,
    confW_diff = A$ConfW - B$ConfW,
    Krating_diff = A$Krating - B$Krating,
    tempo_diff = A$tempo - B$tempo,
    luck_diff = A$luck - B$luck,
    Heff_diff = A$Heff - B$Heff,
    Experience_diff = A$Experience - B$Experience,
    Continuity_diff = A$Continuity - B$Continuity,
    Bench_diff = A$Bench - B$Bench,
    
    efgO_diff = A$efgO - B$efgO,
    efgD_diff = A$efgD - B$efgD,
    TO_O_diff = A$TO_O - B$TO_O,
    TO_D_diff = A$TO_D - B$TO_D,
    OR_O_diff = A$OR_O - B$OR_O,
    OR_D_diff = A$OR_D - B$OR_D,
    FT_Orate_diff = A$FT_Orate - B$FT_Orate,
    FT_Drate_diff = A$FT_Drate - B$FT_Drate,
    FT_Odist_diff = A$FT_Odist - B$FT_Odist,
    FT_Ddist_diff = A$FT_Ddist - B$FT_Ddist,
    TwodistO_diff = A$TwodistO - B$TwodistO,
    TwodistD_diff = A$TwodistD - B$TwodistD,
    ThreedistO_diff = A$ThreedistO - B$ThreedistO,
    ThreedistD_diff = A$ThreedistD - B$ThreedistD,
    
    # Categorical
    ConfA = A$Conf,
    ConfB = B$Conf,
    Round = round_name,
    
    stringsAsFactors = FALSE
  )
  
  return(matchup)
}

matchup <- as.data.frame(create_matchup("UMBC", "Howard", NewwwData, "R64"))


encode_matchup <- function(matchup, train_cols, conf_levels, round_levels) {
  
  row <- data.frame(matrix(0, nrow = 1, ncol = length(train_cols)))
  colnames(row) <- train_cols
  
  # Fill numeric columns
  numeric_cols <- intersect(train_cols, colnames(matchup))
  for (col in numeric_cols) {
    row[[col]] <- matchup[[col]]
  }
  
  # One-hot encode ConfA
  confA_col <- paste0("ConfA", as.character(matchup$ConfA))
  if (confA_col %in% train_cols) row[[confA_col]] <- 1
  
  # One-hot encode ConfB
  confB_col <- paste0("ConfB", as.character(matchup$ConfB))
  if (confB_col %in% train_cols) row[[confB_col]] <- 1
  
  # One-hot encode Round - match exact naming convention from training
  round_col <- paste0("round", as.character(matchup$Round))  # lowercase 'round'
  if (round_col %in% train_cols) row[[round_col]] <- 1
  
  # Force exact column order to match model
  X <- as.matrix(row[, model_cols, drop = FALSE])
  
  return(X)
}

X <- encode_matchup(matchup, train_cols, conf_levels, round_levels)

pred <- predict(final_model, xgb.DMatrix(X))
pred



get_win_prob <- function(teamA, teamB, data, round, threshold = 0.4795446) {
  matchup <- as.data.frame(create_matchup(teamA, teamB, data, round))
  X       <- encode_matchup(matchup, train_cols, conf_levels, round_levels)
  prob    <- predict(final_model, xgb.DMatrix(X))
  
  cat(teamA, "win probability:", round(prob, 4), "\n")
  cat(teamB, "win probability:", round(1 - prob, 4), "\n")
  cat("Predicted winner:", ifelse(prob > threshold, teamA, teamB), "\n")
}

#Get Predictions
get_win_prob("Duke", "Arizona", NewwwData, "C")








