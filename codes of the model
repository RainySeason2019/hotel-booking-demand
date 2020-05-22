suppressMessages(library(tidyr))
suppressMessages(library(tidymodels))
suppressMessages(library(reshape2))
suppressMessages(library(skimr))
suppressMessages(library(nnet))
suppressMessages(library(GGally))
suppressMessages(library(corrplot))
suppressMessages(library(pROC))
suppressMessages(library(plotROC))
suppressMessages(library(e1071))
suppressMessages(library(rpart))
suppressMessages(library(RColorBrewer))
suppressMessages(library(parsnip))
suppressMessages(library(kknn))



hotel = read.csv('hotels.csv')
hotel = as_tibble(hotel)
#head(hotel)
#glimpse(hotel)
#dim(hotel)
#skim(hotel)


hotel = filter(hotel, is_canceled == 0)
#head(hotel)



#is.na(hotel)
which(is.na(hotel), arr.ind = T) # return the rows and columns that have missing values
#which(is.na(hotel), arr.ind = T)


hotel <- hotel %>%
  mutate(
    meal = case_when(
      meal == "SC" ~ "none",
      meal == "Undefined" ~ "none",
      meal == "HB" ~ "meal",
      meal == "FB" ~ "meal",
      meal == "BB" ~ "meal",
      TRUE ~ as.character(meal)
    ),
    required_car_parking_spaces = case_when(
      required_car_parking_spaces > 0 ~ "parking",
      TRUE ~ "none"
    ),
    children = case_when(
      children > 0 ~ "children",
      TRUE ~ "none"
    ),
    babies = case_when(
      babies > 0 ~ "babies",
      TRUE ~ "none"
    )
  )
distinct(hotel, meal)
distinct(hotel, required_car_parking_spaces)
distinct(hotel, children)
distinct(hotel, babies)



#hotel %>% count(children)
#hotel %>% count(babies)


hotel_ml = hotel  %>%
  select(
    children, babies, hotel, arrival_date_month, meal, adr, adults,
    required_car_parking_spaces, total_of_special_requests,
    stays_in_week_nights, stays_in_weekend_nights
  ) %>%
  mutate_if(is.character, factor)



#glimpse(hotel_ml)
#str(hotel_ml)
#head(hotel_ml)



#set.seed(123)
#train_data = sample_frac(hotel_ml, 0.8)
#test_data = sample_frac(hotel_ml, 0.2)


# another expression
set.seed(123)
spilt = initial_split(hotel_ml)
train_data = training(spilt)
test_data = testing(spilt)

model_recipe <- recipe(meal ~ ., data = train_data) %>%
  step_downsample(meal) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>%
  prep()
model_recipe

test_proc = bake(model_recipe, new_data = test_data)
test_proc

# Logistic regression
logit_ml <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")
logit_fit <- logit_ml %>% fit(meal ~., data = juice(model_recipe))
logit_fit

## get prediction on train set
pred_logit_train <- predict(logit_fit, new_data = juice(model_recipe))
## get prediction on test set
pred_logit_test <- predict(logit_fit, new_data = juice(model_recipe))
## get probabilities on test set
prob_logit_test <- predict(logit_fit, new_data = juice(model_recipe), type="prob")

prob_logit_test

# KNN
knn_ml <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")
knn_fit <- knn_ml %>% fit(meal ~ ., data = juice(model_recipe))
knn_fit

# Decision tree
tree_ml <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")
tree_fit <- tree_ml %>% fit(meal ~ ., data = juice(model_recipe))
tree_fit

# Validation
set.seed(123)
validation_splits <- mc_cv(juice(model_recipe), prop = 0.9, strata = meal)
validation_splits

validation_ml <- function(ml, data){
  y <- fit_resamples(
    meal ~ .,
    ml,
    resamples = data,
    control = control_resamples(save_pred = TRUE)
  )
  return(y)
}

logit_res <- validation_ml(logit_ml, validation_splits)
logit_res %>% collect_metrics()
## A tibble: 2 x 5
##.metric  .estimator  mean     n std_err
##<chr>    <chr>      <dbl> <int>   <dbl>
##1 accuracy binary     0.663    25 0.00231
##2 roc_auc  binary     0.700    25 0.00295

knn_res <- validation_ml(knn_ml, validation_splits)
knn_res %>% collect_metrics()
## A tibble: 2 x 5
##.metric  .estimator  mean     n std_err
##<chr>    <chr>      <dbl> <int>   <dbl>
##1 accuracy binary     0.756    25 0.00287
##2 roc_auc  binary     0.823    25 0.00273

tree_res <- validation_ml(tree_ml, validation_splits)
tree_res %>% collect_metrics()
## A tibble: 2 x 5
##.metric  .estimator  mean     n std_err
##<chr>    <chr>      <dbl> <int>   <dbl>
##1 accuracy binary     0.681    25 0.00364
##2 roc_auc  binary     0.720    25 0.00486

# Visualize the results



logit_res %>%
  unnest(.predictions) %>%
  mutate(model = "glm") %>%
  bind_rows(knn_res %>%
              unnest(.predictions) %>%
              mutate(model = "kknn")) %>%
  bind_rows(tree_res %>%
              unnest(.predictions) %>%
              mutate(model = "rpart")) %>%
  group_by(model) %>%
  roc_curve(meal, .pred_meal) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  geom_line(size = 1) +
  geom_abline(
    lty = 2, alpha = 0.7,
    size = 1
  )




# confusion matrix
confusion_mat <- function(x){
  y <- x %>%
    unnest(.predictions) %>%
    conf_mat(meal, .pred_class)
  return(y)
}

logit_cm <- confusion_mat(logit_res)
logit_cm
##Truth
##Prediction  meal  none
##meal  7874  3302
##none  6151 10723
##logit_cm %>% autoplot()

knn_cm <- confusion_mat(knn_res)
knn_cm
##Truth
##Prediction  meal  none
##meal 10174  3002
##none  3851 11023
knn_cm %>% autoplot()
#autoplot(knn_cm)

tree_cm <- confusion_mat(tree_res)
tree_cm
##Truth
##Prediction  meal  none
##meal  8749  3674
##none  5276 10351
tree_cm %>% autoplot()

# test
test_proc = bake(model_recipe, new_data = test_data)
test_proc

test_auc <- function(fit, data_proc, data){
  y <- fit %>%
    predict(new_data = data_proc, type = "prob") %>%
    mutate(truth = data) %>%
    roc_auc(truth, .pred_meal)
  return(y)
}

logit_test <- test_auc(logit_fit, test_proc, test_data$meal)
logit_test
## A tibble: 1 x 3
##.metric .estimator .estimate
##<chr>   <chr>          <dbl>
##1 roc_auc binary         0.716

knn_test <- test_auc(knn_fit, test_proc, test_data$meal)
knn_test
## A tibble: 1 x 3
##.metric .estimator .estimate
##<chr>   <chr>          <dbl>
##1 roc_auc binary         0.828

tree_test <- test_auc(tree_fit, test_proc, test_data$meal)
tree_test
## A tibble: 1 x 3
##.metric .estimator .estimate
##<chr>   <chr>          <dbl>
##1 roc_auc binary         0.728

#rpart.plot::rpart.plot(tree_fit$fit, type = 4, extra = 0, branch.lty = 1, nn = TRUE, roundint = FALSE)

## caculate TPR and TNR
rate <- function(cm){
  truth = matrix(unlist(cm), ncol = 2, byrow = FALSE)
  TPR = truth[1, 1] / (apply(truth, 1, sum)[1])
  TNR = truth[2, 2] / apply(truth, 1, sum)[2]
  y = list(TPR, TNR)
  return(y)
}

logit_rate = rate(logit_cm)
logit_rate
# 0.7045455 0.6354747

knn_rate = rate(knn_cm)
knn_rate
# 0.7721615 0.7410918

tree_rate = rate(tree_cm)
tree_rate
# 0.7042582 0.6623792

prob_knn_test <- predict(knn_fit, new_data = juice(model_recipe), type="prob")
prob_logit_test

