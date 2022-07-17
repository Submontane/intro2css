library(tidyverse)

# 0-D Objects: Values
123
"abc"
T

# 1-D Objects: Vectors
1:10
seq(1, 10, 2)
rep(1:5, 2)
rep(1:5, each = 2)
rep(1:5, 2, each = 2)
c(1, 3, 6, 10)
c("a", "b", "c")
rep("a", 5)
c(T, F, T)
rep(T, 5)

# 1-D Objects: Lists
# A list has many "cells", each containing an object
c(123, "abc", T) %>% class()
list(123, "abc", T)
list(123, "abc", T)[[1]] %>% class()
list(123, "abc", T)[[2]] %>% class()
list(123, "abc", T)[[3]] %>% class()
list(T, 1:10, "a", pi, list(123, "abc", T))

# 2-D Objects: Matrix
matrix(1:9, nrow = 3, ncol = 3)

# 2-D Objects: Tibble/Data Frame
tibble(num = 1:10,
       chr_upper = LETTERS[1:10],
       chr_lower = letters[1:10],
       lgl = rep(c(T, F), 5))

# Vectors and values can be seen as special forms of 2-D objects.
tibble(num = 1:10)
tibble(num = 1:10) %>% pull(num)
tibble(num = 1)
tibble(num = 1) %>% pull(num)

# When I need to manipulate a vector,
# I prefer to first convert it to a tibble.
# Cuz manipulating a tibble is easier.setwd()
setwd("~/Documents/intro2css/assets/files")
getwd()
dir("./names/")
files <- dir("./names/") %>% 
  tibble(files = .) %>% 
  filter(str_detect(files, "^yob\\d{4}\\.txt$")) %>% 
  pull(files)
files
paste0("a", "b", "c")
files <- paste0("./names//", files)


# Uncover the function "readSsaBabyNames"
library(tidyverse)
files
first_file <- files %>% head(1)
first_file

# These functions are all equivalent
# Try to replace "first file" with "files"?
str_replace_all(first_file, "[^0-9]", "")
str_remove_all(first_file, "[^0-9]")
str_extract(first_file, "[0-9]{4}")
str_extract(first_file, "\\d{4}")

data <- read_csv(first_file, col_names = FALSE, show_col_types = FALSE) %>%
  mutate(year = str_extract(first_file, "\\d{4}")) %>% 
  rename_with(~ c("names", "sex", "count", "year"))
data

dat_year <- map_dfr(files, readSsaBabyNames)

dat_year # What is the unit of analysis?
# Baby name * Birth year

# Grouping
library(gapminder)
gapminder
gapminder %>% 
  group_by(continent, year) %>% 
  summarise(pop = sum(pop))

dat_wider <- dat_year %>%
  filter(year > 1970) %>%
  group_by(names, sex) %>%
  summarise(count = sum(count)) %>%
  # we only keep names used by at least 10
  filter(count > 10) %>%
  # reshape the long format to wide format
  pivot_wider(names_from = "sex", values_from = "count")

dat_wider
dat_wider %>% 
  # replace NA with zeros
  replace_na(list(F = 0, M = 0)) %>% 
  mutate(female_prop = F / (F + M))

dat_wider %>% 
  # replace NA with zeros
  replace_na(list(F = 0, M = 0)) %>% 
  mutate(female_prop = F / (F + M)) %>% 
  ggplot(aes(female_prop)) +
  geom_histogram(color = "black", fill = "gray") +
  theme_bw()

dat_all <- dat_wider %>% 
  # replace NA with zeros
  replace_na(list(F = 0, M = 0)) %>% 
  # create our target outcome female
  mutate(female = (F / (F + M) > .5) * 1) %>% 
  # create a series of predictors
  mutate(
    flt1 = str_extract(names, "^.") %>% tolower(), # first letter
    flt2 = str_extract(names, "^.{2}") %>% tolower(),
    llt1 = str_extract(names, ".$") %>% tolower(), # last letter
    llt2 = str_extract(names, ".{2}$") %>% tolower()
  )

flt1_top5 <- dat_all %>% 
  group_by(flt1) %>% 
  summarise(flt1_count = n()) %>% 
  arrange(-flt1_count) %>% 
  head(5)

flt2_top5 <- dat_all %>% 
  group_by(flt2) %>% 
  summarise(flt2_count = n()) %>% 
  arrange(-flt2_count) %>% 
  head(5)

# It is for presentation only.
# "a" in flt1 has nothing to do with "ja" in flt2
bind_cols(flt1_top5, flt2_top5)


# Machine Learning
# install.packages("caret")
library(caret)
# create dummies for last letter, last two letters, fist letter, and first two letters of names
predict(dummyVars( ~ llt1 + llt2 + flt1 + flt2, data = dat_all), 
        newdata = dat_all)
fllt_d <- predict(dummyVars( ~ llt1 + llt2 + flt1 + flt2, data = dat_all), 
        newdata = dat_all) %>% 
  as_tibble()
fllt_d

bind_cols(dat_all %>% head(), fllt_d %>% head())


nzv <- nearZeroVar(fllt_d)
nzv
fllt_nnzv <- fllt_d %>% 
  select(-all_of(nzv))

fllt_nnzv
fllt_d %>%
  summarise(across(everything(), ~ var(.))) %>% 
  print(width = Inf)

c(1:10, NA)
is.na(c(1:10, NA))
!is.na(c(1:10, NA))
df <- dat_all %>%
  ungroup() %>%
  select(names, female) %>%
  bind_cols(fllt_nnzv) %>%
  filter(!is.na(female)) %>%
  mutate(female = if_else(female == 1, "Y", "N") %>% factor())
df

set.seed(2022)
train <- df %>%
  slice_sample(prop = .75)
test <- df %>%
  anti_join(train)

gdata::keep(dat_all, df, train, test, sure = TRUE)

# Sample tibble
tibble(female = rep(0:1, 10)) %>% 
  mutate(female = if_else(female == 1, "Yes", "No") %>% 
           factor())

# K folds cross validation
# try parallel computing
library(doParallel)
library(xgboost)

xgb_training <- function() {
  cl <- makePSOCKcluster(3)
  registerDoParallel(cl)
  
  grid_default <- data.frame(
    nrounds = 100,
    max_depth = 6,
    eta = 0.3,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
  
  xgb_base <- caret::train(
    female ~ ., 
    data = train %>% select(-names),
    trControl = trainControl(
      method = "cv", # cross validation
      number = 3 # we do 10 cv
    ),
    tuneGrid = grid_default,
    method = "xgbTree",
    verbose = TRUE
  )
  
  stopCluster(cl)
  return(xgb_base)
}

xgb_base <- xgb_training()
xgb_base

save(xgb_base, file = "./xgb_base.RData")

# check cf matrix
xgb_base
confusionMatrix(xgb_base)


# Visualisation
xgbImp <- varImp(xgb_base, scale = TRUE)
plot(xgbImp, top = 5)

# predict name_remove
test_df <- test %>% select(-c(names,female))
test_pred <- predict(xgb_base, newdata = test_df) %>% 
  as_tibble() %>% 
  rename_with(~ "xgb_female")

test_pred

data <- test %>% select(names,female) %>% bind_cols(test_pred)
data

data %>% write_csv("./test_pred.csv",na = "")

# check confusion matrix
confusionMatrix(test_pred$xgb_female,
                test$female)

sample_mat <- matrix(c(0, 2, 0, 98), nrow = 2)
rownames(sample_mat) <- c("Pred. -", "Pred. +")
colnames(sample_mat) <- c("Ref. -", "Ref. +")
sample_mat


xgb_training <- function() {
  cl <- makePSOCKcluster(3)
  registerDoParallel(cl)
  
  grid_default <- data.frame(
    nrounds = 100,
    max_depth = 6,
    eta = 0.3,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
  
  xgb_base <- caret::train(
    female ~ ., 
    data = train %>% select(-names),
    trControl = trainControl(
      method = "cv", # cross validation
      number = 3 # we do 10 cv
    ),
    tuneGrid = grid_default,
    method = "xgbTree",
    verbose = TRUE
  )
  
  stopCluster(cl)
  return(xgb_base)
}

xgb_base <- xgb_training()
