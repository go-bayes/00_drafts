# how to use a bradely terry model 


# install if you don't have this package
library(dplyr)

# set the seed (for reproducibility)
set.seed(123)

# all possible sentences
all_sentences <- paste0("S", 1:10)

# create a data frame with the comparison data
data <- expand.grid(
  rater = 1:10,  # each rater rates 5 pairs of sentences
  sentence1 = all_sentences,  # the first sentence in each comparison
  sentence2 = all_sentences  # the second sentence in each comparison
)
data
# remove rows where sentence1 and sentence2 are the same
data <- data[data$sentence1 != data$sentence2, ]

# randomly generate outcomes for each comparison
data$outcome <- sample(c(0, 1), nrow(data), replace = TRUE)

# convert sentence1 and sentence2 to factors (needed for mode to run)
data$sentence1 <- factor(data$sentence1, levels = all_sentences)
data$sentence2 <- factor(data$sentence2, levels = all_sentences)

# check data
str(data)
dim(data)
head(data)


# fit bradely terry model 
library(BradleyTerry2)

model <- BTm(outcome, sentence1, sentence2, data = data)

# check
model

# convert coef's from log-odds to odds
odds <- exp(coef(model))
odds <- data.frame(odds)

# view order by odds
odds |> 
  arrange(desc(odds))


# better assignment method ------------------------------------------------

# n of sentences
n_sentences <- 100

# n of sentences each rater rates
k <- 20

# n of raters
n_raters <- 10

# create data frame of sentences
sentences <- data.frame(sentence = paste0("S", 1:n_sentences))

# create data frame of raters
raters <- data.frame(rater = paste0("R", 1:n_raters))

# assign each rater random sentences
assignments <- lapply(raters$rater, function(rater) {
  data.frame(rater = rater, sentence = sample(sentences$sentence, k))
})

# combaine assignment sinto data frame
assignments <- do.call(rbind, assignments)

# check
assignments

set.seed(123) # Set seed for reproducibility

# function to generate random ratings
generate_ratings <- function() {
  # Assume that ratings are evenly distributed
  return(sample(1:20, 1))
}

# apply function to each row of assignments
assignments$rating <- apply(assignments, 1, function(x) generate_ratings())

# inspect
head(assignments)


# apply function to each row of data
assignments$rating <- apply(assignments, 1, function(x) generate_ratings())

# inspect
head(assignments)
str(assignments)

# function to generate pairwise comparisons
generate_comparisons <- function(df) {
  comparisons <- expand.grid(df$sentence, df$sentence)
  comparisons <- comparisons[comparisons$Var1 != comparisons$Var2, ]
  comparisons$outcome <- df$rating[match(comparisons$Var1, df$sentence)] > 
    df$rating[match(comparisons$Var2, df$sentence)]
  comparisons$rater <- unique(df$rater) # add rater information
  comparisons
}

# load purrr 
library(purrr)
library(dplyr)
# generate pairwise comparisons for each rater
data_2 <- assignments %>%
  group_by(rater) %>%
  group_split() %>%
  map_dfr(generate_comparisons) %>%
  ungroup()

# Rename columns
colnames(data_2) <- c("sentence1", "sentence2", "outcome", "rater")

# Convert outcome to factor
data_2$outcome <- factor(data_2$outcome, levels = c(FALSE, TRUE))

# Take a look at the data
head(data_2)


# Rename columns
colnames(data_2) <- c("sentence1", "sentence2", "outcome", "rater")

# Convert outcome to factor
data_2$outcome <- factor(data_2$outcome, levels = c(FALSE, TRUE))

# Take a look at the data
head(data_2)


# fit bradely terry model 
library(BradleyTerry2)
model_2 <- BTm(outcome, sentence1, sentence2, data = data_2)

# check
model_2

# convert coef's from log-odds to odds
odds_2 <- exp(coef(model_2))
odds_2 <- data.frame(odds_2)

# round
odds_2 <- round(odds_2, 3)

# view order by odds
odds_2 |> 
  arrange(desc(odds_2)) 
