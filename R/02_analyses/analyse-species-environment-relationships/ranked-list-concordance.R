# Dis-/Concordance between ranked lists
# Ruan van Mazijk

library(tidyverse)

set.seed(1234)  # for reproducibility

# Give some labels some ranks
data <- data.frame(label = letters[1:10], rank1 = 1:10)
# Make multiple shuffles of the ranks w.r.t. the labels
data$rank1 <- sample(data$rank1)
data$rank2 <- sample(data$rank1)
data$rank3 <- sample(data$rank1)
# One special set that is the same as rank1
data$rank4 <- data$rank1 - 1
data$rank4 <- ifelse(data$rank4 == 0, 10, data$rank4)
data$rank5 <- data$rank1

# Make data long-form
data <- data %>%
  gather(rank_set, rank, -label) %>%
  mutate(rank_set = as.numeric(as.factor(rank_set)))

# Plot crossed-line plot
ggplot(data, aes(rank_set, rank, col = label)) +
  geom_point() +
  geom_line()


