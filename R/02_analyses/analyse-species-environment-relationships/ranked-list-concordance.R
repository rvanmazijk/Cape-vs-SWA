# Dis-/Concordance between ranked lists: generalised thoughts
# Ruan van Mazijk

library(tidyverse)

set.seed(1234)  # for reproducibility

# Give some labels some ranks
data <- data.frame(label = letters[1:15], rank1 = 1:15)
# Make multiple shuffles of the ranks w.r.t. the labels
data$rank1 <- sample(data$rank1)
data$rank2 <- sample(data$rank1)
data$rank3 <- sample(data$rank1)
# One special set that is the same as rank1
data$rank4 <- data$rank1 - 1
data$rank4 <- ifelse(data$rank4 == 0, 10, data$rank4)
data$rank5 <- data$rank1

# Make data long-form
data2 <- data %>%
  gather(rank_set, rank, -label) %>%
  mutate(rank_set = as.numeric(as.factor(rank_set)))

comparison12 <- filter(data2, rank_set %in% c(1, 2))

# Plot crossed-line plot
ggplot(comparison12, aes(factor(rank_set), factor(rank))) +
  geom_point() +
  geom_line(aes(group = label)) +
  labs(x = "Set", y = "Rank") +
  coord_flip()

# Compute average change in rank between two sets
mean_rank_diff <- data %>%
  select(label, rank1, rank2) %>%
  mutate(rank_diff = abs(rank1 - rank2)) %>%
  summarise(mean_rank_diff = mean(rank_diff)) %>%
  pull("mean_rank_diff")

# Compute *randomised null* average change in rank between two sets
random_mean_rank_diffs <- replicate(999, {
  data %>%
    select(label, rank1, rank2) %>%
    mutate(rank1 = sample(rank1), rank2 = sample(rank2)) %>%
    mutate(rank_diff = abs(rank1 - rank2)) %>%
    summarise(mean_rank_diff = mean(rank_diff)) %>%
    pull("mean_rank_diff")
})
p_value <- rank(c(mean_rank_diff, random_mean_rank_diffs))[1] / 1000
p_value

hist(random_mean_rank_diffs)
abline(v = mean_rank_diff, lty = "dashed")
