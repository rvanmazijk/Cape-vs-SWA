library(tidyverse)
library(microbenchmark)
library(olsrr)
library(visreg)

set.seed(1234)

f <- function(x1, x2, x3) {
  ifelse(x3,
    jitter(x1 + x2) * runif(1, min = 75, max = 125),
    jitter(x1 + x2)
  )
}

d <-
  data.frame(
    x1 =   1:100,
    x2 = 101:200,
    x3 = c(TRUE, FALSE)
  ) %>%
  mutate(
    y  = f(x1, x2, x3),
    x4 = f(y, x2, x3)
  )

m1 <- lm(y ~ x1 + x2,            d)  # 2 terms
m2 <- lm(y ~ x1*x2,              d)  # 3 terms
m3 <- lm(y ~ x2 + x1*x3,         d)  # 4 terms
m4 <- lm(y ~ x1*x3 + x2*x3,      d)  # 5 terms
m5 <- lm(y ~ x1*x3 + x2*x3 + x4, d)  # 6 terms

m1_bm <- microbenchmark(ols_step_all_possible(m1), times = 100)
m2_bm <- microbenchmark(ols_step_all_possible(m2), times = 100)
m3_bm <- microbenchmark(ols_step_all_possible(m3), times = 100)
m4_bm <- microbenchmark(ols_step_all_possible(m4), times = 100)
m5_bm <- microbenchmark(ols_step_all_possible(m5), times = 100)

bm <- rbind(m1_bm, m2_bm, m3_bm, m4_bm, m5_bm) %>%
  as_tibble() %>%
  rename(time_ns = time) %>%
  mutate(time_s = time_ns/1e9) %>%
  group_by(expr) %>%
  summarise_if(is.numeric, mean) %>%
  mutate(
    n_terms    = 2:6,
    s_per_term = time_s/n_terms
  )
bm

m <- lm(time_s ~ exp(n_terms), bm)
visreg(m)

p_time_s <- predict(m, newdata = data.frame(n_terms = 19))
p_time_days <- ((p_time_s/60)/60)/24
p_time_days
