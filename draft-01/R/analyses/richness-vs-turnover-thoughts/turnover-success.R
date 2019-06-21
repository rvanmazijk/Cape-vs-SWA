# Setup ------------------------------------------------------------------------

library(here)
library(visreg)
#source(here("R/analyses/analyse-turnover.R"))

tidy_p <- function(p) {
  ifelse(p == 0,
    "italic(P) < 0.001",
    glue("italic(P) == {p}")
  )
}

richness_turnover_data <- read_csv(here("outputs/turnover/richness_turnover_data.csv"))

# Wrangle data -----------------------------------------------------------------

richness_turnover_data2 <- transmute(richness_turnover_data,
  region = region,
  gamma = HDS_richness,
  beta = mean_QDS_jaccard * HDS_richness,
  beta2 = mean_QDS_jaccard * mean_QDS_richness,
  beta3 = add_residual_turnover,
  beta4 = add_residual_turnover_prop,
  alpha = mean_QDS_richness
)

# Model gamma ~ alpha + beta ---------------------------------------------------

m_Cape <- lm(
  gamma ~ alpha + beta,
  filter(richness_turnover_data2, region == "Cape")
)
m_SWA <- lm(
  gamma ~ alpha + beta,
  filter(richness_turnover_data2, region == "SWA")
)
m_both <- lm(
  gamma ~
    alpha + beta + region +
    alpha * region + beta * region,
  richness_turnover_data2
)

# Summarise models
m_both_summary <-
  tidy(m_both, conf.int = TRUE) %>%
  filter(term %in% c("alpha:regionSWA", "beta:regionSWA")) %>%
  mutate(p.value = p.value %>%
    round(3) %>%
    tidy_p()
  ) %>%
  mutate(region = "Both", model_type = "Both")
m_sep_summary <-
  rbind(
    cbind(region = "Cape", tidy(m_Cape, conf.int = TRUE)),
    cbind(region = "SWA", tidy(m_SWA, conf.int = TRUE))
  ) %>%
  filter(term != "(Intercept)") %>%
  mutate(p.value = p.value %>%
    round(3) %>%
    tidy_p()
  ) %>%
  mutate(model_type = "Separate")
m_summaries <- full_join(m_both_summary, m_sep_summary)

# Visualise --------------------------------------------------------------------

# Plot the data
ggplot(richness_turnover_data2, aes(alpha, gamma, col = region)) +
  geom_point()
ggplot(richness_turnover_data2, aes(beta, gamma, col = region)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = "dashed", col = "grey25")
ggplot(richness_turnover_data2, aes(alpha, beta, col = region)) +
  geom_point()

# Plot combined model's results
visreg(m_both, xvar = "beta", by = "region", overlay = TRUE)
visreg(m_both, xvar = "alpha", by = "region", overlay = TRUE)

# Plot all models' results
ggplot(m_summaries, aes(term, estimate, col = region)) +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    position = position_dodge(0.3)
  ) +
  #geom_text(
  #  aes(y = estimate + 0.4, label = p.value),
  #  parse = TRUE,
  #  position = position_dodge(0.3)
  #) +
  geom_hline(yintercept = 0, lty = "dashed") +
  facet_grid(~ model_type, scales = "free_x")

# Cross predict between separate models ----------------------------------------

predict_Cape_w_SWA <- predict(m_SWA,
  newdata = m_Cape$model[, c("alpha", "beta")]
)
predict_SWA_w_Cape <- predict(m_Cape,
  newdata = m_SWA$model[, c("alpha", "beta")]
)

# Plot that
transparent_grey <- rgb(0, 0, 0, alpha = 0.25)
op <- par()
par(mfrow = c(1, 2))
plot(
  m_SWA$model$gamma, predict_SWA_w_Cape,
  pch = 20, col = transparent_grey,
  xlab = bquote(gamma[Cape]), ylab = bquote(hat(gamma)["Cape|SWA"])
)
abline(m_SWA)
abline(0, 1, lty = "dashed")
plot(
  m_Cape$model$gamma, predict_Cape_w_SWA,
  pch = 20, col = transparent_grey,
  xlab = bquote(gamma[SWA]), ylab = bquote(hat(gamma)["SWA|Cape"])
)
abline(m_Cape)
abline(0, 1, lty = "dashed")
par(op)

# ggplot2 version
cross_pred_gamma_Cape <- tibble(
  region = "Cape",
  obs = m_Cape$model$gamma,
  exp_by_Cape = m_Cape$fitted.values,
  exp_by_SWA = predict_Cape_w_SWA
)
cross_pred_gamma_SWA <- tibble(
  region = "SWA",
  obs = m_SWA$model$gamma,
  exp_by_Cape = predict_SWA_w_Cape,
  exp_by_SWA = m_SWA$fitted.values
)
cross_pred_gamma <-
  full_join(cross_pred_gamma_Cape, cross_pred_gamma_SWA) %>%
  gather(model, exp, exp_by_Cape, exp_by_SWA) %>%
  mutate(model = case_when(
    str_detect(model, "Cape") ~ "Cape model",
    str_detect(model, "SWA") ~ "SWA model"
  ))
ggplot(cross_pred_gamma, aes(obs, exp)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = lm, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, lty = "dashed", col = "grey25") +
  facet_grid(model ~ region) +
  labs(x = bquote(gamma), y = bquote(hat(gamma)))

# Visualise other things -------------------------------------------------------

ggplot(richness_turnover_data2, aes(beta2, gamma, col = region)) +
  geom_point()
ggplot(richness_turnover_data2, aes(beta2, alpha, col = region)) +
  geom_point()

ggplot(richness_turnover_data2, aes(beta3, gamma, col = region)) +
  geom_point()
ggplot(richness_turnover_data2, aes(beta3, alpha, col = region)) +
  geom_point()

ggplot(richness_turnover_data2, aes(beta4, gamma, col = region)) +
  geom_point()
ggplot(richness_turnover_data2, aes(beta4, alpha, col = region)) +
  geom_point()

# Model gamma ~ alpha and beta separately --------------------------------------

m_alpha <- lm(
  gamma ~ alpha + alpha * region,
  richness_turnover_data2
)
m_beta <- lm(
  gamma ~ beta3 + beta3 * region,
  richness_turnover_data2
)

# Summarise models
m_alpha_beta_summary <-
  rbind(
    cbind(param = "alpha", tidy(m_alpha, conf.int = TRUE)),
    cbind(param = "beta3", tidy(m_beta, conf.int = TRUE))
  ) %>%
  filter(term != "(Intercept)", term != "regionSWA") %>%
  mutate(p.value = p.value %>%
    round(3) %>%
    tidy_p()
  ) %>%
  mutate(term = case_when(
    param == "alpha" & term == "alpha"           ~ "Slope",
    param == "alpha" & term == "alpha:regionSWA" ~ "Interaction (SWA)",
    param == "beta3" & term == "beta3"           ~ "Slope",
    param == "beta3" & term == "beta3:regionSWA" ~ "Interaction (SWA)"
  )) %>%
  mutate(term = factor(term, levels = c("Slope", "Interaction (SWA)")))

# Visualise that
# Plot the data
ggplot(richness_turnover_data2, aes(alpha, gamma, col = region)) +
  geom_smooth(method = lm, size = 0.5) +
  geom_point() +
  labs(
    x = bquote(bar(italic("S"))["QDS"]),
    y = bquote(italic("S")["HDS"])
  ) +
  scale_colour_manual(name = "Region", values = my_palette) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
ggplot(richness_turnover_data2, aes(beta3, gamma, col = region)) +
  geom_smooth(method = lm, size = 0.5) +
  geom_point() +
  labs(
    x = bquote(italic("T")["HDS"]),
    y = bquote(italic("S")["HDS"])
  ) +
  scale_colour_manual(name = "Region", values = my_palette) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
ggplot(richness_turnover_data2, aes(alpha, beta3, col = region)) +
  geom_abline(intercept = 0, slope = 1, lty = "dashed", col = "grey25") +
  geom_point() +
  labs(
    x = bquote(bar(italic("S"))["QDS"]),
    y = bquote(italic("T")["HDS"])
  ) +
  lims(
    x = c(0, max(richness_turnover_data2$beta3)),
    y = c(0, max(richness_turnover_data2$beta3))
  ) +
  scale_colour_manual(name = "Region", values = my_palette) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

# Plot model's results
visreg(m_alpha, xvar = "alpha", by = "region", overlay = TRUE)
visreg(m_beta, xvar = "beta3", by = "region", overlay = TRUE)
# ggplot2 version
ggplot(m_alpha_beta_summary, aes(term, estimate)) +
  geom_point(position = position_dodge(0.3)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    position = position_dodge(0.3)
  ) +
  geom_hline(yintercept = 0, lty = "dashed") +
  facet_grid(~ param, labeller = label_parsed) +
  ylab("Estimate") +
  theme(axis.title.x = element_blank())

# ...

ggplot(richness_turnover_data2, aes(beta3 / gamma, fill = region)) +
  geom_histogram(pos = "dodge")
ggplot(richness_turnover_data2, aes(alpha / gamma, fill = region)) +
  geom_histogram(pos = "dodge")
