richness_turnover_model_table <- function(model, AIC_table) {

    model_tidy <- broom::tidy(model)
    deltaAIC <- AIC_table$AIC - min(AIC_table$AIC)
    deltaAIC_tidy <- round(deltaAIC[[2]], digits = 2)

    tidy_terms <- c(
        "Intercept",
        "$log(\\overline{S_{QDS}} + 1)$",
        "$\\overline{J_{QDS}}$",
        "SWAFR",
        "$log(\\overline{S_{QDS}} + 1) \\times$ SWAFR",
        "$\\overline{J_{QDS}} \\times$ SWAFR"
    )

    model_table <- model_tidy %>%
        transmute(Term = tidy_terms,
                  Estimate = round(estimate, digits = 3),
                  `P-value` = p.value %>%
                      round(digits = 3) %>%
                      format(scientific = FALSE) %>%
                      ifelse(. == "0.000", "< 0.001", .))

    return(list(
        raw_terms = model_tidy$term,
        model_table = model_table,
        deltaAIC = deltaAIC_tidy
    ))

}
