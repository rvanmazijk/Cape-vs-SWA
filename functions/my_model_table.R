my_model_table <- function(model = NULL, model_tidy = NULL,
                           tidy_terms = c("Intercept"),
                           AIC_table = NULL) {

    if (!is.null(model_tidy)) {
        message("Using tidied model data-frame")
    } else if (!is.null(model)) {
        model_tidy <- broom::tidy(model)
    } else {
        stop("Please provide either a model object or tidied model data-frame")
    }

    stopifnot(length(tidy_terms) == length(model_tidy$term))

    if (!is.null(AIC_table)) {
        deltaAIC <- AIC_table$AIC - min(AIC_table$AIC)
        deltaAIC_tidy <- round(deltaAIC[[2]], digits = 2)
    } else {
        deltaAIC_tidy <- NULL
    }

    if (!"p.value" %in% colnames(model_tidy)) {
        model_tidy %<>% mutate(p.value = CI_to_P(estimate,
                                                 conf.high,
                                                 conf.low))
    }

    model_table <- model_tidy %>%
        transmute(Term = tidy_terms,
                  Estimate = round(estimate, digits = 3),
                  `P-value` = p.value %>%
                      round(digits = 3) %>%
                      format(scientific = FALSE) %>%
                      ifelse(. == 0.000, "< 0.001", .))

    return(list(
        raw_terms = model_tidy$term,
        model_table = model_table,
        deltaAIC = deltaAIC_tidy
    ))

}
