folder_is_empty <- function(...) {
    paths <- c(...)
    out <- vector(length = length(paths))
    for (i in seq_along(paths)) {
        out[[i]] <-
            !file.exists(paths[[i]]) &
            length(list.files(paths[[i]])) == 0
    }
    return(out)
}
