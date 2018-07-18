if (!require(pacman)) install.packages("pacman", dependencies = TRUE)
pacman::p_load(DiagrammeR, DiagrammeRsvg, here, xfun)
grViz2svg <- function(file) {
  graph <- grViz(file)
  graph <- export_svg(graph)
  write(graph, paste0(sans_ext(file), ".svg"))
}
grViz2svg(here::here("docs/repo-structure.gv"))
grViz2svg(here::here("docs/repo-structure-detailed.gv"))
