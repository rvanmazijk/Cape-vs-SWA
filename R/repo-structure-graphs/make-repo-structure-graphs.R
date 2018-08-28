grViz2svg <- function(file) {
  graph <- DiagrammeR::grViz(file)
  graph <- DiagrammeRsvg::export_svg(graph)
  write(graph, paste0(xfun::sans_ext(file), ".svg"))
}

grViz2svg(here::here("docs/repo-structure.gv"))
grViz2svg(here::here("docs/repo-structure-detailed.gv"))
