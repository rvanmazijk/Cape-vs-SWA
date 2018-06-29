# Compile & render manuscript with Bookdown

if (!require(pacman)) install.packages("pacman", dependencies = TRUE)
library(pacman)
p_load(here, bookdown)

setwd(here::here("manuscript"))
bookdown::render_book("index.Rmd")
setwd(here::here())

system("open manuscript/_book/index.html")
