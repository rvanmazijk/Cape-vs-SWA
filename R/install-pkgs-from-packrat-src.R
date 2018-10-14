# Manual package installation
#   For use on UCT HPC (vmzrua001@hex.uct.ac.za:/home/vmzrua001)
# Cape vs SWA publication
# Ruan van Mazijk

# Install necessary packages from packrat/src/
install.packages(
  pkgs = list.files("~/Cape-vs-SWA/packrat/src", full.names = TRUE),
  lib = "~/R-3.5.1/lib"
)
