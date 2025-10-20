devtools::install_github("e-kotov/r5rgui")

options(java.parameters = '-Xmx16G') # RAM to 16GB
library(r5r)
library(r5rgui)
# r5r_gui_demo()

data_path <- "data/Lisbon/r5r/"
r5r_network <- build_network(data_path = data_path, verbose = FALSE)
r5r_gui(r5r_network, center = c(-9.13, 38.73), zoom = 13)

