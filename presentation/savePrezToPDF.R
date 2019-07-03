# WEBSHOT -----------------------------------------------------------------

# install.packages("webshot")
library(webshot)

# install_phantomjs()

file_name <- paste0("file:///", normalizePath(paste0(here::here(),"/awm_april2019.html")))


webshot(file_name, "awm_april2019_webshotExport.pdf")
