
#install.packages("rnassqs")

library(rnassqs)

# You must set your api key before requesting data
nassqs_auth(key = "E815374B-D025-33BA-AF2A-990B8657C49B")

# Parameters to query on and data call
# look at the link: https://cran.r-project.org/web/packages/rnassqs/vignettes/rnassqs.html

params <- list(commodity_desc = "CORN", year = c(2000), state_alpha = c("IN","VA"))
d <- nassqs(params)




