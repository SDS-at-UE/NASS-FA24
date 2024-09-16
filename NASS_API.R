
#install.packages("rnassqs")

library(rnassqs)

# You must set your api key before requesting data
nassqs_auth(key = "E815374B-D025-33BA-AF2A-990B8657C49B")

# Parameters to query on and data call
# look at the link: https://cran.r-project.org/web/packages/rnassqs/vignettes/rnassqs.html

params <- list(sector = "CROPS", year = c(2002), state_alpha = c("IN"), )
d <- nassqs(params)

library(rnassqs)

df = data.frame(matrix(nrow = 0, ncol = length(colnames(data)))) 
colnames(df) = colnames(data)

for(state in c("IN", "OH", "IL", "MI", "KY", "MO")){
# Set up the parameters
params <- list(
  sector_desc = "CROPS",
  year = 1977,
  state_alpha = state, 
  agg_level_desc = "COUNTY")
  data <- nassqs(params)
  df = rbind(df, data)

}

write.csv(df, "DATA_FILES/1977_county_crops.csv", row.names = FALSE)

# Write them from csv to RData for storage purposes
data <- read.csv("DATA_FILES/2002_county_crops.csv")
save(data, file = "DATA_FILES/2002_county_crops.RData")



