# Libraries
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

# Build dummy data
data <- data.frame(
  day = as.Date("2019-01-01") + 0:99,
  temperature = runif(100) + seq(1,100)^2.5 / 10000,
  price = runif(100) + seq(100,1)^1.5 / 10
)

# Most basic line chart
p1 <- ggplot(data, aes(x=day, y=temperature)) +
  geom_line(color="#69b3a2", size=2) +
  ggtitle("Temperature: range 1-10") +
  theme_ipsum()

p2 <- ggplot(data, aes(x=day, y=price)) +
  geom_line(color="grey",size=2) +
  ggtitle("Price: range 1-100") +
  theme_ipsum()

# Display both charts side by side thanks to the patchwork package
p1 + p2

# Start with a usual ggplot2 call:
ggplot(data, aes(x=day, y=temperature)) +
  
  # Custom the Y scales:
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*10, name="Second Axis")
  ) +
  
  theme_ipsum()

# Value used to transform the data
coeff <- 10

ggplot(data, aes(x=day)) +
  
  geom_line( aes(y=temperature)) + 
  geom_line( aes(y=price / coeff)) + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")
  )

# Value used to transform the data
coeff <- 10

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(data, aes(x=day)) +
  
  geom_line( aes(y=temperature), size=2, color=temperatureColor) + 
  geom_line( aes(y=price / coeff), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature (Celsius °)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Price ($)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  
  ggtitle("Temperature down, price up")