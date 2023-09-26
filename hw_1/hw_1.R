# Due: Sun. Sep. 10 2023 at 11:59 pm - submit on HuskyCT

data(trees)
str(trees)

boxplot(trees$Volume, varwidth = TRUE)

boxplot(trees$Girth, notch = TRUE)

library(ggplot2)

ggplot(trees, aes(x=1, y=Volume)) +
  geom_boxplot(color="black") +
  coord_flip()

library(car)

# QQ plot with confidence bands
qqPlot(trees$Volume, main="QQ Plot with Confidence Envelope", 
       ylab="Volume", cex=0.6, pch=19, col="red", col.lines="orange")

shapiro.test(trees$Volume)

library(nortest)

pearson.test(trees$Volume)

qqPlot(log(trees$Volume), main="QQ Plot with Confidence Envelope", 
       ylab="Volume", cex=0.6, pch=19, col="red", col.lines="orange")

shap <- shapiro.test(log(trees$Volume))
print(shap)

chi <- pearson.test(log(trees$Volume))
print(chi)

data(mtcars)
str(mtcars)

pairs(mtcars)

# Compute the correlation matrix
cor_matrix <- cor(mtcars)
cor_matrix

# Set the diagonal and upper triangle to NA
cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA

# Find the location of the maximum absolute correlation
loc_max <- which(abs(cor_matrix) == max(abs(cor_matrix), na.rm = TRUE), arr.ind = TRUE)

# Extract the variable names for the maximum correlation
var1 <- rownames(cor_matrix)[loc_max[1, 1]]
var2 <- colnames(cor_matrix)[loc_max[1, 2]]

# Extract the maximum correlation value
max_cor <- cor_matrix[loc_max[1, 1], loc_max[1, 2]]

list(variable1 = var1, variable2 = var2, correlation = max_cor)

bangalore <- read.csv("/Users/giovanni-lunetta/uconn_masters/stat5405/hw_1/Temperature_And_Precipitation_Cities_IN/Bangalore_1990_2022_BangaloreCity.csv")
chennai <- read.csv("/Users/giovanni-lunetta/uconn_masters/stat5405/hw_1/Temperature_And_Precipitation_Cities_IN/Chennai_1990_2022_Madras.csv")
delhi <- read.csv("/Users/giovanni-lunetta/uconn_masters/stat5405/hw_1/Temperature_And_Precipitation_Cities_IN/Delhi_NCR_1990_2022_Safdarjung.csv")
lucknow <- read.csv("/Users/giovanni-lunetta/uconn_masters/stat5405/hw_1/Temperature_And_Precipitation_Cities_IN/Lucknow_1990_2022.csv")
mumbai <- read.csv("/Users/giovanni-lunetta/uconn_masters/stat5405/hw_1/Temperature_And_Precipitation_Cities_IN/Bangalore_1990_2022_BangaloreCity.csv")
rajasthan <- read.csv("/Users/giovanni-lunetta/uconn_masters/stat5405/hw_1/Temperature_And_Precipitation_Cities_IN/Rajasthan_1990_2022_Jodhpur.csv")
station <- read.csv("/Users/giovanni-lunetta/uconn_masters/stat5405/hw_1/Temperature_And_Precipitation_Cities_IN/Station_GeoLocation_Longitute_Latitude_Elevation_EPSG_4326.csv")
bhubhneshwar <- read.csv("/Users/giovanni-lunetta/uconn_masters/stat5405/hw_1/Temperature_And_Precipitation_Cities_IN/weather_Bhubhneshwar_1990_2022.csv")
rourkela <- read.csv("/Users/giovanni-lunetta/uconn_masters/stat5405/hw_1/Temperature_And_Precipitation_Cities_IN/weather_Rourkela_2021_2022.csv")

# Add a city column to each dataset
bangalore$city <- "Bangalore"
chennai$city <- "Chennai"
delhi$city <- "Delhi"
lucknow$city <- "Lucknow"
mumbai$city <- "Mumbai"
rajasthan$city <- "Rajasthan"

# Combine all datasets
df <- rbind(bangalore, chennai, delhi, lucknow, mumbai, rajasthan)

str(df)

# Compute year for the entire dataframe
df$year <- as.numeric(format(as.Date(df$time, format="%d-%m-%Y"), "%Y"))

# Compute yearly average temperature for all cities
yearly_avg_all <- aggregate(tavg ~ year + city, data=df, mean, na.rm=TRUE)

# Identify hottest and coldest years for each city
hottest_years <- numeric()
coldest_years <- numeric()

cities <- unique(yearly_avg_all$city)
for (city in cities) {
  city_data <- subset(yearly_avg_all, city == city)
  hottest_years <- c(hottest_years, city_data$year[which.max(city_data$tavg)])
  coldest_years <- c(coldest_years, city_data$year[which.min(city_data$tavg)])
}

# Create bar plot and highlight the hottest and coldest years using facet_grid
ggplot(yearly_avg_all, aes(x=factor(year), y=tavg, fill=(year %in% c(hottest_years, coldest_years)))) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("grey", "red"), guide=FALSE) +
  labs(title="Yearly Average Temperatures", x="Year", y="Average Temperature (°C)") +
  facet_grid(city ~ .) +
  theme_minimal()

# Histogram of Rainfall Distribution
ggplot(df, aes(x=prcp)) + 
  geom_histogram(binwidth=5, fill="skyblue", color="black", alpha=0.7) + 
  labs(title="Rainfall Distribution across Cities", x="Rainfall (mm)", y="Frequency") +
  facet_wrap(~city) +
  theme_minimal()

ggplot(df, aes(x = city, y = tavg, fill = city)) + 
geom_boxplot() + 
labs(title = "Distribution of Average Temperatures by City", x = "City", y = "Average Temperature (°C)")

# Create a function to generate sample QQ data for a given city against a reference
get_qq_data <- function(city_data, ref_data) {
  qq_data <- data.frame(sample_quantiles = quantile(city_data, probs = seq(0, 1, by = 0.01), na.rm = TRUE),
                        ref_quantiles = quantile(ref_data, probs = seq(0, 1, by = 0.01), na.rm = TRUE))
  return(qq_data)
}

# Let's choose Delhi as the reference city
ref_data <- df$tavg[df$city == "Delhi"]

# Create a data frame to store QQ data for all cities
all_qq_data <- data.frame()

for (city in unique(df$city)) {
  city_data <- df$tavg[df$city == city]
  qq_data <- get_qq_data(city_data, ref_data)
  qq_data$city <- city
  all_qq_data <- rbind(all_qq_data, qq_data)
}

# Plot using ggplot
ggplot(all_qq_data, aes(x = ref_quantiles, y = sample_quantiles)) +
  geom_point(aes(color = city), alpha = 0.5, size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  facet_wrap(~ city, scales = "free") +
  labs(title = "QQ Plots: Comparison to Delhi's Temperature Distribution",
       x = "Delhi's Quantiles",
       y = "City's Quantiles") +
  theme_minimal()

# Filter data for years between 2010 and 2015, for Delhi and Lucknow, and remove NA values in tavg column
selected_data <- df[df$year >= 2010 & df$year <= 2015 & !is.na(df$tavg) & (df$city %in% c("Delhi", "Lucknow")),]

# Compute yearly average temperature for all cities
yearly_avg <- aggregate(tavg ~ year + city, data=selected_data, mean, na.rm=TRUE)

# Plot with an exaggerated y-axis
p <- ggplot(yearly_avg, aes(x=factor(year), y=tavg, color=city, group=city)) + 
  geom_line(size=1.5) + 
  geom_point(size=4) +
  scale_y_continuous(limits=c(24, 26.5), breaks = seq(24, 26.5, by = 0.25)) + # This restricts the y-axis
  labs(title="DRAMATIC CHANGES! Yearly Average Temperatures (2010-2015)", 
       x="Year", 
       y="Average Temperature (°C)") +
  theme_minimal() +
  theme(legend.position="bottom", 
        legend.title=element_blank())

print(p)