## Data Viz 2020 - Mini Project 1
## Pedro Freitas

setwd("~/14D007 Data Visualization/class-4")
library(ggthemes)
library(broom)
library(tidyverse)
library(rgdal)
library(lubridate)

# Load price paid data and file with postcodes and latitude/longitude
#load("data/ppdata_lite.csv")
ppdata <- read.csv("data/ppdata_lite.csv", header = TRUE, sep = ',')
ukpostcodes <- read.csv("data/ukpostcodes.csv", header = TRUE, sep = ',')

merge.data <- merge(ppdata, ukpostcodes, by = "postcode")
merge.data$date_of_transfer <- as.Date(merge.data$date_of_transfer)

## FIRST TASK ############################################################################ 

# filter for "greater london" to get the 33 boroughs of interest
london.data <- merge.data %>% filter(county == "GREATER LONDON")

# The upper values of the outliers are completely out of proportion when 
# related to the percentile values, so the box_plot auto scale makes it impossible
# to have a good view of the boxplot. I decided then to limit the yaxis 
# using coord_cartesian scaling so it still consider all data when calculating
# the percentiles of the boxplot but is nicely scaled to view the box plot. 
# I also ordered the boroughs in descending order (in median measures) to make it 
# clear which boroughs have higher median prices.
# I believe that with these adjustments the boxplot is a nice way to visualize
# the prices by borough. I also think it is better to limit the yaxis then to do 
# a log transformation because it distorts a little bit about the comparison.

ggplot(london.data, aes(reorder(district, -price, FUN = median), price, color ="")) +
  geom_boxplot(outlier.size = 0.5, show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 0.9E6)) + 
  labs(title = "Box plot of housing prices by London borough",
       caption = "* Some upper outliers may not be showing in the graph",
       x = NULL, y = NULL ) +
  scale_color_fivethirtyeight()+
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90,size = 7))


# If i am looking for a relationship between prices and floor, the ideal is to 
# do some sort of regression that would capture the impact of floor adjusted for
# other variables. Here, however, I will take the naive approach of regressing only against 
# the log of prices against the floor. The log scale is interesting for two reasons: 
# (i) it makes the yaxis range smaller and (ii) it gives us a nice interpretation for the coefficients
# of the regression as a percentage change.
# I chose shape "." for geom_jitter otherwise the graph would be overplotted because of the size of the data.
# The result of the regression yielded a coefficient of 0.006 for floor, which means that every floor higher,
# the avg price increases in 0.6%. 

# data manipulation to get flat floor from field SAON
flat.data <- subset(merge.data, merge.data$property_type == "F" & !(merge.data$SAON == ""))
flat.data$SAON1 <- gsub('\\D+','', flat.data$SAON)
flat.data$floor <- as.integer(substring(flat.data$SAON1,1,1))
flat.data <- subset(flat.data, flat.data$floor >= 1)

# perform linear regression to get coefficients for the plot
model <- lm(log(price) ~ floor, flat.data)
text2 <- sprintf("log(price) = %.3f + %.3f*floor", model$coefficients[1],model$coefficients[2])

ggplot(flat.data, aes(floor, log(price))) +
  geom_jitter(width = 0.5, show.legend = FALSE, shape = ".",color = "#008FD5", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#FF2700") +
  annotate("text", label = text2, x = 2.5, y = 16, size = 5) +
  labs(title = "Relation between log(prices) and floor",
       xlab = "Floor", ylab = "Log(prices)") +
  scale_color_fivethirtyeight()+
  theme_fivethirtyeight()


## SECOND TASK ############################################################################ 

# I had a lot of trouble using the whole data, so instead of wasting more time on it I decided 
# to use the simpler approach of doing it only for the London boroughs.
# At least for the London boroughs I couldn't really see a lot of difference on the graphs of means or medians, 
# only a couple districts changed their color. Overall, because there a few veery fat tail observations,
# I believe the median is a better statistic to describe the housing prices in each borough.

# load geojson file found online
boundaries <- rgdal::readOGR("london_boroughs.json")

# transfer the spatialpolygon dataframe into a dataframe and merge data of interest so we can use ggplot
boundaries.df <- tidy(boundaries)
boundaries$id <- row.names(boundaries)
boundaries.df <- left_join(boundaries.df, boundaries@data)

# a few data manipulation so districts names match
boundaries.df$name <- toupper(boundaries.df$name)
boundaries.df$name[boundaries.df$name == "WESTMINSTER"] <- "CITY OF WESTMINSTER"

# create medians, means, max, min, tertiles and merge them to dataframe
medians <- london.data %>% 
  group_by(district) %>%
  summarise(med = median(price), mean = mean(price), maxim = max(price), minim = min(price))

medians$medntile <- ntile(medians$med,3)
medians$meanntile <- ntile(medians$mean,3)
boundaries.df <- merge(boundaries.df, medians, by.x = "name", by.y = "district")


# plots for median prices. first for continuous median prices and then for tertiles
ggplot(boundaries.df, aes(x = long, y = lat, group = group, fill = med)) +
  geom_polygon(color = "#F0F0F0", size = 1) +
  coord_quickmap() +
  scale_color_fivethirtyeight()+
  theme_void() +
  theme (plot.background = element_rect(fill = "#F0F0F0"),
         legend.title = element_text(size=10),
         legend.justification=c(0,1), 
         legend.position=c(0.8, 0.32),
         legend.background = element_blank(),
         legend.key = element_blank(),
         legend.direction= "vertical") +
  labs(title = "Map of Greater London",
       subtitle = "Color filled by median house prices",
       caption = "",
       x = NULL, y = NULL, fill = "Median of prices" )


ggplot(boundaries.df, aes(x = long, y = lat, group = group, fill = as.factor(medntile))) +
  geom_polygon(color = "#F0F0F0", size = 1) +
  coord_quickmap() +
  scale_color_fivethirtyeight()+
  theme_void() +
  theme (plot.background = element_rect(fill = "#F0F0F0"),
         legend.title = element_text(size=10),
         legend.justification=c(0,1), 
         legend.position=c(0.02, 0.02),
         legend.background = element_blank(),
         legend.key = element_blank(),
         legend.direction= "horizontal") +
  labs(title = "Map of Greater London",
       subtitle = "Color filled by tertile of median house prices",
       caption = "",
       x = NULL, y = NULL, fill = "Tertile of median prices" )


# plots for mean prices. first for continuous mean prices and then for tertiles
ggplot(boundaries.df, aes(x = long, y = lat, group = group, fill = mean)) +
  geom_polygon(color = "#F0F0F0", size = 1) +
  coord_quickmap() +
  scale_color_fivethirtyeight()+
  theme_void() +
  theme (plot.background = element_rect(fill = "#F0F0F0"),
         legend.title = element_text(size=10),
         legend.justification=c(0,1), 
         legend.position=c(0.8, 0.32),
         legend.background = element_blank(),
         legend.key = element_blank(),
         legend.direction= "vertical") +
  labs(title = "Map of Greater London",
       subtitle = "Color filled by mean house prices",
       caption = "",
       x = NULL, y = NULL, fill = "Mean of prices" )


ggplot(boundaries.df, aes(x = long, y = lat, group = group, fill = as.factor(meanntile))) +
  geom_polygon(color = "#F0F0F0", size = 1) +
  coord_quickmap() +
  scale_color_fivethirtyeight()+
  theme_void() +
  theme (plot.background = element_rect(fill = "#F0F0F0"),
         legend.title = element_text(size=10),
         legend.justification=c(0,1), 
         legend.position=c(0.02, 0.02),
         legend.background = element_blank(),
         legend.key = element_blank(),
         legend.direction= "horizontal") +
  labs(title = "Map of Greater London",
       subtitle = "Color filled by tertile of mean house prices",
       caption = "",
       x = NULL, y = NULL, fill = "Tertile of mean prices" )


## THIRD TASK ############################################################################

# I tried to first plot the time series as a line, but the quantity of observations and high variance
# made it hard to see anything relevant. So I plotted also daily medians to see if we can observe a pattern.
# I tried then running a linear regression on the data and the prices seem independent over time.
# I then tried changing the granularity of data to monthly medians. 
# When plotting the trend for prices, we see that prices are increasing during the year for all type
# of properties. In the search for seasonality, I couldn't find any. I looked at the residuals of the regression and no 
# specific pattern was identified. 
# I then observed the data through a different granularity. I got the monthly median for prices.
# Overall, I think this is a good form to analyze this much data, because it synthesizes all those thousand 
# of observations, just like as if we were constructing an index.
# For that index, I plotted the variation of this monthly medians (bar chart) to see if we observe
# any seasonality and also cumulative sum of these values so we look at the trend. Of course we cannot
# say if there is seasonality observing only one year. We would need to see if the same pattern would happen
# in other years as well. For example, we saw a rise in median prices during summer, but we would need to observe
# this pattern in other years to conclude there is a relationship between the season of the year and prices.
# I did a separated graph for prop_type "O" because the its values are higher than the others and we couldnt
# observe well the variations for the other prop_types.

# separate data for the year of 2015
data.2015 <- filter(merge.data, date_of_transfer >= "2015-01-01" & date_of_transfer <= "2015-12-31")

ggplot(data.2015, aes(date_of_transfer, price)) + 
  geom_line(alpha = 0.1, color = "#008FD5") +
  coord_cartesian(ylim = c(0, 1E6)) +
  stat_summary(geom = "line", 
               fun.y = "mean", 
               colour = "#FF2700", 
               size = 1.5) +
  labs(title = "Evolution of prices over the year 2015",
       subtitle = "Median in red",
       xlab = "Date", ylab = "Price") +
  scale_color_fivethirtyeight()+
  theme_fivethirtyeight()

# plot trends for different types of properties
ggplot(data.2015, aes(date_of_transfer, price, color = property_type)) +
  geom_jitter(width = 0.5, show.legend = FALSE, size = 0.7,alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(ylim = c(150000,450000)) +
  labs(title = "Evolution of prices over 2015 by property type",
       xlab = "Time", ylab = "Prices") +
  theme_fivethirtyeight()

# monthly analysis #####

# data manipulation to get monthly data
data.2015.monthp <- data.2015 %>%
  group_by(month = month(date_of_transfer), prop_type = property_type) %>%
  summarise(med = median(price), mean = mean(price)) %>%
  arrange(prop_type, month)

data.2015.month <- data.2015 %>%
  group_by(month = month(date_of_transfer)) %>%
  summarise(med = median(price), mean = mean(price))

# monthly difference of medians and cumulative sum
data.2015.month$diff.med[2:12] <- diff(data.2015.month$med)
data.2015.month$cumsum[2:12] <- cumsum(data.2015.month$diff.med[2:12])
data.2015.month$diff.med[1] <- 0

data.2015.monthp$diff.med[2:60] <- diff(data.2015.monthp$med)
data.2015.monthp$diff.med[data.2015.monthp$month == 1] <- NA

for(batch in 1:5){
  beg <- 12*(batch-1)+2
  end <- 12*(batch-1)+12
  data.2015.monthp$cumsum[beg:end] <- cumsum(data.2015.monthp$diff.med[beg:end])
}

# plot graphs

ggplot(data.2015.month, aes(month, cumsum, color =""), show.legend = FALSE) +
  geom_line()+
  geom_bar(stat = "identity", aes(month, diff.med, fill = "")) +
  theme_fivethirtyeight() +
  theme(legend.position="none") +
  labs(title = "Cumulative median price monthly - All type of properties",
       subtitle = "Line is cumulative for the year and bars are for difference between consecutive months")+
  scale_x_continuous(labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     breaks = c(1:12))


# because type O had higher values, plotting them together doesnt look good, so I separated the data
# in two plots
data.no.o <- data.2015.monthp[data.2015.monthp$prop_type != "O",]
ggplot(data.no.o, aes(month, cumsum, colour = prop_type)) +
  geom_line()+
  geom_bar(stat = "identity", aes(month, diff.med, fill = prop_type)) +
  theme_fivethirtyeight() +
  labs(title = "Cumulative median price monthly - by property type",
       subtitle = "Line is cumulative for the year and bars are for difference between consecutive months",
       fill = "Type of property")+
  scale_colour_discrete(guide = "none")+
  scale_x_continuous(labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     breaks = c(1:12))


data.o <- data.2015.monthp[data.2015.monthp$prop_type == "O",]
ggplot(data.o, aes(month, cumsum)) +
  geom_line(color ="#008FD5")+
  geom_bar(stat = "identity", aes(month, diff.med), fill = "#008FD5") +
  theme_fivethirtyeight() +
  scale_color_fivethirtyeight()+
  theme(legend.position="none") +
  labs(title = "Cumulative median price monthly - Properties of type 'O'",
       subtitle = "Line is cumulative for the year and bars are for difference between consecutive months")+
  scale_x_continuous(labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     breaks = c(1:12))
