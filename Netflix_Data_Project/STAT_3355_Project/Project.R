# 6.1 Code for Figure 3.1
# 6.1.1 Data Cleaning for Figure 3.1
# For netflix data
netflix <- read.csv("netflix.csv")

library(ggplot2)
library(tidyverse)

# Convert the date_added column to a Date object
netflix$date_added <- lubridate::mdy(netflix$date_added)

# Create a new column with the month and year of each movie's addition to Netflix

netflix <- netflix %>%
  mutate(month_added = format(date_added, "%m"))

# Count the frequency of each movie category added to Netflix by month
title_counts1 <- netflix %>%
  group_by(month_added, title, release_year) %>%
  summarize(count = n()) %>%
  ungroup()

# For Amazon data,
amazon <- read.csv("amazon.csv")

library(ggplot2)
library(tidyverse)

#Convert the date_added column to a Date object
amazon$date_added <- lubridate::mdy(amazon$date_added)


#Create a new column with the month and year of each movie's addition to Netflix

amazon <- amazon %>%
  mutate(month_added = format(date_added, "%m"))

#Count the frequency of each movie category added to Netflix by month
title_counts2 <- amazon %>%
  group_by(month_added, title, release_year) %>%
  summarize(count = n()) %>%
  ungroup()



# 6.1.2 Construction of Graph for Figure 3.1
# For netflix data,
ggplot(data = title_counts1, mapping = aes(x = month_added)) +
  geom_bar() +
  scale_y_continuous(name = "Number of Titles Added") +
  scale_x_discrete(limit = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10","11", "12", "NA"),
                   labels = c("Jan","Feb","Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec", "NA")) + labs(title = "Movie Titles added to Netflix on each Month") 

# For Amazon data,
ggplot(title_counts2, mapping = aes(x = month_added)) +
  geom_bar() +
  scale_y_continuous(name = "Number of Titles Added") +
  scale_x_discrete(limit = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10","11", "12", "NA"),
                   labels = c("Jan","Feb","Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec", "NA"))+ labs(title = "Movie Titles added to Amazon on each Month")

# Line plot for both the streaming platforms,
ggplot(mapping = aes(x= month_added, group = 1)) +
  geom_line(data  = title_counts1, stat = "count", col = "red") +
  geom_line(data = title_counts2, stat = "count", col = "blue")+
  scale_x_discrete(limit = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10","11", "12", "NA"),
                   labels = c("Jan","Feb","Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec", "NA"))+ labs(title = "Movie Titles added to Netflix(Red) and Amazon(blue) on each Month")

# 6.2 Code for Figure 3.2
# 6.2.1 Data Cleaning for Figure 3.2
netflix <- read.csv("netflix.csv")
amazon <- read.csv("amazon.csv")

#seperates Movies from TV Shows
netflixMoviesIndex <- which(netflix$type == "Movie")
netflixTvShowsIndex <- which(netflix$type == "TV Show")
amazonMoviesIndex <- which(netflix$type == "Movie")
amazonTvShowsIndex <- which(netflix$type == "TV Show")

#found the indices where movies are released either before or after the year 2000
before2000Index <- which(netflix$release_year < 2000)
after2000Index <- which(netflix$release_year >= 2000)
Abefore2000Index <- which(amazon$release_year < 2000)
Aafter2000Index <- which(amazon$release_year >= 2000)

#implements the new column
beforeAfter2000 <- rep("Before 2000", nrow(netflix))
netflix <- cbind(netflix, beforeAfter2000)
netflix$beforeAfter2000[after2000Index] <- "2000 and After"
AbeforeAfter2000 <- rep("Before 2000", nrow(amazon))
amazon <- cbind(amazon, AbeforeAfter2000)
amazon$AbeforeAfter2000[Aafter2000Index] <- "2000 and After"

#creates a subset for graph use
netflixMovies <- netflix[netflixMoviesIndex, ]
amazonMovies <- amazon[amazonMoviesIndex, ]

# 6.2.2 Construction of Graph for Figure 3.2
netflix <- read.csv("netflix.csv")
amazon <- read.csv("amazon.csv")

#seperates Movies from TV Shows
netflixMoviesIndex <- which(netflix$type == "Movie")
netflixTvShowsIndex <- which(netflix$type == "TV Show")
amazonMoviesIndex <- which(netflix$type == "Movie")
amazonTvShowsIndex <- which(netflix$type == "TV Show")

#creating separate subset for movies and tv shows before and after 2000
Moviesbefore2000Index <- which(netflixMovies$beforeAfter2000 == "Before 2000")
Moviesafter2000Index <- which(netflixMovies$beforeAfter2000 == "2000 and After")
netflixMoviesBefore2000 <- netflixMovies[Moviesbefore2000Index, ]
netflixMoviesAfter2000 <- netflixMovies[Moviesafter2000Index, ]

netflixTvShows <- netflix[netflixTvShowsIndex, ]
Showsbefore2000Index <- which(netflixTvShows$beforeAfter2000 == "Before 2000")
Showsafter2000Index <- which(netflixTvShows$beforeAfter2000 == "2000 and After")
netflixShowsBefore2000 <- netflixTvShows[Showsbefore2000Index, ]
netflixShowsAfter2000 <- netflixTvShows[Showsafter2000Index, ]

AMoviesbefore2000Index <- which(amazonMovies$AbeforeAfter2000 == "Before 2000")
AMoviesafter2000Index <- which(amazonMovies$AbeforeAfter2000 == "2000 and After")
amazonMoviesBefore2000 <- amazonMovies[AMoviesbefore2000Index, ]
amazonMoviesAfter2000 <- amazonMovies[AMoviesafter2000Index, ]

amazonTvShows <- amazon[amazonTvShowsIndex, ]
AShowsbefore2000Index <- which(amazonTvShows$AbeforeAfter2000 == "Before 2000")
AShowsafter2000Index <- which(amazonTvShows$AbeforeAfter2000 == "2000 and After")
amazonShowsBefore2000 <- amazonTvShows[AShowsbefore2000Index, ]
amazonShowsAfter2000 <- amazonTvShows[AShowsafter2000Index, ]

#library allows for combining graphs into one
library(patchwork)

plot1 <- ggplot(netflixMoviesBefore2000, aes(x = rating, y = after_stat(count/sum(count)))) + 
  geom_bar(fill = "deepskyblue") + ylab("Proportion") + xlab("Rating") + ggtitle("Netflix: Proportion of Ratings of Movies Before 2000")

plot2 <- ggplot(netflixMoviesAfter2000, aes(x = rating, y = after_stat(count/sum(count)))) + 
  geom_bar(fill = "deepskyblue") + ylab("Proportion") + xlab("Rating") + ggtitle("Netflix: Proportion of Ratings of Movies After 2000")

plot3 <- ggplot(amazonMoviesBefore2000, aes(x = amazonMoviesBefore2000$rating, y = after_stat(count/sum(count)))) + 
  geom_bar(fill = "deepskyblue") + ylab("Proportion") + xlab("Rating") + ggtitle("Amazon: Proportion of Ratings of Movies Before 2000")

plot4 <- ggplot(amazonMoviesAfter2000, aes(x = amazonMoviesAfter2000$rating, y = after_stat(count/sum(count)))) + 
  geom_bar(fill = "deepskyblue") + ylab("Proportion") + xlab("Rating") + ggtitle("Amazon: Proportion of Ratings of Movies After 2000")

#creates a singular graph out of the four
layout <- wrap_plots(plot1, plot2, plot3, plot4, ncol = 2)

#displays the graph
layout

# 6.3 Code for Figure 3.3
# 6.3.1 Data Cleaning for Figure 3.3
#Removing all data points with empty values within “country”
netflixCountryIndex <- which(netflix$country != "")
netflixCountryData <- netflix[netflixCountryIndex, ]

#function that finds the mode of a dataset
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

library("tidyr")

#remove the commas between countries listed and separate unique countries
netflixCountryData <- netflixCountryData %>% separate_rows(country, sep=", ")

#fixing spelling differences
unitedStatesIndex <- which(netflixCountryData$country == "United States")
UKIndex <- which(netflixCountryData$country == "United Kingdom")
netflixCountryData[unitedStatesIndex, ]$country <- "USA"
netflixCountryData[UKIndex, ]$country <- "UK"

#create dataset called world
world <- data.frame()
world <- map_data("world")

TopRating = rep(NA, nrow(world))
world = cbind(world, TopRating)

# appends the most common rating for each country to “world”
for (i in unique(netflixCountryData$country)) {
  index <- which(netflixCountryData$country == i)
  x <- names(which.max(table(netflixCountryData[index, ]$rating)))
  if (length(which(world$region == i)) > 0) {
    index2 <- which(world$region == i)
    world[index2, ]$TopRating <- x
  }
}

#same is repeated for Amazon Prime dataset
unitedStatesIndex <- which(amazonCountryData$country == "United States")
UKIndex <- which(amazonCountryData$country == "United Kingdom")

world2 <- data.frame()
world2 <- map_data("world")

amazonCountryIndex <- which(amazon$country != "")
amazonCountryData <- amazon[amazonCountryIndex, ]
amazonCountryData <- amazonCountryData %>% separate_rows(country, sep=", ")

amazonCountryData[unitedStatesIndex, ]$country <- "USA"
amazonCountryData[UKIndex, ]$country <- "UK"

TopRating <- rep(NA, nrow(world2))
world2 <- cbind(world2, TopRating)
allCountries <- unique(world2$region)
print(allCountries)
for (i in unique(amazonCountryData$country)) {
  index <- which(amazonCountryData$country == i)
  x <- names(which.max(table(amazonCountryData[index, ]$rating)))
  
  if (length(which(world2$region == i)) > 0) {
    index2 <- which(world2$region == i)
    world2[index2, ]$TopRating <- x
  }
}

# 6.3.2 Construction of Graph for Figure 3.3
#graph for Netflix dataset
ggplot(world) + geom_polygon(mapping = aes(x = long , y = lat , group = group , fill = TopRating), color = "black") + coord_quickmap() +
  ggtitle("The Most Common Rating on Netflix for Each Country")

#graph for Amazon Prime dataset
ggplot(world2) + geom_polygon(mapping = aes(x = long , y = lat , group = group , fill = TopRating), color = "black") + coord_quickmap() +
  ggtitle("The Most Common Rating on Amazon for Each Country")

# 6.4 Code for Figure 3.4
# 6.4.1 Data Cleaning for Figure 3.4
# Question 1:  Are tv shows or movies released in comparison per year? 
# Read data
netflix <- read.csv("netflix.csv", header = TRUE, sep = ",", 
                        na.strings = "NA", stringsAsFactors = FALSE)
amazon <- read.csv("amazon.csv", header = TRUE, sep = ",", 
                       na.strings = "NA", stringsAsFactors = FALSE)

# Libraries needed: ggplot2 is used to construct graphs
# gridExtra is for grid.arrange capabilities once graphs are constructed
library(ggplot2)
library(gridExtra)

# Create new dataframes for use with this question and graphs
netflix_year <- netflix
amazon_year <- amazon

# Create character vector with names of variables needed to make graphs
year_vars <- c("release_year", "type")

# Dataframes with only variables that are in the year_vars vector 
netflix_year <- netflix[year_vars]
amazon_year <-  amazon[year_vars]

# Factor type variable in each dataframe with two levels: Movie and TV Show
netflix_year$type <- factor(netflix_year$type, levels = c("Movie", "TV Show"), 
                            labels = c("Movie", "TV Show"))
amazon_year$type <- factor(amazon_year$type, levels = c("Movie", "TV Show"), 
                           labels = c("Movie", "TV Show"))

# 6.4.2 Construction of Graph for Figure 3.4
# Create bar graph for netflix_year
# Counts for bar graph displayed using log10 transformation and after_stat in aesthetics
netflix_bar <- ggplot(data = netflix_year) +
  geom_bar(mapping = aes(x = release_year, y = after_stat(log10(count)), fill = type)) +
  labs(x = "Release Year", y = "log10 Count", title = "Available Netflix listings based on title release year")

# Create bar graph for amazon_year
# Counts for bar graph displayed using log10 transformation and after_stat in aesthetics
amazon_bar <- ggplot(data = amazon_year) + 
  geom_bar(mapping = aes(x = release_year, y = after_stat(log10(count)), fill = type)) + 
  labs(x = "Release Year", y = "log10 Count", title = "Available Amazon listings based on title release year")

# Arrange graph to show each on one row
grid.arrange(netflix_bar, amazon_bar, nrow = 2) 

