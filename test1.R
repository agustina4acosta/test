library(dplyr)
library(ggplot2)

# i define the path to the ZIP file
zip_file_path <- "C:/Users/Usuario/Downloads/SecurityAnalystExam_(1).zip"

#I define the directory where i want to unzip the files
unzip_dir <- "C:/Users/Usuario/Downloads/SecurityAnalystExam"

#I unzip the file
unzip(zip_file_path, exdir = unzip_dir)

#I check which files are available in the directory
list.files(unzip_dir)
# I define the files paths for the CSV files
title_basics_file <- file.path(unzip_dir, "title_basics_2018.csv")
title_ratings_file <- file.path(unzip_dir, "title_ratings.csv")
#I read the CSV files into data frames
title_basics <- read.csv(title_basics_file)
title_ratings <- read.csv(title_ratings_file)

#1
#Filter for comedy genre
comedy_films <- filter(title_basics, genres == "Comedy")
#Count the number of Comedy films
num_comedy_films <- nrow(comedy_films)
print(paste("Number of 2018 films categorized as Comedy:", num_comedy_films))

#2
# i merge both "title_basics" and "title_ratings" by the "tconst" column.
merged_data <- merge(title_basics, title_ratings, by = "tconst")

merged_data$averageRating <- as.numeric(as.character(merged_data$averageRating))
#Filter the high rated films
high_rated_films <- merged_data %>%
  filter(averageRating >= 8.0)
num_high_rated_films <- nrow(high_rated_films)
print(paste("Number of 2018 films with a score of 8.0 or higher:", num_high_rated_films))

#3

#Filter the best film
best_films <- merged_data %>%
  filter(averageRating == max(averageRating, na.rm = TRUE))
best_films_title <- best_films$primaryTitle
best_film_rating <- best_films$averageRating
print(paste("The best film of 2018 is:", best_films_title,"with a rating of", best_film_rating))
#4
#Find out if audiences prefer longer or shorter films.
duration_rating <- merged_data %>%
  group_by(runtimeMinutes)%>%
  summarise(avg_rating = mean(averageRating, na.rm = TRUE))
merged_data <- merged_data %>%
  mutate(runtimeBin = cut(runtimeMinutes, breaks = c(0, 60, 120, 180, 240), 
                          labels = c("0-60", "61-120", "121-180", "181-240")))

# Histogram to view the data
ggplot(merged_data, aes(x = averageRating, fill = runtimeBin)) +
  geom_histogram(binwidth = 0.5, position = "dodge") +
  labs(title = "Distribution of Ratings by Runtime Bin", x = "Average Rating", y = "Count") +
  theme_minimal()



 