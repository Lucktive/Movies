
# Load packages
library(tidyverse)
library(lubridate)

# import datasets
ratings<- read_csv("ratings_small.csv")
movies <- read_csv("movies_metadata.csv")
str(movies)

# Data Wrangling
# Join datasets
movies <- merge( movies_data, ratings, by.x = 'id', by.y = 'movieId')


#  Extracting month, weekday, quarter and year from the release_date
movies <- movies %>%
  mutate( weekday = wday(release_date),
          month = month(release_date),
          quarter = quarter(release_date),
          year = year(release_date))

# Get distinct values by original_title
movies <- distinct( movies, original_title, .keep_all = TRUE)

# cleaning up the genres column
movies <- movies %>%
  separate(genres, into = c("a","b", "c","d","e","genres"), sep = "'") %>%
  select(-a, -b, -c, -d, -e)

# cleaning up the production_companies column
movies3 <- movies %>%
  separate(production_companies, into = c("a","b", "c","production_companies"), sep = "'") %>%
  select(-a, -b, -c)

# cleaning up the production_country column
movies5 <- movies3 %>%
  separate(production_countries, into = c("a","b", "c","production_countries"), sep = "'") %>%
  select(-a, -b, -c)

# selecting relevant columns
movies <- movies5 %>%
  select(-adult,-belongs_to_collection, -homepage, -overview, -poster_path, -spoken_languages,-tagline, -video, -userId)

# classifying the budget column
movies <- movies %>%
  mutate(budget_classification = 
           ifelse(budget > 100000000, "Super_Big_Budget",
           ifelse( budget > 50000000, "Big_Budget",
           ifelse( budget > 0000000, "Mediun_Budget","Small_Budget"))))

# classifying the movies
movies <- movies %>%
  mutate(movie_classification = 
           ifelse(revenue > 4*budget, "Superhit",
           ifelse(revenue > 2.5*budget, "Blockbuster",
           ifelse(revenue > 1.5*budget, "Minor_Success","Flop"))))

write_csv(movies, "movies.csv")

# Basic statistics on the dataset

movies_num <- movies %>%
  select (rating, budget, popularity,revenue, runtime
          ,vote_average, vote_count)
movies_sum <- summary(movies_num)
movies_cor <- cor(movies_num)

# Rating distribution
ggplot( data = movies, aes(x = rating)) +
  geom_histogram( binwidth = 1  , fill = "orange", color = "black") +
  ggtitle(" RATINGS DISTRIBUTION")

ggplot( data = movies, aes(x = rating)) +
  geom_boxplot(fill = "orange", color = "black") +
  ggtitle(" RATINGS DISTRIBUTION")

# Budget distribution
ggplot( data = movies, aes( x = budget )) +
  geom_histogram(fill = "orange", color = "black") +
  ggtitle(" BUDGET DISTRIBUTION")

ggplot( data = movies, aes( x = budget )) +
  geom_boxplot(fill = "orange", color = "black") +
  ggtitle(" BUDGET DISTRIBUTION")

# Popularity distribution
ggplot( data = movies, aes( x = popularity)) +
  geom_histogram(fill = "orange", color = "black") +
  ggtitle(" POPULARITY DISTRIBUTION")

ggplot( data = movies, aes( x = popularity)) +
  geom_boxplot(fill = "orange", color = "black") +
  ggtitle(" POPULARITY DISTRIBUTION")

# Release date distribution
ggplot( data= movies, aes( x = release_date)) +
  geom_boxplot(fill = "orange", color = "black") +
  ggtitle(" RELEASE DATE DISTRIBUTION")

ggplot( data= movies, aes( x = release_date)) +
  geom_histogram(fill = "orange", color = "black") +
  ggtitle(" RELEASE DATE DISTRIBUTION")


# Runtime distribution
ggplot( data= movies, aes( x = runtime)) +
  geom_boxplot(fill = "orange", color = "black") +
  ggtitle(" RUNTIME DISTRIBUTION")

ggplot( data= movies, aes( x = runtime)) +
  geom_histogram(fill = "orange", color = "black") +
  ggtitle(" RUNTIME DISTRIBUTION")

# Production Countries distribution
top_countries <- movies %>%
  group_by(production_countries) %>%
  filter(production_countries != "NA")%>%
  summarise( count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

ggplot( data= top_countries, aes( x = reorder(production_countries, count), y = count)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  coord_flip() +
ggtitle("TOP PRODUCTION COUNTRIES DISTRIBUTION")

# Genres distribution
movie_genre <- movies %>%
  group_by(genres) %>%
  summarise(frequency = n())%>%
  arrange(desc(frequency))%>%
  filter(genres != "NA")
ggplot(data = movie_genre, aes(x = reorder(genres, frequency), y = frequency)) +
  geom_bar( stat = "identity" ,fill = "orange", color = "black") +
  coord_flip() +
  xlab("genre") +
  ggtitle(" GENRE DISTRIBUTION")

# Original Language distribution
movie_lang <- movies %>%
  group_by(original_language) %>%
  summarise(frequency = n())%>%
  arrange(desc(frequency))%>%
  filter(frequency >10)

ggplot(data = movie_lang, aes(x = reorder(original_language, frequency), y = frequency)) +
  geom_bar( stat = "identity" ,fill = "orange", color = "black") +
  coord_flip() +
  xlab(  "original language") +
  ggtitle(" ORIGINAL LANGUAGE DISTRIBUTION")

# Production Companies distribution
movie_companies <- movies %>%
  group_by(production_companies) %>%
  summarise(frequency = n())%>%
  filter(frequency >10 & production_companies != "NA") %>%
  arrange(desc(frequency))

ggplot(data = movie_companies, aes(x = reorder(production_companies, frequency), y = frequency)) +
  geom_bar( stat = "identity" ,fill = "orange", color = "black") +
  coord_flip() +
  xlab(  "production companies" ) +
  ggtitle("PRODUCTION COMPANIES DISTRIBUTION")

# TRENDS
# Budget Trend
ggplot( data = movies, aes( x = year ,y = budget, color = genres)) +
  geom_point() +
  theme(legend.position = "right") +
  ggtitle("BUDGET TREND")

# Revenue Trend
revenue_trend <- movies %>%
  select(year, revenue) %>%
  group_by(year)%>%
  summarise(revenue = sum(revenue))%>%
  filter( year > 1990)
ggplot( data = revenue_trend, aes( x = year ,y = revenue)) +
  geom_col(fill = "orange", color = "black") +
  ggtitle("REVENUE TREND")

# Analysis
#top revenue generating movies                      
top_revenue <- movies  %>%
  select( original_title, revenue) %>%
  arrange(desc(revenue))
top_revenue_movies <- head(top_revenue, 10)

ggplot(data = top_revenue_movies, aes(x = reorder(original_title, revenue), y = revenue)) +
  geom_col(fill = "orange", color = "black") +
  coord_flip()
ggtitle(" TOP REVENUE MOVIES")

#top budget  movies                      
top_budget <- movies  %>%
  select( original_title, budget) %>%
  arrange(desc(budget))
top_budget_movies <- head(top_budget, 10)
ggplot(data = top_budget_movies, aes(x = reorder(original_title, budget), y = budget)) +
  geom_col(fill = "orange", color = "black") +
  coord_flip()
ggtitle(" TOP BUDGET MOVIES")

# most popular  movies                      
most_popular <- movies  %>%
  select( original_title, popularity) %>%
  arrange(desc(popularity))
most_popular_movies <- head(most_popular, 10)
ggplot(data = most_popular_movies, aes(x = reorder(original_title, popularity), y = popularity)) +
  geom_col(fill = "orange", color = "black") +
  coord_flip()
ggtitle(" MOST POPULAR MOVIES")

#top budget and revenue  movies                  
budget_revenue <- movies  %>%
  select( original_title, budget, revenue, genres, rating) %>%
  arrange(desc(budget))
ggplot(data = budget_revenue, aes(x =  budget, y = revenue, color = genres)) +
  geom_point() +
  theme(legend.position = "right") +
  ggtitle(" TOP BUDGET AND REVENUE MOVIES BY GENRE")

#top revenue and rating  movies                      
top_revenue_rating <- movies  %>%
  select( original_title, budget, genres, revenue, rating) %>%
  arrange(desc(revenue))
ggplot(data = top_revenue_rating, aes(y =  revenue, x= rating, fill= genres ))+
  geom_col( stat= "identity") +
  ggtitle(" REVENUE AND RATING MOVIES BY GENRE")


#most popular and top revenue                      
top_popular_revenue <- movies  %>%
  select( original_title, revenue,genres,popularity) %>%
  arrange(desc(popularity))
ggplot(data = top_popular_revenue, aes( x =  popularity, y= revenue, fill= genres ))+
  geom_point() +
  ggtitle(" REVENUE AND POPULARITY MOVIES BY GENRE")

# Revenue by month
revenue_month <- movies %>%
  select( month, revenue) %>%
  group_by (month) %>%
  summarise( revenue = sum(revenue))%>%
  filter( month != "NA")
ggplot(data =revenue_month, aes( x= month, y =  revenue))+
  geom_col(  fill= "orange", color = "black") +
  ggtitle(" REVENUE BY MONTH")

# Revenue by quarter
revenue_quarter <- movies %>%
  select( quarter, revenue) %>%
  group_by (quarter) %>%
  summarise( revenue = sum(revenue))%>%
  filter( quarter != "NA")
ggplot(data =revenue_quarter, aes( x= quarter, y =  revenue))+
  geom_col(  fill= "orange", color = "black") +
  ggtitle(" REVENUE BY QUARTER")

# Superhit and Blockbuster movies by  top production companies 
companies <- movies %>%
  filter( movie_classification == "Superhit" |
            movie_classification == "Blockbuster")%>%
  group_by(production_companies) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)
ggplot( data = companies, aes( x= reorder(production_companies, count),
                               y= count)) +
  geom_bar( stat = "identity", color = "black", fill = "orange") +
  coord_flip() +
  labs(x = "Production Companies") +
  ggtitle(" Top Production Companies")

# Top Non_English Original language with Superhit and Blockbuster movies
non_english <- movies %>%
  filter(original_language != "en",
         movie_classification == "Superhit" |
           movie_classification == "Blockbuster") %>%
  group_by(original_language) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)               
ggplot( data = non_english, aes( x= reorder(original_language, count),
                                 y= count)) +
  geom_bar( stat = "identity", color = "black", fill = "orange") +
  coord_flip() +
  labs(x = "Original Languages")
ggtitle(" Top Original Language")

# year with highest produced Flop Movies
flop <- movies %>%
  filter( movie_classification == "Flop" ) %>%
  group_by(year) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
ggplot( data = flop, aes( x= year, y= count)) +
  geom_line() +
  ggtitle(" Flop Movies Trend")

# year with highest produced Superhit Movies
superhit <- movies %>%
  filter( movie_classification == "Superhit" ) %>%
  group_by(year) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
ggplot( data = superhit, aes( x= year, y= count)) +
  geom_line() +
  ggtitle(" Superhit Movies Trend")

#  Superhit Movies Genres Distribution
genre <- movies %>%
  filter( movie_classification == "Superhit", genres != "NA") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
ggplot( data = genre, aes( x= reorder(genres, count),
                           y= count)) +
  geom_bar( stat = "identity", color = "black", fill = "orange") +
  coord_flip() +
  labs(x = "Genres")
ggtitle(" Superhit Movies Genre")

