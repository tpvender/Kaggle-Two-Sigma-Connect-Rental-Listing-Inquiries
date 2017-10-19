libs <- c("jsonlite", "dplyr", "purrr", "lubridate", "nnet", "tidyr", "ggplot2", "scales", "caret","xgboost","MLmetrics","tidytext","reshape2","maps","mapproj","corrplot")
lapply(libs, require, character.only = TRUE)
#json_train holds original JSON data
json_train<-fromJSON('/Users/Taylor/Desktop/Pred 413 Midterm/train.json')
vars <- setdiff(names(json_train), c("photos", "features"))
#tib_train holds tibble of (original) data
tib_train <- map_at(json_train, vars, unlist) %>% tibble::as_tibble(.)
#train holds subset of features in tib_train
train <- select(tib_train, c(listing_id, bedrooms, bathrooms, created, price, street_address, interest_level, longitude, latitude, building_id, manager_id, features))
#create dataframe for eda
train_form <- train
train_form.df <- as.data.frame(bind_cols(train_form))
#put variables into usable format and store in variable train_form
train.df2 <- train_form.df
train.df2$created <- as.Date(unlist(train.df2$created))
train.df2$longitude <- as.numeric(unlist(train.df2$longitude))
train.df2$latitude <- as.numeric(unlist(train.df2$latitude))
train.df2$building_id <- as.character(unlist(train.df2$building_id))
train.df2$manager_id <- as.character(unlist(train.df2$manager_id))
train.df2$interest_level <- as.factor(unlist(train.df2$interest_level))
train.df2$interest_level <- factor(train.df2$interest_level, levels = c("high", "medium", "low"))
#check for missing values
#create function to find missing variable
propmiss <- function(dataframe) {
 	m <- sapply(dataframe, function(x) {
 		data.frame(
 			nmiss=sum(is.na(x)), 
 			n=length(x), 
 			propmiss=sum(is.na(x))/length(x)
 		)
 	})
 	d <- data.frame(t(m))
 	d <- sapply(d, unlist)
 	d <- as.data.frame(d)
 	d$variable <- row.names(d)
 	row.names(d) <- NULL
 	d <- cbind(d[ncol(d)],d[-ncol(d)])
 	return(d[order(d$propmiss), ])
 }
propmiss(train.df2)
 #look at map of interest level with cener of NYC coordinates as center
ny_lat <- 40.785091
ny_lon <- -73.968285
interest_map <- gg(train.df2, aes(x=longitude, y=latitude))
interest_map <- interest_map + geom_point(aes(color = interest_level), size = 1)
interest_map <- interest_map + borders("county")
interest_map <- interest_map + coord_map("ortho", orientation = c(ny_lat, ny_lon, 0), xlim = c(-74.25909, -73.700009), ylim = c(40.477399, 40.917577))
interest_map <- interest_map + ggtitle("Rental Listing Interest Level by Location")
#check to see how many of the listings in the training set fall into the three interest level 
interest_map

summary(train.df2$interest_level)
#property manager and interest level
managers <- train.df2 %>% group_by(manager_id, interest_level) %>% 
         summarise(no_rows = length(interest_level)) %>% 
         spread(interest_level, no_rows) %>% filter(!is.na(high))
managers$medium[is.na(managers$medium)] <- 0
managers$low[is.na(managers$low)] <- 0
managers <- filter(managers, (low + medium + high) > 20)
managers <- managers %>% mutate(per = 100 * high / (low + medium + high))
managers <- arrange(managers, desc(per))
managers <- head(managers, 15)
manage_plot <- ggplot(managers, aes(x = reorder(manager_id, per, sum),
                    y = per))
manage_plot <- manage_plot + labs(x="Manager Id", y="High Interest (% of Total Listings)")
manage_plot <- manage_plot + ggtitle("Most Popular Managers") + 
         theme(plot.title = element_text(hjust = 0.5))
manage_plot <- manage_plot + geom_bar(stat = "identity",  colour = "green", fill = "lightgreen") + coord_flip()
manage_plot <- manage_plot + theme(legend.position="bottom", legend.direction="horizontal",
               legend.title = element_blank())
manage_plot
#time series
time.df <- train.df2 %>% group_by(created) %>% count(interest_level)
timeplt <- ggplot(time.df, aes(created, n))
timeplt <- timeplt + geom_line(aes(color = interest_level), size = 0.75)
timeplt <- timeplt + ylab("Number of Listings") + theme(legend.position = "bottom")
timeplt

#now looking at correlations
rentals <- select(train, c(listing_id, bedrooms, bathrooms, created, price, street_address, interest_level, longitude, latitude))
#create new features
rentals$numPhotos <- sapply(1:nrow(tibble_train), function(x) length(unlist(tibble_train$photos[x])))
rentals$numFeatures <- sapply(1:nrow(tibble_train), function(x) length(unlist(tibble_train$features[x])))
rentals$created_dates <- ymd_hms(rentals$created)
#interest_level is a categorical variable so it makes sense for it to be a factor    
rentals['interest_level_factor'] <- factor(rentals[['interest_level']], levels = c("low", "medium", "high"))

summary(rentals)

options(warn = -1)
#creates an outlier function for me to use FROM Exploring Renthop Data - The Missing Stock
detect_outlier <- function(column) {
  iqr = quantile(column, .75) - quantile(column, .25)
  low = quantile(column, .25) - 1.5 * iqr
  high = quantile(column, .75) + 1.5 * iqr
  outlier_index = ifelse(column < low | column > high, TRUE, FALSE)
  results <- list("index" = outlier_index, "values"  = column[outlier_index], 'low' = low, 'high' = high, 'iqr' = iqr)
  return(results)
}

#create new dataframe with outliers removed
outliers <- detect_outlier(rentals[['price']])
cleaned_rentals <- rentals[!outliers$index, ]

#visualizing price distribution by interest level with and without outliers

#plot of the price distributions with outliers removed
ggplot(cleaned_rentals, aes(x = interest_level_factor, y = price) +
    geom_violin(aes(fill = interest_level_factor)) +
    scale_y_continuous(label = comma) +
    guides(fill = FALSE)

q = quantile(rentals$price, .99)
#plot of the price dist without outliers removed
ggplot(rentals, aes(x = interest_level_factor, y = price)) +
    geom_violin(aes(fill = interest_level_factor)) + 
    scale_y_continuous(label = comma, limits =c(0, q)) +
    guides(fill = FALSE)
    
#summarizing room counts table for bar chart  
room_counts <- group_by(rentals, interest_level_factor, bedrooms) %>%
  summarize(room_count = n()) %>% 
  ungroup() %>% 
  group_by(interest_level_factor) %>% 
  mutate(total_bedrooms = sum(room_count)) %>%
  mutate(p = room_count/total_bedrooms)

#number of bedrooms probability bar chart  
ggplot(room_counts, aes(bedrooms, p)) +
geom_bar(aes(fill = interest_level_factor), stat = "identity")  +
facet_grid(.~interest_level_factor) +
ylab("probability") +
guides(fill = FALSE) + 
list()

group_by(rentals, interest_level_factor) %>%
    summarize(
              count = n(),
              min = min(bedrooms),
              q.25 = quantile(bedrooms, .25),
              median_bedrooms = quantile(bedrooms, .5),
              average_bedrooms = mean(bedrooms),
              q.75 = quantile(bedrooms, .75),
              max = max(bedrooms)
             ) 
#bathrooms
#create prop 
counts_by_variable <- group_by(rentals, interest_level_factor, bathrooms) %>% 
  summarize(counts = n())
counts_by_group <- group_by(rentals, interest_level_factor) %>% 
  summarize(totals = n()) 
proportions <- left_join(counts_by_variable, counts_by_group, by = "interest_level_factor") %>% 
  mutate(probability = counts/totals)
#histogram
ggplot(proportions, aes(x = bathrooms, y = probability)) +
  geom_bar(aes(fill = interest_level_factor), stat = "identity") +
  facet_grid(~interest_level_factor) +
  scale_x_continuous(limits = c(-1, 5)) +
  guides(fill = FALSE) +   
  list()
#summary of bathrooms by interest level 
group_by(rentals, interest_level_factor) %>%
    summarize(
              count = n(),
              min = min(bathrooms),
              q.25 = quantile(bathrooms, .25),
              median_bathrooms = quantile(bathrooms, .5),
              average_bathrooms = mean(bathrooms),
              q.75 = quantile(bathrooms, .75),
              max = max(bathrooms)
             )

