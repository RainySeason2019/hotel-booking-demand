library(tibble)
library(tidyr)
library(dplyr)
library(skimr)
library(nnet)
library(GGally)
library(ggplot2)
library(corrplot)
library(pROC)
library(plotROC)
library(RColorBrewer)
library(e1071)

suppressMessages(library(tibble))
suppressMessages(library(tidyr))
suppressMessages(library(reshape2))
suppressMessages(library(dplyr))
suppressMessages(library(skimr))
suppressMessages(library(nnet))
suppressMessages(library(GGally))
suppressMessages(library(ggplot2))
suppressMessages(library(corrplot))
suppressMessages(library(pROC))
suppressMessages(library(plotROC))
suppressMessages(library(e1071))
suppressMessages(library(rpart))
suppressMessages(library(RColorBrewer))


## Load the dataset

hotel = read.csv('hotels.csv')
hotel = as_tibble(hotel)
head(hotel)
# glimpse(hotel)

dim(hotel)

## Let's get an overview of the dataset’s characteristics.

skim(hotel)


# Exploratory data analysis

## Handle the missing values

is.na(hotel)
which(is.na(hotel), arr.ind = T) # column 11 has missing values
names(hotel)[11]
hotel = replace_na(hotel, list(children = 0))
#which(is.na(hotel), arr.ind = T)


## Handle the columns "meal" and "required_car_parking_spaces"

hotel <- hotel %>%
  mutate(
    meal = case_when(
      meal == "SC" ~ "meal_none",
      meal == "Undefined" ~ "meal_none",
      TRUE ~ as.character(meal)
    ),
    required_car_parking_spaces = case_when(
      required_car_parking_spaces > 0 ~ "parking",
      TRUE ~ "none"
    ),
  )
X_meal = distinct(hotel, meal)
X_meal

## Set dummy variables based on column "meal"
meal_dummy = as_tibble(class.ind(hotel$meal))
hotel = hotel %>% transform("BB" = meal_dummy$BB) %>%
  transform("FB" = meal_dummy$FB) %>%
  transform("HB" = meal_dummy$HB) %>%
  transform("meal_none" = meal_dummy$meal_none)

# dim(hotel)  119390 X 36

## Let’s filter to only the bookings that did not cancel.

hotel_0 = filter(hotel, is_canceled == 0)
head(hotel_0)

## Let's plot how the features are correlated to each other.

hotel_numeric = tibble(zero = rep(0, nrow(hotel_0)))
temp_name = c("zero")
for(i in 1:ncol(hotel_0))
{
  if(class(hotel_0[[i]]) == "integer" | class(hotel_0[[i]]) == "numeric")
  {
    temp = unlist(select(hotel_0, i))
    temp_name = c(temp_name, names(select(hotel_0, i)))
    hotel_numeric = add_column(hotel_numeric, temp)
    names(hotel_numeric) = paste("X", 1:ncol(hotel_numeric),sep = "")
  }
}
names(hotel_numeric) = temp_name
# dim(hotel_numeric) ## 75166    18

hotel_numeric = select(hotel_numeric, -c(zero, is_canceled))


## plot heatmap
cor_hotel = as.data.frame(cor(hotel_numeric))
cor_hotel$ID = rownames(cor_hotel)
require(reshape2)
cor_hotel_0 = melt(cor_hotel, id.vars = c("ID"))
#view(cor_hotel_0)
p <- ggplot(cor_hotel_0, aes(x = variable, y = ID, fill = value))+
  theme(panel.background = element_blank(),axis.line = element_line(colour="black"))+
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_blank()) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p + geom_raster()

## Do elders with children choose to eat in hotels? Is this different in the city and the resort hotel?

ggplot(hotel_0, aes(x = children)) + 
  geom_bar(aes(fill = factor(meal)), position="fill") +
  labs(y = "proportion")

ggplot(hotel_0, aes(x = children)) + 
  geom_bar(aes(fill = factor(meal)), position="fill") +
  labs(y = "proportion")

ggplot(hotel_0, aes(x = babies)) + 
  geom_bar(aes(fill = factor(meal)), position="fill") +
  labs(y = "proportion") +
  scale_x_continuous(breaks = seq(min(hotel_0$babies), max(hotel_0$babies), 1))

#theme(panel.background = element_blank())


#ggplot(hotel_0, aes(children)) +
geom_bar(aes(fill = meal)) +
  coord_polar(theta = "y")


## Does the availability of parking affect people's choice to eat in hotels? Is this different in the city and the resort hotel?

hotel_0 %>%
  count(hotel, required_car_parking_spaces, meal) %>%
  group_by(hotel, meal) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(required_car_parking_spaces, proportion, fill = meal)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow = 2) +
  labs(x = NULL, y = "Proportion of hotel stays")

########################################
#hotel_0 %>%
#  select(
#    meal, adr,
#    required_car_parking_spaces,
#    total_of_special_requests
#  ) %>%
#  ggpairs(mapping = aes(color = meal))
########################################

## Does the average daily rate affect people's choice to eat in hotels?

ggplot(hotel_0, aes(x = meal, y = adr)) + 
  geom_boxplot(fill = brewer.pal(7, "Blues")[4], color = brewer.pal(7, "Blues")[6]) +
  labs(y = "Average Daily Rate")


# Formal Modeling



## From the plot, we can know the number of children does not have much effect on whether people choose to eat in a hotel.

hotel = hotel %>% mutate(
  children = case_when(
    children > 0 ~ "children",
    TRUE ~ "none"
  )
)
distinct(hotel, children)

# "meal_none" <- 0
# "HB" <- 1
# "FB" <- 1
# "BB" <- 1

temp_meal = sub("meal_none", "0", hotel$meal) 
temp_meal = sub("HB", "1", temp_meal) 
temp_meal = sub("FB", "1", temp_meal) 
temp_meal = sub("BB", "1", temp_meal)
temp_meal = as.numeric(temp_meal)
hotel = add_column(hotel, temp_meal)
hotel = rename(hotel, order_meal = temp_meal)









