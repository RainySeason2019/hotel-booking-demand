# hotel-booking-demand

# Data preparation and description
This dataset comes from the Kaggle(https://www.kaggle.com/jessemostipak/hotel-booking-demand#hotel_bookings.csv) and contains booking information for a city hotel and a resort hotel, and includes information such as when the booking was made, length of stay, the number of adults, children, and/or babies, and the number of available parking spaces, among other things. 


Additionally,the data is originally from the article Hotel Booking Demand Datasets, written by Nuno Antonio, Ana Almeida, and Luis Nunes for Data in Brief, Volume 22, February 2019. The paper claimed that the data comprehend bookings due to arrive between the 1st of July of 2015 and the 31st of August 2017, including bookings that effectively arrived and bookings that were canceled. All personally identifying information has been removed from the data.


There are a total of 119,390 observations and 32 variables in this data set, among which 14 are factor variables and the rest are all numerical variables.

# Codes
codes of EDA.R : data precessing and EDA

codes of the model.R : using tidymodels to make formal modeling and compare three different methods -- logistic regresson, k-nearest-neighbor and decision tree.

# Reproducibility
All data preparation, analysis, plots are reproduced. Run codes of EDA.R  and codes of the model.R can obtain the reproduced results.
