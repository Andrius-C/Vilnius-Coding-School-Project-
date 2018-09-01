# Vilnius-Coding-School-Project-

Information: 

1. “Capital Bikeshare” bicycle rent data from Washington, D.C.,USA
2. 2 years hourly data for short-term bicycle rents along with weather data
3. Data is taken from online Data Science competition site Kaggle> https://www.kaggle.com/c/bike-sharing-demand
4. Data is split between Train and Test datasets. Train set includes all the data where days of the month are between 1 and 19, Test data includes all the data where days of the month are between 20 and 31. Test data set doesn’t contain bike rent numbers

Data Fields:

datetime - hourly date + timestamp 
season -  1 = winter, 2 = spring, 3 = summer, 4 = autumn 
holiday - whether the day is considered a holiday
workingday - whether the day is neither a weekend nor holiday
weather - 1: Clear, Few clouds, Partly cloudy, Partly cloudy 
2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds 
4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
temp - temperature in Celsius
atemp - "feels like" temperature in Celsius
humidity - relative humidity
windspeed - wind speed
casual - number of non-registered user rentals initiated
registered - number of registered user rentals initiated
count - number of total rentals

Objectives:

Compare linear regression and random forest models performance in predicting bicycle rent demand for "Capital Bikeshare" 
