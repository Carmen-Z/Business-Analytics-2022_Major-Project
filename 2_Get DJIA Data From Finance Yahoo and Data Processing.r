
# 1. Get all the DJIA index so that we can plot with R
install.packages('pdfetch')
library(pdfetch)
pdfetch_YAHOO('^DJI;')
DJIA = pdfetch_YAHOO('^DJI;')
View(DJIA)

plot.ts(DJIA$`^DJI;.close`)

### Get the DJIA index with specific period so that we can plot with R
install.packages('quantmod')
library(quantmod)

getSymbols('^DJI;', from='2011-01-01', to = '2022-01-01')
library(ggplot2)

dj_close <- DJI[,'DJI.Adjusted']

### compute log returns by taking advantage of 
### CalculateReturns within PerformanceAnalytics package.
install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)

dj_ret <- CalculateReturns(dj_close, method = 'log')

dj_ret <- na.omit(dj_ret)

### Sharp increases and decreases in volatility can be eye-balled
plot(dj_ret)

## Basic Statistics

#### Conversion from xts to dataframe with year and value column.
#### That allows for summarizations and plots on year basis.
xts_to_dataframe <- function(data_xts) {
  df_t <- data.frame(year = factor(year(index(data_xts))), value = coredata(data_xts))
  colnames(df_t) <- c( "year", "value")
  df_t
}

dj_ret_df <- xts_to_dataframe(dj_ret)

basicStats(dj_ret)

#### Enhanced summaries statistics for data stored as data frame columns.
install.packages("fBasics")
library(fBasics)
bs_rn <- rownames(basicStats(rnorm(10,0,1)))# gathering the basic stats dataframe output row names that get lost with tapply()
dataframe_basicstats <- function(dataset) {
  result <- with(dataset, tapply(value, year, basicStats))
  df_result <- do.call(cbind, result)
  rownames(df_result) <- bs_rn
  as.data.frame(df_result)
}

(dj_stats <- dataframe_basicstats(dj_ret_df))

### Correlation plots
### Here below are the total and partial correlation plots.
acf(dj_ret)
pacf(dj_ret)

# use library 'quantmod' in package 'tidyquant'
install.packages('tidyquant')
library(tidyquant)

# 2. Prepare for the DJIA index data
## get the stock data from Yahoo Finance 
## so that we can process the data with Excel.

dji_11_21_stock <- tq_get('^DJI;',
               from = "2011-01-01",
               to = "2022-01-01",
               get = "stock.prices")

head(dji_11_21_stock)

## Then, store the data on the local computer

write.csv(dji_11_21_stock,"D:\\1-NUI Galway\\new_dji_stock_11_21.csv", 
          row.names = FALSE)

### Plot it
library(ggplot2)

dji_11_21_stock %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  theme_classic() +
  labs(x = 'date',
       y = "adjusted",
       title = "DJIA price chart" +
         scale_y_continuous(breaks = seq(0,300,10)))


# 3. combine reddit news headlines from 2011 to 2021 with only the top 25 headlines each day
## read the reddit news headlines data year by year.

year_2016_dataset <- read.csv("D:\\0-MS5115 Business Analytics Major Project\\2-Data Sets\\web scraping_Reddit_News\\year_2016_dataset.csv",
                          header = T)
year_2017_dataset <- read.csv("D:\\0-MS5115 Business Analytics Major Project\\2-Data Sets\\web scraping_Reddit_News\\year_2017_dataset.csv",
                              header = T)
year_2018_dataset <- read.csv("D:\\0-MS5115 Business Analytics Major Project\\2-Data Sets\\web scraping_Reddit_News\\year_2018_dataset.csv",
                              header = T)
year_2019_dataset <- read.csv("D:\\0-MS5115 Business Analytics Major Project\\2-Data Sets\\web scraping_Reddit_News\\year_2019_dataset.csv",
                              header = T)
year_2020_dataset <- read.csv("D:\\0-MS5115 Business Analytics Major Project\\2-Data Sets\\web scraping_Reddit_News\\year_2020_dataset.csv",
                              header = T)
year_2021_dataset <- read.csv("D:\\0-MS5115 Business Analytics Major Project\\2-Data Sets\\web scraping_Reddit_News\\year_2021_dataset.csv",
                              header = T)

attach(year_2016_dataset)
dim(year_2016_dataset)
head(year_2016_dataset)


## Put the columns into a new data frame

new_year_2016_dataset <- data.frame(date = year_2016_dataset$date,
                                    score = year_2016_dataset$score, 
                                    num_comments = year_2016_dataset$num_comments, 
                                    title = year_2016_dataset$title)

new_year_2017_dataset <- data.frame(date = year_2017_dataset$date,
                                    score = year_2017_dataset$score, 
                                    num_comments = year_2017_dataset$num_comments, 
                                    title = year_2017_dataset$title)

new_year_2018_dataset <- data.frame(date = year_2018_dataset$date,
                                    score = year_2018_dataset$score, 
                                    num_comments = year_2018_dataset$num_comments, 
                                    title = year_2018_dataset$title)

new_year_2019_dataset <- data.frame(date = year_2019_dataset$date,
                                    score = year_2019_dataset$score, 
                                    num_comments = year_2019_dataset$num_comments, 
                                    title = year_2019_dataset$title)

new_year_2020_dataset <- data.frame(date = year_2020_dataset$date,
                                    score = year_2020_dataset$score, 
                                    num_comments = year_2020_dataset$num_comments, 
                                    title = year_2020_dataset$title)

new_year_2021_dataset <- data.frame(date = year_2021_dataset$date,
                                    score = year_2021_dataset$score, 
                                    num_comments = year_2021_dataset$num_comments, 
                                    title = year_2021_dataset$title)

## Convert utc time to the date format '%Y-%m-%d'

new_year_2016_dataset$date <- format(as.POSIXct(new_year_2016_dataset$date), 
                             format = '%Y-%m-%d')

new_year_2017_dataset$date <- format(as.POSIXct(new_year_2017_dataset$date), 
                                     format = '%Y-%m-%d')

new_year_2018_dataset$date <- format(as.POSIXct(new_year_2018_dataset$date), 
                                     format = '%Y-%m-%d')

new_year_2019_dataset$date <- format(as.POSIXct(new_year_2019_dataset$date), 
                                     format = '%Y-%m-%d')

### new_year_2020_dataset <- new_year_2020_dataset[!(new_year_2020_dataset$date == ""), ]

new_year_2020_dataset$date <- format(as.POSIXct(new_year_2020_dataset$date),
                                     format = '%Y-%m-%d')

new_year_2021_dataset$date <- format(as.POSIXct(new_year_2021_dataset$date), 
                                     format = '%Y-%m-%d')

## sort date in ascending order, 
## followed by score and num_comments in descending order

library(dplyr)

new_year_2016_dataset <- new_year_2016_dataset[
  order(new_year_2016_dataset$date,
        desc(new_year_2016_dataset$score), 
        desc(new_year_2016_dataset$num_comments)),]

new_year_2017_dataset <- new_year_2017_dataset[
  order(new_year_2017_dataset$date,
        desc(new_year_2017_dataset$score), 
        desc(new_year_2017_dataset$num_comments)),]

new_year_2018_dataset <- new_year_2018_dataset[
  order(new_year_2018_dataset$date,
        desc(new_year_2018_dataset$score), 
        desc(new_year_2018_dataset$num_comments)),]

new_year_2019_dataset <- new_year_2019_dataset[
  order(new_year_2019_dataset$date,
        desc(new_year_2019_dataset$score), 
        desc(new_year_2019_dataset$num_comments)),]

new_year_2020_dataset <- new_year_2020_dataset[
  order(new_year_2020_dataset$date,
        desc(new_year_2020_dataset$score), 
        desc(new_year_2020_dataset$num_comments)),]

new_year_2021_dataset <- new_year_2021_dataset[
  order(new_year_2021_dataset$date,
        desc(new_year_2021_dataset$score), 
        desc(new_year_2021_dataset$num_comments)),]

## Use rbind instead of merge to combine all the Reddit news headlines 
## from 2011 to 2021 (since the structure is the same)

news_headlines_dataset <- rbind(new_year_2011_dataset,
                                new_year_2012_dataset,
                                new_year_2013_dataset,
                                new_year_2014_dataset,
                                new_year_2015_dataset,
                                new_year_2016_dataset,
                                new_year_2017_dataset, 
                                new_year_2018_dataset,
                                new_year_2019_dataset, 
                                new_year_2020_dataset, 
                                new_year_2021_dataset)

## Get daily top 25 headlines with score and number of comments
install.packages('dplyr')
library(dplyr)

top_news_headlines_dataset <- news_headlines_dataset %>% 
  group_by(date) %>% 
  top_n(25, score)

top_news_headlines_dataset <- news_headlines_dataset %>% 
  group_by(date) %>% 
  top_n(25, num_comments)

write.csv(top_news_headlines_dataset,"D:\\0-MS5115 Business Analytics Major Project\\2-Data Sets\\web scraping_Reddit_News\\news_headlines_top.csv", 
          row.names = FALSE)

### after using Excel to get the exact top 25 for 2020 news headlines
### load the data here
top_news_headlines_2020_dataset <- read.csv("D:\\0-MS5115 Business Analytics Major Project\\2-Data Sets\\web scraping_Reddit_News\\top_news_headlines_2020_dataset.csv",
                                            header = T)



# If there are more than 25 headlines each day 
# because there could be a similar number in num_comments, 
# check the file with Excel and then process 
# and store the data with only 25 headlines per day.

### Creating a wide table of News Headlines Data from a long table
install.packages('tidyr')
library(tidyr)
Reddit_News_Headlines_2011_2021 <- read.csv("D:\\0-MS5115 Business Analytics Major Project\\2-Data Sets\\2011-2021 Finalized Data for ML\\2.1_Reddit News Headlines 2011_2021.csv",
                              header = T)
attach(Reddit_News_Headlines_2011_2021)
Reddit_News_Headlines_2011_2021.wide <- pivot_wider(Reddit_News_Headlines_2011_2021, 
                                                    names_from = top, 
                                                    values_from = title)

write.csv(Reddit_News_Headlines_2011_2021.wide,"D:\\0-MS5115 Business Analytics Major Project\\2-Data Sets\\2011-2021 Finalized Data for ML\\2.2_Reddit_News_Headlines_Wide_2011_2021.csv", 
          row.names = FALSE)
