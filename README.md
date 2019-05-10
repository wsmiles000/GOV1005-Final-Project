# Effects of President Trump's Tweets on Stock Price & Volume

The following repository contains an interactive [RShiny](https://shiny.rstudio.com/) application that allows users to compare President Trump's tweets with intraday stock pricing and trade volume fluctuations. Using data from the [Twitter API](https://developer.twitter.com/) and the [QuantTools](https://cran.r-project.org/web/packages/QuantTools/QuantTools.pdf) library, users can use the dashboard to search for keywords in Trump's tweets, click the relevant date value in the table, and then view an interactive pricing visualization of certain stocks on that exact tweet's date. 

Because not all the financial data is uniformly collected, users can select a different trading interval in order to best fit the graph. Should a certain interval appear to have missing values, I'd recommend switching the view period to 30 min for a more complete graph. The financial data contains most large-cap publicly trade companies, but many large-cap intraday data is still unavailable due to public data limitations. 


# Usage 

##  Data Scraping
 In order to pull the most up to date tweets, users can run the `Twitter.R` script in their console which will automatically update the tweet data to pull the latest tweets and store them in the `tweet_data` subfolder. 

## RStudio 
The interface can be run through the RStudio desktop client, but users must first install all relevant packages and libraries (available at the top of the `app.R` file. After installing the relevant dependencies, users can run the application locally through the RStudio desktop client. 

### Check out the live applicaiton [here](https://wsmiles000.shinyapps.io/Trump-Tweets-Stock-Pricing/)! 


