---
title: "Project 3"
description: "Nullam et orci eu lorem consequat tincidunt vivamus et sagittis magna sed nunc rhoncus condimentum sem. In efficitur ligula tate urna. Maecenas massa sed magna lacinia magna pellentesque lorem ipsum dolor. Nullam et orci eu lorem consequat tincidunt. Vivamus et sagittis tempus."
slug: "Project3"
image: pic06.jpg
keywords: ""
categories: 
    - ""
    - ""
date: 2017-10-31T22:26:13-05:00
draft: false
---

```{r, setup}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```

```{r load-libraries, warning=FALSE, message=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(vroom)
library(tidyquant)
```

# Returns of financial stocks

```{r load_nyse_data, message=FALSE, warning=FALSE}
nyse <- read_csv(here::here("data","nyse.csv"))

```

Based on this dataset, create a table and a bar plot that shows the number of companies per sector, in descending order

```{r companies_per_sector, warning=FALSE, message=FALSE}
# YOUR CODE GOES HERE
sector<-nyse%>%
  group_by(sector)%>%
  summarize(count_sector=count(sector))%>%
  arrange(desc(count_sector))%>%
  mutate(sector = fct_reorder(sector, count_sector))

sector
ggplot(sector, aes(x=count_sector, y=sector))+geom_col() + labs(title = "Companies per sector", x = "Count")

```

Next, let's choose some stocks and their ticker symbols and download some data. You **MUST** choose 6 different stocks from the ones listed below; You should, however, add `SPY` which is the SP500 ETF (Exchange Traded Fund).

```{r get_price_data, message=FALSE, warning=FALSE, cache=TRUE}
# Notice the cache=TRUE argument in the chunk options. Because getting data is time consuming, 
# cache=TRUE means that once it downloads data, the chunk will not run again next time you knit your Rmd

myStocks <-c("AAPL","JPM","DIS","DPZ","ANF","TSLA","XOM","SPY", "BTI", "CVX", "KKR", "PFE", "UBER", "AXP") %>%
  tq_get(get  = "stock.prices",
         from = "2011-01-01",
         to   = "2022-08-31") %>%
  group_by(symbol) 

glimpse(myStocks) # examine the structure of the resulting data frame
```

Financial performance analysis depend on returns; If I buy a stock today for 100 and I sell it tomorrow for 101.75, my one-day return, assuming no transaction costs, is 1.75%. So given the adjusted closing prices, our first step is to calculate daily and monthly returns.

```{r calculate_returns, message=FALSE, warning=FALSE, cache=TRUE}
# calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

# calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

# calculate yearly returns
myStocks_returns_annual <- myStocks %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic",
               col_rename = "yearly_returns",
               cols = c(nested.col))

```

Create a table where you summarise monthly returns for each of the stocks and `SPY`; min, max, median, mean, SD.

```{r summarise_monthly_returns}
# YOUR CODE GOES HERE
sum_myStocks_returns_monthly<-myStocks_returns_monthly%>%
  group_by(symbol)%>%
  summarise(min_ret=min(monthly_returns), max_ret=max(monthly_returns), mean_ret=mean(monthly_returns), median_ret=median(monthly_returns), sd_ret=sd(monthly_returns))

sum_myStocks_returns_monthly

```

Plot a density plot, using `geom_density()`, for each of the stocks

```{r density_monthly_returns}
# YOUR CODE GOES HERE
ggplot(myStocks_returns_monthly, aes(x=monthly_returns, fill=symbol))+
  geom_density()+
  coord_cartesian(xlim=c(-0.4,0.4)) + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 2))+
  facet_grid(rows = (vars(symbol))) + 
  theme_bw()+
  labs(x="Monthly Returns", 
       y="Density") +
  guides(fill=FALSE) +
  labs(title = "Charting the distribution of monthly returns", x = "Monthly Returns", y = "Density")

```

What can you infer from this plot? Which stock is the riskiest? The least risky?

> TYPE YOUR ANSWER AFTER (AND OUTSIDE!) THIS BLOCKQUOTE.

> Because the volatility of the stocks is related to how fat the tails of its density plot are, our opinion is that TSLA is the riskiest stock while SPY is the least risky. This is because TSLA returns are spread out (the mode has fewer observations than other stocks, and the tails more oservations), while the complete opposite is true for SPY. This is to be expected since SPY tracks an index of the 500 biggest stocks while all the other symbols stand for individual companies.

Finally, make a plot that shows the expected monthly return (mean) of a stock on the Y axis and the risk (standard deviation) in the X-axis. Please use `ggrepel::geom_text_repel()` to label each stock

```{r risk_return_plot}
# YOUR CODE GOES HERE
library(ggrepel)
ggplot(sum_myStocks_returns_monthly, aes(x=sd_ret, y=mean_ret, label=symbol))+geom_point()+ geom_text_repel()+
  labs(title = "Risk-return profile of chosen stocks", x = "standard deviation of return", y = "mean return")

```

What can you infer from this plot? Are there any stocks which, while being riskier, do not have a higher expected return?

> TYPE YOUR ANSWER AFTER (AND OUTSIDE!) THIS BLOCKQUOTE.

> The stocks AAPL, KKR, JPM, DIS, CVX, XOM, BTI, ANF, and UBER are strictly dominated by the other 4 stocks (DPZ, AXP, PFE, and SPY). This is because for each of the 9 dominated stocks you can find at least one of the other 4 stocks that has a higher mean return and a lower standard deviation. The same cannot be said about the 4 dominant stocks (DPZ, AXP, PFE, and SPY). ANF and UBER are clearly the worst stocks in terms of risk-return profile since they feature the lowest mean returns and highest standard deviations by far.