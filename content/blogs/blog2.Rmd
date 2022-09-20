---
title: "Project 2"
description: ""
slug: "Project2"
image: pic05.jpg
keywords: ""
categories: 
    - ""
    - ""
date: 2017-10-31T22:26:09-05:00
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

# IMDB ratings: Differences between directors

> Null hypothesis: The average rating of Steve Spielberg's movies is the same as that of Tim Burton's. 
Alternative hypothesis: The average rating of Steve Spielberg's movies is different from that of Tim Burton's

You can load the data and examine its structure

```{r load-movies-data}
movies <- read_csv(here::here("data","movies.csv"))
glimpse(movies)
```

Your R code and analysis should go here. If you want to insert a blank chunk of R code you can just hit `Ctrl/Cmd+Alt+I`


```{r movies_graph}
spielberg_burton <- movies %>% 
  select(-c(title, genre, year, duration, 
            gross, budget, cast_facebook_likes, votes,
            reviews)) %>% 
  filter(director %in% c("Steven Spielberg", "Tim Burton")) %>% 
  group_by(director) %>% 
  summarise(mean_rating = mean(rating, na.rm = TRUE),
            sd_rating = sd(rating, na.rm = TRUE),
            count = n(),
            se_rating = sd_rating/sqrt(count),
            t_critical_rating = qt(0.975, count - 1),
            moe_rating = t_critical_rating * se_rating,
            lower_ci_rating = mean_rating - moe_rating,
            upper_ci_rating = mean_rating + moe_rating)

spielberg_burton_plot <- ggplot(spielberg_burton, aes(x = mean_rating, y = director, color = director)) +
  geom_point(shape = 19, size = 6) +
  # annotate(x = as.character(mean_rating)) +
  geom_errorbar(aes(xmin = lower_ci_rating, xmax  = upper_ci_rating), size = 1.5) +
  theme_bw() +
  labs(title = "Do Spielberg and Burton have the same IMDB ratings?", subtitle = "95% confidence intervals overlap", 
       x = "Mean IMDB Rating") +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  geom_rect(xmin = 7.27,
            xmax = 7.33,
            ymin = -Inf,
            ymax = Inf,
            fill = "gray",
            color = NA,
            alpha = 0.5) +
  geom_text(aes(x = lower_ci_rating, label = round(lower_ci_rating, 2), vjust = 7, hjust = 0.55)) +
  geom_text(aes(x = upper_ci_rating, label = round(upper_ci_rating, 2), vjust = 7, hjust = 0.55)) +
  geom_text(aes(x = mean_rating, label = round(mean_rating, 2), vjust = 3, hjust = 0.55))

spielberg_burton_plot
```

```{r rating_hypothesis_testing}
# hypothesis testing using t.test() 
movies_rating_comparison <- movies %>% 
  filter(director == c("Steven Spielberg","Tim Burton")) %>%
  drop_na(director, rating) %>%
  group_by(director)

t.test(rating ~ director, data = movies_rating_comparison)

# hypothesis testing using infer package
# initialize the test
obs_diff <- movies_rating_comparison %>%
  specify(rating ~ director) %>%
  calculate(stat = "diff in means", order = c("Steven Spielberg","Tim Burton"))

set.seed(1234)
null_rating_difference <- movies_rating_comparison %>%
  
  # specify variables
  specify(rating ~ director) %>%
    
  # assume independence, i.e, there is no difference
  hypothesize(null = "independence") %>% 
  
  # generate 1000 reps, of type "permute"
  generate(reps = 1000, type = "permute") %>%
  
  # calculate statistic of difference, namely "diff in means"
  calculate(stat = "diff in means", order = c("Steven Spielberg","Tim Burton"))

null_rating_difference %>% visualize() +
  shade_p_value(obs_stat = obs_diff, direction = "two-sided")

null_rating_difference %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")