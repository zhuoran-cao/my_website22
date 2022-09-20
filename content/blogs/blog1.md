---
title: "Project 1"
description: ""
slug: "Project 1"
image: pic04.jpg
keywords: ""
categories: 
    - ""
    - ""
date: 2017-10-31T21:28:43-05:00
draft: false
---

# Biden's Approval Margins

As we saw in class, fivethirtyeight.com has detailed data on [all polls
that track the president's
approval](https://projects.fivethirtyeight.com/biden-approval-ratings)

```{r, cache=TRUE}
# Import approval polls data directly off fivethirtyeight website
approval_polllist <- read_csv('https://projects.fivethirtyeight.com/biden-approval-data/approval_polllist.csv') 

glimpse(approval_polllist)

# Use `lubridate` to fix dates, as they are given as characters.
```

```{r}
library(lubridate)
approval_polllist <- approval_polllist %>% 
  mutate(enddate = mdy(enddate),
         startdate = mdy(startdate),
         modeldate = mdy(modeldate),
         week = week(enddate)) 
```

## Create a plot

What I would like you to do is to calculate the average net approval
rate (approve- disapprove) for each week since he got into office. I
want you plot the net approval for each week in 2022, along with its 95%
confidence interval. There are various dates given for each poll, please
use `enddate`, i.e., the date the poll ended. Your plot should look
something like this:

```{r net_approve_rate graph}
approval_polllist %>% 
  mutate(year = year(enddate)) %>% 
  filter(year == 2022, week<50) %>%
  mutate(net_approval_rate = (approve-disapprove)) %>% 
  group_by(week, subgroup) %>% 
  summarise(
    mean_net_approve = mean(net_approval_rate),
    sd = sd(net_approval_rate),
    count = n(),
    se = sd/sqrt(count),
    lower95 = mean_net_approve - qt(0.975,count-1)*se,
    upper95 = mean_net_approve + qt(0.975,count-1)*se) %>% 

ggplot(aes(x = week, y=mean_net_approve))+
  geom_line()+
  facet_wrap(~subgroup, nrow=3)+
  geom_ribbon(aes(ymin = lower95, ymax = upper95), alpha=0.35, fill = "orange", 
             linetype = "solid", nrow=3)+
  theme_bw()+
  labs(title = "Biden's Net Approval Ratings in 2022",
    subtitle = "Weekly Data, Approve - Disapprove, %",
    y= "", 
    x = ""
  )
```

```{r trump_margins, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "biden_approval_margin.png"), error = FALSE)
```