library(tidyverse)
library(ggalt)
library(janitor)

data <- read_csv("data/tabn322_10_clean.csv")
data <- clean_names(data)

data_l <- gather(data,
                 key = "year",
                 value = "count",
                 x1970_71:x2015_16) %>%
    group_by(year) %>%
    mutate(yr_pct = count / sum(count)*100)


data_comp <- data_l %>%
    filter(year == "x1995_96" | year == "x2015_16") %>%
    select(-count) %>%
    spread(year, yr_pct) %>%
    mutate(delta = x2015_16 - x1995_96,
           growth = delta > 0)


p <- ggplot(data_comp,
            aes(x = x1995_96,
                xend = x2015_16,
                y = reorder(field_of_study, x1995_96),
                yend = reorder(field_of_study, x1995_96),
                color = growth))


p + geom_segment(size = 0.7,
                 arrow = arrow(type = "closed",
                               angle = 35,
                               length = unit(0.01, "npc"))) +
    scale_color_manual(labels = c("Decline", "Growth"),
                       values = my.colors()) +
    labs(title = "Change in Percentage of all Bachelor's Degrees Awarded\n  by Field of Study between 1995-1996 and 2015-16",
         x = "Percentage of all Bachelor's degrees",
         y = NULL,
         color = "Direction of Change",
         caption = "Data calculated from NCES Digest 2017, Table 322.10.") +
    theme_minimal() +
    theme(legend.position = "bottom")
