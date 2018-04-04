library(tidyverse)
library(ggalt)
library(janitor)
library(stringr)

data <- read_csv("data/tabn322_10_clean.csv")
data <- clean_names(data)

data_l <- gather(data,
                 key = "year",
                 value = "count",
                 x1970_71:x2015_16) %>%
    group_by(year) %>%
    mutate(yr_pct = count / sum(count)*100,
           yr = as.integer(str_extract(year, "\\d{4}")))



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


p <- ggplot(data_l, aes(x = yr,
                        y = yr_pct,
                        group = field_of_study))

p + geom_line() +
    facet_wrap(~ reorder(field_of_study, -yr_pct),
               labeller = label_wrap_gen(width = 35),
               ncol = 5) +
    labs(x = "Year", y = "Percent of all BAs conferred",
                  caption = "Data from NCES Digest 2017, Table 322.10.") +
    theme_minimal() +
    theme(strip.text.x = element_text(size = 6, face = "bold"))


area_pcts <- data_l %>% group_by(field_of_study) %>%
    summarize(mean_pct = mean(yr_pct)) %>%
    mutate(cutoff = mean_pct < 2)

area_pcts %>% arrange(desc(mean_pct)) %>% data.frame()

data_l <- data_l %>% mutate(field_sum = field_of_study)

ind <- area_pcts$field_of_study[!area_pcts$cutoff]

data_l$cutoff <- data_l$field_of_study %in% ind


p <- ggplot(subset(data_l, cutoff == FALSE),
            aes(x = yr,
                y = yr_pct,
                group = field_of_study))

p + geom_line() +
    facet_wrap(~ reorder(field_of_study, -yr_pct),
               labeller = label_wrap_gen(width = 35),
               ncol = 5) +
    labs(x = "Year",
         y = "Percent of all BAs conferred",
         caption = "Data from NCES Digest 2017, Table 322.10.",
         title = "US Trends in Bachelor's Degrees Conferred, 1970-2015,\nfor Areas averaging less than 2% of all degrees",
         subtitle = "Observations are every 5 years from 1970-1995, and annually thereafter") +
    theme_minimal() +
    theme(strip.text.x = element_text(size = 6))



p <- ggplot(subset(data_l, cutoff == TRUE),
            aes(x = yr,
                y = yr_pct,
                group = field_of_study))

p + geom_line() +
    facet_wrap(~ reorder(field_of_study, -yr_pct),
               labeller = label_wrap_gen(width = 35),
               ncol = 5) +
    labs(x = "Year",
         y = "Percent of all BAs conferred",
         caption = "Data from NCES Digest 2017, Table 322.10.",
         title = "US Trends in Bachelor's Degrees Conferred, 1970-2015,\nfor Areas averaging more than 2% of all degrees",
         subtitle = "Observations are every 5 years from 1970-1995, and annually thereafter") +
    theme_minimal() +
    theme(strip.text.x = element_text(size = 6))
