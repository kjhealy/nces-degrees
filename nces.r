library(tidyverse)
library(ggalt)
library(janitor)
library(stringr)


my_colors <- function(palette = "cb") {
    cb.palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    rcb.palette <- rev(cb.palette)
    bly.palette <- c("#E69F00", "#0072B2", "#000000", "#56B4E9",
        "#009E73", "#F0E442", "#D55E00", "#CC79A7")
    if (palette == "cb")
        return(cb.palette)
    else if (palette == "rcb")
        return(rcb.palette)
    else if (palette == "bly")
        return(bly.palette)
    else stop("Choose cb, rcb, or bly ony.")
}

data <- read_csv("data/tabn322_10_clean.csv")
data <- clean_names(data)

data_l <- gather(data,
                 key = "year",
                 value = "count",
                 x1970_71:x2015_16) %>%
    group_by(year) %>%
    mutate(yr_pct = count / sum(count)*100) %>%
    ungroup()

yr <-  as.integer(str_extract(data_l$year, "\\d{4}"))

data_l <- data_l %>% add_column(yr, .before = 2) %>%
    select(-year) %>% group_by(yr)


data_comp <- data_l %>%
    filter(yr == 1995 | yr == 2015) %>%
    select(-count) %>%
    tidyr::spread(yr, yr_pct) %>%
    mutate(delta = `2015` - `1995`,
           growth = delta > 0)

area_pcts <- data_l %>% group_by(field_of_study) %>%
    summarize(mean_pct = mean(yr_pct)) %>%
    mutate(cutoff = mean_pct < 2)

ind <- area_pcts$field_of_study[!area_pcts$cutoff]

data_l$cutoff <- data_l$field_of_study %in% ind


###--------------------------------------------------
### Plots
###--------------------------------------------------

p <- ggplot(data_comp,
            aes(x = `1995`,
                xend = `2015`,
                y = reorder(field_of_study, `1995`),
                yend = reorder(field_of_study, `1995`),
                color = growth))


p + geom_segment(size = 0.7,
                 arrow = arrow(type = "closed", angle = 35,
                               length = unit(0.01, "npc"))) +
    scale_color_manual(labels = c("Decline", "Growth"),
                       values = my_colors()) +
    labs(title = "Change in Percentage of all Bachelor's Degrees Awarded\n  by Field of Study between 1995-1996 and 2015-16",
         x = "Percentage of all Bachelor's degrees",
         y = NULL,
         color = "Direction of Change",
         caption = "Data calculated from NCES Digest 2017, Table 322.10.") +
    theme_minimal() +
    theme(legend.position = "bottom")

p <- ggplot(data_l, aes(x = yr, y = yr_pct,
                        group = field_of_study))

p + geom_line() +
    facet_wrap(~ reorder(field_of_study, -yr_pct),
               labeller = label_wrap_gen(width = 35),
               ncol = 5) +
    labs(x = "Year", y = "Percent of all BAs conferred",
                  caption = "Data from NCES Digest 2017, Table 322.10.") +
    theme_minimal() +
    theme(strip.text.x = element_text(size = 6, face = "bold"))


###--------------------------------------------------
### Ordering example
###--------------------------------------------------

### ordering jiggery-pokery
### I should make this more general
### prompted by @drdrang.

my_xlab = "Year"
my_ylab = "Percent of all BAs conferred"
my_caption = "Data from NCES Digest 2017, Table 322.10"
my_subtitle = "Observations are every 5 years from 1970-1995, and annually thereafter"
my_title_1 = "US Trends in Bachelor's Degrees Conferred, 1970-2015,\nfor Areas averaging more than 2% of all degrees"
my_title_2 = "US Trends in Bachelor's Degrees Conferred, 1970-2015,\nfor Areas averaging less than 2% of all degrees"

### The usual way: properly ordered, but filled from the top left.
p <- ggplot(subset(data_l, cutoff == TRUE),
            aes(x = yr,
                y = yr_pct,
                group = field_of_study))

p + geom_line() +
    facet_wrap(~ reorder(field_of_study, -yr_pct),
               labeller = label_wrap_gen(width = 35),
               ncol = 5) +
    labs(x = my_xlab,
         y = my_ylab,
         caption = my_caption,
         title = my_title_1,
         subtitle = my_subtitle) +
    theme_minimal() +
    theme(strip.text.x = element_text(size = 6))


### Turn off table ordering: filled from the bottom right, but this breaks the
### ordering because it's read the wrong way
p + geom_line() +
    facet_wrap(~ reorder(field_of_study, -yr_pct),
               labeller = label_wrap_gen(width = 35),
               ncol = 5, as.table = FALSE) +
    labs(x = my_xlab,
         y = my_ylab,
         caption = my_caption,
         title = my_title_1,
         subtitle = my_subtitle) +
    theme_minimal() +
    theme(strip.text.x = element_text(size = 6))


### Force the ordering we cant using as.table = FALSE and
### by using factor() on field of study to specify it's order
### correctly.
vars <- area_pcts[!area_pcts$cutoff,] %>% arrange(desc(mean_pct))
o <- c(10:14, 5:9, 1:4)

p <- ggplot(subset(data_l, cutoff == TRUE),
            aes(x = yr,
                y = yr_pct,
                group = field_of_study))

p + geom_line() +
    facet_wrap(~ factor(field_of_study, levels = vars$field_of_study[o], ordered = TRUE),
               labeller = label_wrap_gen(width = 35),
               ncol = 5, as.table = FALSE) +
    labs(x = my_xlab,
         y = my_ylab,
         caption = my_caption,
         title = my_title_1,
         subtitle = my_subtitle) +
    theme_minimal() +
    theme(strip.text.x = element_text(size = 6))


### Do the same for areas <2% of degrees
vars <- area_pcts[area_pcts$cutoff,] %>% arrange(desc(mean_pct))
o <- c(15:19, 10:14, 5:9, 1:4)

p <- ggplot(subset(data_l, cutoff == FALSE),
            aes(x = yr,
                y = yr_pct,
                group = field_of_study))

p + geom_line() +
    facet_wrap(~ factor(field_of_study, levels = vars$field_of_study[o], ordered = TRUE),
               labeller = label_wrap_gen(width = 35),
               ncol = 5, as.table = FALSE) +
    labs(x = my_xlab,
         y = my_ylab,
         caption = my_caption,
         title = my_title_2,
         subtitle = my_subtitle) +
    theme_minimal() +
    theme(strip.text.x = element_text(size = 6))
