library(tidyverse)
library(here)
library(ggalt)
library(ggrepel)
library(janitor)
library(stringr)

library(kjhmisc)
setup_socviz()

theme_set(theme_socviz_semi())

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

data <- read_csv("data/new/tabn322.10-new-clean.csv") |> 
  clean_names() |> 
  pivot_longer(x1970_71:x2021_22, 
               names_to = "year", 
               values_to = "count") |> 
  group_by(year)  |> 
  mutate(yr_prop = count / sum(count), 
         year = as.integer(str_extract(year, "\\d{4}")), 
         label_wrap = str_wrap(field_of_study, width = 20)) 

data_comp <- data |>
  filter(year %in% c(1995, 2021)) |>
  select(-count) |> 
  pivot_wider(names_from = year, values_from =  yr_prop) |>
  mutate(delta = `2021` - `1995`, growth = delta > 0)

area_pcts <- data |>
  group_by(field_of_study) |>
  summarize(mean_pct = mean(yr_prop)) |>
  mutate(cutoff = mean_pct < 0.02)

ind <- area_pcts$field_of_study[!area_pcts$cutoff]

data$cutoff <- data$field_of_study %in% ind

data <- data |> ungroup()

###--------------------------------------------------
### Plots
###--------------------------------------------------

p <- ggplot(data_comp,
            aes(x = `1995`,
                xend = `2021`,
                y = reorder(field_of_study, `1995`),
                yend = reorder(field_of_study, `2021`),
                color = growth))


p_out <- p + geom_segment(linewidth = 0.7,
                 arrow = arrow(type = "closed", angle = 35,
                               length = unit(0.01, "npc"))) +
  scale_x_continuous(labels = scales::label_percent()) + 
    scale_color_manual(labels = c("Decline", "Growth"),
                       values = my_colors()) +
    labs(title = "Change in Percentage of all Bachelor's Degrees Awarded\n  by Field of Study between 1995-1996 and 2021-22",
         x = "Percentage of all Bachelor's degrees",
         y = NULL,
         color = "Direction of Change",
         caption = "Data calculated from NCES Digest 2023, Table 322.10.") +
    theme(legend.position = "bottom")

p <- ggplot(data, aes(x = year, y = yr_prop,
                        group = field_of_study))

p + geom_line() +
  scale_y_continuous(labels = scales::label_percent()) + 
  facet_wrap(~ reorder(field_of_study, -yr_prop),
               labeller = label_wrap_gen(width = 35),
               ncol = 5) +
    labs(x = "Year", y = "Percent of all BAs conferred",
                  caption = "Data from NCES Digest 2023, Table 322.10.") 


###--------------------------------------------------
### Ordering example
###--------------------------------------------------

### ordering jiggery-pokery
### I should make this more general
### prompted by @drdrang.

my_xlab = "Year"
my_ylab = "Percent of all Bachelor’s Degrees conferred"
my_caption = "Figure: Kieran Healy @kjhealy.co / Data from NCES Digest 2023, Table 322.10"
my_subtitle = "Estimates are every 5 years from 1970-1995, and annually thereafter"
my_title_1 = "US Trends in Bachelor's Degrees Conferred, 1970-2023,\nfor areas averaging more than 2% of all degrees"
my_title_2 = "US Trends in Bachelor's Degrees Conferred, 1970-2023,\nfor Areas averaging less than 2% of all degrees"

### The usual way: properly ordered, but filled from the top left.
df_labs <- data |> 
  summarize(
    yr_mean = mean(yr_prop), 
    year = 1983,
    field_of_study = first(field_of_study), 
    cutoff = first(cutoff),
    .by = label_wrap
    ) |> 
  filter(cutoff == TRUE)

df_labs <- df_labs |> 
  mutate(
    x_fudge = c(
      0,# Bio
      0,# Business
      0,# Communication
      0,# Computers
      2,# Education
      0,# Engineering
      0,# English
      -0.5,# Health
      0,# Homeland
      0,# Lib Arts
      0,# Multidisc
      0,# Psych
      0,# Social sci
      0# Visual
    ),
    y_fudge = c(0.03, # Bio
                -0.01,# Business
                0.045,# Communication
                0.045, # Computers
                0.05,# Education
                0.04, # Engineering
                0.05, # English
                0.01,    # Health
                0.03, # Homeland
                0.05, # Lib Arts
                0.016,# Multidisc
                0.025,# Psych
                0.06, # Social sci
                0.03  # Visual
                ), 
         year = year + x_fudge, 
         yr_prop = yr_mean + y_fudge)


p_out <- data |> 
  filter(cutoff == TRUE) |> 
  ggplot() +
  geom_line(aes(x = year,
                 y = yr_prop), 
            linewidth = rel(1.1)) +
  geom_text_repel(data = df_labs, 
                  mapping = aes(x = year, 
                                y = yr_prop, 
                                label = label_wrap, 
                                family = "Socviz Condensed"), 
                  max.overlaps = 20,
                  hjust = 0,
                  size = rel(6),
                  lineheight = rel(0.7), 
                  seed = 12345, force = 0, force_pull = 2) + 
  scale_y_continuous(labels = scales::label_percent()) +
  facet_wrap(~ reorder(field_of_study, -yr_prop),
               ncol = 5) +
    labs(x = my_xlab,
         y = my_ylab,
         caption = my_caption,
         title = my_title_1,
         subtitle = my_subtitle) +
  theme(strip.text = element_blank(), 
        axis.title = element_text(size = rel(1.15)),
        panel.spacing = unit(3, "mm"),
        panel.border = element_rect(color = "black", fill = NA), 
        plot.title = element_text(size = rel(1.6)))


kjhmisc::save_figure(here("figures", "all-fields"), p_out, height = 10, width = 14)


p_out <- data |> 
  filter(cutoff == TRUE) |> 
  ggplot() +
  geom_line(aes(x = year,
                y = yr_prop), 
            linewidth = rel(1.1)) +
  geom_text_repel(data = df_labs, 
                  mapping = aes(x = year, 
                                y = yr_prop, 
                                label = label_wrap, 
                                family = "Socviz Condensed"), 
                  max.overlaps = 20,
                  hjust = 0,
                  size = rel(6),
                  lineheight = rel(0.7), 
                  seed = 12345, force = 0, force_pull = 2) + 
  scale_y_continuous(labels = scales::label_percent()) +
  facet_wrap(~ reorder(field_of_study, -yr_prop),
             ncol = 2) +
  labs(x = my_xlab,
       y = my_ylab,
       caption = my_caption,
       title = my_title_1,
       subtitle = my_subtitle) +
  theme(strip.text = element_blank(), 
        axis.title = element_text(size = rel(1.15)),
        panel.spacing = unit(3, "mm"),
        panel.border = element_rect(color = "black", fill = NA), 
        plot.title = element_text(size = rel(1.6)))


kjhmisc::save_figure(here("figures", "all-fields-tall"), p_out, height = 20, width = 8)

### Turn off table ordering: filled from the bottom right, but this breaks the
### ordering because it's read the wrong way

p <- ggplot(subset(data_l, cutoff == TRUE),
            aes(x = yr,
                y = yr_prop,
                group = field_of_study))

p + geom_line() +
  scale_y_continuous(labels = scales::label_percent())
    facet_wrap(~ reorder(field_of_study, yr_prop),
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
# vars <- area_pcts[!area_pcts$cutoff,] |> arrange(desc(mean_pct))
# o <- c(10:14, 5:9, 1:4)
# 
# p <- ggplot(subset(data_l, cutoff == TRUE),
#             aes(x = yr,
#                 y = yr_prop,
#                 group = field_of_study))
# 
# p + geom_line() +
#     facet_wrap(~ factor(field_of_study, 
#                         levels = vars$field_of_study[o], 
#                         ordered = TRUE),
#                labeller = label_wrap_gen(width = 35),
#                ncol = 5, as.table = FALSE) +
#     labs(x = my_xlab,
#          y = my_ylab,
#          caption = my_caption,
#          title = my_title_1,
#          subtitle = my_subtitle) +
#     theme_minimal() +
#     theme(strip.text.x = element_text(size = 6))
# 
# 
# ### Do the same for areas <2% of degrees
# vars <- area_pcts[area_pcts$cutoff,] |> arrange(desc(mean_pct))
# o <- c(15:19, 10:14, 5:9, 1:4)
# 
# p <- ggplot(subset(data_l, cutoff == FALSE),
#             aes(x = yr,
#                 y = yr_prop,
#                 group = field_of_study))
# 
# p + geom_line() +
#     facet_wrap(~ factor(field_of_study, levels = vars$field_of_study[o], ordered = TRUE),
#                labeller = label_wrap_gen(width = 35),
#                ncol = 5, as.table = FALSE) +
#     labs(x = my_xlab,
#          y = my_ylab,
#          caption = my_caption,
#          title = my_title_2,
#          subtitle = my_subtitle) +
#     theme_minimal() +
#     theme(strip.text.x = element_text(size = 6))
