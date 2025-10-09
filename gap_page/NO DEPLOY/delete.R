library(tidyverse)

full_dat <- readRDS("./data/full inequalities data.RDS")

ordered <- full_dat %>%
  filter(ref_area == "FIN", dimension %in% c("YOUNG", "MID", "OLD"),
         !measure %in% c("10_2_DEP", "11_3_Anger_DEP", "11_3_Enjoy_DEP",
                         "11_3_Laugh_DEP", "11_3_Pain_DEP", "11_3_Sadness_DEP",
                         "11_3_Wellrest_DEP", "11_3_Worry_DEP", "14_3_DEP",
                         "14_7_DEP")) %>%
  group_by(measure) %>%
  filter(time_period == max(time_period)) %>%
  ungroup() %>%
  select(label, dimension, rank = icon, performance = perf_val) %>%
  mutate(
    rank = case_when(
      rank == "1-circle-fill" ~ "top",
      rank == "2-circle-fill" ~ "mid",
      rank == "3-circle-fill" ~ "bottom"
    ),
    performance = case_when(
      performance == "#0F8554" ~ "top",
      performance == "goldenrod" ~ "mid",
      performance == "#CF597E" ~ "bottom"
    ),
    ranking = case_when(
      rank == "top" & performance == "top" ~ "1",
      rank == "top" & performance == "mid" ~ "2",
      rank == "top" & performance == "bottom" ~ "3",
      rank == "mid" & performance == "top" ~ "4",
      rank == "mid" & performance == "mid" ~ "5",
      rank == "mid" & performance == "bottom" ~ "6",
      rank == "bottom" & performance == "top" ~ "7",
      rank == "bottom" & performance == "mid" ~ "8",
      rank == "bottom" & performance == "bottom" ~ "9"
    )
  ) %>%
  drop_na(ranking)

set.seed(123)

plot_dat <- ordered %>%
  filter(dimension == "MID") %>%
  arrange(ranking) %>%
  complete(ranking = as.character(1:9)) %>%
  group_by(ranking) %>%
  mutate(
    x = runif(n(), 0, 100),  # random horizontal position
    y = runif(n(), 0, 100)   # random vertical position
  ) %>%
  ungroup()

ggplot(plot_dat, aes(x = x, y = y, label = str_wrap(label, 15))) +
  ggrepel::geom_text_repel(segment.colour = NA) +
  facet_wrap(~ranking) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(color = "transparent", fill = "transparent"),
        panel.background = element_rect(color = "grey", fill = "transparent"))

ggsave(filename = "mid outcomes.png", path="/Users/Kate/OneDrive/WISE/hsl_dashboard/inequalities/NO DEPLOY/")
?ggsave

