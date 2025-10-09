
full_dat <- readRDS("./data/full inequalities data.RDS")

measure_name <- "2_1"
dimension_name <- "F"
country_name <- "FIN"

gap_dat <- full_dat %>%
  filter(measure == measure_name, dimension == dimension_name, ref_area == country_name) %>%
  mutate(gap_image =  paste0(tolower(gap_value), ".png"),
         gap_value = tools::toTitleCase(gap_value)) %>%
  select(gap, gap_image, gap_value) %>%
  distinct()

fluidRow(
  div(paste0(gap$gap_value)),
  br(),
  img(src = gap_dat$gap_image, height = gap_dat$gap, width = gap_dat$gap)
)

short_dat |>
  e_charts(measure) |>
  e_scatter(measure, symbol_size = short_dat$gap) |>
  e_y_axis(type = "category",
           axisLabel = list(show = F),
           axisLine = list(show = F),
           axisTicks = list(show = F)) |>
  e_x_axis(type = "category",
           axisLabel = list(show = F),
           axisLine = list(show = F),
           axisTicks = list(show = F)) |>
  e_color(color = short_dat$color_gap) |>
  e_legend(show = F)



