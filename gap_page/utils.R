break_wrap <- function (string, width = 80, indent = 0, exdent = 0, whitespace_only = TRUE) {
  out <- stringi::stri_wrap(string, width = width, indent = indent,
                            exdent = exdent, whitespace_only = whitespace_only, simplify = FALSE)
  vapply(out, str_c, collapse = "<br>", character(1))
}

lollipopPlotter_fun <- function(full_dat, measure_name, dimension_group_name) {

  # measure_name <- "2_7"
  # dimension_group_name <- "gender"
  # country_name <- "OECD"

  lollipop_dat <- full_dat %>%
    filter( measure == measure_name, dimension_group == dimension_group_name) %>%
    select(measure, dimension_long, dimension_color, ratio) %>%
    distinct() %>%
    group_by(dimension_long) |>
    mutate(
      ratio_val = case_when(
        ratio >= 1.5 ~ 1.5,
        ratio <= 0.5 ~ 0.5,
        TRUE ~ ratio
      ),
      ratio_val = round(ratio_val, 3)
      ) %>%
    ungroup()

  lines <- lapply(measure_name, function(indicator) {
    points <- lollipop_dat[lollipop_dat$measure == indicator, ] %>% select(-ratio) %>% pivot_longer(!c(measure, dimension_long, dimension_color))
    list(
      list(coord = c(min(points$value), indicator)),  # Start point
      list(coord = c(max(points$value), indicator))   # End point
    )
  })

  lines <- do.call(c, lines)

  vec_colors <- setNames(lollipop_dat$dimension_color, lollipop_dat$dimension_long)
  cols <- unname(vec_colors[sort(unique(lollipop_dat$dimension_long))])

  lollipop_dat |>
    group_by(dimension_long) |>
    e_charts(ratio_val) |>
    e_scatter(measure, symbol_size = 30, bind = dimension_long, z = 5) |>
    e_color(cols) |>
    e_y_axis(type = "category",
             axisLabel = list(show = F),
             axisLine = list(show = F),
             axisTicks = list(show = F)) |>
    e_x_axis(axisLine = list(show = F),
             axisTicks = list(show = F),
             min = 0.5,
             max = 1.5,
             interval = 0.25
             ) |>
    e_mark_line(data = lines[1:2],
                lineStyle = list(
                  type = "solid",
                  width = 2,
                  color = "grey50"),
                symbol = "none",
                silent = T,
                label = list(show = F),
                z = -1) |>
    e_legend(show = F) |>
    e_mark_line(data = list(xAxis = 1), title = "", z = -1,
                lineStyle = list(color = "black", type = "solid"),
                silent = T,
                symbol = "none") |>
    e_tooltip()

}

inequalitySeriesPlotter_fun <- function(full_dat, measure_name, dimension_name) {

  # measure_name <- "8_1_DEP"
  # dimension_name <- "F"
  # country_name <- "AUS"
  # full_dat <- full_dat %>% filter(ref_area == country_name)

  short_dat <- full_dat %>% filter(measure == measure_name)
  min_val <- min(short_dat$obs_value, na.rm=T) - (sd(short_dat$obs_value, na.rm=T)*2)
  max_val <- max(short_dat$obs_value, na.rm=T) + (sd(short_dat$obs_value, na.rm=T)*2)

  time_series <- full_dat %>%
    filter(measure == measure_name, dimension == dimension_name) %>%
    arrange(time_period) %>%
    mutate(time_period = as.numeric(time_period),
           obs_value = round(obs_value, round_val)) %>%
    rename(" " = "obs_value")

  if(nrow(time_series) > 1) {

    time_series |>
      e_charts(time_period, height = 100) |>
      e_line(
        serie = ` `,
        areaStyle = list(opacity = 0.2),
        lineStyle = list(opacity = 100),
        itemStyle = list(opacity = 0)
      ) |>
      e_color(time_series$perf_val) |>
      e_x_axis(min = min(time_series$time_period),
               max = max(time_series$time_period),
               axisLabel = list(NULL), axisLine = list(NULL)) |>
      e_y_axis(min = min_val, max = max_val, axisLabel = list(NULL),
               axisLine = list(NULL)) |>
      e_tooltip(
        trigger = "axis",
        formatter = htmlwidgets::JS(paste0("
    function(params){

      // Start tooltip with red font color span
      let rez = `<span style='font-size:10px!important;line-height:0px;'>` +
                `<b style='font-size:10px!important'>Year: </b>` + params[0].value[0] +
                `<br>`;

      params.forEach(function(item){
        rez += `<b style='font-size:10px!important'>Value:</b> ` + item.value[1];
      });

      // Close the red font span
      rez += '</span>';

      return rez;
    }
  "))
      ) |>
      e_legend(show = F) |>
      e_grid(top=0,right=0,bottom=0,left=0) |>
      e_hide_grid_lines()


  } else if (nrow(time_series) == 1) {

    no_data <- data.frame(x = 1, y = 1, label = "Time series\nnot available")

    no_data |>
      e_charts(y, height = 100) |>
      e_text_g(
        style = list(
          text = " ",
          fontSize = 14,
          opacity = 0.7,
          textAlign = "center",
          textVerticalAlign = "middle",
          lineHeight = 22
        ),
        left = "center",
        top = "middle",
        z = 100
      ) |>
      e_x_axis(show = FALSE) |>
      e_y_axis(show = FALSE) |>
      e_grid(top = 0, right = 0, bottom = 0, left = 0) |>
      e_tooltip(show = FALSE) |>
      e_legend(show = FALSE) |>
      e_hide_grid_lines()

  }


}

gapPlotter_fun <- function(full_dat, measure_name, dimension_name) {

  # measure_name <- "2_1"
  # dimension_name <- "F"
  # country_name <- "FIN"

  if(dimension_name %in% c("F", "M")) {
    dimension_filter <- c("F", "M")
  } else if (dimension_name %in% c("YOUNG", "MID", "OLD")) {
    dimension_filter <- c("YOUNG", "MID", "OLD")
  } else {
    dimension_filter <- c("ISCED11_2_3", "ISCED11_5T8")
  }

  gap_dat <- full_dat %>%
    filter(measure == measure_name, dimension == dimension_name) %>%
    select(gap, gap_image, gap_value) %>%
    distinct()

  fastest_text <- full_dat %>%
    filter(measure == measure_name, dimension %in% dimension_filter) %>%
    select(dimension, direction, change) %>%
    distinct() %>%
    mutate(
      change_val = abs(change),
      change = case_when(
        change_val == max(change_val) & change > 0 & direction == "positive" ~ paste0("Improving fastest"),
        change_val == max(change_val) & change < 0 & direction == "negative" ~ paste0("Improving fastest"),
        change_val == max(change_val) & change < 0 & direction == "positive" ~ paste0("Deteriorating fastest"),
        change_val == max(change_val) & change > 0 & direction == "negative" ~ paste0("Deteriorating fastest")
      )
    ) %>%
    drop_na()

  if(nrow(fastest_text) > 0 && dimension_name %in% fastest_text$dimension) {
    if(gap_dat$gap_value == "Narrowing") {
      fluidRow(style = "position:absolute;top:5px;left:35px",
        # fluidRow(style = "font-size: 10px;",paste0(gap_dat$gap_value)),
        fluidRow(HTML(paste0("<a class='a-image'>
                             <img src='", gap_dat$gap_image, "' height=20px width=20px>
                             <span class='a-span'>", gap_dat$gap_value,"</span>
                             </a>"))),
        # fluidRow(style = "font-size: 10px;",paste0(fastest_text$change)),
      )

    } else {
      fluidRow(style = "position:absolute;top:5px;left:35px",
        # fluidRow(style = "font-size: 10px;",paste0(gap_dat$gap_value)),
        fluidRow(HTML(paste0("<a class='a-image'>
                             <img src='", gap_dat$gap_image, "' height=20px width=20px>
                             <span class='a-span'>", gap_dat$gap_value,"</span>
                             </a>"))),
        # fluidRow(style = "font-size: 10px;",paste0(fastest_text$change)),
      )
    }

  } else {
    if(gap_dat$gap_value == "Narrowing") {
      fluidRow(style = "position:absolute;top:5px;left:35px",
        # fluidRow(style = "font-size: 10px;",paste0(gap_dat$gap_value)),
        fluidRow(HTML(paste0("<a class='a-image'>
                             <img src='", gap_dat$gap_image, "' height=20px width=20px>
                             <span class='a-span'>", gap_dat$gap_value,"</span>
                             </a>")))
      )

    } else {
      fluidRow(style = "position:absolute;top:5px;left:35px",
        # fluidRow(style = "font-size: 10px;",paste0(gap_dat$gap_value)),
        fluidRow(HTML(paste0("<a class='a-image'>
                             <img src='", gap_dat$gap_image, "' height=20px width=20px>
                             <span class='a-span'>", gap_dat$gap_value,"</span>
                             </a>")))
      )
    }
  }

}

