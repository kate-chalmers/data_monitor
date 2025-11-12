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
    filter( measure == measure_name) %>%
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
      list(coord = c(min(points$value, na.rm=T), indicator)),  # Start point
      list(coord = c(max(points$value, na.rm=T), indicator))   # End point
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

  # measure_name <- "2_1"
  # dimension_name <- "F"
  # country_name <- "OECD"
  # full_dat <- full_dat %>% filter(ref_area == country_name)

  short_dat <- full_dat %>% filter(measure == measure_name)
  min_val <- min(short_dat$obs_value, na.rm=T) - (sd(short_dat$obs_value, na.rm=T)*2)
  max_val <- max(short_dat$obs_value, na.rm=T) + (sd(short_dat$obs_value, na.rm=T)*2)

  # Checks if min_val is negative when negative values don't exist in data
  if (exists("min_val") && !is.null(min_val) && !is.na(min_val) && min_val < 0 && !any(short_dat$obs_value < 0, na.rm = TRUE)) {
    min_val <- 0
  }

  # Checks if max_val is greater than 100 when percentage and doesn't occur in data
  if (exists("max_val") && !is.null(max_val) && !is.na(max_val) && max_val > 100 && !any(short_dat$obs_value > 100, na.rm = TRUE)) {
    max_val <- 100
  }


  df <- full_dat %>%
    filter(measure == measure_name, dimension == dimension_name) %>%
    arrange(time_period) %>%
    mutate(time_period = as.numeric(time_period),
           obs_value = round(obs_value, round_val)) %>%
    rename(" " = "obs_value")


  if(exists("min_val") && !is.null(min_val) && !is.na(min_val) && round(min_val, 3) == round(max_val, 3)) {

    df |>
      e_charts(time_period, height = 100) |>
      e_line(
        serie = ` `,
        areaStyle = list(opacity = 0.2),
        lineStyle = list(opacity = 100),
        itemStyle = list(opacity = 0)
      ) |>
      e_color(df$perf_val) |>
      e_x_axis(min = min(df$time_period),
               max = max(df$time_period),
               axisLabel = list(NULL), axisLine = list(NULL)) |>
      e_y_axis(axisLabel = list(NULL),
               axisLine = list(NULL)) |>
      e_tooltip(
        trigger = "axis",
        formatter = htmlwidgets::JS(paste0("
    function(params){

      // Start tooltip with red font color span
      let rez = `<span style='font-size:10px!important;line-height:0px;'>` +
                `<b style='font-size:10px!important;margin-bottom:0px;'>Year: </b>` + params[0].value[0] +
                `<br>`;

      params.forEach(function(item){
        rez += `<b style='font-size:10px!important'>Value:</b> ` + item.value[1];
      });

      // Close the red font span
      rez += '</span>';

      return rez;
    }
  "))
      )|>
      e_legend(show = F) |>
      e_grid(top=0,right=0,bottom=0,left=0) |>
      e_hide_grid_lines()

  } else {

    df |>
      e_charts(time_period, height = 100) |>
      e_line(
        serie = ` `,
        areaStyle = list(opacity = 0.2),
        lineStyle = list(opacity = 100),
        itemStyle = list(opacity = 0)
      ) |>
      e_color(df$perf_val) |>
      e_x_axis(min = min(df$time_period),
               max = max(df$time_period),
               axisLabel = list(NULL), axisLine = list(NULL)) |>
      e_y_axis(min = min_val,
               max = max_val,
               axisLabel = list(NULL),
               axisLine = list(NULL)) |>
      e_tooltip(
        trigger = "axis",
        # In case country name in tooltip
        # <b style='font-size:10px!important;margin-bottom:0px;'>Country: </b>", country_name,"
        # <br>
        formatter = htmlwidgets::JS(paste0("
    function(params){

      // Start tooltip with red font color span
      let rez = `<span style='font-size:10px!important;line-height:0px;'>` +
                `<b style='font-size:10px!important;margin-bottom:0px;'>Year: </b>` + params[0].value[0] +
                `<br>`;

      params.forEach(function(item){
        rez += `<b style='font-size:10px!important'>Value:</b> ` + item.value[1];
      });

      // Close the red font span
      rez += '</span>';

      return rez;
    }
  "))
      )|>
      e_legend(show = F) |>
      e_grid(top=0,right=0,bottom=0,left=0) |>
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
      HTML(paste0("<div class='gap-pin'>",
                  "<a class='a-image'>
                             <img src='", gap_dat$gap_image, "' height=20px width=20px>
                             <span class='a-span'>", gap_dat$gap_value,"</span>
                             </a>", "</div>")
           )

    } else {
      HTML(paste0("<div class='gap-pin'>",
      "<a class='a-image'>
                             <img src='", gap_dat$gap_image, "' height=20px width=20px>
                             <span class='a-span'>", gap_dat$gap_value,"</span>
                             </a>", "</div>")
           )
    }

  } else {
    if(gap_dat$gap_value == "Narrowing") {
      HTML(paste0("<div class='gap-pin'>",
      "<a class='a-image'>
                             <img src='", gap_dat$gap_image, "' height=20px width=20px>
                             <span class='a-span'>", gap_dat$gap_value,"</span>
                             </a>", "</div>")
           )

    } else {
      HTML(paste0("<div class='gap-pin'>",

      "<a class='a-image'>
                             <img src='", gap_dat$gap_image, "' height=20px width=20px>
                             <span class='a-span'>", gap_dat$gap_value,"</span>
                             </a>", "</div>")
           )

    }
  }

}

