
break_wrap <- function (string, width = 80, indent = 0, exdent = 0, whitespace_only = TRUE) {
  out <- stringi::stri_wrap(string, width = width, indent = indent, 
                   exdent = exdent, whitespace_only = whitespace_only, simplify = FALSE)
  vapply(out, str_c, collapse = "<br>", character(1))
}

areaPlotter <- function(df) {
  
  # df <- qual_ts[["9_2"]]

  # Set axises for area plot
  min_val <- min(df$obs_value) - (sd(df$obs_value)*2)
  max_val <- max(df$obs_value) + (sd(df$obs_value)*2)
  
  # Checks if min_val is negative when negative values don't exist in data
  if (exists("min_val") && !is.null(min_val) && !is.na(min_val) && min_val < 0 && !any(df$obs_value < 0, na.rm = TRUE)) {
    min_val <- 0
  }
  
  # Checks if max_val is greater than 100 when percentage and doesn't occur in data
  if (exists("max_val") && !is.null(max_val) && !is.na(max_val) 
      && max_val > 100 && !any(df$obs_value > 100, na.rm = TRUE) && unique(df$unit_tag_clean) == "%") {
    max_val <- 100
  }

  df <- df %>% 
    mutate(round_val = ifelse(measure == "9_2", 3, round_val),
           time_period = as.integer(time_period),
           obs_value = round(obs_value, round_val)) %>%
    rename(" " = "obs_value")
  
  if(exists("min_val") && !is.null(min_val) && !is.na(min_val) && round(min_val, 1) == round(max_val, 1)) {
    
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
        # In case country name in tooltip
        # <b style='font-size:10px!important;margin-bottom:0px;'>Country: </b>", country_name,"
        # <br>
        formatter = htmlwidgets::JS(paste0("
    function(params){

      // Start tooltip with red font color span
      let rez = `<span style='font-size:10px!important;line-height:0px;'>` +
                `<b style='font-size:10px!important;margin-bottom:0px;'>An : </b>` + params[0].value[0] +
                `<br>`;

      params.forEach(function(item){
        rez += `<b style='font-size:10px!important'>Valeur :</b> ` + item.value[1];
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
                `<b style='font-size:10px!important;margin-bottom:0px;'>An : </b>` + params[0].value[0] +
                `<br>`;

      params.forEach(function(item){
        rez += `<b style='font-size:10px!important'>Valeur :</b> ` + item.value[1];
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

pillBox <- function(avg_df, cluster_filter) {
  
  # avg_df <- avg_vals %>% filter(ref_area == "BEL")
  # cluster_filter <- material_headline
  
  priority <- c("Improving", "No significant change", "Deteriorating", "Not enough data")
  
  plot_df_raw <- avg_df %>% 
    filter(measure %in% cluster_filter) %>%
    mutate(
      perf_val = ifelse(is.na(perf_val), "#999999", perf_val),
      perf_val_name = case_when(
        perf_val == "#0F8554"   ~ "Improving",
        perf_val == "goldenrod" ~ "No significant change",
        perf_val == "#CF597E"   ~ "Deteriorating",
        TRUE                    ~ "Not enough data"
      ),
      perf_val_light = case_when(
        perf_val == "#0F8554"   ~ "#d6e8e0",
        perf_val == "goldenrod" ~ "#f8edd8",
        perf_val == "#CF597E"   ~ "#f5e1e6",
        TRUE                    ~ "#ececec"
      )
    ) 
  
  indicator_names <- plot_df_raw %>% 
    group_by(perf_val_name) %>%
    summarize(
      label_text = paste0(
        "<img src='", image,"' height=10 width=10> ",
        label_name, collapse = "<br>")
    )
  
  plot_df <- plot_df_raw %>%
    count(perf_val, perf_val_light, perf_val_name) %>%
    mutate(
      n   = ifelse(is.na(n), 0, n),
      pct = 100 * n / sum(n)
    ) %>%
    merge(indicator_names, by = "perf_val_name") %>%
    arrange(match(perf_val_name, rev(priority))) %>%
    mutate(
      first_seg = row_number() == 1L,
      last_seg  = row_number() == n(),
      standard_style = "display:inline-block;position:relative;overflow:visible;height:30px;line-height:25px;",
      hover_text = paste0("<span class='hidden-text'>", label_text, "</span>")
    ) %>%
    mutate(
      html = case_when(
        first_seg ~ paste0("<span class = 'vis-text' style='", standard_style,
                           "border: solid 1.5px ", perf_val, ";border-right:0px;",
                           "border-radius: 25px 0px 0px 25px; width:", pct, "%;",
                           "background:", perf_val_light,";'><b>", n, "</b>", hover_text,"</span>"),
        last_seg ~ paste0("<span class = 'vis-text' style='", standard_style,
                          "border: solid 1.5px ", perf_val, ";border-left:0px;",
                          "border-radius: 0px 25px 25px 0px; width:", pct, "%;",
                          "background:", perf_val_light,";'><b>", n, "</b>", hover_text,"</span>"),
        TRUE ~ paste0("<span class = 'vis-text' style='", standard_style,
                      "border: solid 1.5px ", perf_val, ";border-right:0px;border-left:0px;",
                      "width:", pct, "%;background:", perf_val_light,";'><b>", n, "</b>", hover_text,"</span>")
      )
    )
  
  HTML(paste0(plot_df$html, collapse = ""))
  
}


