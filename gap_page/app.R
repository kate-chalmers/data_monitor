source("./global.R")

ui <- fluidPage(shinyjs::useShinyjs(),
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")),
                tags$head(tags$script(src="modal.js")),
                bsModal(id = "modal1",
                        title = "Download the data",
                        trigger = NULL,
                        uiOutput("defText")),

                column(12, align="right", style = "font-size:1rem",
                       paste0("Last updated: ", last_updated, "   ")
                ),
                br(),
                br(),
                fluidRow(align="center",
                         column(2),
                         column(8, align = "center",
                                selectInput("countrySelector",
                                            label = "Choose a country",
                                            choices = list(
                                              "Averages" = average_vector,
                                              "OECD countries" = oecd_vector,
                                              "Non-OECD country" = accession_vector
                                            ),
                                            selected = "OECD"
                                ),
                                HTML("<span style='font-size:9px'>Tier is not shown when an average or a non-OECD country is selected</span>"),
                                br(),
                                br(),
                                br(),
                                radioGroupButtons("clusterSelector",
                                                  label = "Select the Well-being cluster",
                                                  choices = c("Material conditions" = "mats",
                                                              "Quality of life" = "qualts",
                                                              "Community relationships" = "coms"),
                                                  status = "secondary"
                                )
                         ),
                         column(2)
                ),
                br(),
                br(),
                fluidRow(
                  uiOutput("inequality_table") %>% withSpinner(color="#0dc5c1")
                )

)

server <- function(input, output, session) {

  # Modal text
  observeEvent(input$clicked_class, {

    req(input$clicked_class)

    long_df <- readxl::read_excel("./data/hows_life_dictionary.xlsx")

    short_df <- long_df %>% filter(measure == input$clicked_class)

    main_title <- paste0("<b style='font-size:1.75rem'>", short_df$label, "</b>")

    long_title <- paste0("<i>", short_df$indicator, "</i>")

    unit_title <- paste0("<i>", short_df$unit, "</i>")

    desc_text <- paste0(short_df$definition)

    indicator_text <- paste0(
      main_title, "<br>",
      "<b>Technical name: </b>", long_title, "<br>",
      "<b>Unit: </b>", unit_title, "<br><br>",
      desc_text, "<br><br>"
    )

    download_text <- paste0("<center>
                             <br>
                             <b style = 'font-size:2rem;'>Download ", short_df$label," data for ", countryName(), " from the
                                 <a href='", dataExplorerURL(), "' target='_blank'>OECD How's Life? database</a>
                            </b>
                            <br>
                            <br>
                            </center>
                            <hr>")

    output$defText <- renderUI({
      div(
        div(
          HTML(download_text)
        ),
        div(
          HTML(indicator_text)
        )
      )
    })

  })

  output$defText <- renderUI({ tagList("") })

  outputOptions(output, "defText", suspendWhenHidden = FALSE)

  countryName <- eventReactive(input$countrySelector, {

    country_name <- which(country_name_vector == input$countrySelector) %>% names

    if(country_name %in% the_thes) {
      country_name <- paste0("the ", country_name)
    }

    return(country_name)

  })

  dataExplorerURL <- eventReactive(
    c(input$clicked_class, input$clicked_class2, input$countrySelector),
    {
      cat_num <- str_split_fixed(input$clicked_class, "_", 2)[1] %>% as.numeric()

      dim_filter <- "_T._T._T"
      if (identical(input$clicked_class2, "gender_dim")) dim_filter <- "F+M._T._T"
      if (identical(input$clicked_class2, "age_dim"))    dim_filter <- "_T.YOUNG+MID+OLD._T"
      if (identical(input$clicked_class2, "educ_dim"))   dim_filter <- "_T._T.ISCED11_1+ISCED11_2_3+ISCED11_5T8"

      if (input$countrySelector == "OECD") {
        ref_area <- paste0(oecd_countries, collapse = "+")
      } else {
        ref_area <- input$countrySelector
      }

      if (!cat_num %in% c(12:15)) {
        data_explorer_url <- paste0(
          "https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CSociety%23SOC%23%7CWell-being%20and%20beyond%20GDP%23SOC_WEL%23&pg=0&fc=Topic&bp=true&snb=26&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_HSL%40DF_HSL_CWB&df[ag]=OECD.WISE.WDP&df[vs]=1.1&dq=",
          ref_area, ".", input$clicked_class, "..", dim_filter, ".&pd=%2C&to[TIME_PERIOD]=false"
        )
      } else {
        data_explorer_url <- paste0(
          "https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CSociety%23SOC%23%7CWell-being%20and%20beyond%20GDP%23SOC_WEL%23&pg=0&fc=Topic&bp=true&snb=26&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_HSL%40DF_HSL_FWB&df[ag]=OECD.WISE.WDP&df[vs]=1.1&dq=",
          ref_area, ".", input$clicked_class, "..", dim_filter, ".&pd=%2C&to[TIME_PERIOD]=false"
        )
      }

      data_explorer_url
    })

  # Page builder ------------------------------------------------------------

  inequalityCardBuilder <- function(indicator_list, dim_label) {

    inequality_table <- fluidRow(

      uiOutput(paste0(dim_label, "_dimension_title")) %>% withSpinner(color="#0dc5c1"),
      fluidRow(
        column(3),
        column(6,
               HTML("<center>Since 2010, this number of indicators have...</center>
                    <br>"),
               fluidRow(
                 uiOutput(paste0(dim_label, "_summary_F")),
                 uiOutput(paste0(dim_label, "_summary_M")),
                 br(),
                 uiOutput(paste0(dim_label, "_summary_YOUNG")),
                 uiOutput(paste0(dim_label, "_summary_MID")),
                 uiOutput(paste0(dim_label, "_summary_OLD")),
                 br(),
                 uiOutput(paste0(dim_label, "_summary_ISCED11_2_3")),
                 uiOutput(paste0(dim_label, "_summary_ISCED11_5T8")))
        ),
        column(3)
      ),
      fluidRow(align = "center", style="margin-top:0px;",
               HTML(
                 "<br>
                  <span style='color:#999999;'>●</span> not enough data
                  <span style='color:#CF597E;'>●</span> deteriorated
                  <span style='color:goldenrod;'>●</span> no significant change
                  <span style='color:#0F8554;'>●</span> improved")
      ),
      br(),
      br(),

      lapply(seq_along(indicator_list), function(i) {

        val <- strsplit(indicator_list[i], "_")[[1]][1]
        dim_color <- dim_colors %>% filter(cat == val) %>% pull(color)

        opacity_val <- dat_country() %>% filter(measure == indicator_list[i]) %>% select(dimension_group, opacity) %>% distinct()

        fluidRow(style="margin-bottom:15px",
                 column(1),
                 column(10,
                        fluidRow(class = "folder-row",
                                 fluidRow(style = paste0("background:", dim_color,"; border-radius: 25px 25px 0px 0px; border:solid 1.5px lightgrey;"),
                                          column(1, class="card-grow", align="center", uiOutput(paste0(dim_label, "_dimension_", i))),
                                          column(9, style="margin-top:10px", align = "left", uiOutput(paste0(dim_label, "_label_text_", i))),
                                          column(2, style = "margin-top:10px", align = "right", uiOutput(paste0(dim_label, "_population_value_", i)))
                                 ),
                                 fluidRow(
                                   column(4, align = "center", class = "container",
                                          style = paste0("border-left: solid 1.5px lightgrey;
                                                         border-radius: 0px 0px 0px 0px;opacity:", opacity_val %>% filter(dimension_group == "gender") %>% pull(opacity), "!important;"),
                                          div(class = "folder",
                                              fluidRow(class = paste0("card slide slide1 ", indicator_list[i], " gender_dim"),
                                                       column(5, class = "card-front card-grow",
                                                              fluidRow(style = "height: 40%; z-index:1;",
                                                                       uiOutput(paste0(dim_label, "_women_gap_", i), height=25),
                                                                       uiOutput(paste0(dim_label, "_icon_", i, "_F")),
                                                                       uiOutput(paste0(dim_label, "_text_", i, "_F"))
                                                              ),
                                                              fluidRow(style = "z-index:2",
                                                                       echarts4rOutput(paste0(dim_label, "_women_ts_", i), height="100%")
                                                              )
                                                       ),
                                                       column(1),
                                                       column(5, class = "card-front card-grow",
                                                              fluidRow(style = "height: 40%; z-index:1;",
                                                                       uiOutput(paste0(dim_label, "_men_gap_", i), height=25),
                                                                       uiOutput(paste0(dim_label, "_icon_", i, "_M")),
                                                                       uiOutput(paste0(dim_label, "_text_", i, "_M"))
                                                              ),
                                                              fluidRow(style = "z-index:2",
                                                                       echarts4rOutput(paste0(dim_label, "_men_ts_", i), height="100%")
                                                              )
                                                       )
                                              )
                                              # fluidRow(class = "slide slide2", style = "height:160px",
                                              #          br(),
                                              #          br(),
                                              #          fluidRow(align = "center",
                                              #                   echarts4rOutput(paste0(dim_label, "_gender_pop_", i), height = 50),
                                              #                   column(4, align = "left", HTML("<span style='font-size:1rem;line-height:1rem;display:inline-block;'>← Group is worse off</span>")),
                                              #                   column(4, align = "center", HTML("<span style='font-size:1rem;line-height:1rem;display:inline-block;margin-top:5px'>Group is same<br>as population</span>")),
                                              #                   column(4, align = "right", HTML("<span style='font-size:1rem;line-height:1rem;display:inline-block;'>Group is better off→</span>"))
                                              #          )
                                              # )
                                          )
                                   ),
                                   column(4, align = "center", class = "container", style = paste0("opacity:", opacity_val %>% filter(dimension_group == "age") %>% pull(opacity), "!important;"),
                                          div(class = "folder",
                                              fluidRow(class = paste0("card slide slide1 ", indicator_list[i], " age_dim"),
                                                       column(3, class = "card-front card-grow",
                                                              fluidRow(style = "height: 40%; z-index:1;",
                                                                       uiOutput(paste0(dim_label, "_young_gap_", i), height = 25),
                                                                       uiOutput(paste0(dim_label, "_icon_", i, "_YOUNG")),
                                                                       uiOutput(paste0(dim_label, "_text_", i, "_YOUNG"))
                                                              ),
                                                              fluidRow(style = "z-index:2",
                                                                       echarts4rOutput(paste0(dim_label, "_young_ts_", i), height="100%"))
                                                       ),
                                                       column(1),
                                                       column(3, class = "card-front card-grow",
                                                              fluidRow(style = "height: 40%; z-index:1;",
                                                                       uiOutput(paste0(dim_label, "_mid_gap_", i), height = 25),
                                                                       uiOutput(paste0(dim_label, "_icon_", i, "_MID")),
                                                                       uiOutput(paste0(dim_label, "_text_", i, "_MID"))
                                                              ),
                                                              fluidRow(style = "z-index:2",
                                                                       echarts4rOutput(paste0(dim_label, "_mid_ts_", i), height="100%"))
                                                       ),
                                                       column(1),
                                                       column(3, class = "card-front card-grow",
                                                              fluidRow(style = "height: 40%; z-index:1;",
                                                                       uiOutput(paste0(dim_label, "_old_gap_", i), height = 25),
                                                                       uiOutput(paste0(dim_label, "_icon_", i, "_OLD")),
                                                                       uiOutput(paste0(dim_label, "_text_", i, "_OLD"))
                                                              ),
                                                              fluidRow(style = "z-index:2",
                                                                       echarts4rOutput(paste0(dim_label, "_old_ts_", i), height="100%"))
                                                       )
                                              )
                                          )
                                   ),
                                   column(4,  align = "center", class = "container", style = paste0("border-right: solid 1.5px lightgrey;
                                                                                                    opacity:", opacity_val %>% filter(dimension_group == "educ") %>% pull(opacity), "!important;"),
                                          div(class = "folder",
                                              fluidRow(class = paste0("card slide slide1 ", indicator_list[i], " educ_dim"),
                                                       column(5, class = "card-front card-grow",
                                                              fluidRow(style = "height: 40%; z-index:1;",
                                                                       uiOutput(paste0(dim_label, "_secondary_gap_", i), height = 25),
                                                                       uiOutput(paste0(dim_label, "_icon_", i, "_ISCED11_2_3")),
                                                                       uiOutput(paste0(dim_label, "_text_", i, "_ISCED11_2_3"))
                                                              ),
                                                              fluidRow(style = "z-index:2",
                                                                       echarts4rOutput(paste0(dim_label, "_secondary_ts_", i), height="100%")
                                                              )
                                                       ),
                                                       column(1),
                                                       column(5, class = "card-front card-grow",
                                                              fluidRow(style = "height: 40%; z-index:1;",
                                                                       uiOutput(paste0(dim_label, "_tertiary_gap_", i), height = 25),
                                                                       uiOutput(paste0(dim_label, "_icon_", i, "_ISCED11_5T8")),
                                                                       uiOutput(paste0(dim_label, "_text_", i, "_ISCED11_5T8"))
                                                              ),
                                                              fluidRow(style = "z-index:2",
                                                                       echarts4rOutput(paste0(dim_label, "_tertiary_ts_", i), height="100%")
                                                              )
                                                       )
                                              )
                                              # fluidRow(class = "slide slide2", style = "height:160px",
                                              #          br(),
                                              #          br(),
                                              #          fluidRow(
                                              #            echarts4rOutput(paste0(dim_label, "_educ_pop_", i), height = 50),
                                              #            column(4, align = "left", HTML("<span style='font-size:1rem;line-height:1rem;display:inline-block;'>← Group is worse off</span>")),
                                              #            column(4, align = "center", HTML("<span style='font-size:1rem;line-height:1rem;display:inline-block;margin-top:5px'>Group is same<br>as population</span>")),
                                              #            column(4, align = "right", HTML("<span style='font-size:1rem;line-height:1rem;display:inline-block;'>Group is better off →</span>")),
                                              #            br(),
                                              #            column(12, align="left", HTML("<span style='font-size:0.75rem'>Note: Education data excludes primary education attainment due to small samples.</span>"))
                                              #          )
                                              # )

                                          )
                                   )
                                 ),
                                 fluidRow(class = "slide slide2",
                                          style = "height:160px; border-left:solid 1.5px lightgrey; border-right: solid 1.5px lightgrey; border-bottom: solid 1.5px lightgrey; border-radius:0px 0px 25px 25px;",
                                          align = "center",
                                          column(2),
                                          column(8,
                                                 HTML("<b>Parity with population average</b>"),
                                                 br(),
                                                 br(),
                                                 echarts4rOutput(paste0(dim_label, "_age_pop_", i), height = 50),
                                                 column(4, align = "right", HTML("<span style='font-size:1rem;line-height:1rem;display:inline-block;'>← Group is worse off</span>")),
                                                 column(4, align = "center", HTML("<span style='font-size:1rem;line-height:1rem;display:inline-block;margin-top:5px'>Group is same<br>as population</span>")),
                                                 column(4, align = "left", HTML("<span style='font-size:1rem;line-height:1rem;display:inline-block;'>Group is better off →</span>"))
                                          ),
                                          column(2)
                                 )
                        )

                 ),
                 column(1)

        )

      })
    )

    return(inequality_table)

  }

  country <- reactive(input$countrySelector)

  dat_country <- reactive({ full_dat %>% filter(ref_area == country()) })

  dat_latest <- reactive({ latest_idx %>% filter(ref_area == country()) })

  dat_total <- reactive({ latest_tot %>% filter(ref_area == country()) })

  country_data_by_measure <- reactive({
    d <- dat_country(); req(nrow(d))
    split(d, d$measure)
  })

  measures_by_cluster <- reactive({
    ml <- unique(dat_country()$measure)

    if(input$clusterSelector == "mats") {
      ml[ml %in% mat_cluster]
    } else if(input$clusterSelector == "coms") {
      ml[ml %in% c(coms_cluster, social_cluster)]
    } else if(input$clusterSelector == "qualts") {
      ml[ml %in% c(qualts_cluster)]
    }

  })

  make_renderers <- function(dim_label, indicator_list) {

    if(dim_label == "mats") {
      dim_title <- paste0("Material conditions in ", ifelse(countryName() == "OECD Average", "the OECD", countryName()))
      dim_desc <- "The conditions that shape people’s economic options: income and wealth, housing and work and job quality indicators."
    } else if(dim_label == "qualts") {
      dim_title <- paste0("Quality of life in ", ifelse(countryName() == "OECD Average", "the OECD", countryName()))
      dim_desc <- "The factors that encompass how well people are (and how well they feel they are), what they know and can do, and how healthy and safe their places of living are:<br>health, knowledge and skills, environmental quality, subjective well-being and safety."
    } else if(dim_label == "coms") {
      dim_title <-  paste0("Community relationships in ", ifelse(countryName() == "OECD Average", "the OECD", countryName()))
      dim_desc <- "Community relationships encompass how connected and engaged people are, and how and with whom they spend their time: work-life balance, social connections, civic engagement. Social capital indicators are also included here."
    }

    output[[paste0(dim_label, "_dimension_title")]] <- renderUI({

      fluidRow(
        fluidRow(align = "center",
                 HTML(paste0("<b style='font-size:22px;'>", dim_title,"</b>
                                 <br>
                                 <span style = 'font-size:1.5rem'>", dim_desc,"</span>
                                <br>"))
        ),
        br()
      )

    })


    lapply(c("F", "M", "YOUNG", "MID", "OLD", "ISCED11_2_3", "ISCED11_5T8"), function(dim_val) {

      # dat_country <- full_dat %>% filter(ref_area == "OECD")
      # dim_val <- "YOUNG"

      priority <- c("Improving", "No significant change", "Deteriorating", "Not enough data")

      dim_color <- dat_country() %>%
        filter(dimension == dim_val) %>%
        distinct(dimension_color) %>%
        drop_na() %>%
        pull

      dim_name <- dat_country() %>%
        filter(dimension == dim_val) %>%
        distinct(dimension_long) %>%
        drop_na() %>%
        pull

      plot_df <- dat_country() %>%
        filter(measure %in% measures_by_cluster(), dimension == dim_val) %>%
        select(measure, perf_val, perf_val_name) %>%
        distinct() %>%
        mutate(
          # remove bc cleaned
          perf_val_name = ifelse(perf_val_name == "No signifcant change", "No significant change", perf_val_name),
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
        ) %>%
        count(perf_val, perf_val_light, perf_val_name) %>%
        mutate(
          n   = ifelse(is.na(n), 0, n),
          pct = 100 * n / sum(n)
        ) %>%
        arrange(match(perf_val_name, rev(priority))) %>%
        mutate(
          first_seg = row_number() == 1L,
          last_seg  = row_number() == n(),
          standard_style = "display:inline-block;height:25px;line-height:20px;"
        ) %>%
        mutate(
          html = case_when(
            first_seg & !last_seg ~ paste0("<span style='", standard_style,
                               "border: solid 1.5px ", perf_val, ";border-right:0px;",
                               "border-radius: 25px 0px 0px 25px; width:", pct, "%;",
                               "background:", perf_val_light,";'><b>", n, "</b></span>"),
            last_seg & !first_seg ~ paste0("<span style='", standard_style,
                              "border: solid 1.5px ", perf_val, ";border-left:0px;",
                              "border-radius: 0px 25px 25px 0px; width:", pct, "%;",
                              "background:", perf_val_light,";'><b>", n, "</b></span>"),
            first_seg & last_seg ~ paste0("<span style='", standard_style,
                                           "border: solid 1.5px ", perf_val, ";",
                                           "border-radius: 25px; width:", pct, "%;",
                                           "background:", perf_val_light,";'><b>", n, "</b></span>"),
            TRUE ~ paste0("<span style='", standard_style,
                          "border: solid 1.5px ", perf_val, ";border-right:0px;border-left:0px;",
                          "width:", pct, "%;background:", perf_val_light,";'><b>", n, "</b></span>")
          )
        )

      output[[paste0(dim_label, "_summary_", dim_val)]] <- renderUI({

        div(
          fluidRow(align="center", style="margin-right:0px;padding-left:0px;", HTML(paste0("<b style='font-size:1rem;color:", dim_color, "'>", dim_name, "</b>"))),
          fluidRow(class="card-grow", align = "center", style="margin-left:0px;padding-left:0px;", HTML(paste0(plot_df$html, collapse = "")))
        )

      })

    })


    lapply(seq_along(indicator_list), function(i) {

      measure_name <- as.character(indicator_list[[i]])

      # Build time series
      output[[paste0(dim_label,"_women_ts_",i)]] <- renderEcharts4r({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        inequalitySeriesPlotter(d, measure_name, "F")
      })

      output[[paste0(dim_label,"_men_ts_",i)]] <- renderEcharts4r({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        inequalitySeriesPlotter(d, measure_name, "M")
      })

      output[[paste0(dim_label,"_young_ts_",i)]] <- renderEcharts4r({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        inequalitySeriesPlotter(d, measure_name, "YOUNG")
      })

      output[[paste0(dim_label,"_mid_ts_",i)]] <- renderEcharts4r({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        inequalitySeriesPlotter(d, measure_name, "MID")
      })

      output[[paste0(dim_label,"_old_ts_",i)]] <- renderEcharts4r({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        inequalitySeriesPlotter(d, measure_name, "OLD")
      })

      output[[paste0(dim_label,"_secondary_ts_",i)]] <- renderEcharts4r({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        inequalitySeriesPlotter(d, measure_name, "ISCED11_2_3")
      })

      output[[paste0(dim_label,"_tertiary_ts_",i)]] <- renderEcharts4r({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        inequalitySeriesPlotter(d, measure_name, "ISCED11_5T8")
      })


      # Build gap icons
      output[[paste0(dim_label,"_women_gap_",i)]] <- renderUI({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        gapPlotter(d, measure_name, "F")
      })

      output[[paste0(dim_label,"_men_gap_",i)]] <- renderUI({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        gapPlotter(d, measure_name, "M")
      })

      output[[paste0(dim_label,"_young_gap_",i)]] <- renderUI({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        gapPlotter(d, measure_name, "YOUNG")
      })

      output[[paste0(dim_label,"_mid_gap_",i)]] <- renderUI({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        gapPlotter(d, measure_name, "MID")
      })

      output[[paste0(dim_label,"_old_gap_",i)]] <- renderUI({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        gapPlotter(d, measure_name, "OLD")
      })

      output[[paste0(dim_label,"_secondary_gap_",i)]] <- renderUI({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        gapPlotter(d, measure_name, "ISCED11_2_3")
      })

      output[[paste0(dim_label,"_tertiary_gap_",i)]] <- renderUI({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        gapPlotter(d, measure_name, "ISCED11_5T8")
      })


      # Build lollipops
      output[[paste0(dim_label,"_gender_pop_",i)]] <- renderEcharts4r({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        lollipopPlotter(d, measure_name, "gender")
      })

      output[[paste0(dim_label,"_age_pop_",i)]] <- renderEcharts4r({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        lollipopPlotter(d, measure_name, "age")
      })

      output[[paste0(dim_label,"_educ_pop_",i)]] <- renderEcharts4r({
        d <- country_data_by_measure()[[measure_name]]; req(nrow(d))
        lollipopPlotter(d, measure_name, "educ")
      })

      # Build dimension icon
      output[[paste0(dim_label, "_dimension_", i)]] <- renderUI({
        dl <- dat_latest(); req(nrow(dl))
        td <- dl %>% filter(measure == measure_name) %>% slice(1)
        if (!nrow(td)) return(div())
        img <- td$image[1]
        img_cap <- td$image_caption[1]
        dim_name <-str_remove_all(img, "\\.png") %>% tools::toTitleCase(.)
        dim_color <- td$dim_color[1]
        # HTML(paste0("<img style='margin-top:10px;margin-left:5%;' src='", img,"' height=60 width=60>"))
        #

        HTML(paste0("<a class = 'dim-icon-hover'>
                      <img style='margin-top:10px;margin-bottom:10px;margin-left:5%;' src='", img,"' height=60 width=60>
                       <span style='overflow:visible !important;z-index:0' class='dim-icon-text'>", img_cap, "</span>
                    </a>
                    "))
      })


      # Build measure labels
      output[[paste0(dim_label, "_label_text_", i)]] <- renderUI({
        dl <- dat_latest(); req(nrow(dl))
        td <- dl %>% filter(measure == measure_name, !dimension == "_T")
        if (!nrow(td)) return(div())
        latest_year_text <- td %>% filter(!is.na(latest_year)) %>% pull(latest_year) %>% unique()
        earliest_year_text <- td %>% filter(!is.na(earliest_year)) %>% pull(earliest_year) %>% unique()
        year_text <- ifelse(length(latest_year_text) == 0, " ", paste0(earliest_year_text, "-", latest_year_text))
        HTML(paste0("<b>", unique(td$label), "</b>
                      <br>
                      <span style='display:inline-block'>", unique(td$unit), "<br>
                        <i>", year_text,"</i>
                      </span>"))
      })

      output[[paste0(dim_label, "_population_value_", i)]] <- renderUI({
        dl <- dat_total(); req(nrow(dl))
        td <- dl %>% filter(measure == measure_name)
        if (!nrow(td)) return(div())
        latest_value_total <- td %>% pull(value_tidy) %>% unique()
        HTML(paste0("<b>Population average</b><br><span style='font-size:2rem;line-height:2rem'>", latest_value_total, "</span>"))
      })

      for(dv in c("F", "M", "YOUNG", "MID", "OLD", "ISCED11_2_3", "ISCED11_5T8")) {
        local({
          dim_name <- dv

          output[[paste0(dim_label, "_text_", i, "_", dim_name)]] <- renderUI({
            dl <- dat_latest(); req(nrow(dl))
            tidy <- dl %>% filter(measure == measure_name, dimension == dim_name)
            if (!nrow(tidy)) {
              div()
            } else {
              tagList(div(style="margin:0px", HTML(tidy$dimension_tidy, tidy$value_tidy)))
            }
          })

          output[[paste0(dim_label, "_icon_", i, "_", dim_name)]] <- renderUI({
            dl <- dat_latest(); req(nrow(dl))
            tidy <- dl %>% filter(measure == measure_name, dimension == dim_name)
            if (!nrow(tidy) || unique(tidy$ref_area) %in% c("OECD", partner_countries)) return(div())
            div(
              div(style="position:absolute;top:5px;left:32.5px", class=paste0("type_", tidy$icon),
                  bs_icon(tidy$icon, size="1.825rem")),
              div(style="position:absolute;top:5px;left:52.5px",
                  HTML("<span style='font-size:8px;display:inline-block;line-height:8px;text-align:left;'>OECD<br>tier</span>"))
            )
          })
        })
      }

    })

  }

  observeEvent(c(input$countrySelector, input$clusterSelector), {

    make_renderers(input$clusterSelector,  measures_by_cluster())

    inequality_output <- inequalityCardBuilder(measures_by_cluster(), input$clusterSelector)
    output$inequality_table <- renderUI({ inequality_output })


  })

}

shinyApp(ui = ui, server = server)
