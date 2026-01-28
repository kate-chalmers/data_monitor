source("./global.R")

ui <- fluidPage(shinyjs::useShinyjs(),
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")),
                tags$head(tags$script(src="modal.js")),

                bsModal(id = "modal1",
                        title = "Télécharger les données",
                        trigger = NULL,
                        uiOutput("defText")),

                fluidRow(align="center",
                         column(2),
                         column(8, align = "center",
                                br(),
                                div(style = "z-index:0;",
                                    selectInput("countrySelector",
                                                label = "Choisir un pays",
                                                choices = list(
                                                  "Moyennes" = average_vector,
                                                  "Pays de l'OCDE" = oecd_vector,
                                                  "Pays partenaires" = accession_vector
                                                ),
                                                selected = "OECD"
                                    )
                                ),
                                HTML("<span style='font-size:9px'>Le niveau n’est affiché que pour les pays de l’OCDE</span>"),
                                br(),
                                br(),
                                br(),
                                radioGroupButtons("clusterSelector",
                                                  choices = c("Conditions de vie matérielles" = "mats",
                                                              "Qualité de vie" = "qualts",
                                                              "Relations sociales" = "coms"),
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

    if(short_df$note == "") {
      note_text <- ""
    } else {
      note_text <- paste0(short_df$note)
    }

    indicator_text <- paste0(
      main_title, "<br>",
      "<b>Technical name: </b>", long_title, "<br>",
      "<b>Unit: </b>", unit_title, "<br><br>",
      desc_text, "<br><br>",
      "<b>Note: </b>", note_text
    )

    download_text <- paste0("<center>
                             <br>
                             <b style = 'font-size:2rem;'>Télécharger ", short_df$label," les données pour ", countryName(), " de la
                                 <a href='", dataExplorerURL(), "' target='_blank'>base de données Comment va la vie? de l'OCDE</a>
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

    if(country_name %in% article_en) {
      country_name <- paste0("en ", country_name)
    } else if (country_name %in% article_aux) {
      country_name <- paste0("aux ", country_name)
    } else if (country_name %in% article_au) {
      country_name <- paste0("au ", country_name)
    } else if (country_name == "OCDE") {
      country_name <- "dans la zone OCDE"
    } else {
      country_name <- country_name
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

      fluidRow(
        fluidRow(
          column(2),
          column(8, align = "center",
                 uiOutput(paste0(dim_label, "_dimension_title")) %>% withSpinner(color="#0dc5c1"),
                 br(),
                 br()
          ),
          column(2)
        ),
        fluidRow(
          column(2),
          column(8, align="center",
                 HTML("Nombre de résultats en matière de bien-être qui se sont améliorés, sont restés globalement stables ou se sont dégradés entre 2015 et la dernière année disponible :
                      <br>"),
                 fluidRow(
                   column(4, style = "margin-top: 20px",
                          uiOutput(paste0(dim_label, "_summary_F")),
                          uiOutput(paste0(dim_label, "_summary_M"))
                   ),
                   column(4,
                          uiOutput(paste0(dim_label, "_summary_YOUNG")),
                          uiOutput(paste0(dim_label, "_summary_MID")),
                          uiOutput(paste0(dim_label, "_summary_OLD"))
                   ),
                   column(4, style = "margin-top: 20px",
                          uiOutput(paste0(dim_label, "_summary_ISCED11_2_3")),
                          uiOutput(paste0(dim_label, "_summary_ISCED11_5T8")))
                 )
          ),
          column(2)
        )

      ),
      fluidRow(align = "center", style="margin-top:0px;",
               HTML(
                 "<br>
                  <span style='color:#CF597E !important;'>●</span> dégradation
                  <span style='color:goldenrod !important;'>●</span> stabilité
                  <span style='color:#0F8554 !important;'>●</span> amélioration
                  <span style='color:#999999 !important;'>●</span> données insuffisantes
                  ")
      ),
      br(),
      br(),
      br(),

      lapply(seq_along(indicator_list), function(i) {

        val <- strsplit(indicator_list[i], "_")[[1]][1]
        dim_color <- dim_colors %>% filter(cat == val) %>% pull(color)

        opacity_val <- dat_country() %>% filter(measure == indicator_list[i]) %>% select(dimension, opacity) %>% distinct()

        column(12, class = "folder-row", align = "center",
               style = "margin-bottom:10px; border-radius: 25px; border:solid 1.5px lightgrey; padding: 0px !important; width: 95%",
               fluidRow(style = paste0("background:", dim_color, "; margin: 0px; border-radius: 23px 23px 0px 0px;"),
                        column(1, class="card-grow", align="center", uiOutput(paste0(dim_label, "_dimension_", i))),
                        column(9, style="margin-top:10px", align = "left", uiOutput(paste0(dim_label, "_label_text_", i))),
                        column(2, style = "margin-top:10px", align = "right", uiOutput(paste0(dim_label, "_population_value_", i)))
               ),
               fluidRow(align= "center", class = "cards-row",
                        column(4, class = "gender_dim",

                               div(class = paste("card-front card-grow", indicator_list[i]),
                                   style = paste0("opacity:", opacity_val %>% filter(dimension == "F") %>% pull(opacity), "!important;"),

                                   fluidRow(style = "height: 40%; z-index:1;",
                                            uiOutput(paste0(dim_label, "_women_gap_", i), height=25),
                                            uiOutput(paste0(dim_label, "_icon_", i, "_F")),
                                            uiOutput(paste0(dim_label, "_text_", i, "_F"))
                                   ),
                                   fluidRow(style = "z-index:2",
                                            echarts4rOutput(paste0(dim_label, "_women_ts_", i), height="100%")
                                   )

                               ),
                               div(class = paste("card-front card-grow", indicator_list[i]),
                                   style = paste0("opacity:", opacity_val %>% filter(dimension == "M") %>% pull(opacity), "!important;"),

                                   fluidRow(style = "height: 40%; z-index:1;",
                                            uiOutput(paste0(dim_label, "_men_gap_", i), height=25),
                                            uiOutput(paste0(dim_label, "_icon_", i, "_M")),
                                            uiOutput(paste0(dim_label, "_text_", i, "_M"))
                                   ),
                                   fluidRow(class = "chart-row", style = "z-index:2;",
                                            echarts4rOutput(paste0(dim_label, "_men_ts_", i), height="100%")
                                   )
                               )
                        ),
                        column(4, class = paste0(indicator_list[i], " age_dim"),
                               div(class = paste("card-front card-grow", indicator_list[i]),
                                   style = paste0("opacity:", opacity_val %>% filter(dimension == "YOUNG") %>% pull(opacity), "!important;"),

                                   fluidRow(style = "height: 40%; z-index:1;",
                                            uiOutput(paste0(dim_label, "_young_gap_", i), height = 25),
                                            uiOutput(paste0(dim_label, "_icon_", i, "_YOUNG")),
                                            uiOutput(paste0(dim_label, "_text_", i, "_YOUNG"))
                                   ),
                                   fluidRow(style = "z-index:2",
                                            echarts4rOutput(paste0(dim_label, "_young_ts_", i), height="100%"))
                               ),
                               div(class = paste("card-front card-grow", indicator_list[i]),
                                   style = paste0("opacity:", opacity_val %>% filter(dimension == "MID") %>% pull(opacity), "!important;"),

                                   fluidRow(style = "height: 40%; z-index:1;",
                                            uiOutput(paste0(dim_label, "_mid_gap_", i), height = 25),
                                            uiOutput(paste0(dim_label, "_icon_", i, "_MID")),
                                            uiOutput(paste0(dim_label, "_text_", i, "_MID"))
                                   ),
                                   fluidRow(style = "z-index:2",
                                            echarts4rOutput(paste0(dim_label, "_mid_ts_", i), height="100%"))
                               ),
                               div(class = paste("card-front card-grow", indicator_list[i]),
                                   style = paste0("opacity:", opacity_val %>% filter(dimension == "OLD") %>% pull(opacity), "!important;"),

                                   fluidRow(style = "height: 40%; z-index:1;",
                                            uiOutput(paste0(dim_label, "_old_gap_", i), height = 25),
                                            uiOutput(paste0(dim_label, "_icon_", i, "_OLD")),
                                            uiOutput(paste0(dim_label, "_text_", i, "_OLD"))
                                   ),
                                   fluidRow(style = "z-index:2",
                                            echarts4rOutput(paste0(dim_label, "_old_ts_", i), height="100%"))
                               )
                        ),
                        column(4, class = paste0(indicator_list[i], " educ_dim"),

                               div(class = paste("card-front card-grow", indicator_list[i]),
                                   style = paste0("opacity:", opacity_val %>% filter(dimension == "ISCED11_2_3") %>% pull(opacity), "!important;"),

                                   fluidRow(style = "height: 40%; z-index:1;",
                                            uiOutput(paste0(dim_label, "_secondary_gap_", i), height = 25),
                                            uiOutput(paste0(dim_label, "_icon_", i, "_ISCED11_2_3")),
                                            uiOutput(paste0(dim_label, "_text_", i, "_ISCED11_2_3"))
                                   ),
                                   fluidRow(style = "z-index:2",
                                            echarts4rOutput(paste0(dim_label, "_secondary_ts_", i), height="100%")
                                   )
                               ),
                               div(class = paste("card-front card-grow", indicator_list[i]),
                                   style = paste0("opacity:", opacity_val %>% filter(dimension == "ISCED11_5T8") %>% pull(opacity), "!important;"),

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
               ),
               fluidRow(style = "height:160px;", align = "center",
                        column(2),
                        column(8,
                               HTML("<b>Écart par rapport à la moyenne de la population</b>"),
                               br(),
                               br(),
                               echarts4rOutput(paste0(dim_label, "_age_pop_", i), height = 50),
                               column(4, align = "right", HTML("<span style='font-size:1rem;line-height:1rem;display:inline-block;'>← La situation du groupe est moins bonne</span>")),
                               column(4, align = "center", HTML("<span style='font-size:1rem;line-height:1rem;display:inline-block;margin-top:5px'>", break_wrap("Les résultats du groupe sont similaires à la moyenne de la population", 20), "</span>")),
                               column(4, align = "left", HTML("<span style='font-size:1rem;line-height:1rem;display:inline-block;'>La situation du groupe est meilleure →</span>"))
                        ),
                        column(2)
               )
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
      dim_title <- paste0("<b style='font-size:28px;margin-bottom:5px;color:#101d40!important;'>Conditions de vie matérielles pour différents groupes de population ", countryName(), "</b>")
      dim_desc <- "Éléments qui déterminent les caractéristiques économiques individuelles, comme le revenu et le patrimoine, le logement, le travail et la qualité de l’emploi."
    } else if(dim_label == "qualts") {
      dim_title <- paste0("<b style='font-size:28px;margin-bottom:5px;color:#101d40!important;'>Qualité de vie pour différents groupes de population ", countryName(), "</b>")
      dim_desc <- "Éléments qui reflètent la qualité de vie des individus : santé, connaissances et compétences, qualité de l'environnement, bien-être subjectif et sécurité."
    } else if(dim_label == "coms") {
      dim_title <-  paste0("<b style='font-size:28px;margin-bottom:5px;color:#101d40!important;'>Relations sociales pour différents groupes de population ", countryName(), "</b>")
      dim_desc <- "Éléments qui montrent le degré d’interaction et la vie sociale des individus, ainsi que la façon dont ils occupent leur temps : équilibre travail-vie privée, liens sociaux et engagement civique."
    }

    output[[paste0(dim_label, "_dimension_title")]] <- renderUI({

      HTML(paste0("<b style='font-size:22px;'>", dim_title,"</b>
                                 <br>
                                 <span style = 'font-size:1.5rem'>", dim_desc,"</span>"))


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

      plot_df_raw <- dat_country() %>%
        filter(measure %in% measures_by_cluster(), dimension == dim_val) %>%
        select(measure, label, image, perf_val, perf_val_name) %>%
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
        )

      indicator_names <- plot_df_raw %>%
        group_by(perf_val_name) %>%
        summarize(
          label_text = paste0( "<img src='", image,"' height=10 width=10> ", label, collapse = "<br>")
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
          standard_style = "display:inline-block;height:25px;line-height:20px;",
          hover_text = paste0("<span class='hidden-text'>", label_text, "</span>")
        ) %>%
        mutate(
          html = case_when(
            first_seg & !last_seg ~ paste0("<span class = 'vis-text' style='", standard_style,
                                           "border: solid 1.5px ", perf_val, ";border-right:0px;",
                                           "border-radius: 25px 0px 0px 25px; width:", pct, "%;",
                                           "background:", perf_val_light,";'><b>", n, "</b>", hover_text,"</span>"),
            last_seg & !first_seg ~ paste0("<span class = 'vis-text' style='", standard_style,
                                           "border: solid 1.5px ", perf_val, ";border-left:0px;",
                                           "border-radius: 0px 25px 25px 0px; width:", pct, "%;",
                                           "background:", perf_val_light,";'><b>", n, "</b>", hover_text,"</span>"),
            first_seg & last_seg ~ paste0("<span class = 'vis-text' style='", standard_style,
                                          "border: solid 1.5px ", perf_val, ";",
                                          "border-radius: 25px; width:", pct, "%;",
                                          "background:", perf_val_light,";'><b>", n, "</b>", hover_text,"</span>"),
            TRUE ~ paste0("<span class = 'vis-text' style='", standard_style,
                          "border: solid 1.5px ", perf_val, ";border-right:0px;border-left:0px;",
                          "width:", pct, "%;background:", perf_val_light,";'><b>", n, "</b>", hover_text,"</span>")
          )
        )

      output[[paste0(dim_label, "_summary_", dim_val)]] <- renderUI({

        div(
          fluidRow( align="center", style="margin-right:0px;padding-left:0px;", HTML(paste0("<span style='font-size:1rem;'>", dim_name, "</span>"))),
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

        HTML(paste0("<a class = 'dim-icon-hover'>
                      <img style='margin-top:10px;margin-bottom:10px;margin-left:5%;' src='", img,"' height=60 width=60>
                       <span style='overflow:visible !important;z-index:0;' class='dim-icon-text'>", img_cap, "</span>
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

        ly <- if (exists("latest_year_text") && length(latest_year_text) > 0) latest_year_text else numeric(0)
        ey <- if (exists("earliest_year_text") && length(earliest_year_text) > 0) earliest_year_text else numeric(0)

        # Compute year_text
        if (length(ly) == 0 && length(ey) == 0) {
          year_text <- ""
        } else if (length(ly) == 0) {
          year_text <- as.character(min(ey))
        } else if (length(ey) == 0) {
          year_text <- as.character(max(ly))
        } else {
          earliest <- min(ey)
          latest   <- max(ly)
          year_text <- if (earliest == latest) as.character(latest) else paste0(earliest, " - ", latest)
        }

        # Special manual override
        if (measure_name == "2_5") {
          year_text <- "2015 - 2016"
        }

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
        HTML(paste0("<b>Moyenne de la population</b><br><span style='font-size:2rem;line-height:2rem'>", latest_value_total, "</span>"))
      })

      for(dv in c("F", "M", "YOUNG", "MID", "OLD", "ISCED11_2_3", "ISCED11_5T8")) {
        local({
          dim_name <- dv

          output[[paste0(dim_label, "_text_", i, "_", dim_name)]] <- renderUI({
            dl <- dat_latest(); req(nrow(dl))
            tidy <- dl %>% filter(measure == measure_name, dimension == dim_name)
            if(grepl("Niveau", tidy$dimension_tidy)) {
              margin_val <- 0
            } else {
              margin_val <- 7.5
            }
            if (!nrow(tidy)) {
              div()
            } else {
              tagList(div(style=paste0("margin-top:", margin_val, "px"), HTML(tidy$dimension_tidy, tidy$value_tidy)))
            }
          })

          output[[paste0(dim_label, "_icon_", i, "_", dim_name)]] <- renderUI({
            dl <- dat_latest(); req(nrow(dl))
            tidy <- dl %>% filter(measure == measure_name, dimension == dim_name)
            if (!nrow(tidy) || unique(tidy$ref_area) %in% c("OECD", partner_countries) || tidy$icon == "three-dots.png") return(div())
            HTML(paste0(
              "<div class='indicator-pin'>",
              "<img src='", tidy$icon, "' height='25' width='25'>",
              "<span class='indicator-label'>OECD<br>tier</span>",
              "</div>"
            ))
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
