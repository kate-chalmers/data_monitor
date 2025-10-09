source("./global.R")

ui <- fluidPage(shinyjs::useShinyjs(),
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")),
                tags$link(rel = "stylesheet", type = "text/css", href = "css/sora.css"),
                tags$link(rel = "stylesheet", type = "text/css", href = "css/caveat.css"),
                
                fluidRow(
                  useShinyjs(),
                  tags$head(
                    tags$script(HTML("
        $(document).ready(function() {
          $(document).on('click', '.card', function() {
            var classList = $(this).attr('class').split(' ');
            var measureClass = classList.find(c => c.includes('_'));
            Shiny.setInputValue('clicked_class', measureClass, {priority: 'event'});
            $('#modal1').modal('show');
          });
        });

      "))
                  ),
                  br(),
                  
                  bsModal(id = "modal1",
                          title = "Indicator definition",
                          trigger = NULL,
                          uiOutput("defText"),
                          easyClose = T)
                  
                ),
                
                fluidRow(
                  column(1),
                  column(10, align = "center",
                         HTML("
           When the indicator has consistently improved since 2010 the indicator trend line is shaded <b style='color:#0F8554;font-size:18px'>green</b>.
           When the indicator has consistently deteorirated, it is shaded <b style='color:#CF597E;font-size:18px'>red</b>.
           When there has been no significant change since the earliest period, the indicator trend line is shaded <b style='color:goldenrod;font-size:18px'>yellow</b>.
           When there is not enough data to judge change over time the trend line is either not displayed or shaded <b style='color:grey60;font-size:18px'>grey</b>.
           <br>
           <br>
           Want to know more? Download the data <a href='https://sdmx.oecd.org/public/rest/data/OECD.WISE.WDP,DSD_HSL@DF_HSL_CWB,1.1/.11_2+11_1+9_3+9_2+8_2+8_1_DEP+7_2+7_1_DEP+6_2_DEP+6_2+5_3+5_1+4_3+4_1+3_2+3_1+2_7+2_2+2_1+1_3+1_2+1_1.._T._T._T.?lastNObservations=1&dimensionAtObservation=AllDimensions&format=csvfilewithlabels'>here.</a>
           <br>
           <br>")
                  ),
                  column(1)
                ),
                fluidRow(align = "center",
                         selectInput("countrySelector",
                                     label = "Choose a country",
                                     choices = country_name_vector,
                                     selected = "OECD"
                         ),
                         HTML("<span style='font-size:10px'>When the OECD average is selected no rank will be given</span>"),
                         br(),
                         br(),
                         fluidRow(
                           column(3),
                           column(3, radioButtons("allToggle", 
                                                  label = "Indicator view options", 
                                                  choices = c("View only headline indicators", "View all indicators"),
                                                  selected = "View only headline indicators")),
                           column(3, radioButtons("orderButton", 
                                                  label = "Indicator order options", 
                                                  choices = c("Arrange by dimension", "Arrange by performance and rank"),
                                                  selected = "Arrange by dimension")),
                           column(3)
                         )
                ),
                br(),
                fluidRow(align = "center",
                         column(1),
                         column(10,
                                HTML("<b style='font-size:28px;margin-bottom:5px;'>Current well-being</b><br>
                         <span style='font-size:1.5rem'>The 11 dimensions of current well-being relate to material conditions that shape people’s economic options and quality-of-life 
                         factors that encompass how well people are (and how well they feel they are), what they know and can do, and how healthy and safe their places of living are. 
                         Quality of life also encompasses how connected and engaged people are, and how and with whom they spend their time.</span>"),
                                br(), 
                                br(), 
                                uiOutput("currentWellbeingSummary")
                         ),
                         column(1)
                ),
                br(),
                fluidRow(uiOutput("material_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                fluidRow(uiOutput("quality_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                fluidRow(uiOutput("community_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                fluidRow(align = "center",
                         column(1),
                         column(10,
                                HTML("<b style='font-size:28px;margin-bottom:5px;'>Future well-being</b><br>
                              <span style='font-size:22px'>The systemic resources that underpin future well-being over time are expressed in terms of four types of capital: Natural, Human, Social, Economic.</span>"),
                                br(),
                                br(), 
                                uiOutput("futureWellbeingSummary")
                         ),
                         column(1)
                ),
                br(),
                fluidRow(uiOutput("nature_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                fluidRow(uiOutput("econ_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                fluidRow(uiOutput("social_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                fluidRow(uiOutput("human_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                fluidRow(HTML("&nbsp<br><br><br>"))
                
                
                
)

server <- function(input, output, session) {
  
  
  # Dashboard main
  observeEvent(c(input$countrySelector, input$allToggle, input$orderButton), {
    
    # input <- c()
    # input$countrySelector <- "OECD"
    
    avg_vals <- readRDS("./data/latest point data.RDS") %>% filter(grepl(input$countrySelector, ref_area))
    ts_vals <- readRDS("./data/time series.RDS") %>% filter(grepl(input$countrySelector, ref_area))
    
    gap_filler <- avg_vals %>%
      distinct(measure, unit_measure) %>%
      mutate(measure2 = measure) %>%
      separate(measure2, into=c("cat", "subcat")) %>%
      mutate(cat = as.numeric(cat),
             subcat = as.numeric(subcat)) %>%
      arrange(cat, subcat, measure) %>%
      select(-subcat) %>%
      mutate(measure = fct_infreq(measure))
    
    if (input$allToggle == "View only headline indicators") {
      avg_vals <- avg_vals %>% filter(measure %in% headline_indicators)
      avg_vals <- avg_vals[order(match(avg_vals$measure,unique(as.character(gap_filler$measure)))),]
      ts_vals <- ts_vals %>% filter(measure %in% headline_indicators)
      cwb_indicator_text_filter <- c(material_headline, quality_headline, community_headline)
      fwb_indicator_text_filter <- c(nature_headline, econ_headline, social_headline, human_headline)
    } else {
      avg_vals <- avg_vals[order(match(avg_vals$measure,unique(as.character(gap_filler$measure)))),]
      ts_indics <- gap_filler %>% filter(measure %in% avg_vals$measure) %>% arrange(measure) %>% pull(measure) %>% as.character()
      ts_vals <- ts_vals %>% filter(measure %in% ts_indics)
      cwb_indicator_text_filter <- gap_filler %>% filter(cat %in% 1:11) %>% pull(measure)
      fwb_indicator_text_filter <- gap_filler %>% filter(cat %in% 12:15) %>% pull(measure)
    }
    
    if(input$orderButton == "Arrange by performance and rank") {
      avg_vals <- avg_vals %>% arrange(arrangement)
    }
    
    mats_ts <- ts_vals %>% filter(cat %in% c("1", "2", "3")) %>% split(f = .$measure)
    qual_ts <- ts_vals %>% filter(cat %in% c("5", "6", "9", "10", "11")) %>% split(f = .$measure)
    coms_ts <- ts_vals %>% filter(cat %in% c("4", "7", "8")) %>% split(f = .$measure)
    nature_ts <- ts_vals %>% filter(cat %in% c("12")) %>% split(f = .$measure)
    human_ts <- ts_vals %>% filter(cat %in% c("13")) %>% split(f = .$measure)
    social_ts <- ts_vals %>% filter(cat %in% c("14")) %>% split(f = .$measure)
    econ_ts <- ts_vals %>% filter(cat %in% c("15")) %>% split(f = .$measure)
    
    mats <- avg_vals %>% filter(cat %in% c("1", "2", "3"))
    qual <- avg_vals %>% filter(cat %in% c("5", "6", "9", "10", "11"))
    coms <- avg_vals %>% filter(cat %in% c("4", "7", "8"), !measure == "8_2")
    nature <- avg_vals %>% filter(cat %in% c("12"))
    human <- avg_vals %>% filter(cat %in% c("13"))
    social <- avg_vals %>% filter(cat %in% c("14"))
    econ <- avg_vals %>% filter(cat %in% c("15"))
    
    # CWB
    output$currentWellbeingSummary <- renderUI({ HTML(indicatorText(avg_vals, cwb_indicator_text_filter)) })
    
    # FWB
    output$futureWellbeingSummary <- renderUI({ HTML(indicatorText(avg_vals, fwb_indicator_text_filter)) })
    
    cardBuilder <- function(point_avg, ts_avg, cluster_name) {
      
      # point_avg <- mats
      # ts_avg <- mats_ts
      
      country_name <- names(country_name_vector[country_name_vector == input$countrySelector])
      
      lapply(1:nrow(point_avg), function(i) {
        
        avg_measure <- point_avg[i,] %>% pull(measure)
        avg_ref_area <- point_avg[i,] %>% pull(ref_area)
        
        label_text <- paste0("<br><br><span style='font-size:12px;'>", break_wrap(point_avg[i,]$label_name, 30), "</span><br>")
        
        if(is.na(point_avg[i,]$latest)) {
          value_text <- paste0("<span style='font-size:12px'>No data available for ", country_name, "</span><br>")
        } else {
          
          if(!is.na(point_avg[i,]$unit_tag)) {
            
            if(point_avg[i,]$position == "before") {
              value_text <- paste0(point_avg[i,]$unit_tag,"<span style='font-size:24px'>", prettyNum(round(point_avg[i,]$latest, point_avg[i,]$round_val), big.mark = " "), "</span><br>")
            } else {
              value_text <- paste0("<span style='font-size:24px'>", prettyNum(round(point_avg[i,]$latest, point_avg[i,]$round_val), big.mark = " "), point_avg[i,]$unit_tag,"</span><br>")
            }
          } else {
            value_text <- paste0("<span style='font-size:24px'>", prettyNum(round(point_avg[i,]$latest, point_avg[i,]$round_val), big.mark = " "), "</span><br>")
          }
        }
        
        value_text <- paste0("<span style='line-height:0px;'>", value_text, "</span>")
        
        year_text <- paste0("<span style='font-size:12px'>", point_avg[i,]$time_period, "</span><br><br>")
        
        if(!is.null(ts_avg[[avg_measure]])) {
          plot_output <- plotlyPlotter(ts_avg[[avg_measure]])
        } else {
          plot_output <- NULL
        }
        
        if(!is.na(point_avg[i,]$explanation)) {
          explanation_text <- point_avg[i,]$explanation
          output[[paste0(cluster_name, "_cards_", i)]] <- renderUI({ tagList(div(style = "margin:0px;display:inline-block;", HTML(label_text, value_text, year_text, explanation_text))) })
          
        } else {
          output[[paste0(cluster_name, "_cards_", i)]] <- renderUI({ tagList(div(style = "margin:0px;display:inline-block;", HTML(label_text, value_text, year_text))) })
        }
        
        output[[paste0(cluster_name, "_sparkline_", i)]] <- renderPlotly({ plot_output })
        
        output[[paste0(cluster_name, "_icon_", i)]] <- renderUI({
          if(grepl("OECD", avg_ref_area)) {
            div(style = "position:absolute;top:5px;left:10px", HTML(paste0("<img src = '", point_avg[i,]$image, "' height=25px width=25px")))
          } else {
            div(
              div(style = "position:absolute;top:5px;left:10px", HTML(paste0("<img src = '", point_avg[i,]$image, "' height=25px width=25px"))),
              div(style = "position:absolute;top:-2px;left:25px", HTML(paste0("<img src='", ifelse(is.na(point_avg[i,]$icon), "three-dots", point_avg[i,]$icon), "' height=30px width=30px>"))),
              # div(style = "position:absolute;top:1px;left:30px", bs_icon(class = point_avg[i,]$icon, ifelse(is.na(point_avg[i,]$icon), "three-dots", point_avg[i,]$icon), size = "2.5rem")),
              div(style = "position:absolute;top:4px;left:55px", HTML("<span style='font-size:9px;display:inline-block;line-height:11px;text-align:left;'>OECD<br>tier</span>"))
            )
          }
        })
        
        
      })
      
      if(cluster_name == "material") {
        output[[paste0(cluster_name, "dim_desc_text")]] <- renderUI({
          HTML("<span class='cluster-header' style='font-size:22px'>Material conditions</span>
                <br>
                <span style = 'font-size:1.5rem'>The conditions that shape people’s economic options:
                income and wealth, housing and work and job quality indicators.</span><br><br>")
        })
      } else if(cluster_name == "quality") {
        output[[paste0(cluster_name, "dim_desc_text")]] <- renderUI({
          HTML("<span class='cluster-header' style='font-size:22px'>Quality of life</span>
                                   <br>
                                   <span style = 'font-size:1.5rem'>The factors that encompass how well people are (and how well they feel they are),
                                   what they know and can do, and how healthy and safe their places of living are: health, knowledge and skills, environmental quality,
                                   subjective well-being and safety.</span><br><br>")
        })
      } else if(cluster_name == "community") {
        
        output[[paste0(cluster_name, "dim_desc_text")]] <- renderUI({
          HTML("<span class='cluster-header' style='font-size:22px'>Community relationships</span>
                      <br>
                      <span style = 'font-size:1.5rem'>Community relationships encompasses how connected and engaged people are, and how
                     and with whom they spend their time: work-life balance, social connections, civic engagement.</span><br><br>")
        })
        
      } else if(cluster_name == "nature") {
        
        output[[paste0(cluster_name, "dim_desc_text")]] <- renderUI({
          HTML("<span class='cluster-header' style='font-size: 22px'>Natural capitals</span>
                        <br>
                        <span style = 'font-size:1.5rem'>The four systemic resources that underpin future well-being over time are expressed in terms of different
                                                      types of capital: Economic, Natural, Human and Social.</span><br><br>")
        })
        
      } else if(cluster_name == "social") {
        
        output[[paste0(cluster_name, "dim_desc_text")]] <- renderUI({
          HTML("<span class='cluster-header' style='font-size: 22px'>Social capitals</span>
                        <br>
                        <span style = 'font-size:1.5rem'>The four systemic resources that underpin future well-being over time are expressed in terms of different
                                                      types of capital: Economic, Natural, Human and Social.</span><br><br>")
        })
        
      } else if(cluster_name == "human") {
        
        output[[paste0(cluster_name, "dim_desc_text")]] <- renderUI({
          HTML("<span class='cluster-header' style='font-size: 28px'>Human capitals</span>
                        <br>
                        <span style = 'font-size:1.5rem'>The four systemic resources that underpin future well-being over time are expressed in terms of different
                                                      types of capital: Economic, Natural, Human and Social.</span><br><br>")
        })
        
      } else if(cluster_name == "econ") {
        
        output[[paste0(cluster_name, "dim_desc_text")]] <- renderUI({
          HTML("<span class='cluster-header' style='font-size: 22px'>Economic capitals</span>
                        <br>
                        <span style = 'font-size:1.5rem'>The four systemic resources that underpin future well-being over time are expressed in terms of different
                                                      types of capital: Economic, Natural, Human and Social.</span><br><br>")
        })
        
      }
      
      output_list <- div(
        column(3, uiOutput(paste0(cluster_name, "dim_desc_text"))), 
        column(9,
               lapply(1:nrow(point_avg), function(i) {
                 column(3, style = "margin-bottom: 10px; ",
                        column(12, class = paste("card", point_avg[i,]$measure, if (i == 1 && cluster_name == "material") "bounce" else NULL),
                               div(class = "card-front card-grow",
                                   fluidRow(style = "height: 40%; z-index:2;",
                                            align = "center",
                                            uiOutput(paste0(cluster_name, "_icon_", i)),
                                            uiOutput(paste0(cluster_name, "_cards_", i))
                                   ),
                                   fluidRow(style = "height:60%; position:relative; margin-bottom:100%; z-index:1",
                                            div(style = "position: absolute; bottom: 0; width: 100%; height: 100%;",
                                                plotlyOutput(paste0(cluster_name, "_sparkline_", i), height = "100%"))
                                   )
                               )
                        )
                 )
               })
        )
      )
      
      return(output_list)
      
    }
    
    output$material_boxes <- renderUI({ cardBuilder(mats, mats_ts, "material") })
    output$quality_boxes <- renderUI({ cardBuilder(qual, qual_ts, "quality") })
    output$community_boxes <- renderUI({ cardBuilder(coms, coms_ts, "community") })
    output$nature_boxes <- renderUI({ cardBuilder(nature, nature_ts, "nature") })
    output$social_boxes <- renderUI({ cardBuilder(social, social_ts, "social") })
    output$human_boxes <- renderUI({ cardBuilder(human, human_ts, "human") })
    output$econ_boxes <- renderUI({ cardBuilder(econ, econ_ts, "econ") })
    
  })
  
  # Modal text
  observeEvent(input$clicked_class, {
    
    req(input$clicked_class)
    
    long_df <- readxl::read_excel("./data/hows_life_dictionary.xlsx")
    heatmap_dat <- readRDS("./data/final dataset.RDS")
    
    
    short_df <- long_df %>% filter(measure == input$clicked_class)
    
    main_title <- paste0("<b style='font-size:18px'> Indicator: ",
                         short_df$measure, " ", short_df$label, "</b>")
    
    long_title <- paste0("<i style='font-size:16px'>", short_df$indicator, "</i>")
    
    unit_title <- paste0("<i style='font-size:16px'>", short_df$unit, "</i>")
    
    desc_text <- paste0(short_df$definition)
    
    source_text <- paste0("Source: ", short_df$source)
    
    indicator_text <- paste0(
      main_title, "<br>",
      long_title, "<br>",
      unit_title, "<br><br>",
      desc_text, "<br><br>",
      source_text, "<br>"
    )
    
    heatmap_short <- heatmap_dat %>%
      filter(measure == input$clicked_class) %>%
      select(ref_area, time_period) %>%
      group_by(time_period) %>%
      mutate(n = n(),
             time_period = as.numeric(time_period)) %>%
      group_by(time_period) %>%
      complete(ref_area = oecd_countries) %>%
      group_by(ref_area) %>%
      complete(time_period = 2004:as.numeric(format(Sys.Date(), "%Y"))) %>%
      ungroup() %>%
      mutate(
        val = case_when(
          is.na(n) ~ 0,
          TRUE ~ 1
        )
      )
    
    output$dataHeatmap <- renderEcharts4r({
      
      heatmap_short %>%
        e_charts(time_period) |>
        e_heatmap(ref_area, val,
                  itemStyle = list(
                    borderColor = "#5c5b5b",
                    borderWidth = 0.35
                  )
        ) |>
        e_visual_map(
          val,
          inRange = list(color = c("#c4c2c2", "#002F6C")),
          type = "piecewise",
          pieces = list(
            list(value = 0, label = "No data", color = "#c4c2c2"),
            list(value = 1, label = "Data ", color = "#002F6C")
          ),
          orient = "horizontal",
          left = "center",
          top = "5%"
        ) |>
        e_y_axis(
          axisLabel = list(
            show = TRUE,
            fontSize = 10
          )
        ) |>
        e_x_axis(
          axisLabel = list(
            show = TRUE,
            fontSize = 10
          )
        )
      
    })
    
    output$defText <- renderUI({
      tagList(
        HTML(indicator_text),
        echarts4rOutput("dataHeatmap")
      )
    })
  })
  
  
}

shinyApp(ui = ui, server = server)
