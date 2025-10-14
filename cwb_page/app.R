source("./global.R")

ui <- fluidPage(shinyjs::useShinyjs(),
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")),
                fluidRow(
                  useShinyjs(),
                  tags$head(tags$script(src="bounce.js")),
                  tags$head(tags$script(src="modal.js")),
                  column(12, align="right", style = "font-size:1rem",
                         paste0("Last updated: ", last_updated, "   ")
                  ),
                  br(),
                  bsModal(id = "modal1",
                          title = "Download the data",
                          trigger = NULL,
                          uiOutput("defText"))
                  
                ),
                br(),
                br(),
                fluidRow(align = "center",
                         selectInput("countrySelector",
                                     label = "Choose a country",
                                     choices = list(
                                       "Averages" = average_vector,
                                       "OECD countries" = oecd_vector,
                                       "Non-OECD countries" = accession_vector
                                     ),
                                     selected = "OECD"
                         ),
                         HTML("<span style='font-size:9px'>Tier is not shown when an average or a non-OECD country is selected</span>"),
                         br(),
                         br(),
                         br(),
                         fluidRow(
                           column(3),
                           column(3, radioButtons("allToggle", 
                                                  label = "Indicator view options", 
                                                  choices = c("View only headline indicators", "View all indicators"))),
                           column(3, radioButtons("orderButton", 
                                                  label = "Indicator order options", 
                                                  choices = c("Arrange by dimension", "Arrange by performance"),
                                                  selected = "Arrange by dimension")),
                           column(3)
                         )
                ),
                br(),
                br(),
                fluidRow(align = "center",
                         column(1),
                         column(10,
                                fluidRow(
                                  uiOutput("section_title"),
                                  HTML("
                         The 11 dimensions of current well-being relate to material conditions that shape people’s economic options and quality-of-life 
                         factors that encompass how well people are (and how well they feel they are), what they know and can do, and how healthy and safe their places of living are. 
                         Quality of life also encompasses how connected and engaged people are, and how and with whom they spend their time."),
                                  
                                ),
                                br(), 
                                fluidRow(
                                  column(3),
                                  column(6, 
                                         HTML("
                                         Since 2010, this number of indicators have...
                                         <br>
                                         <br>"),
                                         div(class = "card-grow", uiOutput("currentWellbeingSummary")),
                                         HTML(
                                           "<br>
                                            <span style='color:#999999;'>●</span> not enough data
                                            <span style='color:#CF597E;'>●</span> deteriorated  
                                            <span style='color:goldenrod;'>●</span> no significant change 
                                            <span style='color:#0F8554;'>●</span> improved")
                                  ),
                                  column(3)
                                )
                         ),
                         column(1)
                ),
                br(),
                br(),
                fluidRow(class="first-set", uiOutput("material_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                fluidRow(uiOutput("quality_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                fluidRow(uiOutput("community_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                fluidRow(HTML("&nbsp<br><br><br>"))
                
                
                
)

server <- function(input, output, session) {
  
  output$section_title <- renderUI({
    if(countryName() == "OECD Average") {
      HTML(paste0("<b style='font-size:28px;margin-bottom:5px;'>Current well-being in the OECD</b><br>"))
    } else {
      HTML(paste0("<b style='font-size:28px;margin-bottom:5px;'>Current well-being in ", countryName(),"</b><br>"))
    }
  })
  
  
  # Dashboard main
  observeEvent(c(input$countrySelector, input$allToggle, input$orderButton), {
    
    # input <- c()
    # input$countrySelector <- "BEL"
    # input$allToggle <- "View only headline indicators"
    
    avg_vals <- avg_vals_full %>% filter(startsWith(ref_area, input$countrySelector))
    ts_vals <- ts_vals_full %>% filter(startsWith(ref_area, input$countrySelector))
    
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
    
    if(input$orderButton == "Arrange by performance") {
      avg_vals <- avg_vals %>% arrange(arrangement) 
    }
    
    mats_ts <- ts_vals %>% filter(cat %in% c("1", "2", "3")) %>% split(f = .$measure)
    qual_ts <- ts_vals %>% filter(cat %in% c("5", "6", "9", "10", "11")) %>% split(f = .$measure)
    coms_ts <- ts_vals %>% filter(cat %in% c("4", "7", "8")) %>% split(f = .$measure)

    mats <- avg_vals %>% filter(cat %in% c("1", "2", "3"))
    qual <- avg_vals %>% filter(cat %in% c("5", "6", "9", "10", "11"))
    coms <- avg_vals %>% filter(cat %in% c("4", "7", "8"), !measure == "8_2")
    
    # CWB
    output$currentWellbeingSummary <- renderUI({ pillBox(avg_vals, cwb_indicator_text_filter) })
    
    cardBuilder <- function(point_avg, ts_avg, cluster_name) {
      
      # point_avg <- coms
      # ts_avg <- coms_ts
      
      country_name <- names(country_name_vector[country_name_vector == input$countrySelector])
      
      lapply(1:nrow(point_avg), function(i) {
        
        avg_measure <- point_avg[i,] %>% pull(measure)
        avg_ref_area <- point_avg[i,] %>% pull(ref_area)
        
        label_text <- paste0("<br><br><span style='font-size:12px;'>", break_wrap(point_avg[i,]$label_name, 30), "</span><br>")
        
        if(is.na(point_avg[i,]$latest)) {
          value_text <- paste0("<br><span style='font-size:24px;'>No data</span><br>")
        } else {
          if(!is.na(point_avg[i,]$unit_tag)) {
            
            if(point_avg[i,]$position == "before") {
              value_text <- paste0(point_avg[i,]$unit_tag,"<span style='font-size:24px;line-height:25px;'>", prettyNum(round(point_avg[i,]$latest, point_avg[i,]$round_val), big.mark = " "), "</span><br>")
            } else {
              value_text <- paste0("<span style='font-size:24px;line-height:25px;'>", prettyNum(round(point_avg[i,]$latest, point_avg[i,]$round_val), big.mark = " "), "</span>", point_avg[i,]$unit_tag,"<br>")
            }
          } else {
            value_text <- paste0("<span style='font-size:24px;line-height:25px;'>", prettyNum(round(point_avg[i,]$latest, point_avg[i,]$round_val), big.mark = " "), "</span><br>")
          }
        }
        
        value_text <- paste0("<span>", value_text, "</span>")
        
        year_text <- paste0("<span style='font-size:12px'>", point_avg[i,]$time_period, "</span><br><br>")
        
        if(!is.null(ts_avg[[avg_measure]])) {
          plot_output <- areaPlotter(ts_avg[[avg_measure]], country_name)
        } else {
          plot_output <- NULL
        }
        
        # REMOVE BEFORE LAUNCH
        if(!is.na(point_avg[i,]$explanation)) {
          explanation_text <- point_avg[i,]$explanation
          explanation_text <- ""
          output[[paste0(cluster_name, "_cards_", i)]] <- renderUI({ tagList(div(style = "margin-top:5px;margin-left:0px;margin-right:0px;margin-bottom:0px;display:inline-block;", HTML(label_text, value_text, year_text, explanation_text))) })
        } else {
          output[[paste0(cluster_name, "_cards_", i)]] <- renderUI({ tagList(div(style = "margin-top:5px;margin-left:0px;margin-right:0px;margin-bottom:0px;display:inline-block;", HTML(label_text, value_text, year_text))) })
        }
        
        output[[paste0(cluster_name, "_sparkline_", i)]] <- renderEcharts4r({ plot_output })
        
        output[[paste0(cluster_name, "_icon_", i)]] <- renderUI({
          if(startsWith(avg_ref_area, "OECD") | avg_ref_area %in% partner_countries) {
            div(style = "position:absolute;top:5px;left:10px", HTML(paste0("<a class = 'dim-icon-hover'>
                                                                              <img src = '", point_avg[i,]$image, "' height=25px width=25px>
                                                                              <span style='overflow:visible !important;z-index:0' class='dim-icon-text'>", point_avg[i,]$image_caption, "</span>
                                                                           </a>
                                                                           ")
            ))
          } else {
            div(
              div(style = "position:absolute;top:5px;left:10px", HTML(paste0("<a class = 'dim-icon-hover'>
                                                                              <img src = '", point_avg[i,]$image, "' height=25px width=25px>
                                                                              <span style='overflow:visible !important;z-index:0' class='dim-icon-text'>", point_avg[i,]$image_caption, "</span>
                                                                           </a>
                                                                           ")
              )),
              div(style = "position:absolute;top:2px;left:36px", HTML(paste0("<img src='", ifelse(is.na(point_avg[i,]$icon), "three-dots", point_avg[i,]$icon), "' height=30px width=30px>"))),
              div(style = "position:absolute;top:7px;left:66px", HTML("<span style='font-size:9px;display:inline-block;line-height:11px;text-align:left;'>OECD<br>tier</span>"))
            )
          }
        })
        
        
      })
      
      if(cluster_name == "material") {
        
        cluster_html <- HTML("<span class='cluster-header' style='font-size:22px'>Material conditions</span>
                 <br>
                 The conditions that shape people’s economic options like income and wealth, housing and work and job quality 
                 indicators.
                 <br>
                 <br>")
        
      } else if(cluster_name == "quality") {
        
        cluster_html <- HTML("<br><span class='cluster-header' style='font-size:22px'>Quality of life</span>
                                   <br>
                                   The factors that encompass how well people are (and how well they feel they are),
                                   what they know and can do, and how healthy and safe their places of living are: health, knowledge and skills, environmental quality,
                                   subjective well-being and safety.<br><br>")
        
      } else if(cluster_name == "community") {
        
        cluster_html <- HTML("<br><span class='cluster-header' style='font-size:22px'>Community relationships</span>
                      <br>
                      Community relationships encompasses how connected and engaged people are, and how
                      and with whom they spend their time: work-life balance, social connections, civic engagement.<br><br>")
        
      } else if(cluster_name == "nature") {
        
        cluster_html <- HTML("<span class='cluster-header' style='font-size: 22px'>Natural capital</span>
                        <br>
                        Natural Capital consists of naturally occurring assets and ecosystems. 
                        The scope of Natural Capital is vast: indicators selected for this chapter represent a small headline set of all the possible stocks, flows, and risk and resilience factors of relevance.<br><br>")
        
      } else if(cluster_name == "social") {
        
        cluster_html <- HTML("<br><span class='cluster-header' style='font-size: 22px'>Social capital</span>
                        <br>
                        Social Capital is about the social norms, shared values and institutional arrangements that foster co-operation among population groups.<br><br>")
        
      } else if(cluster_name == "human") {
        
        
        cluster_html <- HTML("<br><span class='cluster-header' style='font-size: 22px'>Human capital</span>
                        <br>
                        Human Capital refers to the skills, competencies and health status of individuals. 
                        Beyond technical skills, the concept of human capital has been expanded to include aspects of motivation and behaviour, as well as the physical, emotional and mental health of individuals.<br><br>")
        
      } else if(cluster_name == "econ") {
        
        
        cluster_html <- HTML("<br><span class='cluster-header' style='font-size: 22px'>Economic capital</span>
                        <br>
                        Economic Capital consists of produced and financial capital. Produced capital refers to man-made tangible assets, intellectual property and inventories of final and intermediate goods. Financial capital includes financial assets.<br><br>")
        
      }
      
      output[[paste0(cluster_name, "dim_desc_text")]] <- renderUI({
        fluidRow(
          column(1),
          column(10, cluster_html),
          column(1)
        )
      })
      
      output_list <- div(
        fluidRow(align = "center", uiOutput(paste0(cluster_name, "dim_desc_text"))), 
        fluidRow(
          lapply(1:nrow(point_avg), function(i) {
            column(3, class="card-grow", style = paste0("margin-bottom: 10px; opacity:", point_avg[i,]$opacity, ";"),
                   div(style="z-index:0;margin-left:5px;margin-right:5px;", class = paste("card", point_avg[i,]$measure, if (i == 1 && cluster_name == "material") "bounce" else NULL),
                       div(class = paste0(if (i == 1 && cluster_name == "material") "bounce-label" else "bounce-label-off"), "Click cards to download data"),
                       div(class = "card-front",
                           div(class = "card-top", style="height:40%;z-index:2;",
                               fluidRow(align = "center",
                                        uiOutput(paste0(cluster_name, "_icon_", i)),
                                        uiOutput(paste0(cluster_name, "_cards_", i))
                               )
                           ),
                           div(class = "sparkline-wrap", style="height:60%;z-index:1;",
                                   echarts4rOutput(paste0(cluster_name, "_sparkline_", i), height="100%")
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

  })
  
  countryName <- eventReactive(input$countrySelector, { 
    
    country_name <- which(country_name_vector == input$countrySelector) %>% names 
    
    if(country_name %in% the_thes) {
      country_name <- paste0("the ", country_name)
    }
    
    return(country_name) 
    
  })
  
  dataExplorerURL <- eventReactive(c(input$clicked_class, input$countrySelector), {
    
    cat_num <- str_split_fixed(input$clicked_class, "_", 2)[1] %>% as.numeric()
    
    if(input$countrySelector == "OECD") {
      ref_area <- paste0(oecd_countries, collapse = "+")
    } else {
      ref_area <- input$countrySelector
    }
    
    if(!cat_num %in% c(12:15)) {
      data_explorer_url <- paste0("https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CSociety%23SOC%23%7CWell-being%20and%20beyond%20GDP%23SOC_WEL%23&pg=0&fc=Topic&bp=true&snb=26&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_HSL%40DF_HSL_CWB&df[ag]=OECD.WISE.WDP&df[vs]=1.1&dq=", ref_area,".", input$clicked_class,".._T._T._T.&pd=%2C&to[TIME_PERIOD]=false")
    } else {
      data_explorer_url <- paste0("https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CSociety%23SOC%23%7CWell-being%20and%20beyond%20GDP%23SOC_WEL%23&pg=0&fc=Topic&bp=true&snb=26&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_HSL%40DF_HSL_FWB&df[ag]=OECD.WISE.WDP&df[vs]=1.1&dq=", ref_area, ".", input$clicked_class, ".._T._T._T.&pd=%2C&to[TIME_PERIOD]=false")
    }
    
    return(data_explorer_url)
    
  })
  
  # Modal text
  observeEvent(input$clicked_class, {
    
    req(input$clicked_class)
    
    long_df <- readxl::read_excel("./data/hows_life_dictionary.xlsx")
    # heatmap_dat <- readRDS("./data/final dataset.RDS")
    
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
  
}

shinyApp(ui = ui, server = server)
