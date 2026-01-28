source("./global.R")

ui <- fluidPage(shinyjs::useShinyjs(),
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")),
                fluidRow(
                  useShinyjs(),
                  tags$head(tags$script(src="bounce.js")),
                  tags$head(tags$script(src="modal.js")),
                  
                  bsModal(id = "modal1",
                          title = "Download the data",
                          trigger = NULL,
                          uiOutput("defText"))
                  
                ),

                fluidRow(align = "center",
                         div(style = "z-index:0;",
                             selectInput("countrySelector",
                                         label = "Choose a country",
                                         choices = list(
                                           "Averages" = average_vector,
                                           "OECD countries" = oecd_vector,
                                           "Partner countries" = accession_vector
                                         ),
                                         selected = "OECD"
                             )
                         ),
                         HTML("<span style='font-size:9px'>Tier is displayed for OECD countries only</span>"),
                         br(),
                         br(),
                         br(),
                         fluidRow(
                           column(3),
                           column(3, radioButtons("allToggle", 
                                                  label = "Indicator view", 
                                                  choiceNames = list(HTML("View only <span style='text-decoration: underline dotted' class='head-show'>headline indicators</span><span class='head-hide'>Headline indicators are the 12 most representative indicators of future well-being in the database</span>"),
                                                                     "View all indicators"),
                                                  choiceValues = c(1, 2))),
                           column(3, radioButtons("orderButton", 
                                                  label = "Indicator order", 
                                                  choices = c("Order by dimension", "Order by performance"),
                                                  selected = "Order by dimension")),
                           column(3)
                         )
                ),
                br(),
                fluidRow(align = "center",
                         column(1),
                         column(10,
                                fluidRow(
                                  column(3),
                                  column(6, 
                                         HTML("
                                         Number of resources for future well-being that have improved, shown no clear change or have deteriorated from 2015 to the latest available year:
                                         <br>
                                         <br>"),
                                         div(class = "card-grow", uiOutput("futureWellbeingSummary")),
                                         HTML(
                                           "<br>
                                            <span style='color:#CF597E !important;'>●</span> deteriorated  
                                            <span style='color:goldenrod !important;'>●</span> no clear change 
                                            <span style='color:#0F8554 !important;'>●</span> improved
                                            <span style='color:#999999 !important;'>●</span> not enough data to assess change
                                           ")
                                  ),
                                  column(3)
                                )
                         ),
                         column(1)
                ),
                br(),
                br(),
                column(12, class="first-set", uiOutput("nature_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                column(12, uiOutput("econ_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                column(12, uiOutput("social_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                column(12, uiOutput("human_boxes") %>% withSpinner(color="#0dc5c1")),
                br(),
                fluidRow(HTML("&nbsp<br><br><br>"))
                
                
                
)

server <- function(input, output, session) {

  # Dashboard main
  observeEvent(c(input$countrySelector, input$allToggle, input$orderButton), {
    
    # input <- c()
    # input$countrySelector <- "OECD"
    # input$allToggle <- 2
    
    avg_vals <- avg_vals_full %>% filter(startsWith(ref_area, input$countrySelector))
    ts_vals <- ts_vals_full %>% filter(startsWith(ref_area, input$countrySelector))
    
    if (input$allToggle == 2) {
      avg_vals <- avg_vals[order(match(avg_vals$measure,unique(as.character(gap_filler$measure)))),]
      ts_indics <- gap_filler %>% filter(measure %in% avg_vals$measure) %>% arrange(measure) %>% pull(measure) %>% as.character()
      ts_vals <- ts_vals %>% filter(measure %in% ts_indics)
      fwb_indicator_text_filter <- gap_filler %>% filter(cat %in% 12:15) %>% pull(measure)
    } else {
      avg_vals <- avg_vals %>% filter(measure %in% headline_indicators)
      avg_vals <- avg_vals[order(match(avg_vals$measure,unique(as.character(gap_filler$measure)))),]
      ts_vals <- ts_vals %>% filter(measure %in% headline_indicators)
      fwb_indicator_text_filter <- c(nature_headline, econ_headline, social_headline, human_headline)
    }
    
    if(input$orderButton == "Order by performance") {
      avg_vals <- avg_vals %>% arrange(arrangement) 
    }
    
    nats_ts <- ts_vals %>% filter(cat %in% c("12")) %>% split(f = .$measure)
    hum_ts <- ts_vals %>% filter(cat %in% c("13")) %>% split(f = .$measure)
    soc_ts <- ts_vals %>% filter(cat %in% c("14")) %>% split(f = .$measure)
    econ_ts <- ts_vals %>% filter(cat %in% c("15")) %>% split(f = .$measure)

    nats <- avg_vals %>% filter(cat %in% c("12"))
    hum <- avg_vals %>% filter(cat %in% c("13"))
    soc <- avg_vals %>% filter(cat %in% c("14"))
    econ <- avg_vals %>% filter(cat %in% c("15"))
    
    # CWB
    output$futureWellbeingSummary <- renderUI({ pillBox(avg_vals, fwb_indicator_text_filter) })
    
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
        
        year_text <- paste0("<span style='font-size:12px'>", point_avg[i,]$latest_year, "</span><br><br>")
        
        if(!is.null(ts_avg[[avg_measure]])) {
          plot_output <- areaPlotter(ts_avg[[avg_measure]], country_name)
        } else {
          plot_output <- NULL
        }
        
        output[[paste0(cluster_name, "_cards_", i)]] <- renderUI({ tagList(div(style = "margin-top:5px;margin-left:0px;margin-right:0px;margin-bottom:0px;display:inline-block;", HTML(label_text, value_text, year_text))) })
        
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
      
      if(cluster_name == "nature") {
        
        cluster_html <- HTML(paste("<span class='cluster-header' style='font-size:22px;color:#101d40!important;'>Natural capital in ", countryName(),"</span>
                              <br>
                              Natural capital concerns the state of natural assets (e.g. natural land cover, biodiversity) and ecosystems and their services (e.g. oceans, forests, soil and the atmosphere), as well as risk factors affecting these systems.
                              <br>
                              <br>"))
        
      } else if(cluster_name == "economic") {
        
        cluster_html <- HTML(paste("<br>
                              <span class='cluster-header' style='font-size:22px; color:#101d40!important;'>Economic capital in ", countryName(), "</span>
                              <br>
                              Economic capital consists of produced  and financial capital. Produced capital refers to man-made tangible assets (such as roads and buildings) and  intellectual property. Financial capital includes financial assets and liabilities.
                              <br>
                              <br>"))
        
      } else if(cluster_name == "social") {
        
        cluster_html <- HTML(paste("<br>
                              <span class='cluster-header' style='font-size:22px; color:#101d40!important;'>Social capital in ", countryName(),"</span>
                              <br>
                              Social capital is about the social norms, shared values and institutional arrangements that foster co-operation among population groups.
                              <br>
                              <br>"))
        
      } else if(cluster_name == "human") {
        
        cluster_html <- HTML(paste("<br>
                              <span class='cluster-header' style='font-size:22px; color:#101d40!important;'>Human capital in ", countryName(),"</span>
                              <br>
                              Human capital refers to the skills, competencies (including education and tacit knowledge) and health status of individuals.
                              <br>
                              <br>"))
        
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
                       div(class = paste0(if (i == 1 && cluster_name == "material") "bounce-label" else "bounce-label-off"), 
                           "Click cards to download data"),
                       div(class = "card-front",
                           div(class = "card-top", style="height:40%;z-index:1;",
                               fluidRow(align = "center",
                                        uiOutput(paste0(cluster_name, "_icon_", i)),
                                        uiOutput(paste0(cluster_name, "_cards_", i))
                               )
                           ),
                           div(class = "sparkline-wrap", style="height:60%;z-index:2;",
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
    
    output$nature_boxes <- renderUI({ cardBuilder(nats, nats_ts, "nature") })
    output$human_boxes <- renderUI({ cardBuilder(hum, hum_ts, "human") })
    output$social_boxes <- renderUI({ cardBuilder(soc, soc_ts, "social") })
    output$econ_boxes <- renderUI({ cardBuilder(econ, econ_ts, "economic") })
    
  })
  
  countryName <- eventReactive(input$countrySelector, { 
    
    country_name <- which(country_name_vector == input$countrySelector) %>% names 
    
    if(country_name %in% the_thes) {
      country_name <- paste0("the ", country_name)
    }
    
    if(country_name == "OECD Average") {
      country_name <- "the OECD"
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
