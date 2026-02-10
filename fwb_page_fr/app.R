source("./global.R")

ui <- fluidPage(shinyjs::useShinyjs(),
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")),
                fluidRow(
                  useShinyjs(),
                  tags$head(tags$script(src="bounce.js")),
                  tags$head(tags$script(src="modal.js")),
                  
                  bsModal(id = "modal1",
                          title = "Télécharger les données",
                          trigger = NULL,
                          uiOutput("defText"))
                  
                ),
                
                fluidRow(align = "center",
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
                         fluidRow(
                           column(3),
                           column(3, radioButtons("allToggle", 
                                                  label = "Affichage des indicateurs", 
                                                  choiceNames = list(HTML("Afficher uniquement <span style='text-decoration: underline dotted' class='head-show'>les indicateurs principaux</span><span class='head-hide'>Les indicateurs principaux sont les 12 indicateurs les plus représentatifs du bien-être futur dans la base de données</span>"),
                                                                     "Afficher tous les indicateurs"),
                                                  choiceValues = c(1, 2))),
                           column(3, radioButtons("orderButton", 
                                                  label = "Classer les indicateurs", 
                                                  choices = c("Classer par dimension du bien-être", "Classer par résultat"),
                                                  selected = "Classer par dimension du bien-être")),
                           column(3)
                         )
                ),
                br(),
                fluidRow(align = "center",
                         column(1),
                         column(10,
                                # uiOutput("section_title"),
                                fluidRow(
                                  column(3),
                                  column(6, 
                                         HTML("
                                         Nombre de résultats en matière de bien-être qui se sont améliorés, sont restés globalement stables ou se sont dégradés entre 2015 et la dernière année disponible :
                                         <br>
                                         <br>"),
                                         div(class = "card-grow", uiOutput("futureWellbeingSummary")),
                                         HTML(
                                           "<br>
                                            <span style='color:#CF597E !important;'>●</span> dégradation
                                            <span style='color:goldenrod !important;'>●</span> stabilité
                                            <span style='color:#0F8554 !important;'>●</span> amélioration
                                            <span style='color:#999999 !important;'>●</span> données insuffisantes
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
    # input$countrySelector <- "ARG"
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
    
    if(input$orderButton == "Classer par résultat") {
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
          value_text <- paste0("<br><span style='font-size:24px;'>Absence de données</span><br>")
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
              div(style = "position:absolute;top:7px;left:66px", HTML("<span style='font-size:9px;display:inline-block;line-height:11px;text-align:left;'>Niveau<br>OCDE</span>"))
            )
          }
        })
        
        
      })
      
      if(cluster_name == "nature") {
        
        cluster_html <- HTML(paste("<span class='cluster-header' style='font-size:22px;color:#101d40!important;'>Le capital naturel ", countryName(),"</span>
                              <br>
                              Le capital naturel concerne l’état des actifs naturels (occupation des sols, biodiversité, etc.) et des écosystèmes et services correspondants (océans, forêts, sols, atmosphère, etc.), ainsi que les facteurs de risque affectant ces systèmes.
                              <br>
                              <br>"))
        
      } else if(cluster_name == "economic") {
        
        cluster_html <- HTML(paste("<br>
                              <span class='cluster-header' style='font-size:22px; color:#101d40!important;'>Le capital économique ", countryName(), "</span>
                              <br>
                              Le capital économique se compose du capital produit et du capital financier. Le capital produit désigne les actifs corporels artificiels (comme les routes et les bâtiments) et la propriété intellectuelle. Le capital financier comprend les actifs et passifs financiers.
                              <br>
                              <br>"))
        
      } else if(cluster_name == "human") {
        
        cluster_html <- HTML(paste("<br>
                              <span class='cluster-header' style='font-size:22px; color:#101d40!important;'>Le capital humain ", countryName(),"</span>
                              <br>
                              Le capital humain désigne les aptitudes, les compétences (qui recouvrent à la fois la formation et les connaissances tacites) et l'état de santé des individus.
                              <br>
                              <br>"))
        
      } else if(cluster_name == "social") {
        
        cluster_html <- HTML(paste("<br>
                              <span class='cluster-header' style='font-size:22px; color:#101d40!important;'>Le capital social ", countryName(),"</span>
                              <br>
                              Le capital social se rapporte aux normes sociales, aux valeurs communes et aux modalités institutionnelles qui favorisent la coopération entre les groupes de population.
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
                           "Cliquer sur la fiche pour télécharger les données"),
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
    
    long_df <- openxlsx::read.xlsx("https://github.com/kate-chalmers/data_monitor/raw/refs/heads/main/hows_life_dictionary_fr.xlsx")
    # heatmap_dat <- readRDS("./data/final dataset.RDS")
    
    short_df <- long_df %>% filter(measure == input$clicked_class)
    
    main_title <- paste0("<b style='font-size:1.75rem'>", short_df$label, "</b>")
    
    long_title <- paste0("<i>", short_df$indicator, "</i>")
    
    unit_title <- paste0("<i>", short_df$unit, "</i>")
    
    desc_text <- paste0(short_df$definition)
    
    if(!is.na(short_df$note) & countryName() == "dans la zone OCDE") {
      note_text <- paste0("<b>Note: </b>", short_df$note)
    } else {
      note_text <- ""
    }
    
    indicator_text <- paste0(
      main_title, "<br>",
      # "<b>Intitulé technique : </b>", long_title, "<br>",
      "<b>Unité de mesure : </b>", unit_title, "<br><br>",
      desc_text, "<br><br>",
      note_text
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
  
}

shinyApp(ui = ui, server = server)
