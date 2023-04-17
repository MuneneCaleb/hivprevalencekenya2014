pacman::p_load(shiny,
               shinydashboard,
               leaflet,
               rgdal,
               tidyverse,
               RColorBrewer,
               plotly,
               htmltools,
               readr,
               rio,
               sf)

#import data
hiv_2014 <- import("hiv_2014.csv")
county_shp <- read_sf("County.shp")

#Data cleaning
county_shp <- county_shp %>%
  select(COUNTY, geometry)

hiv_2014 <- hiv_2014 %>%
  select(new_hiv_infections_adults_15, new_hiv_infections_children_0_14,
         art_coverage, hiv_adults, hiv_prevalence_men,hiv_prevalence_women, total_population,
         adults_in_need_of_art,adults_receiving_art,children_in_need_of_art,children_receiving_art,
         aids_related_deaths_15,aids_related_deaths_0_14)
#Bind county shp with hiv 2014

county_shp <- county_shp %>%
  cbind(hiv_2014)

county_shp <- county_shp %>%
  janitor::clean_names()

# Defining UI
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "Mapping Kenya's HIV Prevalence by County: 2014 Statistics", titleWidth = 700,
    tags$li(actionLink("LinkedIn",
                       label = "",
                       icon = icon("linkedin"),
                       onclick = "window.open('https://www.linkedin.com/in/caleb-munene')"),
            class = "dropdown"),
    tags$li(actionLink("GitHub",
                       label = "",
                       icon = icon("github"),
                       onclick = "window.open('https://github.com/MuneneCaleb/MuneneCaleb.git')"),
            class = "dropdown")
  ),

  dashboardSidebar(

    sidebarUserPanel("Caleb Karanja",
                     subtitle = "HRIO|Data Analyst|M&E "
    ),
    sidebarMenu(
      menuItem(
        "Introduction",
        tabName = "intro",
        icon = icon("info")
      ),

      menuItem(
        "Map",
        tabName = "map",
        icon = icon("map")
      ),

      menuItem(
        "Data",
        tabName = "data",
        icon = icon("table")
      ),

      menuItem(
        "About me",
        tabName = "author",
        icon = icon("user")
      )

    )

  ),

  dashboardBody(
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 20px;
                              }
                              '))),
    tabItems(

      tabItem(
        tabName = "intro",


        fluidRow(

          h2(HTML("<strong>Overview.</strong>")),
          h3(tags$p("HIV, or Human Immunodeficiency Virus, is a virus that attacks the immune system of the human body. If left untreated, HIV can lead to acquired immunodeficiency syndrome (AIDS), which can be fatal. HIV is transmitted through the exchange of bodily fluids such as blood, semen, vaginal fluids, and breast milk.

Since the beginning of the HIV epidemic, millions of people have been infected with the virus, with the majority of cases occurring in sub-Saharan Africa. HIV/AIDS has had a devastating impact on individuals, families, and entire communities, and has been responsible for the deaths of over 35 million people worldwide.

The HIV epidemic has also had a significant impact on global health and development, with countries and communities struggling to address the social, economic, and health consequences of the disease. HIV/AIDS has been a major challenge for healthcare providers, researchers, and policymakers, who have worked tirelessly to develop effective prevention, treatment, and care strategies.

Despite the challenges, there have been significant advances in the fight against HIV/AIDS, including the development of antiretroviral therapy (ART) and the scale-up of HIV testing and treatment programs. These interventions have been instrumental in reducing the number of new HIV infections and improving the quality of life for people living with HIV.

However, the HIV epidemic is far from over, and there is still much work to be done to achieve the goal of ending AIDS by 2030. Monitoring and tracking HIV prevalence and incidence rates at the local level is critical for identifying areas where interventions are most needed and for assessing the impact of prevention and treatment programs.

The dashboard on HIV prevalence by county provides a comprehensive view of the HIV epidemic in Kenya, highlighting the geographic distribution of HIV cases and trends over time. The data presented in this dashboard can help inform decision-making and guide the development of effective HIV prevention and treatment strategies..")),

        )
        

      ), 

      tabItem(
        tabName = "map",

        fluidRow(

          valueBoxOutput(
            "total"

          ),

          valueBoxOutput(
            "hivadult"

          ), 

          valueBoxOutput(
            "children"

          ),

          valueBoxOutput(
            "art"

          ),
          valueBoxOutput(
            "aidsdeathschildren"

          ),

          valueBoxOutput(
            "aidsdeathsadults"

          )

        ),
        
    

        fluidRow(
          column(width = 3,
                 selectInput(
                   inputId = "stats",
                   label = "Select Indicator",
                   choices =c(
                     "New HIV infection adults "=1,
                     "New HIV infection children 0-14"=2,
                     "Adults in need of ART"=3,
                     "Children in need of ART"=4,
                     "AIDS related deaths children"=5,
                     "Adults receiving ART"=6,
                     "Children receiving ART"=7
                   ),selected = 1
                   
                 ) 
                 
                 
          ) 
          
          
        ),
        
        fluidRow(
          column(
            width = 6,
          
            leafletOutput("maps",height = 500)
            
          ),
          
          column(width = 3,
                 plotlyOutput("top",height = 500)
          ),
          
          column(width = 3,
                 plotlyOutput("bottom",height = 500)
          )
          
          
          
        )
        
        
        
      ), 
      

      tabItem(
        tabName = "data",

        div(
          h1(strong("Dataset")),
        
        ),
        dataTableOutput("table")
      ),
      tabItem(
        tabName = "author",
        fluidRow(
          br(),

          img(src ="Caleb.jpeg", width = "17%", style = "display: block; margin-left: auto; margin-right: auto;")

        ),

        fluidRow(
          h3(strong("Caleb Karanja"), style = "text-align: center"),
          h4("mkcaleb@outlook.com", style = "text-align: center")
        ),

        hr(),
        fluidRow(column(5, ""),
                 column(
                   3,
                   tags$h3(
                     HTML('&nbsp;'),
                     HTML('&nbsp;'),
                     HTML('&nbsp;'),
                     tags$a(
                       href = 'https://www.linkedin.com/in/caleb-munene',
                       img(
                         src = 'linkn.png',
                         height = "50px"
                       )
                     ),
                     HTML('&nbsp;'),
                     tags$a(href = 'https://github.com/MuneneCaleb/MuneneCaleb.git', img(
                       src = 'github.png',
                       height = "50px"
                     ))
                   )
                 )),


        fluidRow(
          column(2, ""),
          column(
            1,
            h3(icon("briefcase"), style = "text-align: right; line-height: 165%;"),
            br(),
            br(),
            h3(icon("globe"), style = "text-align: right; line-height: 200%"),
            br(),
            h3(icon("heart"), style = "text-align: right; line-height: 170%;")
          ),
          column(
            6,
            h4(
              "At present, I hold the position of Research Assistant at the KU/MOH Micronutrients Embu project 
              and I am actively seeking fresh and stimulating opportunities in the realm of data science.",
              style = "text-align: left; line-height: 150%;"
            ),
            br(),
            h4(
              "Data is rapidly becoming the driving force behind many industries. I specialize in transforming 
              data into actionable insights that facilitate informed decision-making. In my view, the key to future 
              success lies in harnessing the power of data to create innovative products. As a professional, I am 
              passionate about designing aesthetically pleasing, organized, and informative data visualizations.",
              style = "text-align: left; line-height: 150%;"
            ),
            br(),
            h4(
              "I have a strong passion for exceptional music and enjoy embarking on exciting adventures,
              including traveling to various destinations, immersing myself in diverse cultures, and 
              establishing connections with individuals from all walks of life! ",
              style = "text-align: left; line-height: 150%;"
            )

          ),

          column(3, "")



        )

      )
    )

  )


)

#### Define server 
server <- function(input, output,session) {
  
  output$total<-renderValueBox({
    valueBox(
      "41,617,612", "TOTAL POPULATION", icon = icon("users"),
      color = "green"
    )
  })
  
  output$hivadult<-renderValueBox({
    valueBox(
      "1,345,782", "HIV IN ADULTS", icon = icon("users"),
      color = "blue"
    )
  })
  
  output$art<-renderValueBox({
    valueBox(
      "71%", "ART COVERAGE", icon = icon("users"),
      color = "orange"
    )
  })
  
  output$aidsdeathschildren<-renderValueBox({
    valueBox(
      "10,401", "AIDS RELATED DEATHS CHILDREN", icon = icon("users"),
      color = "green"
    )
  })
  
  output$aidsdeathsadults<-renderValueBox({
    valueBox(
      "47,928", "AIDS RELATED DEATHS ADULTS", icon = icon("users"),
      color = "blue"
    )
  })
  
  output$children<-renderValueBox({
    valueBox(
      "141,963", "CHILDREN IN NEED OF ART", icon = icon("users"),
      color = "orange"
    )
  })
  
  
  ##Rendering emap
  output$maps<-renderLeaflet(
    leaflet(county_shp) %>%
      setView(lng=37.9083,lat=0.1769,zoom = 6) %>%
      addPolygons(
        color = ~newadults(new_hiv_infections_adults_15),
        smoothFactor = 0.5,
        weight = 2, opacity = 1.0,
        fillOpacity = 1.0,
        highlightOptions = highlightOptions(
          weight = 1,
          color = "black",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = paste(
          "<strong>county:</strong>",county_shp$county,
          "<br>",
          "<strong>new_hiv_infections_adults_15:</strong>",county_shp$new_hiv_infections_adults_15
          
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                  padding = "3px 8px"), 
                                     textsize = "13px", direction = "auto"),
        
        popup = ~paste(
          "<strong>county:</strong>",county,
          "<br>",
          "<strong>new_hiv_infections_adults_15:</strong>",new_hiv_infections_adults_15
          
        )
        
      ) %>%
      addLegend(title = "new_hiv_infections_adults_15",
                pal = newadults, values = county_shp$new_hiv_infections_adults_15, opacity = 1)
    
  )
  
  
  
  #color functions
  #new adults hiv
  newadults <-colorBin("YlOrRd", county_shp$new_hiv_infections_adults_15)
  
  #new children hiv
  newchildren <-colorBin("YlOrRd", county_shp$new_hiv_infections_children_0_14)
  
  #Adult in need of art
  adultnart<-colorBin("YlOrRd", county_shp$adults_in_need_of_art)
  
  #children in need of art
  childrennart<-colorBin("YlOrRd", county_shp$children_in_need_of_art)
  
  #deaths  adults
  deathsadults<-colorBin("YlOrRd", county_shp$aids_related_deaths_15)
  
  #children receiving art
  Childart<-colorBin("YlOrRd", county_shp$children_receiving_art)
  
  #Adults receiving art
  adultrart<-colorBin("YlOrRd", county_shp$adults_receiving_art)
  
  
  observe({
    proxy<-leafletProxy("maps") %>% clearControls()
    if ("1" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color = ~newadults(new_hiv_infections_adults_15),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>county:</strong>",county_shp$county,
            "<br>",
            "<strong>New HIV infections adults:</strong>",county_shp$new_hiv_infections_adults_15
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          
          popup = ~paste(
            "<strong>county:</strong>",county,
            "<br>",
            "<strong>New HIV infections adults:</strong>",new_hiv_infections_adults_15
            
          )
          
        ) %>%
        addLegend(title = "New HIV infections adults",
                  pal = newadults, values = county_shp$new_hiv_infections_adults_15, opacity = 1)
    }
    
    else if ("2" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color =  ~newchildren(new_hiv_infections_children_0_14),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>county:</strong>",county_shp$county,
            "<br>",
            "<strong>new_hiv_infections_children_0_14:</strong>",county_shp$new_hiv_infections_children_0_14
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>county:</strong>",county,
            "<br>",
            "<strong>new_hiv_infections_children_0_14:</strong>",new_hiv_infections_children_0_14
            
          )
          
        ) %>%
        addLegend(title = "New hiv infections children 0_14",
                  pal = newchildren, values = county_shp$new_hiv_infections_children_0_14, opacity = 1)
    }
    
    
    else if ("3" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color =  ~adultnart(adults_in_need_of_art),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>county:</strong>",county_shp$adults_in_need_of_art,
            "<br>",
            "<strong>Adults in need of ART:</strong>",county_shp$adults_in_need_of_art
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>county:</strong>",county,
            "<br>",
            "<strong>Adults in need of ART:</strong>",adults_in_need_of_art
            
          )
          
        ) %>%
        addLegend(title = "Adults in need of ART",
                  pal = adultnart, values = county_shp$adults_in_need_of_art, opacity = 1)
    }
    
    
    else if ("4" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color =  ~childrennart(children_in_need_of_art),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>county:</strong>",county_shp$county,
            "<br>",
            "<strong>Children in need of ART:</strong>",county_shp$children_in_need_of_art
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>county:</strong>",county,
            "<br>",
            "<strong>Children in need of ART:</strong>",children_in_need_of_art
            
          )
          
        ) %>%
        addLegend(title = "Children in need of ART",
                  pal = childrennart, values = county_shp$children_in_need_of_art, opacity = 1)
    }
    
    
    else  if ("5" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color =  ~deathsadults(aids_related_deaths_0_14),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp$county,
            "<br>",
            "<strong>Intersex Population:</strong>",county_shp$aids_related_deaths_0_14
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>county:</strong>",county,
            "<br>",
            "<strong>AIDS related deaths children:</strong>",aids_related_deaths_0_14
            
          )
          
        ) %>%
        addLegend(title = "AIDS related deaths children",
                  pal = deathsadults, values = county_shp$aids_related_deaths_0_14, opacity = 1)
    }
    
    
    else if ("6" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color =  ~adultrart(adults_receiving_art),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp$county,
            "<br>",
            "<strong>Adults receiving art:</strong>",county_shp$adults_receiving_art
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",county,
            "<br>",
            "<strong>Adults receiving art:</strong>",adults_receiving_art
            
          )
          
        ) %>%
        addLegend(title = "Adults receiving art",
                  pal = adultrart, values = county_shp$adults_receiving_art, opacity = 1)
    }
    
    
    
    else if ("7" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color =  ~Childart(children_receiving_art),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp$county,
            "<br>",
            "<strong>Average household size:</strong>",county_shp$children_receiving_art
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>county:</strong>",county,
            "<br>",
            "<strong>:</strong>",children_receiving_art
            
          )
          
        ) %>%
        addLegend(title = "Children receiving art",
                  pal = Childart, values = county_shp$children_receiving_art, opacity = 1)
    }
    
  })
  
  
  
  
  output$top<-renderPlotly({
    if("1" %in% input$stats){
      county_shp %>% select(county,new_hiv_infections_adults_15) %>%
        arrange(desc(new_hiv_infections_adults_15)) %>%
        head(5) %>% ggplot(aes(x=reorder(county,new_hiv_infections_adults_15),y= new_hiv_infections_adults_15))+
        geom_col(fill="red")+
        geom_text(aes(label=new_hiv_infections_adults_15))+
        labs(
          title = "Top 5 counties",
          y="HIV infection adults",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("2" %in% input$stats){
      county_shp %>% select(county,new_hiv_infections_children_0_14) %>%
        arrange(desc(new_hiv_infections_children_0_14)) %>%
        head(5) %>% ggplot(aes(x=reorder(county,new_hiv_infections_children_0_14),y=new_hiv_infections_children_0_14))+
        geom_col(fill="red")+
        geom_text(aes(label=new_hiv_infections_children_0_14))+
        labs(
          title = "Top 5 counties",
          y="HIV infections children",
          x="County"
        )+
        coord_flip()
    }
    
    else if("3" %in% input$stats){
      county_shp %>% select(county,adults_in_need_of_art) %>%
        arrange(desc(adults_in_need_of_art)) %>%
        head(5) %>% ggplot(aes(x=reorder(county,adults_in_need_of_art),y=adults_in_need_of_art))+
        geom_col(fill="red")+
        geom_text(aes(label=adults_in_need_of_art))+
        labs(
          title = "Top 5 counties",
          y="adults in need of ART",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("4" %in% input$stats){
      county_shp %>% select(county,children_in_need_of_art) %>%
        arrange(desc(children_in_need_of_art)) %>%
        head(5) %>% ggplot(aes(x=reorder(county,children_in_need_of_art),y=children_in_need_of_art))+
        geom_col(fill="red")+
        geom_text(aes(label=children_in_need_of_art))+
        labs(
          title = "Top 5 counties ",
          y="children in need of ART",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("5" %in% input$stats){
      county_shp %>% select(county,aids_related_deaths_0_14) %>%
        arrange(desc(aids_related_deaths_0_14)) %>%
        head(5) %>% ggplot(aes(x=reorder(county,aids_related_deaths_0_14),y=aids_related_deaths_0_14))+
        geom_col(fill="red")+
        geom_text(aes(label=aids_related_deaths_0_14 ))+
        labs(
          title = "Top 5 counties",
          y="AIDS related deaths children ",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("6" %in% input$stats){
      county_shp %>% select(county,adults_receiving_art) %>%
        arrange(desc(adults_receiving_art)) %>%
        head(5) %>% ggplot(aes(x=reorder(county,adults_receiving_art),y=adults_receiving_art))+
        geom_col(fill="red")+
        geom_text(aes(label=adults_receiving_art))+
        labs(
          title = "Top 5 counties",
          y="adults_receiving_art",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("7" %in% input$stats){
      county_shp %>% select(county,children_receiving_art) %>%
        arrange(desc(children_receiving_art)) %>%
        head(5) %>% ggplot(aes(x=reorder(county,children_receiving_art),y=children_receiving_art))+
        geom_col(fill="red")+
        geom_text(aes(label=children_receiving_art))+
        labs(
          title = "Top 5 counties",
          y="Children receiving ART",
          x="County"
        )+
        coord_flip()
      
    }  
    
    
  })
  
  
  output$bottom<-renderPlotly({
    if("1" %in% input$stats){
      county_shp %>% select(county,new_hiv_infections_adults_15) %>%
        arrange(desc(new_hiv_infections_adults_15)) %>%
        tail(5) %>% ggplot(aes(x=reorder(county,new_hiv_infections_adults_15),y=new_hiv_infections_adults_15))+
        geom_col(fill="orange")+
        geom_text(aes(label= new_hiv_infections_adults_15))+
        labs(
          title = "Bottom 5 counties  ",
          y="HIV infections Adults",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("2" %in% input$stats){
      county_shp %>% select(county,new_hiv_infections_children_0_14) %>%
        arrange(desc(new_hiv_infections_children_0_14)) %>%
        tail(5) %>% ggplot(aes(x=reorder(county,new_hiv_infections_children_0_14),y=new_hiv_infections_children_0_14))+
        geom_col(fill="orange")+
        geom_text(aes(label=new_hiv_infections_children_0_14))+
        labs(
          title = "Bottom 5 counties",
          y="HIV infection children",
          x="County"
        )+
        coord_flip()
    }
    
    else if("3" %in% input$stats){
      county_shp %>% select(county,adults_in_need_of_art) %>%
        arrange(desc(adults_in_need_of_art)) %>%
        tail(5) %>% ggplot(aes(x=reorder(county,adults_in_need_of_art),y= adults_in_need_of_art))+
        geom_col(fill="orange")+
        geom_text(aes(label= adults_in_need_of_art))+
        labs(
          title = "Bottom 5 counties",
          y="adults need ART",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("4" %in% input$stats){
      county_shp %>% select(county,children_in_need_of_art) %>%
        arrange(desc(children_in_need_of_art)) %>%
        tail(5) %>% ggplot(aes(x=reorder(county,children_in_need_of_art),y=children_in_need_of_art))+
        geom_col(fill="orange")+
        geom_text(aes(label=children_in_need_of_art))+
        labs(
          title ="Bottom 5 counties",
          y="Children in need of ART",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("5" %in% input$stats){
      county_shp %>% select(county,aids_related_deaths_0_14) %>%
        arrange(desc(aids_related_deaths_0_14)) %>%
        tail(5) %>% ggplot(aes(x=reorder(county,aids_related_deaths_0_14),y=aids_related_deaths_0_14))+
        geom_col(fill="orange")+
        geom_text(aes(label=aids_related_deaths_0_14))+
        labs(
          title = "Bottom 5 counties",
          y="AIDS related deaths children",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("6" %in% input$stats){
      county_shp %>% select(county,adults_receiving_art) %>%
        arrange(desc(adults_receiving_art)) %>%
        tail(5) %>% ggplot(aes(x=reorder(county,adults_receiving_art),y=adults_receiving_art))+
        geom_col(fill="orange")+
        geom_text(aes(label=adults_receiving_art))+
        labs(
          title = "Bottom 5 counties",
          y="adults_receiving_art",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("7" %in% input$stats){
      county_shp %>% select(county,children_receiving_art) %>%
        arrange(desc(children_receiving_art)) %>%
        tail(5) %>% ggplot(aes(x=reorder(county,children_receiving_art),y=children_receiving_art))+
        geom_col(fill="orange")+
        geom_text(aes(label=children_receiving_art))+
        labs(
          title = "Bottom 5 counties",
          y="Children receiving ART",
          x="County"
        )+
        coord_flip()
      
    }  
    
  })
  
  
  
  output$table<-renderDataTable({
    datatable( county_shp,
               class = 'cell-border stripe',
               editable = TRUE,
               options = list(scrollX = T)
    ) 
  }
  )
  
}

#### Run the application 
shinyApp(ui = ui, server = server)
