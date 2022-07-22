library(shiny)
library(knitr)
library(tidyverse)
library(DT)
library(readxl)
library(viridis)
library(leaflet)
library(tigris)
library(plotly)
library(shinyWidgets)
library(table1)
library(htmlTable)
library(scales)
library(flexdashboard)
library(RColorBrewer)
library(grid)
library(Dict)
library(lubridate)
library(zipcodeR)
library(htmltools)
library(fst)
### URL

url1 = "https://twitter.com/intent/tweet?text=Hello%20world&url=https://msph.shinyapps.io/drinking-water-dashboard/"
url2 = "https://www.facebook.com/sharer.php?u=https://msph.shinyapps.io/drinking-water-dashboard/"
url3 = "https://www.instagram.com/columbiapublichealth/"
url4 = "https://www.linkedin.com/shareArticle?mini=true&url=https://msph.shinyapps.io/drinking-water-dashboard/&title=&summary=&source="
url5 = "mailto:aen2136@cumc.columbia.edu?&subject=&body=https://msph.shinyapps.io/drinking-water-dashboard/"
url6 = "whatsapp://send?text=https://msph.shinyapps.io/drinking-water-dashboard/"
url7 = "https://service.weibo.com/share/share.php?url=https://msph.shinyapps.io/drinking-water-dashboard/&title="

### DATA import
leafletSizingPolicy <- function(
    defaultWidth = "100%",
    defaultHeight = 400,
    padding = 0,
    browser.fill = TRUE,
    ...
    # not adding extra arguments as htmlwidgets::sizingPolicy can change their own args
) {
    htmlwidgets::sizingPolicy(
        defaultWidth = defaultWidth,
        defaultHeight = defaultHeight,
        padding = padding,
        browser.fill = browser.fill,
        ...
    )
}



c_12 = function(name, nnn1){
    d1 = strsplit(name, 2)[[1]][1]
    m = nnn1%>% as.matrix() %>% as.numeric()
    if(d1 == "Arsenic."){
        m[m >= 10] = 10
    }
    
    #Sb
    if(d1 == "Antimony."){
        m[m >= 6] = 6
    }
    
    #Ba
    if(d1 == "Barium."){
        m[m >= 200] = 200
    }
    
    #Be
    if(d1 == "Beryllium."){
        m[m >= 4] = 4
    }
    
    #Cd
    if(d1 == "Cadmium."){
        m[m >= 5] = 5
    }
    
    #Cr
    if(d1 == "Chromium."){
        m[m >= 100] = 100
    }
    
    #Hg
    if(d1 == "Mercury."){
        m[m >= 2] = 2
    }
    
    #Se
    if(d1 == "Selenium."){
        m[m >= 50] = 50
    }
    
    #Ti
    if(d1 == "Thallium."){
        m[m >= 2] = 2
    }
    
    #CN
    if(d1 == "Cyanide."){
        m[m >= 200] = 200
    }
    
    #U
    if(d1 == "Uranium."){
        m[m >= 30] = 30
    }
    
    return(m)
    
}







### Metal

load("data.RData")
cdata_1 = read.fst("cdata_1.fst")
icons <- makeIcon("ico2.png", iconWidth = 24, iconHeight = 24)


### DBPS
dbp_c1 = read.fst("dbp_c1.fst")
load("dbp_data.RData")



r_name = function(nname){
    nn1 = strsplit(nname, ",")[[1]][1]
    nn2 = strsplit(nname, ",")[[1]][2]
    
    nn3 = strsplit(nn2, "-")[[1]][1]
    nn4 = strsplit(nn2, "-")[[1]][2]
    
    r_nn = paste(nn1, nn3, nn4, sep = ".")
    return(r_nn)
}

pc = function(df1,nnnz, df2){
    
    nnn = r_name(nnnz)
    
    nnn1 = df1@data %>% pull(nnn)
    
    nnn2 = c_12(nnn, nnn1)
    m = leaflet(df1) %>%
        setView(-96, 37.8, 5) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
            id = "mapbox.light",
            accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) #%>% addPolygons()
    
    
    pal <- colorNumeric(c("#f0f9e8","#fed98e","#fe9929","#d95f0e","#993404"), domain= nnn2%>% as.numeric())
    
    
    popup_sb <- paste0("<b> State: </b>", df1$State.Code,
                       "<br>",
                       "<b> County: </b>",df1$GEOID," " , df1$NAME_2,
                       "<br>", 
                       "<b> Metal Concentration: </b>",nnn1, "μg/L")
    
    popup_sb2 <- paste0(
        "PWSID:", df2$PWSID,
        " ,", df2$PWSID.Name,
        " ,Concentration:",df2 %>% pull(nnn), "μg/L")
    
    
    m %>% addPolygons(
        fillColor = ~pal(nnn2 %>% as.numeric()),
        weight = 0.2,
        opacity = 1,
        color = "black",
        #dashArray = "3",
        fillOpacity = 0.7,
        popup = ~popup_sb)  %>%
        addLegend(pal = pal, 
                  values =  nnn2 %>% as.numeric(), 
                  position = "bottomright", 
                  title = "Concentration",
                  labFormat = labelFormat(suffix = "+ μg/L"),
                  na.label = "NA")%>%
        addMarkers( 
            clusterOptions = markerClusterOptions(),
            clusterId = "quakesCluster",
            ~df2$lng, ~df2$lat,
            icon = icons,
            #~as.character(nn1),
            #color = "red",
            label = ~popup_sb2,
            labelOptions = labelOptions(
                style = list("font-size" = "12px")
            )#,
            #fillOpacity = 0.005,
            #radius = 0.000001
        )  %>% 
        #addMiniMap() %>%
        addEasyButton(easyButton(
            states = list(
                easyButtonState(
                    stateName="unfrozen-markers",
                    icon="ion-toggle",
                    title="Freeze Clusters",
                    onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
                ),
                easyButtonState(
                    stateName="frozen-markers",
                    icon="ion-toggle-filled",
                    title="UnFreeze Clusters",
                    onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
                )
            )
        ))
}


c_22 = function(name, nnn1){
    d1 = strsplit(name, 2)[[1]][1]
    m = nnn1%>% as.matrix() %>% as.numeric()
    if(d1 == "bromate,"){
        m[m >= 10] = 10
    }
    
    #Sb
    if(d1 == "chlorite,"){
        m[m >= 1000] = 1000
    }
    
    #Ba
    if(d1 == "chloroform,"){
        m[m >= 80] = 80
    }
    
    #Be
    if(d1 == "bromodichloromethane,"){
        m[m >= 80] = 80
    }
    
    #Cd
    if(d1 == "dibromochloromethane,"){
        m[m >= 80] = 80
    }
    
    #Cr
    if(d1 == "bromoform,"){
        m[m >= 80] = 80
    }
    
    #Hg
    if(d1 == "monochloroacetic_acid,"){
        m[m >= 60] = 60
    }
    
    #Se
    if(d1 == "dichloroacetic_acid,"){
        m[m >= 60] = 60
    }
    
    #Ti
    if(d1 == "trichloroacetic_acid,"){
        m[m >= 60] = 60
    }
    
    #CN
    if(d1 == "monobromoacetic_acid,"){
        m[m >= 60] = 60
    }
    
    #U
    if(d1 == "dibromoacetic_acid,"){
        m[m >= 60] = 60
    }
    
    #U
    if(d1 == "TTHM,"){
        m[m >= 80] = 80
    }
    
    #U
    if(d1 == "HAA5,"){
        m[m >= 60] = 60
    }
    
    return(m)
    
}



pc2 = function(df1,nnnz, df2){
    
    nnn = r_name(nnnz)
    
    nnn1 = df1@data %>% pull(nnn)
    
    nnn2 = c_22(nnn, nnn1)
    m = leaflet(df1) %>%
        setView(-96, 37.8, 5) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
            id = "mapbox.light",
            accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) #%>% addPolygons()
    
    
    pal <- colorNumeric(c("#f0f9e8","#fed98e","#fe9929","#d95f0e","#993404"), domain= nnn2%>% as.numeric())
    
    
    popup_sb <- paste0("<b> State: </b>", df1$State.Code,
                       "<br>",
                       "<b> County: </b>",df1$GEOID," " , df1$NAME,
                       "<br>", 
                       "<b> DBPs Concentration: </b>",nnn1, "μg/L")
    
    popup_sb2 <- paste0(
        "PWSID:", df2$PWSID,
        " ,", df2$PWSID.Name,
        " ,Concentration:",df2 %>% pull(nnn), "μg/L")
    
    
    m %>% addPolygons(
        fillColor = ~pal(nnn2 %>% as.numeric()),
        weight = 0.2,
        opacity = 1,
        color = "black",

        fillOpacity = 0.7,
        popup = ~popup_sb)  %>%
        addLegend(pal = pal, 
                  values =  nnn2 %>% as.numeric(), 
                  position = "bottomright", 
                  title = "Concentration",
                  labFormat = labelFormat(suffix = "+ μg/L"),
                  na.label = "NA")%>%
        addMarkers( 
            clusterOptions = markerClusterOptions(),
            clusterId = "quakesCluster",
            ~df2$lng, ~df2$lat,
            icon = icons,

            label = ~popup_sb2,
            labelOptions = labelOptions(
                style = list("font-size" = "12px")
            )

        )  %>% 

        addEasyButton(easyButton(
            states = list(
                easyButtonState(
                    stateName="unfrozen-markers",
                    icon="ion-toggle",
                    title="Freeze Clusters",
                    onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
                ),
                easyButtonState(
                    stateName="frozen-markers",
                    icon="ion-toggle-filled",
                    title="UnFreeze Clusters",
                    onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
                )
            )
        ))
}

### UI

ui <- navbarPage(

    title = div(img(src='logo.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
    windowTitle = " Columbia University Drinking Water Dashboard",
    id = 'menus',
    tabPanel('Home',
             shinyjs::useShinyjs(),

               br(),
               fluidRow(column(width = 10, offset = 1, span(strong("Columbia University Drinking Water Dashboard"), style="font-size: 35px;line-height:150%")),
               column(width = 10, offset = 1, h2(""))),

             fluidRow(column(width = 8, offset = 1, span(htmlOutput("Ht1"), style="font-size: 15px;line-height:150%"))),
             br(),
             
             fluidRow(column(8, offset = 1, div(img(src = "home_plot.png", height = "35%",width = "80%"),
                                       style="text-align: center;")),
                      column(width = 10, offset = 1, h2(""))
                      ),
             
             fluidRow(column(width = 10, offset = 1, 
                             span(strong("How to use the Dashboard"), style="font-size: 25px;line-height:150%"))), 
             br(),

             fluidRow(column(width = 8, offset = 1, span(htmlOutput("Ht2"), style="font-size: 15px;line-height:150%"))),
             br(),
             fluidRow(column(width = 8, offset = 1, span(htmlOutput("Ht3"), style="font-size: 15px;line-height:150%"))),
             br(),
             fluidRow(column(width = 8, offset = 1, span(htmlOutput("Ht4"), style="font-size: 15px;line-height:150%"))),
             br(),
             fluidRow(column(width = 8, offset = 1, span(htmlOutput("Ht5"), style="font-size: 15px;line-height:150%"))),
             br(),
             fluidRow(column(width = 8, offset = 1, span(htmlOutput("Ht6"), style="font-size: 15px;line-height:150%"))),
             br(),
             
             fluidRow(column(width = 10, offset = 1, 
                             span(strong("Methods"), style="font-size: 25px;line-height:150%"))), 
             
             br(),
             
             fluidRow(column(width = 8, offset = 1, span(htmlOutput("Ht7"), style="font-size: 15px;line-height:150%"))),
             br(),
             fluidRow(column(width = 8, offset = 1, span(htmlOutput("Ht8"), style="font-size: 15px;line-height:150%"))),
             br(),
             fluidRow(column(width = 8, offset = 1, span(htmlOutput("Ht9"), style="font-size: 15px;line-height:150%"))),
             fluidRow(column(width = 8, offset = 1, span(htmlOutput("Ht10"), style="font-size: 15px;line-height:150%"))),
             br(),
             
             
             
             fluidRow(align="center",
                      span(htmlOutput("bannertext", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),

                      h5("Share on", style="color:white;font-size:12px"),
                      actionButton("twitter_index",
                                   label = "",
                                   icon = icon("twitter"),
                                   onclick = sprintf("window.open('%s')", url1),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      actionButton("fb_index",
                                   label = "",
                                   icon = icon("facebook"),
                                   onclick = sprintf("window.open('%s')", url2),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),

                      actionButton("linkedin_index",
                                   label = "",
                                   icon = icon("linkedin"),
                                   onclick = sprintf("window.open('%s')", url4),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      actionButton("whats_index",
                                   label = "",
                                   icon = icon("whatsapp"),
                                   onclick = sprintf("window.open('%s')", url6),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      actionButton("email_index",
                                   label = "",
                                   icon = icon("envelope"),
                                   onclick = sprintf("window.open('%s')", url5),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                      
             )
             
    ),
    tabPanel('Inorganics/Radionuclides',
             shinyjs::useShinyjs(),
             fluidRow(
                 br(),
                 fluidRow(column(width = 10, offset = 1, span(strong("Inorganic chemicals and Radionuclides"), style="font-size: 30px;line-height:150%")))),
             fluidRow(
                 column(width = 10, offset = 1, h2(""))),
             fluidRow(
                 column(4, offset = 1,
                        column(12, pickerInput(inputId = "p1_m", 
                                               label = "Choose Contaminant & Year Range:", 
                                               choices = metals,
                                               selected = metals[1])))),
             br(),
             fluidRow(column(width = 8, offset = 1,leafletOutput(outputId = "p1",width="120%",height="720px"))),
             br(),
             fluidRow(column(width = 8, offset = 1, span(htmlOutput("icr"), style="font-size: 15px;line-height:150%"))),
             br(),

             fluidRow(
               column(width = 10, offset = 1, span(strong("County Level Estimates"), style="font-size: 23px;line-height:150%")),
               column(width = 10, offset = 1, h4("Tracking inequalities in public water exposures in counties across the United States")),
               column(2, offset = 1,
                      selectInput("m_t2",
                                  "Contaminant:",
                                  c(unique(as.character(cdata_1$Metal)))),
                      selectInput("yr_t2",
                                  "Year Range:",
                                  c(unique(as.character(cdata_1$`Year Range`))))),
               
               column(2, offset = 1,
                      selectInput("cf_t2",
                                  "County FIPS:",
                                  c("All",
                                    unique(as.character(cdata_1$`County FIPS`)))),
                      selectInput("cn_t1",
                                  "County Name:",
                                  c("All",
                                    unique(as.character(cdata_1$`County Name`))))),
               column(2, offset = 1,
                      selectInput("sc_t2",
                                  "State Code:",
                                  c("All",
                                    unique(as.character(cdata_1$`State Code`))))),
               # Create a new row for the table.
               column(width = 10, offset = 1, align="center", DT::dataTableOutput("t_2"))
             ),
             br(),
             fluidRow(column(width = 8, offset = 1, span(htmlOutput("t2text2"), style="font-size: 15px;line-height:150%"))),
             br(),
             fluidRow(column(width = 8, offset = 1, span(htmlOutput("t2text"), style="font-size: 15px;line-height:150%"))),
             br(),                        
             
             fluidRow(align="center",
                      span(htmlOutput("bannertext1", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),

                      h5("Share on", style="color:white;font-size:12px"),
                      actionButton("twitter_index",
                                   label = "",
                                   icon = icon("twitter"),
                                   onclick = sprintf("window.open('%s')", url1),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      actionButton("fb_index",
                                   label = "",
                                   icon = icon("facebook"),
                                   onclick = sprintf("window.open('%s')", url2),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),

                      actionButton("linkedin_index",
                                   label = "",
                                   icon = icon("linkedin"),
                                   onclick = sprintf("window.open('%s')", url4),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      actionButton("whats_index",
                                   label = "",
                                   icon = icon("whatsapp"),
                                   onclick = sprintf("window.open('%s')", url6),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      actionButton("email_index",
                                   label = "",
                                   icon = icon("envelope"),
                                   onclick = sprintf("window.open('%s')", url5),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                      
             )
             
    ),
    
    
    tabPanel(
        # Application title
        title= "Microorganisms/Disinfectants/DBPs",
        value = "DBPs",
        
        br(),
        fluidRow(column(width = 10, offset = 1, span(strong("Microorganisms, Disinfectants, and Disinfection Byproducts"), style="font-size: 30px;line-height:150%"))),           

        fluidRow(
          column(width = 10, offset = 1, h2(""))),
        fluidRow(
            column(4, offset = 1,
                   column(12, pickerInput(inputId = "p2_m", 
                                          label = "Choose Contaminant & Year Range:", 
                                          choices = dbp_nn,
                                          selected = dbp_nn[1]
                   )))
            
        ),
        br(),
        fluidRow(column(width = 8, offset = 1,leafletOutput(outputId = "p2",width="120%",height="720px"))),
        br(),
        fluidRow(column(width = 8, offset = 1, span(htmlOutput("MDB1"), style="font-size: 15px;line-height:150%"))),
        br(),
        fluidRow(column(width = 8, offset = 1, span(htmlOutput("MDB2"), style="font-size: 15px;line-height:150%"))),
        br(),
        fluidRow(
          column(width = 10, offset = 1, span(strong("County Level Estimates"), style="font-size: 23px;line-height:150%")),
          column(width = 10, offset = 1, h4("Tracking inequalities in public water exposures in counties across the United States")),
          column(2, offset = 1,
                 selectInput("m_t4",
                             "Contaminant:",
                             c(unique(as.character(dbp_c1$Contaminants)))),
                 selectInput("yr_t4",
                             "Year Range:",
                             c(unique(as.character(dbp_c1$`Year Range`))))),
          #column(2, offset = 1,
          #       selectInput("yr_t1",
          #                   "Year Range:",
          #                  c("All",
          #                    unique(as.character(cws_1$`Year Range`))))),
          column(2, offset = 1,
                 selectInput("cf_t4",
                             "County FIPS:",
                             c("All",
                               unique(as.character(dbp_c1$`County FIPS`)))),
                 selectInput("cn_t4",
                             "County Name:",
                             c("All",
                               unique(as.character(dbp_c1$`County Name`))))),
          column(2, offset = 1,
                 selectInput("sc_t4",
                             "State Code:",
                             c("All",
                               unique(as.character(dbp_c1$`State Code`))))),
          # Create a new row for the table.
          column(width = 10, offset = 1, align="center", DT::dataTableOutput("t_4"))
        ),
        br(),
        #fluidRow(column(width = 8, offset = 1, span(htmlOutput("t2text2"), style="font-size: 15px;line-height:150%"))),
        br(),
        #fluidRow(column(width = 8, offset = 1, span(htmlOutput("t2text"), style="font-size: 15px;line-height:150%"))),
        br(), 
        fluidRow(align="center",
                 span(htmlOutput("bannertext2", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),

                 h5("Share on", style="color:white;font-size:12px"),
                 actionButton("twitter_index",
                              label = "",
                              icon = icon("twitter"),
                              onclick = sprintf("window.open('%s')", url1),
                              style = "border-color: #225091;color: #fff; background-color: #225091;"),
                 actionButton("fb_index",
                              label = "",
                              icon = icon("facebook"),
                              onclick = sprintf("window.open('%s')", url2),
                              style = "border-color: #225091;color: #fff; background-color: #225091;"),

                 actionButton("linkedin_index",
                              label = "",
                              icon = icon("linkedin"),
                              onclick = sprintf("window.open('%s')", url4),
                              style = "border-color: #225091;color: #fff; background-color: #225091;"),
                 actionButton("whats_index",
                              label = "",
                              icon = icon("whatsapp"),
                              onclick = sprintf("window.open('%s')", url6),
                              style = "border-color: #225091;color: #fff; background-color: #225091;"),
                 actionButton("email_index",
                              label = "",
                              icon = icon("envelope"),
                              onclick = sprintf("window.open('%s')", url5),
                              style = "border-color: #225091;color: #fff; background-color: #225091;"),
                 style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                 
        )
    ),
    
    tabPanel(
      # Application title
      title= "Organics",
      value = "Organics",
      
      br(),
      fluidRow(column(width = 10, offset = 1, span(strong("Organics"), style="font-size: 30px;line-height:150%"))),           
      
      #fluidRow(column(width = 10, offset = 1, h2(""))),
      #fluidRow(
      #  column(4, offset = 1,
      #         column(12, pickerInput(inputId = "p2_m", 
      #                                label = "Choose Contaminant & Year Range:", 
      #                                choices = dbp_nn,
      #                                selected = dbp_nn[1]
      #         )))
        
      #),
     # br(),
      #fluidRow(column(width = 8, offset = 1,leafletOutput(outputId = "p2",width="120%",height="720px"))),
      #br(),
      fluidRow(column(width = 8, offset = 1, span(htmlOutput("org1"), style="font-size: 15px;line-height:150%"))),
      br(),
      fluidRow(column(width = 8, offset = 1, span(htmlOutput("org2"), style="font-size: 15px;line-height:150%"))),
      br(),
      
      fluidRow(align="center",
               span(htmlOutput("bannertext3", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
               
               h5("Share on", style="color:white;font-size:12px"),
               actionButton("twitter_index",
                            label = "",
                            icon = icon("twitter"),
                            onclick = sprintf("window.open('%s')", url1),
                            style = "border-color: #225091;color: #fff; background-color: #225091;"),
               actionButton("fb_index",
                            label = "",
                            icon = icon("facebook"),
                            onclick = sprintf("window.open('%s')", url2),
                            style = "border-color: #225091;color: #fff; background-color: #225091;"),
               
               actionButton("linkedin_index",
                            label = "",
                            icon = icon("linkedin"),
                            onclick = sprintf("window.open('%s')", url4),
                            style = "border-color: #225091;color: #fff; background-color: #225091;"),
               actionButton("whats_index",
                            label = "",
                            icon = icon("whatsapp"),
                            onclick = sprintf("window.open('%s')", url6),
                            style = "border-color: #225091;color: #fff; background-color: #225091;"),
               actionButton("email_index",
                            label = "",
                            icon = icon("envelope"),
                            onclick = sprintf("window.open('%s')", url5),
                            style = "border-color: #225091;color: #fff; background-color: #225091;"),
               style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
               
      )
    ),
    
    
    
    
    tabPanel("About Us",
             fluidRow(column(10, offset = 1, h2(strong("About Us"))),
                      column(8, offset = 1,span(uiOutput("ab1",style = "font-size: 17px; line-height:150%"))),
                      column(8, offset = 1,span(uiOutput("ab2",style = "font-size: 17px; line-height:150%"))),
                      column(8, offset = 1,span(uiOutput("ab3",style = "font-size: 17px; line-height:150%"))),
                      column(8, offset = 1, div(img(src = "cc.jpg", height = "40%",width = "80%"),
                                                style="text-align: center;"))
             ),
             br(),
             fluidRow(align="center",
                      span(htmlOutput("bannertext4", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),

                      h5("Share on", style="color:white;font-size:12px"),
                      actionButton("twitter_index",
                                   label = "",
                                   icon = icon("twitter"),
                                   onclick = sprintf("window.open('%s')", url1),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      actionButton("fb_index",
                                   label = "",
                                   icon = icon("facebook"),
                                   onclick = sprintf("window.open('%s')", url2),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),

                      actionButton("linkedin_index",
                                   label = "",
                                   icon = icon("linkedin"),
                                   onclick = sprintf("window.open('%s')", url4),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      actionButton("whats_index",
                                   label = "",
                                   icon = icon("whatsapp"),
                                   onclick = sprintf("window.open('%s')", url6),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      actionButton("email_index",
                                   label = "",
                                   icon = icon("envelope"),
                                   onclick = sprintf("window.open('%s')", url5),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                      
             ))
    
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    shinyjs::addClass(id = "menus", class = "navbar-right")
    
    output$bannertext = renderText({
        return(
            
            "<b> Columbia <b> University <b> Drinking <b> Water <b> Dashboard"
        )
    })
    
    output$bannertext1 = renderText({
        return(
            "<b> Columbia <b> University <b> Drinking <b> Water <b> Dashboard"
        )
    })
    
    output$bannertext2 = renderText({
      return(
        "<b> Columbia <b> University <b> Drinking <b> Water <b> Dashboard"
      )
    })
    

    
    output$bannertext3 = renderText({
        return(
            "<b> Columbia <b> University <b> Drinking <b> Water <b> Dashboard"
        )
    })
    
    
    
    output$bannertext4 = renderText({
      return(
        "<b> Columbia <b> University <b> Drinking <b> Water <b> Dashboard"
      )
    })
    
    
    output$sharetext = renderText({
        return(
            "<b> Share on </b> "
        )
    })
    
    
    
    output$Ht1 = renderUI({
        syr = a("Six Year Review of Drinking Water Standards.", href = "https://www.epa.gov/dwsixyearreview", target="_blank")
        ra2022 = a("Ravalli et al. 2022", href = "https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(22)00043-2/fulltext", target="_blank")
        rn2020 = a("Nigra et al. 2020", href = "https://ehp.niehs.nih.gov/doi/full/10.1289/EHP7313", target="_blank")
        cwsh = a("Community water systems", herf = "https://www.epa.gov/dwreginfo/information-about-public-water-systems", target = "_blank")
        
        tagList(
            "The Columbia University Drinking Dashboard is a tracker and data visualization 
            tool of regulated contaminant concentration estimates in community water systems 
            across the US for the both researchers and the public. Users can explore spatial 
            and temporal variability in contaminant concentrations and download our datasets 
            for use in epidemiologic studies. The Dashboard is an accompaniment to the manuscript 
             ", ra2022,", Sociodemographic inequalities in uranium and other metals in 
            community water systems across the US, 2006-2011. Estimates are derived from the US 
            Environmental Protection Agency’s datasets in support of the ", syr," 
      
            See ", ra2022," and ", rn2020 ," for a detailed description of materials and methods. ", cwsh, 
            " are regulated, public water systems that serve the same population year-round."
        )
    })
    
    
    output$Ht2 = renderUI({
      HTML(
        "Interactive maps of community water system-level and county-level concentration estimates 
        and interactive tables of county-level concentration estimates are organized into class 
        specific tabs (Inorganics/Radionuclides, Microorganisms/Disinfectants/DBPs, Organics). 
        <b>These pages may take several seconds to load as the datasets underlying them are large.</b> 
        The full datasets at the community water system-level and county-level are also available 
        for download on these pages. We welcome you to use these datasets in your work."
      )
    })
    
    output$Ht3 = renderUI({
      HTML(
        "<b>Citation for this Dashboard and associated datasets:</b>"
      )
    })
    
    output$Ht4 = renderUI({
      HTML(
        "Ravalli F, Yuanzhi Y, Bostick BC, Chillrud SN, Schilling K, Basu A, Navas-Acien A, Nigra AE. 2022. 
        Sociodemographic inequalities in uranium and other metals in community water systems across the USA, 2006-11: a cross-sectional study.  
        <i>The Lancet Planetary Health, 6</i>(4), pp.e320-e330."
      )
    })
    
    output$Ht5 = renderUI({
      
      aen21 = a("aen2136@cumc.columbia.edu", href = "mailto: aen2136@cumc.columbia.edu", target="_blank")
      
      tagList(
        "We love to collaborate, and would be happy to help you make the best and most appropriate use of these 
        datasets for your work. Please contact Dr. Annie Nigra at " , aen21," (see About Us tab) with any questions."
      )
    })
    
    output$Ht6 = renderUI({
      HTML(
        "<b>What about other geographic resolutions?</b> Our group has developed population-weighted and area-weighted 
        concentration estimates at the census tract and zip code level for many states using publicly available 
        distribution boundaries. Please reach out if you would like to collaborate or use these estimates."
      )
    })
    
    output$Ht7 = renderUI({
      
      syr = a("Six Year Review of Drinking Water Standards.", href = "https://www.epa.gov/dwsixyearreview", target="_blank")
      ra2022 = a("Ravalli et al. 2022", href = "https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(22)00043-2/fulltext", target="_blank")
      rn2020 = a("Nigra et al. 2020", href = "https://ehp.niehs.nih.gov/doi/full/10.1289/EHP7313", target="_blank")
      #cwsh = a("Community water systems", herf = "https://www.epa.gov/dwreginfo/information-about-public-water-systems", target = "_blank")
      
      tagList(
        "Estimates are derived from the US Environmental Protection Agency’s datasets in support of the ", syr," 
        Individual monitoring record values measured below the limit of detection were replaced by the limit of detection divided by the square root 
        of two prior to averaging concentrations. Low estimated averages may therefore reflect water systems which did not measure concentrations of 
        a particular contaminant above the limit of detection. Estimated concentrations account for reported water treatment to reflect concentrations 
        distributed to consumers. Concentration estimates are averaged to time periods that reflect US EPA compliance monitoring period requirements 
        to reduce information bias. See ", ra2022," and ", rn2020," for a detailed description of materials and methods."
      )
    })
    
    
    output$Ht8 = renderUI({
      
      sdwis = a("Safe Drinking Water Information System (SDWIS).", href = "https://www.epa.gov/ground-water-and-drinking-water/safe-drinking-water-information-system-sdwis-federal-reporting", target="_blank")
      
      tagList(
        "Information about each water system (including county and zip code information) is derived from the US Environmental Protection 
        Agency’s ", sdwis," Please note that EPA is aware of inaccuracies and underreporting of some 
        data in the Safe Drinking Water Information System, and is working with the states to improve the quality of the data. Administrative 
        zip codes may not overlap with water system distribution boundaries. At the county-level, average concentrations were weighted by the 
        population served by each community water system to estimate the county-level weighted average community water system concentrations. 
        Counties may appear to be missing data (NA) when either no community water systems in the Six Year Review database served that county, 
        or when the total number of people served by all community water systems serving that county and reporting records to the Six Year Review 
        was less than 50% of the total number of people reliant on public drinking water in that county. In our interactive maps, individual 
        community water systems appear as points randomly jittered within the administrative zip-code associated with that water system."
      )
    })
    
    
    output$Ht9 = renderUI({
      
      stable = a("Community Water System Estimates dashboard", href = "https://msph.shinyapps.io/pww_tracker/", target="_blank")
      
      tagList(
        "A searchable table of community water system-level exposure estimates is available on the ", stable,"."
      )
    })
    
    output$Ht10 = renderUI({
      
      HTML(
        "<b>These pages may take approximately one minute to load as the datasets underlying them are large.</b>"
      )
    })
    
    
    output$icr = renderUI({
      
      tagList(
        "Individual community water systems appear as points randomly jittered within the administrative zip-code associated with that water system. 
        Please note that EPA is aware of inaccuracies and underreporting of some data in the Safe Drinking Water Information System, and is working 
        with the states to improve the quality of the data. Administrative zip codes may not overlap with water system distribution boundaries. 
        At the county-level, average concentrations were weighted by the population served by each community water system to estimate the county-level
        weighted average community water system concentrations. Counties may appear to be missing data (NA) when either no community water systems 
        in the Six Year Review database serve that county, or when the total number of people served by all community water systems serving that county 
        and reporting records to the Six Year Review was less than 50% of the total number of people reliant on public drinking water in that county."
      )
    })
    
    
    output$MDB1 = renderUI({
      
      HTML(
        "<b>We are currently working to update these estimates to include additional contaminants. Seasonal and yearly estimates for disinfection byproducts
        are also available. Please contact us for more information.</b> "
      )
    })
    
    output$MDB2 = renderUI({
      
      tagList(
        "Individual community water systems appear as points randomly jittered within the administrative zip-code associated with that water system. 
        Please note that EPA is aware of inaccuracies and underreporting of some data in the Safe Drinking Water Information System, and is working 
        with the states to improve the quality of the data. Administrative zip codes may not overlap with water system distribution boundaries. At 
        the county-level, average concentrations were weighted by the population served by each community water system to estimate the county-level 
        weighted average community water system concentrations. Counties may appear to be missing data (NA) when either no community water systems 
        in the Six Year Review database serve that county, or when the total number of people served by all community water systems serving that 
        county and reporting records to the Six Year Review was less than 50% of the total number of people reliant on public drinking water in that county."
      )
    })
    
    
    
    
    output$org1 = renderUI({
        
        HTML(
            "<b>COMING SOON</b>"
        )
    })
    
    
    output$org2 = renderUI({
      
      tagList(
        "Individual community water systems appear as points randomly jittered within the administrative zip-code associated with that water system. 
        Please note that EPA is aware of inaccuracies and underreporting of some data in the Safe Drinking Water Information System, and is working 
        with the states to improve the quality of the data. Administrative zip codes may not overlap with water system distribution boundaries. 
        At the county-level, average concentrations were weighted by the population served by each community water system to estimate the county-level 
        weighted average community water system concentrations. Counties may appear to be missing data (NA) when either no community water systems in 
        the Six Year Review database serve that county, or when the total number of people served by all community water systems serving that county 
        and reporting records to the Six Year Review was less than 50% of the total number of people reliant on public drinking water in that county."
      )
    })
    
    
    
    
    output$ab1 = renderUI({
        cumsph = a("Columbia University Mailman School of Public Health", href = "https://www.publichealth.columbia.edu/", target="_blank")
        ehs = a("Environmental Health Sciences", href = "https://www.publichealth.columbia.edu/academics/departments/environmental-health-sciences-ehs", target="_blank")
        bios = a("Biostatistics,", href = "https://www.publichealth.columbia.edu/academics/departments/biostatistics", target="_blank")
        cura = a("Columbia University Superfund Research Program,", href = "https://www.publichealth.columbia.edu/research/columbia-superfund-research-program", target="_blank")
        ldeo = a("Lamont-Doherty Earth Observatory.", href = "https://www.ldeo.columbia.edu/", target ="_blank")
        tagList(
            "The Columbia University Drinking Water Dashboard is an interdisciplinary project housed in the",cumsph , 
            "Departments of", ehs,"and", bios,"the", cura,"and the",ldeo
        )
    })
    
    
    output$ab2 = renderUI({
        yyz = a("Yuanzhi Yu,", href = "https://yy3019.github.io/", target="_blank")
        fr = a("Filippo Ravalli,", href = "https://www.linkedin.com/public-profile/in/filippo-ravalli/", target="_blank") 
        an = a("Dr. Anne Nigra,", href = "https://www.publichealth.columbia.edu/people/our-faculty/aen2136", target="_blank")
        ana = a("Dr. Ana Navas-Acien.", href = "https://www.publichealth.columbia.edu/people/our-faculty/an2737", target="_blank")    
        
        tagList(
            "The Dashboard was developed by", yyz, fr, an, ana
        )
    })
    
    output$ab3 = renderUI({
        pp =  a("Nigra et al. 2020", href = "https://ehp.niehs.nih.gov/doi/full/10.1289/EHP7313", target="_blank")   
        pp2 =  a("Ravalli et al. 2022", href = "https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(22)00043-2/fulltext", target="_blank") 
        aen212 = a("aen2136@cumc.columbia.edu", href = "mailto: aen2136@cumc.columbia.edu", target="_blank")
        
        tagList(
            "If you believe there is an error on our site, please feel free to contact us (email Dr. Annie Nigra at ", aen212,"). See ", pp2," and", pp," for detailed methodologic information."
        )
    })
 
    output$p1 <- renderLeaflet({
        df1 <- df
        df2 = test1
        pc(df1, input$p1_m, df2)
        
        
    })  
    
    
    output$p2 <- renderLeaflet({
        dbp_df1 <- dbp_df
        dbp_df2 = dbp_test1
        pc(dbp_df1, input$p2_m, dbp_df2)
        
        
    })
    
    output$t_2 <- DT::renderDataTable(DT::datatable({
      data <- cdata_1
      if (input$m_t2 != "All") {
        data <- data[data$Metal == input$m_t2,]
      }
      if (input$yr_t2 != "All") {
        data <- data[data$`Year Range` == input$yr_t2,]
      }
      if (input$sc_t2 != "All") {
        data <- data[data$`State Code` == input$sc_t2,]
      }
      if (input$cf_t2 != "All") {
        data <- data[data$`County FIPS` == input$cf_t2,]
      }
      if (input$cn_t1 != "All") {
        data <- data[data$`County Name` == input$cn_t1,]
      }
      data
    }))
    
    
    output$t_4 <- DT::renderDataTable(DT::datatable({
      data <- dbp_c1
      if (input$m_t4 != "All") {
        data <- data[data$Contaminants == input$m_t4,]
      }
      if (input$yr_t4 != "All") {
        data <- data[data$`Year Range` == input$yr_t4,]
      }
      if (input$sc_t4 != "All") {
        data <- data[data$`State Code` == input$sc_t4,]
      }
      if (input$cf_t4 != "All") {
        data <- data[data$`County FIPS` == input$cf_t4,]
      }
      if (input$cn_t4 != "All") {
        data <- data[data$`County Name` == input$cn_t4,]
      }
      data
    }))
    
    output$t2text = renderUI({
      
      tagList(
        "Individual monitoring record values measured below the limit of detection were replaced by the limit of detection divided by the square 
      root of two prior to averaging concentrations. Low estimated county-level averages may therefore reflect water systems which did not measure 
      concentrations of a particular contaminant above the limit of detection. Method detection limits in 
      μg/L were 0.5 (As), 0.4 (Sb), 0.8 (Ba), 0.2 (Be), 0.05 (Cd), 0.08 (Cr), 0.2 (Hg), 0.6 (Se), 0.3 (Tl), and 5.0 (CN). 
      Because EPA did not publish a method detection limit for U in the SYR3 documentation, we assumed a value of 0.5 μg/L for U."
      )
    })
    
    
    output$t2text2 = renderUI({
      datalink2 = a("Download Link", href = "https://github.com/annenigra/US-PublicWaterSystem-Metal-Estimates", target="_blank")
      tagList(
        
        "Full datasets at the community water county-level are available through ", datalink2, "."
      )
    })  
    
}



shinyApp(ui, server)