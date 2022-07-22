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


### Metal

cws_1 = read.fst("cws_1.fst")



### DBPS

dbp_cws1 = read.fst("dbp_cws1.fst")



### UI

ui <- navbarPage(
  #theme = "shiny.css",
  title = div(img(src='logo.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
  windowTitle = " Columbia University Drinking Water Dashboard",
  id = 'menus',
  
  tabPanel('Inorganic chemicals and Radionuclides',
           shinyjs::useShinyjs(),
           fluidRow(
             br(),
             fluidRow(column(width = 10, offset = 1, span(strong("Inorganic chemicals and Radionuclides"), style="font-size: 30px;line-height:150%")))),
  
           fluidRow(
             column(width = 10, offset = 1, span(strong("Community Water System Estimates"), style="font-size: 23px;line-height:150%")),
             column(width = 10, offset = 1, h4("Tracking inequalities in public water exposures across the United States")),        
             column(2, offset = 1,
                    selectInput("m_t1",
                                "Contaminant:",
                                c(unique(as.character(cws_1$Metal)))),
                    selectInput("yr_t1",
                                "Year Range:",
                                c(unique(as.character(cws_1$`Year Range`))))),
             
             column(2, offset = 1,
                    selectInput("p_t1",
                                "PWSID:",
                                c("All",
                                  unique(as.character(cws_1$PWSID)))),
                    selectInput("pn_t1",
                                "PWSID Name:",
                                c("All",
                                  unique(as.character(cws_1$`PWSID Name`))))),
             column(2, offset = 1,
                    selectInput("cf_t1",
                                "County FIPS:",
                                c("All",
                                  unique(as.character(cws_1$`County FIPS`)))),
                    selectInput("ac_t1",
                                "Administrative City:",
                                c("All",
                                  unique(as.character(cws_1$`Administrative City`))))),
             # Create a new row for the table.
             column(width = 10, offset = 1, align="center", DT::dataTableOutput("t_1"))),
           br(),
           fluidRow(column(width = 8, offset = 1, span(htmlOutput("t1text2"), style="font-size: 15px;line-height:150%"))),
           br(),
           fluidRow(column(width = 8, offset = 1, span(htmlOutput("t1text"), style="font-size: 15px;line-height:150%"))),
           br(),
 
           fluidRow(align="center",
                    span(htmlOutput("bannertext2", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
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
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
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
    fluidRow(
      br(),
      fluidRow(column(width = 10, offset = 1, span(strong("Microorganisms/Disinfectants/DBPs"), style="font-size: 30px;line-height:150%")))),
    fluidRow(
      column(width = 10, offset = 1, span(strong("Community Water System Estimates"), style="font-size: 23px;line-height:150%")),
      column(width = 10, offset = 1, h4("Tracking inequalities in public water exposures across the United States")),        
      column(2, offset = 1,
             selectInput("m_t3",
                         "Contaminant:",
                         c(unique(as.character(dbp_cws1$Contaminants)))),
             selectInput("yr_t3",
                         "Year Range:",
                         c(unique(as.character(dbp_cws1$`Year Range`))))),
      #column(2, offset = 1,
      #       selectInput("yr_t1",
      #                   "Year Range:",
      #                  c("All",
      #                    unique(as.character(dbp_cws1$`Year Range`))))),
      column(2, offset = 1,
             selectInput("p_t3",
                         "PWSID:",
                         c("All",
                           unique(as.character(dbp_cws1$PWSID)))),
             selectInput("pn_t3",
                         "PWSID Name:",
                         c("All",
                           unique(as.character(dbp_cws1$`PWSID Name`))))),
      column(2, offset = 1,
             selectInput("cf_t3",
                         "County FIPS:",
                         c("All",
                           unique(as.character(dbp_cws1$`County FIPS`)))),
             selectInput("ac_t3",
                         "Administrative City:",
                         c("All",
                           unique(as.character(dbp_cws1$`Administrative City`))))),
      # Create a new row for the table.
      column(width = 10, offset = 1, align="center", DT::dataTableOutput("t_3"))),
    br(),
    #fluidRow(column(width = 8, offset = 1, span(htmlOutput("t1text2"), style="font-size: 15px;line-height:150%"))),
    br(),
    #fluidRow(column(width = 8, offset = 1, span(htmlOutput("t1text"), style="font-size: 15px;line-height:150%"))),
    br(),
 
    
    
    fluidRow(align="center",
             span(htmlOutput("bannertext3", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
             #span(htmlOutput("sharetext", style="color:white")),
             #br(),
             #img(src='bottomlogo.png', height="20%", width="20%"),
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
             #actionButton("ins_index",
             #             label = "",
             #             icon = icon("instagram"),
             #             onclick = sprintf("window.open('%s')", url3),
             #             style = "border-color: #FFFFFF;"),
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
    fluidRow(
      br(),
      fluidRow(column(width = 10, offset = 1, span(strong("Organic chemicals"), style="font-size: 30px;line-height:150%")))),
    fluidRow(
      column(width = 10, offset = 1, span(strong("Coming Soon"), style="font-size: 23px;line-height:150%"))),        

    br(), 
    
    
    fluidRow(align="center",
             span(htmlOutput("bannertext4", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
             #span(htmlOutput("sharetext", style="color:white")),
             #br(),
             #img(src='bottomlogo.png', height="20%", width="20%"),
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
             #actionButton("ins_index",
             #             label = "",
             #             icon = icon("instagram"),
             #             onclick = sprintf("window.open('%s')", url3),
             #             style = "border-color: #FFFFFF;"),
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
                    
                    #column(10, offset = 1,helpText("MSPH photo source: https://globalcenters.columbia.edu/content/yusuf-hamied-fellowships-program")),
                    column(8, offset = 1,span(uiOutput("ab1",style = "font-size: 17px; line-height:150%"))),
                    column(8, offset = 1,span(uiOutput("ab2",style = "font-size: 17px; line-height:150%"))),
                    column(8, offset = 1,span(uiOutput("ab3",style = "font-size: 17px; line-height:150%"))),
                    column(8, offset = 1, div(img(src = "cc.jpg", height = "40%",width = "80%"),
                                              style="text-align: center;"))
           ),
           br(),
           fluidRow(align="center",
                    span(htmlOutput("bannertext5", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
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
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
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
  
  output$bannertext5 = renderText({
    return(
      "<b> Columbia <b> University <b> Drinking <b> Water <b> Dashboard"
    )
  })
  
  
  output$sharetext = renderText({
    return(
      "<b> Share on </b> "
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
    an = a("Dr. Anne Nigra,", href = "https://annenigra.github.io/", target="_blank")
    ana = a("Dr. Ana Navas-Acien.", href = "https://www.publichealth.columbia.edu/people/our-faculty/an2737", target="_blank")    
    
    tagList(
      "The Dashboard was developed by", yyz, fr, an, ana
    )
  })
  
  output$ab3 = renderUI({
    pp =  a("Nigra et al. 2020", href = "https://ehp.niehs.nih.gov/doi/full/10.1289/EHP7313", target="_blank")   
    
    tagList(
      "If you believe there is an error on our site, please feel free to contact us. See Ravalli et al. 2021 and", pp," for detailed methodologic information."
    )
  })
  output$t_1 <- DT::renderDataTable(DT::datatable({
    data <- cws_1
    if (input$m_t1 != "All") {
      data <- data[data$Metal == input$m_t1,]
    }
    if (input$yr_t1 != "All") {
      data <- data[data$`Year Range` == input$yr_t1,]
    }
    if (input$p_t1 != "All") {
      data <- data[data$PWSID == input$p_t1,]
    }
    if (input$pn_t1 != "All") {
      data <- data[data$`PWSID Name` == input$pn_t1,]
    }
    if (input$cf_t1 != "All") {
      data <- data[data$`County FIPS` == input$cf_t1,]
    }
    if (input$ac_t1 != "All") {
      data <- data[data$`Administrative City` == input$ac_t1,]
    }
    data
  }))
  

  
  output$t1text = renderUI({
    
    tagList(
      "CWS average contaminant concentrations that were estimated below the EPA’s maximum method detection limit were replaced 
      by <LOD for display in this table. Datasets available for download via GitHub retain the original estimated or imputed 
      value, even if the imputed value reflects a system with concentrations measured below the detection limit. Method detection 
      limits in μg/L were 0.5 (As), 0.4 (Sb), 0.8 (Ba), 0.2 (Be), 0.05 (Cd), 0.08 (Cr), 0.2 (Hg), 0.6 (Se), 0.3 (Tl), and 5.0 (CN). 
      Because EPA did not publish a method detection limit for U in the SYR3 documentation, we assumed a value of 0.5 μg/L for U."
    )
  })
  

  
  
  
  
  output$t1text2 = renderUI({
    datalink1 = a("Download Link", href = "https://github.com/annenigra/US-PublicWaterSystem-Metal-Estimates", target="_blank")
    tagList(
      
      "Full datasets at the community water system-level are available through ", datalink1, "."
    )
  }) 
  
 
  
  
  
  output$t_3 <- DT::renderDataTable(DT::datatable({
    data <- dbp_cws1
    if (input$m_t3 != "All") {
      data <- data[data$Contaminants == input$m_t3,]
    }
    if (input$yr_t3 != "All") {
      data <- data[data$`Year Range` == input$yr_t3,]
    }
    if (input$p_t3 != "All") {
      data <- data[data$PWSID == input$p_t3,]
    }
    if (input$pn_t3 != "All") {
      data <- data[data$`PWSID Name` == input$pn_t3,]
    }
    if (input$cf_t3 != "All") {
      data <- data[data$`County FIPS` == input$cf_t3,]
    }
    if (input$ac_t3 != "All") {
      data <- data[data$`Administrative City` == input$ac_t3,]
    }
    data
  }))
  

  
  
  
  
  
  
  
}



shinyApp(ui, server)