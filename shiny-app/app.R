library(shiny)
library(shinythemes)
library(textdata)
library(tidyverse)
library(leaflet)
library(rgdal)
library(viridis)

#This imports the necessary files
neonDomain<- readOGR('data/NEONDomains')

tickData <- read_csv('data/DomainSummaryData.csv')

#This mutates sample year so that we can use it as a select input and filter for the 
#user




ui <- fluidPage(
    
    #This sets up the app, and labels everything and creates the inputs to select
    #and text for user to understand the app. It also organizes the graphs into tabs, and 
    #create sidebar panels for the aesthetic appeal. 
    theme = shinytheme("united"),
    titlePanel("Population Densities of Different Species Across NEON Sites, Domains, 
               and Time"),
    br(),
    hr(),
    sidebarPanel(  
        selectInput(inputId = "SexorAge",
                    label = "Choose a Sex or Age",
                    choices = c("Larva", "Nymph", "Adult"), 
                    selected='Nymph'), 
        
        selectInput(inputId = "Species",
                    label = "Choose a Species",
                    choices = levels(factor(tickData$acceptedTaxonID)), 
                    selected='AMBAME'), 
        
        selectInput(inputId = "year",
                    label = "Choose a Year to Find Data From",
                    choices = levels(factor(tickData$sample_year)), 
                    selected= "2014"),
        
        actionButton(inputId = "submit",
                     label = 'Apply Changes'),
        
        br(),
        br(),
        ("Please start with the live stage (sex or age) and work down through the 
         filters. Each step will propagate choices for the next input. If making 
         a selection removes an option from an input this is because there was no 
         data collected. Only includes tick species where more than ten individuals
         were sampled between 2014 and 2019.")),
    ("This interactive application allows you to explore the tick dragging and sampling 
    from NEON, a national ecological database. In the application, you are able 
    to choose the species, age, and sampling year for the tick data, 
     and with this data, the leaflet map will show the tick population density 
     of that species across the different NEON domains. By clicking tabs for different 
     graphs, a subsequent bar graph below will also show the tick population 
     density at each site sampling for ticks during that year, and the color of 
     each bar denotes the NEON domain in which the site exists. Lastly, by 
     clicking on each domain in the leaflet with the mouse, it presents the name 
     of the NEON domain site. Ultimately, to finalize the guidelines and parameters 
     for the data, simply click apply changes, and the two graphs will change based 
     on the given inputs. Please select a year and apply changes in order to get 
     started!"),
    br(),
    br(),
    mainPanel(
        tabsetPanel(
            tabPanel(title = "Map of Tick Population Density",
                     leafletOutput(outputId = "domainmap")
            ),
            tabPanel(title = "Graph of Tick Population Density",
                     br(),
                     plotOutput(outputId= 'barplot')
            ),
            tabPanel(title = "Tick Species Key",
                     br(),
                     p("AMBAME = lone star tick", em("(Amblyomma americanum)")),
                     p("DERVAR = American dog tick", em("(Dermacentor variabilis)")),
                     p("IXOPAC = western blacklegged tick", em("(Ixodes pacificus)")),
                     p("IXOSCA = blacklegged tick", em("(Ixodes scapularis,"),"also called the deer tick)"),
                     p("IXOSP = Larval hard-bodied tick"),
            )
        )
    )
)


server <- function(input, output, session) {
    
    #This creates the leaflet plot, by filtering through the data by the user inputs, similar to what 
    #we did in our practice file except for filtering by the selected user inputs
    
    #Dynamic Dropdowns =========================================================
    
    observeEvent(input$SexorAge, {
        if(input$SexorAge == "Larva"){
            updateSelectInput(session,
                              inputId = "Species",
                              choices = "IXOSP",
                              selected = NULL)
            updateSelectInput(session,
                              inputId = "year",
                              selected = NULL)
            
        }
        else if(input$SexorAge == "Nymph"){
            updateSelectInput(session,
                              inputId = "Species",
                              choices = c("AMBAME","DERVAR","IXOSCA"),
                              selected = NULL)
            updateSelectInput(session,
                              inputId = "year",
                              selected = NULL)
        }
        else if(input$SexorAge == "Adult"){
            updateSelectInput(session,
                              inputId = "Species",
                              choices = c("AMBAME","DERVAR","IXOPAC","IXOSCA"),
                              selected = NULL)
            updateSelectInput(session,
                              inputId = "year",
                              selected = NULL)
        }
    })
    
    
    observeEvent(input$Species, {
        if(input$Species == "IXOPAC"){
            updateSelectInput(session,
                              inputId = "year",
                              choices = c("2018","2019","All Years"),
                              selected = NULL)
        }
        else if(input$Species == "DERVAR"){
            updateSelectInput(session,
                              inputId = "year",
                              choices = c("2014","2015","2018","2019","All Years"),
                              selected = NULL)
            
        }
        else if(input$Species == "IXODEN"){
            updateSelectInput(session,
                              inputId = "year",
                              selected = NULL)
            
        }
        else if(input$Species == "IXOMUR"){
            updateSelectInput(session,
                              inputId = "year",
                              selected = NULL)
            
        }
        else {
            updateSelectInput(session,
                              inputId = "year",
                              choices = levels(factor(tickData$sample_year)),
                              selected = NULL)
        }
    })
    
    
    
    #PANEL 1 ===================================================================
    
    
    output$domainmap<-renderLeaflet({
        input$submit
        
        tryCatch(
            {
                joinedtickData<-tickData%>%
                    filter(acceptedTaxonID == isolate(input$Species),
                           sexOrAge==isolate(input$SexorAge),
                           sample_year == isolate(input$year))%>%
                    group_by(IDDOMAIN)%>%
                    summarise(mean_density = mean(mean_density))
                
                
                
                neonDomain@data<-left_join(neonDomain@data, joinedtickData,  
                                           by= c("DomainID" = 'IDDOMAIN'))
                
                neonDomain@data<-neonDomain@data%>%
                    mutate(mean_density = ifelse(is.na(mean_density), 0, mean_density))
                
                sites.domain.density<-neonDomain@data
                
                domain.colors<-colorNumeric(palette= "YlOrRd", 
                                            domain=neonDomain@data$mean_density)
                neonDomain%>%
                    leaflet()%>%
                    addTiles()%>%
                    addPolygons(fillColor = ~domain.colors(mean_density),
                                fillOpacity =~.8,
                                color = "black",
                                opacity = 1,
                                weight = .5,
                                popup = ~DomainName, 
                                layerId = ~DomainID)%>%
                    setView(-96,37.8,3.5)%>%
                    addLegend(pal = domain.colors,
                              values = neonDomain@data$`mean_density`,
                              title = "Tick Per Square Meter \t",
                              position = "bottomleft",
                              na.label = "No Data",
                              labFormat = labelFormat(
                                  suffix = "-"
                              ))
                
                
            },
            error = function(err) {
                neonDomain%>%
                    leaflet()%>%
                    addControl(html = "The filters you have applied have caused an errer. Please try other filters.", position = "topleft")
            })   
        
        
    })
    
    #PANEL 2 ===================================================================
    
    output$barplot <- renderPlot({
        input$submit
        #This filters the data based off of the user inputs and creates a bar plot, 
        #to help visualize tick population across the different neon sites
        
        joinedtickData<-tickData%>%
            filter(acceptedTaxonID == isolate(input$Species),
                   sexOrAge==isolate(input$SexorAge),
                   sample_year == isolate(input$year))%>%
            group_by(siteID, IDDOMAIN)%>%
            summarise(mean_density = mean(mean_density))
        
        neonDomain@data<-left_join(neonDomain@data, joinedtickData,  
                                   by= c("DomainID" = 'IDDOMAIN'))
        neonDomain@data<-neonDomain@data%>%
            drop_na(mean_density)
        
        neonDomain@data%>%
            ggplot(aes(x=reorder(siteID,-mean_density), 
                       y =mean_density))+ 
            geom_bar(stat='identity', color='black', 
                     aes(fill= DomainName),
                     na.rm = TRUE) +
            labs(y='Average Tick Density (Count/Square Meter)', 
                 x='Site') + theme(axis.text.x = element_text(angle=90))+
            ggtitle('Density of Tick Population By Site')+
            scale_fill_viridis(discrete = TRUE, alpha = 0.8, option = "B")+
            theme_minimal()+
            labs(fill='NEON Domains')
        
    })
}

shinyApp(ui, server)