require(leaflet)
require(maps)
require(shiny)
#require(magrittr)
require(xtable)
require(DBI)
require(RPostgres)
require(dplyr)
require(ggplot2)

#tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),

######
require(tools)

Logged = FALSE;
load("shinyPass.Rdata")

ui1 <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui2 <- function(){tagList(tabPanel("Test"))}

ui = (htmlOutput("page"))
server = (function(input, output,session) {
  
  USER <- reactiveValues(Logged = Logged)
  
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
  })
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE)
    {
      ##############END LOGIN STUFF, begin ui part#####################
      
      output$page <- renderUI({navbarPage("Planteimport - overvåking av fripassagerer",
             tabPanel('Accumulation graphs and data download',
                      sidebarLayout(
                        sidebarPanel(width=2,
                                     dateRangeInput("daterange", "Date range:",
                                                    start = Sys.Date() -365*5,
                                                    end   = Sys.Date()),
                                     selectInput("taxa", "Species", c("Insekter", "Planter"), selected = "Insekter"),
                                     uiOutput("container_species"),
                                     uiOutput("country"),
                                     selectInput("plotLevel", "What to Plot", c("Species", "Individuals"), selected = "Species"),
                                     downloadButton('downloadPlot', 'Last ned figur')), 
                        #mainPanel(fluidRow(column(12, leafletOutput("mymap", height=600)))
                        mainPanel(plotOutput("cumPlot"),
                                  fluidRow(column(1, offset=0,"Database dialog:"), column(11, verbatimTextOutput("nText"))))
                      )
             ),
             tabPanel("Oversikt Containere",
                      DT::dataTableOutput('containers')),
             tabPanel("Oversikt Insektsfunn",
                      DT::dataTableOutput('insekt_records')),
             tabPanel("Insektarter funne",
                      DT::dataTableOutput('insekt_species'))
             
  )}
)


########SERVER PART HERE INSTEAD OF IN A SERVER FUNCTION CALL

  source("planteShinyFunctions.R")
  
  con <- DBI::dbConnect(RPostgres::Postgres(), 
                        dbname = "planteimport", 
                        user = "planteimport_shiny", 
                        password = "container", 
                        host = "ninpgsql02.nina.no")
  
  
  datasetInput <- reactive({
    fields()
  })
  
  # #old, for downloading data 
  # output$downloadData <- downloadHandler(
  #   filename = function() { "planteimport.csv" },
  #   content = function(file) {
  #     write.csv(datasetInput(), file, row.names=F)})
  # 
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$taxa, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = cumPlot(input = fields(), what = plotInput()$what), device = "png")
    }
  )
  
  select_categories<-function(){
    
    dbSendQuery(con, "SET search_path = common, public;")
    
    cat.query<-"SELECT  netting_type as netting_cat, exporter as exporter_cat, country as country_cat
    , transport_type as transport_type_cat, species_latin as species_cat
    FROM common.containers
    GROUP BY netting_type, exporter_cat, country_cat, transport_type, species_cat
    "
    
    suppressWarnings(categories <- dbGetQuery(con, cat.query))
    
    #categories<-fetch(res,-1)
    #dbClearResult(res)
    
    categories
  }
  
  
  output$container_species <- renderUI({
    selectInput("container_species", "Import species", c("All", sort(as.character(unique(select_categories()$species_cat)))), selected="All")
  })
  
  output$country <- renderUI({
    selectInput("country", "Import country", c("All", sort(as.character(unique(select_categories()$country_cat)))), selected="All")
  })
  
  
  #output$taxa<- renderUI({
  #  selectInput('taxa', 'Species', c("Insekter", "Planter"), selected = "Insekter")
  #})
  
  
  output$containers <- DT::renderDataTable({
    containers <- dbGetQuery(con, "SELECT * FROM common.containers")
    out <- containers[names(containers) != "id"]
    out
  })
  
  output$insekt_species <- DT::renderDataTable({
    species <- dbGetQuery(con, "SELECT phylum,
                          class,
                          subclass,
                          \"order\",
                          underorder,
                          family 
                          old_description,
                          species_latin,
                          stadium,
                          indetermined,
                          alien,
                          blacklist_cat,
                          native
                          FROM insects.species")
    
    species
  })
  
  output$insekt_records <- DT::renderDataTable({
    insect_records <- dbGetQuery(con, "SELECT container, subsample, species_latin, amount FROM insects.records")
    insect_records
  })
  
  locationsQuery <- reactive({
    fetch.q <- "SELECT *, ST_X(ST_transform(geom, 4326)) lon, ST_Y(ST_transform(geom, 4326)) lat
    FROM common.locality"
    fetch.q 
  })
  
  recordsQuery<-reactive({
    if (is.null(input$taxa)){
      return(NULL)
    } else
      
      start_time <- as.character(input$daterange[1])
    end_time <- as.character(input$daterange[2])
    
    date_range <- paste("\n LEFT JOIN common.containers c 
                        ON r.container = c.container
                        WHERE c.date_sampled >= '", 
                        start_time,
                        "' ", 
                        "AND c.date_sampled <= '", 
                        end_time, 
                        "'", 
                        sep="")
    
    
    if(input$taxa == "Insekter"){
      fetch.q <- paste0("SELECT r.*
                        FROM insects.records r"
                        , date_range,
                        "\n")
    }
    
    if(input$taxa == "Planter"){
      fetch.q <- paste0("SELECT r.*
                        FROM plants.records r"
                        , date_range,
                        "\n"
      )
    }
    
    if(input$container_species != "All"){
      
      fetch.q <- paste0(fetch.q, 
                        "AND c.species_latin = '", 
                        input$container_species, 
                        "'",
                        "\n")
    }
    
    
    if(input$country != "All"){
      
      fetch.q <- paste0(fetch.q, 
                        "AND c.country = '",
                        input$country, 
                        "'",
                        "\n")
    }
    
    return(fetch.q)
    #return(input$taxa)
  })
  
  
  
  
  fields <- reactive({
    # if (is.null(input$taxa)){
    #  return(NULL)
    #} else
    
    dbSendQuery(con, "SET CLIENT_ENCODING TO 'UTF8'")
    
    suppressWarnings(post.fields <- dbGetQuery(con, as.character(recordsQuery())))
    post.fields
  })
  
  
  locations <- reactive({
    # if (is.null(input$taxa)){
    #   return(NULL)
    # } else
    
    dbSendQuery(con, "SET CLIENT_ENCODING TO 'UTF8'")
    
    suppressWarnings(post.fields <- dbGetQuery(con, as.character(locationsQuery())))
    return(post.fields)
  })
  
  
  
  output$mymap<-renderLeaflet({
    if (is.null(input$taxa)){
      return(NULL)
    } else
    {
      leaflet() %>%
        addProviderTiles("Esri.NatGeoWorldMap") %>%
        addCircleMarkers(radius=6, 
                         stroke= FALSE, 
                         fillOpacity=0.5, 
                         lng=locations()$lon, 
                         lat=locations()$lat,
                         popup=paste("Location: ",
                                     as.character(locations()$locality), 
                                     col = "#E57200")
        )
      
    }
  }
  )
  
  
  
  ntext<-reactive(
    recordsQuery()
  )
  
  output$nText <- renderText({
    ntext()
  })
  
  plotInput <- reactive({
    out <- list("what" =  input$plotLevel)
    
    out
  })
  
  output$cumPlot <- renderPlot({
    
    #input <- read.table("planteimport2.csv", sep = ",", header = T)
    cumPlot(input = fields(), 
            what = plotInput()$what, 
            country = input$country, 
            species = input$container_species)
    
  })
  
    }
  
})

})

shinyApp(ui= ui, server= server)
