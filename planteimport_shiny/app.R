require(leaflet)
require(maps)
require(shiny)
#require(magrittr)
require(xtable)
require(DBI)
require(RPostgres)
require(dplyr)

#tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),


ui<-shinyUI(
  navbarPage("Planteimport - overvåking av fripassagerer",
             tabPanel('Data download',
                      sidebarLayout(
                        sidebarPanel(width=2,
                                     dateRangeInput("daterange", "Date range:",
                                                    start = Sys.Date() -365*5,
                                                    end   = Sys.Date()),
                                     uiOutput("choose_species"),
                                     downloadButton('downloadData', 'Last ned CSV')), 
                        mainPanel(fluidRow(column(12,leafletOutput("mymap", height=600)))))
                      ),
             tabPanel("Oversikt Containere",
                      DT::dataTableOutput('containers')),
             tabPanel("Oversikt Insektsfunn",
                      DT::dataTableOutput('insekt_records')),
             tabPanel("Insektarter funne",
                      DT::dataTableOutput('insekt_species'))

))






server<-function(input, output, session) {
  
  con <- DBI::dbConnect(RPostgres::Postgres(), 
                        dbname = "planteimport", 
                        user = "planteimport_shiny", 
                        password = "container", 
                        host = "ninpgsql02.nina.no")
  
  
  datasetInput <- reactive({
    fields()$fields})
  
  output$downloadData <- downloadHandler(
    filename = function() { "planteimport.csv" },
    content = function(file) {
      write.csv(datasetInput(), file, row.names=F)})
  
  
  select_categories<-function(){
    
    dbGetQuery(con,"SET search_path = positions, public;")
    
    cat.query<-"SELECT  species as species_cat, colony as colony_cat, data_responsible as responsible_cat
    , ring_number as ring_number_cat
    FROM positions.postable
    GROUP BY species, colony, data_responsible, ring_number
    "
    
    suppressWarnings(categories<-dbGetQuery(con, cat.query))
    
    #categories<-fetch(res,-1)
    #dbClearResult(res)
    
    categories
  }
  
  

  
  output$choose_species<- renderUI({
    selectInput('species', 'Species', c("Insekter", "Planter"), selected = "Insekter")
  })
  
  
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
      
    start_time<-as.character(input$daterange[1])
    end_time<-as.character(input$daterange[2])
    
    date_range<-paste("\n WHERE date_time::date >= '", 
                      start_time,
                      "' ", 
                      "AND date_time::date <= '", 
                      end_time, 
                      "'", 
                      sep="")


    if(input$taxa=="Insekter"){
    fetch.q<-paste("SELECT *
                   FROM insects.records"
                   , date_range, 
                   sep="")
    }
    
    if(input$taxa=="Planter"){
      fetch.q<-paste("SELECT *
                     FROM plants.records"
                     , date_range, 
                     sep="")
    }
    
    fetch.q
    
  })
  
  
  
  fields<-reactive({
    if (is.null(input$taxa)){
      return(NULL)
    } else
  
      dbGetQuery(con, "SET CLIENT_ENCODING TO 'UTF8'")
    
    suppressWarnings(post.fields <- dbGetQuery(con,as.character(recordsQuery())))
  list(fields=post.fields)
 })
  
  
  locations <- reactive({
    # if (is.null(input$taxa)){
    #   return(NULL)
    # } else
      
    dbGetQuery(con, "SET CLIENT_ENCODING TO 'UTF8'")
    
    suppressWarnings(post.fields <- dbGetQuery(con,as.character(locationsQuery())))
    return(post.fields)
  })
  
  
  
  output$mymap<-renderLeaflet({
    if (is.null(input$species)){
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
    query()
    
  )
  
  output$nText <- renderText({
    ntext()
  })
  
}

shinyApp(ui= ui, server= server)
