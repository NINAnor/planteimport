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
                        mainPanel(fluidRow(column(12,leafletOutput("mymap", height=600))))),
             tabPanel("Oversikt Containere",
                      DT::dataTableOutput('container')),
             tabPanel("Oversikt Insekter",
                      DT::dataTableOutput('insekter')),
             tabPanel("Oversikt Planter", 
                     DT::dataTableOutput('planter'))))
)






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
  
  
  
  output$activeLoggingSessions <- DT::renderDataTable({
    sessions <- dbGetQuery(con, "SELECT * FROM views.active_logging_sessions")
    sessions
  })
  
  output$shortTable <- DT::renderDataTable({
    
    shortSum <-"
    SELECT count(distinct(species)) \"Antall arter\", count(distinct(colony)) \"Antall kolonier\",
    count(distinct(year_tracked)) \"Antall år\", count(*) \"Antall positions\",
    count(distinct(ring_number)) \"Antall individer\"
    FROM postable"
    
    # shortSum <- "
    # SELECT * FROM
    # shorttable"
    
    shortTable <- dbGetQuery(con, shortSum)
    rownames(shortTable) <- ""
    colnames(shortTable) <- c("Antall arter", "Antall kolonier", "Antall år", "Antall posisjoner", "Antall individer")
    
    shortTable
    
  }
  ,
  caption = "Summeringstabell for alle funn",
  rownames = F
  # caption.placement = getOption("xtable.caption.placement", "bottom"),
  # caption.width = getOption("xtable.caption.width", NULL)
  )
  
  
  output$shortTableEqfilter3 <- DT::renderDataTable({
    
    
    shortSumEqfilter3 <-"
    SELECT count(distinct(species)) antall_arter, count(distinct(colony)) antall_kolonier, count(distinct(year_tracked)) antall_år, count(*) antall_positions,
    count(distinct(ring_number)) antall_individer
    FROM postable
    WHERE eqfilter3 = 1"
    
    # shortSumEqfilter3 <- "
    # SELECT *
    # FROM shorttableeqfilter3"
    
    shortTable <- dbGetQuery(con, shortSumEqfilter3)
    rownames(shortTable) <- ""
    colnames(shortTable) <- c("Antall arter", "Antall kolonier", "Antall år", "Antall posisjoner", "Antall individer")
    shortTable
    
    
  }
  ,
  caption = "Summeringstabell for alle funn med eqfilter = 3",
  rownames = F
  # caption.placement = getOption("xtable.caption.placement", "bottom"),
  # caption.width = getOption("xtable.caption.width", NULL)
  )
  
  
  
  output$longerTable <- DT::renderDataTable({
    
    longerSum <-"
    SELECT year_tracked år, species, count(distinct(ring_number)) antall_unike_ring_nummer, count(*) antall_posisjoner, count(distinct(colony)) antall_kolonier
    FROM postable
    GROUP BY år, species
    ORDER BY år, species"
    
    # longerSum <- "
    # SELECT *
    # FROM longersum
    # "
    
    longerTable <- dbGetQuery(con, longerSum)
    #rownames(shortTable) <- ""
    colnames(longerTable) <- c("Logger-år", "Art", "Antall individer", "Antall posisjoner", "Antall kolonier")
    longerTable
    
  }
  ,
  caption = "Tabell over ringer",
  rownames = F
  # caption.placement = getOption("xtable.caption.placement", "bottom"),
  # caption.width = getOption("xtable.caption.width", NULL)
  )
  
  
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
