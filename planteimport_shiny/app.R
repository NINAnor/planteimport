require(leaflet)
#require(maps)
require(shiny)
#require(magrittr)
#require(xtable)
require(DBI)
require(RPostgres)
require(dplyr)
require(ggplot2)
require(tidyr)
require(NinaR)
require(forcats)
require(tibble)
require(extrafont)


#tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),

######
require(tools)

# Logged = FALSE;
# load("shinyPass.Rdata")
# 
# ui1 <- function(){
#   tagList(
#     div(id = "login",
#         wellPanel(textInput("userName", "Username"),
#                   passwordInput("passwd", "Password"),
#                   br(),actionButton("Login", "Log in"))),
#     tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
#   )}
# 
# ui2 <- function(){tagList(tabPanel("Test"))}
# 
# ui = (htmlOutput("page"))
# server = (function(input, output, session) {
#   
#   USER <- reactiveValues(Logged = Logged)
#   
#   observe({
#     if (USER$Logged == FALSE) {
#       if (!is.null(input$Login)) {
#         if (input$Login > 0) {
#           Username <- isolate(input$userName)
#           Password <- isolate(input$passwd)
#           Id.username <- which(my_username == Username)
#           Id.password <- which(my_password == Password)
#           if (length(Id.username) > 0 & length(Id.password) > 0) {
#             if (Id.username == Id.password) {
#               USER$Logged <- TRUE
#             }
#           }
#         }
#       }
#     }
#   })
#   observe({
#     if (USER$Logged == FALSE) {
#       
#       output$page <- renderUI({
#         div(class="outer",do.call(bootstrapPage,c("",ui1())))
#       })
#     }
#     if (USER$Logged == TRUE)
#     {
#       ##############END LOGIN STUFF, begin ui part#####################
#       

ui <- fluidPage(navbarPage("Planteimport - overvåking av fripassagerer",
             tabPanel('Akkumuleringskurver',
                      sidebarLayout(
                        sidebarPanel(width=2,
                                     dateRangeInput("daterange", "Tidsspann:",
                                                    start = "2014-01-01",
                                                    end   = Sys.Date()),
                                     selectInput("taxa", "Taxa", c("Insekter", "Karplanter"), selected = "Insekter"),
                                     uiOutput("container_species"),
                                     uiOutput("country"),
                                     selectInput("plotLevel", "Vad skal plottes?", c("Taxon", "Individuals"), selected = "Taxa"),
                                     selectInput("plotType", "Typ av plot", c("Area", "Line"), selected = "Area"),
                                     checkboxInput("removeJuveniles", "Ekskludere juveniler", TRUE),
                                     checkboxInput("alien", "Vis kun fremmande arter", FALSE),
                                     downloadButton('downloadPlot', 'Last ned figur')), 
                        #mainPanel(fluidRow(column(12, leafletOutput("mymap", height=600)))
                        mainPanel(imageOutput("acumPlotEng")
                                  #,fluidRow(column(1, offset=0,"Database dialog:"), column(11, verbatimTextOutput("nText")))
                                  )
                      )
             ),
             tabPanel("Vernalisering",
             mainPanel(imageOutput("vernPlot")),
             tableOutput("vernSpecTable")),
             tabPanel("Tabell Containere",
                      DT::dataTableOutput('containers')),
             tabPanel("Tabell Insektsfunn",
                      DT::dataTableOutput('insekt_records')),
             tabPanel("Insektarter funne",
                      DT::dataTableOutput('insekt_species'))
             
  ))




########SERVER PART HERE INSTEAD OF IN A SERVER FUNCTION CALL

server <- function(input, output, session){
  source("planteShinyFunctions.R")
  
  tags$head( tags$style(type="text/css", "text {font-family: 'Comic Sans MS'}"))
      
  con <- DBI::dbConnect(RPostgres::Postgres(), 
                        dbname = "planteimport", 
                        user = "shinyuser", 
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
      ggsave(file, plot = acumPlotEng(input = prepAcumPlot(), what = plotInput()$what), 
             device = "png",
             width = 20,
             height = 12,
             units = "cm",
             family = "Verdana",
             dpi = 600)
    }
  )
  
  select_categories <- function(){
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
    selectInput("container_species", "Importerte planter", c("All", sort(as.character(unique(select_categories()$species_cat)))), selected="All")
  })
  
  output$country <- renderUI({
    selectInput("country", "Importland", c("All", sort(as.character(unique(select_categories()$country_cat)))), selected="All")
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
    insect_records <- dbGetQuery(con, "SELECT container, subsample, species_latin, amount FROM insects.container_records")
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
    
    date_range <- paste("\n 
                        WHERE c.date_sampled >= '", 
                        start_time,
                        "' ", 
                        "AND c.date_sampled <= '", 
                        end_time, 
                        "'", 
                        sep="")
    
    
    if(input$taxa == "Insekter"){
      fetch.q <- paste0("SELECT r.*, c.country, s.alien, s.blacklist_cat
                        FROM insects.container_records r
                        LEFT JOIN common.containers c 
                          ON r.container = c.container
                        AND r.subsample = c.subsample
                        LEFT JOIN insects.species s 
                          ON r.species_latin = s.species_latin"
                        , date_range,
                        "\n")
    }
    
    if(input$taxa == "Karplanter"){
      fetch.q <- paste0("SELECT r.*, c.country, s.alien, s.blacklist_cat
                        FROM plants.container_records r
                        LEFT JOIN common.containers c 
                          ON r.container = c.container
                        AND r.subsample = c.subsample
                        LEFT JOIN plants.species s 
                          ON r.species_latin = s.species_latin"
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
    
    if(input$alien){
      
      fetch.q <- paste0(fetch.q, 
                        "AND s.alien IS True",
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
  
  prepAcumPlot <- reactive({
   
    tt <- fields()
    tt$blacklist_cat[tt$alien == F] <- "Stedegne"
    tt$blacklist_cat[is.na(tt$blacklist_cat)] <- "Ikke vurd."
    
    toPlot <-  tt %>% acumData(removeJuveniles = input$removeJuveniles)
    if(is.null(toPlot)){return(NULL)}
    
    toPlot
    
  })
  
  vernData <- reactive({
    
    vernq <- "SELECT foo.*, (foo.after_vern - foo.pre_vern)::integer as add_vern
    FROM(
    SELECT a.container, count(distinct b.species_latin)::integer as pre_vern, count(distinct(a.species_latin))::integer as after_vern
    FROM (SELECT DISTINCT ON (container, species_latin) container, species_latin
    FROM plants.container_records) a LEFT JOIN
    (SELECT DISTINCT ON (container, species_latin) container, species_latin
    FROM plants.container_records
    WHERE vernalisation IS NOT TRUE) b ON (a.species_latin = b.species_latin AND a.container = b.container)
    GROUP BY a.container) foo"
    suppressWarnings(vern <- dbGetQuery(con, vernq))
    
    vern %>% as_tibble()
    vern <- vern %>%
      select(-after_vern) %>%
      gather(vernalisation, no_species, -container)
    vern
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
    out <- list("what" =  input$plotLevel,
                "type" = input$plotType)
    
    out
  })
  
  # output$cumPlot <- renderPlot({
  #   
  #   #input <- read.table("planteimport2.csv", sep = ",", header = T)
  #   cumPlot(input = fields(), 
  #           what = plotInput()$what, 
  #           country = input$country, 
  #           species = input$container_species)
  #   
  # })
  # 
  
  
  output$acumPlotEng <- renderImage({

    # Read myImage's width and height. These are reactive values, so this
    # expression will re-run whenever they change.
    width  <- session$clientData$output_acumPlotEng_width
    height <- session$clientData$output_acumPlotEng_height
    
    # For high-res displays, this will be greater than 1
    pixelratio <- session$clientData$pixelratio
    
    # A temp file to save the output.
    outfile <- tempfile(fileext='.png')
    
    # Generate the image file
    png(outfile, width=width*pixelratio, height=height*pixelratio,
        res=120*pixelratio)
    
  
    g <- acumPlotEng(prepAcumPlot(),
                  what = plotInput()$what,
                  type = plotInput()$type)

    if(plotInput()$what == "Taxa") {
      g <- g + ggtitle("Kumulativt antall arter funne i kontainene, etter fremmedartskategori")
    }

    if(plotInput()$what == "Individer") {
      g <- g + ggtitle("Kumulativt antall individer funne i kontainene, etter fremmedartskategori")
    }

    g <- g + theme(text=element_text(family = "Verdana"))

    plot(g)
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         width = width,
         height = height,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
   




  output$vernPlot <- renderImage({
    
    # Read myImage's width and height. These are reactive values, so this
    # expression will re-run whenever they change.
    width  <- session$clientData$output_vernPlot_width
    height <- session$clientData$output_vernPlot_height
    
    # For high-res displays, this will be greater than 1
    pixelratio <- session$clientData$pixelratio
    
    # A temp file to save the output.
    outfile <- tempfile(fileext='.png')
    
    # Generate the image file
    png(outfile, width = width*pixelratio, height = height*pixelratio,
        res = 120*pixelratio)
    
    
    toPlot <- vernData()
    
    palette(NinaR::ninaPalette())
   g <-  ggplot(toPlot) +
      geom_bar(aes(x = container, y = no_species, group = vernalisation, fill = vernalisation), stat = "identity") +
      ylab("Antall arter") +
      xlab("Kontainere") +
      scale_fill_manual(name = "Vernalisering", values = c(3, 2), labels=c("Etter","Før")) +
      ggtitle("Antall plantearter som spirte føre og etter vernalisering") +
      theme(text = element_text(family = "Verdana"))
    
    plot(g)
  
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         width = width,
         height = height,
         alt = "This is alternate text")
  }, deleteFile = TRUE)

  
  


  output$vernSpecTable <- renderTable({

    vernSpeciesQ <- "SELECT spec_after FROM
      (SELECT distinct species_latin spec_after
      FROM plants.container_records
      WHERE vernalisation IS TRUE) _after LEFT JOIN
      (SELECT distinct species_latin spec_before
      FROM plants.container_records
      WHERE vernalisation IS NOT TRUE) _before
       ON _after.spec_after = _before.spec_before
      WHERE spec_before IS NULL
      ORDER BY spec_after"
    vernSpecies <- dbGetQuery(con, vernSpeciesQ)
    names(vernSpecies) <- "Taxa kun funne\netter vernalisering"

    vernSpecies
  }, rownames = T
  )
  
  
    }

#})

shinyApp(ui= ui, server= server)
