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


##Todo
# Figure out how to have consistent legend order while automatically reorder the area graph. Needs to be responsive to what to plot
#
#

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

ui <- fluidPage(navbarPage("Survey results of stowaways in imported ornamental plants to Norway",
             tabPanel('Cumulative graphs',
                      sidebarLayout(
                        sidebarPanel(width=2,
                                     dateRangeInput("daterange", "Sampling period:",
                                                    start = "2014-01-01",
                                                    end   = Sys.Date()),
                                     selectInput("taxa", "Taxa", c("Arthropods", "Vascular plants"), selected = "Arthropods"),
                                     uiOutput("container_species"),
                                     uiOutput("country"),
                                     selectInput("plotLevel", "Data to plot", c("Taxon", "Individuals"), selected = "Taxon"),
                                     selectInput("plotType", "Type of plot", c("Area", "Line"), selected = "Area"),
                                     checkboxInput("removeJuveniles", "Exclude juveniles", TRUE),
                                     checkboxInput("alien", "Show only alien species", FALSE),
                                     downloadButton('downloadPlot', 'Download figure')), 
                        #mainPanel(fluidRow(column(12, leafletOutput("mymap", height=600)))
                        mainPanel(fluidRow(mainPanel(strong("Starting in the year 2014, NINA performs yearly spot surveys on semitrailers arriving at major garden centers in Norway.
                                                     Soil samples are taken from imported potted plants and searched for stowaway arthropods and vascular plants. Living arthropods are extracted using a Berlese/Tullgren-funnel and identified under stereo-microscope.
                                                     The remaining soil is then planted in greenhouses for germination of viable stowaway seeds. This process includes a cold treatment (vernalisation) with a second germination period to induce germination of cold-adapted plants.
                                                     Identification of seedlings is done to species level when possible. Further information of the project is available at:", a("Link", href="https://www.nina.no/V%C3%A5re-fagomr%C3%A5der/Fremmede-arter/Planteimport-og-fremmede-arter")
                                                            ),
                                                     style = "background: lightgrey;
                                                              border-style: solid;
                                                              border-color: grey;
                                                              border-radius: 5px"
                                                     )
                                           )
                                 #fluidRow(column(1, offset=0,"Database dialog:"), column(11, verbatimTextOutput("nText")))
                                  , plotOutput("acumPlotEng"),
                                 fluidRow(mainPanel(strong("The figure above shows the cumulative number of distinct taxa (or individuals) of stowaways found in imported plant containers withing the survey program.
                                          The species are colorized according to their alien species risk assessment status (if available). More information on the alien species risk assessment can be found at:", a("Link", href="https://www.biodiversity.no/alien-species?Key=872"))
                                                    )
                                 )
                                 )
                      )
             ),
             tabPanel("Vernalisation",
             mainPanel(imageOutput("vernPlot")),
             tableOutput("vernSpecTable")),
             tabPanel("Sampled containers",
                      DT::dataTableOutput('containers')),
             tabPanel("Arthropod data",
                      DT::dataTableOutput('insect_records')),
             tabPanel("Arthropod species",
                      DT::dataTableOutput('insect_species')),
             tabPanel("Plant data",
                      DT::dataTableOutput('plant_records')),
             tabPanel("Plant species",
                      DT::dataTableOutput('plant_species'))
             
  ))




########SERVER PART HERE INSTEAD OF IN A SERVER FUNCTION CALL

server <- function(input, output, session){
  source("planteShinyFunctions.R")
  load("shinyPass.Rdata")
  
  tags$head( tags$style(type="text/css", "text {font-family: 'Comic Sans MS'}"))
      
  con <- DBI::dbConnect(RPostgres::Postgres(), 
                        dbname = "planteimport", 
                        user = my_username, 
                        password = my_password, 
                        host = "ninradardata01.nina.no")
 
  
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
      ggsave(file, plot = acumPlotEng(toPlot = prepAcumPlot(),
                                      what = plotInput()$what,
                                      type = plotInput()$type,
                                      subheader = input$taxa), 
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
    selectInput("container_species", "Imported item", c("All", sort(as.character(unique(select_categories()$species_cat)))), selected="All")
  })
  
  output$country <- renderUI({
    selectInput("country", "Export country", c("All", sort(as.character(unique(select_categories()$country_cat)))), selected="All")
  })
  
  
  #output$taxa<- renderUI({
  #  selectInput('taxa', 'Species', c("Insekter", "Planter"), selected = "Insekter")
  #})
  
  
  output$containers <- DT::renderDataTable({
    containers <- dbReadTable(con, Id(schema = "views", table = "containers"))
    #out <- containers[names(containers) != "id"]
    out <- containers
    out
  })
  
  output$insect_species <- DT::renderDataTable({
    species <- dbReadTable(con, Id(schema = "views", table = "insect_species"))
    species
  })
  
  output$insect_records <- DT::renderDataTable({
    insect_records <- dbReadTable(con, Id(schema = "views", table = "insect_container_records"))
    insect_records
  })
  
  output$plant_species <- DT::renderDataTable({
    species <- dbReadTable(con, Id(schema = "views", table = "plant_species"))
    species
  })
  
  
  output$plant_records <- DT::renderDataTable({
    plant_records <- dbReadTable(con, Id(schema = "views", table = "plant_container_records"))
    plant_records
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
    
    date_range <- paste0("\n 
                        WHERE c.date_sampled >= '", 
                        start_time,
                        "' ", 
                        "AND c.date_sampled <= '", 
                        end_time, 
                        "'")
    
    
    if(input$taxa == "Arthropods"){
      fetch.q <- paste0("SELECT r.*, c.country, s.alien, s.blacklist_cat
                        FROM insects.container_records r
                        LEFT JOIN common.containers c 
                          ON r.container = c.container
                        AND r.subsample = c.subsample
                        LEFT JOIN insects.species s 
                          ON r.species_latin = s.species_latin",
                         date_range)
    }
    
    if(input$taxa == "Vascular plants"){
      fetch.q <- paste0("SELECT r.*, c.country, s.alien, s.blacklist_cat
                        FROM plants.container_records r
                        LEFT JOIN common.containers c 
                          ON r.container = c.container
                        AND r.subsample = c.subsample
                        LEFT JOIN plants.species s 
                          ON r.species_latin = s.species_latin", 
                        date_range)
    }
    
    if(input$container_species != "All"){
      
      fetch.q <- paste0(fetch.q, 
                        "\nAND c.species_latin = '", 
                        input$container_species, 
                        "'")
    }
    
    
    if(input$country != "All"){
      
      fetch.q <- paste0(fetch.q, 
                        "\nAND c.country = '",
                        input$country, 
                        "'")
    }
    
    if(input$alien){
      
      fetch.q <- paste0(fetch.q, 
                        "\n AND s.alien IS True"
                        )
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
    
    toPlot <- tt %>% acumData(removeJuveniles = input$removeJuveniles)
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
    
  
    g <- acumPlotEng(toPlot = prepAcumPlot(),
                  what = plotInput()$what,
                  type = plotInput()$type,
                  subheader = input$taxa)

    if(plotInput()$what == "Taxa") {
      g <- g + ggtitle("Cumulative number of species found in containers, by risk assessment category.")
    }

    if(plotInput()$what == "Individer") {
      g <- g + ggtitle("Cumulative number of individuals found in containers, by risk assessment category.")
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
    
   
   g <-  ggplot(toPlot) +
      geom_bar(aes(x = container, y = no_species, group = vernalisation, fill = vernalisation), stat = "identity") +
      ylab("No. species") +
      xlab("Containers") +
      ggtitle("Number of germinating vascular plant species before and after cold-treatment") +
      theme(text = element_text(family = "Verdana")) +
     scale_fill_nina(palette = "blue-orange", name = "Vernalisation", labels = c("After", "Before"),
                     reverse = T)
    
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
    names(vernSpecies) <- "Taxa found only after vernalisation"

    vernSpecies
  }, rownames = T
  )
  
  
    }

#})

shinyApp(ui= ui, server= server)
