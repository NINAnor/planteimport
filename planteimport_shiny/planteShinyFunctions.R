
## Cumulative plot function

cumPlot <- function(input, 
                    what = c("Taxa", "Individer"),
                    species = "All",
                    country = "All",
                    col = NinaR::ninaPalette()[3]){
  what = match.arg(what)
  
  aggData <- input %>% 
    arrange(container,subsample) %>% 
    mutate(noContainer = cumsum(!duplicated(container)),
           noInd = cumsum(amount),
           noSpec = cumsum(!duplicated(species_latin))) %>% 
    group_by(container) %>% 
    summarize(noContainer = last(noContainer),
              noInd = last(noInd),
              noSpec = last(noSpec)) 
  
  g <- ggplot(aggData)  
  
  if(what == "Taxa"){
    if(nrow(aggData) == 1){
     g <-  g + geom_point(mapping = aes(x = noContainer,
                                   y = noSpec, group = 1),
                     col = col,
                     size = 2) 
    } else
     g <-  g + geom_line(mapping = aes(x = noContainer,
                                  y = noSpec),
                    col = col,
                    size = 2) 
  } else
    
    if(what == "Individer"){
      
      if(nrow(aggData) == 1){
        g <-  g + geom_point(mapping = aes(x = noContainer,
                                     y = noInd, group = 1),
                       col = col,
                       size = 2) 
      } else
        g <- g + geom_line(mapping = aes(x = noContainer,
                                    y = noInd),
                      col = col,
                      size = 2) 
    }
  
  g + labs(title = paste0(what, " from shipping country: ", country, ", shipping species: ", species),
           x = "Number of containers",
           y = paste("Number of", what))
  
} 


cumPlot2 <- function(input,
                     what = c("Taxa", "Individer"),
                     col =  c(ninaLogoPalette(), NinaR::ninaPalette())){
  what = match.arg(what)

  g <- ggplot(input)

  myColors <- c(ninaLogoPalette(), NinaR::ninaPalette())
  names(myColors) <- c("Stedegne", "LO","Ikke vurd.", "NR", "NK", "PH", "HI", "SE")
  colScale <- scale_fill_manual(name = "Fremmed-\nartkategori", values = myColors)


  if(what == "Taxa"){
    g <- g +
      geom_area(aes(x = noContainer, y = noSpec, fill = blacklist_cat)) +
      ylab("Artsantall")
  } else
  {
    g <- g +
      geom_area(aes(x = noContainer, y = noInd, fill = blacklist_cat)) +
      ylab("Individantall")
  }
  g <- g +
    colScale +
    xlab("Kontainer")
  g <- g 

  g
}

acumData <- function(input,
                     removeJuveniles = T){
  
  
  if(removeJuveniles){
    input <- input %>% 
      filter(!grepl("juv", species_latin))
  }
  
 # input$blacklist_cat[input$blacklist_cat == "Ikke vurd."] <- "NR" 
  
  aggData <- input %>%
    arrange(container, subsample) %>% 
    mutate(noContainer = cumsum(!duplicated(container))) %>%
    group_by(blacklist_cat) %>% 
    mutate(noInd = cumsum(amount),
           noSpec = cumsum(!duplicated(species_latin))) %>% 
    group_by(container, blacklist_cat) %>% 
    summarize(noContainer = last(noContainer),
              noInd = last(noInd),
              noSpec = last(noSpec)) 
  
  
  filler <- expand.grid(unique(aggData$noContainer), unique(aggData$blacklist_cat), stringsAsFactors = F)
  names(filler) <- c("noContainer", "blacklist_cat")
  #filler$noInd <- 0
  #filler$noSpec <- 0
  
  hm <- aggData %>% 
    right_join(filler, by = c("noContainer" = "noContainer", "blacklist_cat" = "blacklist_cat")) %>%
    arrange(noContainer, blacklist_cat)
  
  
  hm <- hm %>% 
    group_by(noContainer) %>% 
    mutate(container = ifelse(noContainer == min(noContainer) & is.na(container), min(container, na.rm = T), container)) %>%
    ungroup() %>%
    mutate(noInd = ifelse(noContainer == min(noContainer) & is.na(noInd), 0, noInd),
           noSpec = ifelse(noContainer == min(noContainer) & is.na(noSpec), 0, noSpec)) %>%
    arrange(noContainer, blacklist_cat) %>%
    group_by(blacklist_cat) %>% 
    fill(noInd, noSpec) %>% 
    arrange(container, blacklist_cat) %>% 
    ungroup()
  
  
  hm
}

acumPlot <- function(input, 
                     type = c("Area", "Line"),
                     what = c("Taxon", "Individuals"),
                     col =  c(ninaLogoPalette(), NinaR::ninaPalette()),
                     lwd = 1.2,
                     ...){
  what = match.arg(what)
  type = match.arg(type)
  
  if(what == "Taxon"){
    input <- input %>% 
      ungroup() %>%
      mutate(blacklist_cat = fct_reorder(blacklist_cat, noSpec, .fun = max, .desc = T))
  } else {
    input <- input %>% 
      ungroup() %>%
      mutate(blacklist_cat = fct_reorder(blacklist_cat, noInd, .fun = max, .desc = T))
  }
  
  g <- ggplot(input)
  
  myColors <- c(ninaLogoPalette(), NinaR::ninaPalette())
  names(myColors) <- c("Stedegne", "LO","Ikke vurd.", "NR", "NK", "PH", "HI", "SE")
  colScale <- scale_fill_manual(name = "Fremmed-\nartkategori", values = myColors)
  colFill <- scale_color_manual(name = "Fremmed-\nartkategori", values = myColors)
  
  
  if(type == "Line"){
    
    if(what == "Taxon"){
      g <- g + 
        geom_line(aes(x = noContainer, y = noSpec, color = blacklist_cat), lwd = lwd, ...) +
        ylab("Artsantall")
    } else
    {   
      g <- g + 
        geom_line(aes(x = noContainer, y = noInd, color = blacklist_cat), lwd = lwd, ...) +
        ylab("Individantall")
    }
  } else
    
    if(what == "Taxon"){
      g <- g + 
        geom_area(aes(x = noContainer, y = noSpec, fill = blacklist_cat), ...) +
        ylab("Artsantall")
    } else 
    {
      g <- g +  
        geom_area(aes(x = noContainer, y = noInd, fill = blacklist_cat), ...) +
        ylab("Individantall")
    }
  
  if(type == "Line"){
    g <- g +
      colFill +
      xlab("Antall prøvetatte kontainere")
  } else {
    g <- g +
      colScale +
      xlab("Antall prøvetatte kontainere")
  }
  
  
  g
}


acumPlotEng <- function(toPlot, 
                        type = c("Area", "Line"),
                        what = c("Taxon", "Individuals"),
                        subheader = NULL,
                        col =  c(ninaLogoPalette(), NinaR::ninaPalette()),
                        lwd = 1.2,
                        ...){
  what = match.arg(what)
  type = match.arg(type)
  

  if(what == "Taxon"){toPlot <- toPlot %>% 
      ungroup() %>%
      mutate(blacklist_cat = fct_reorder(blacklist_cat, noSpec, .fun = max, .desc = T))
    } else {
      toPlot <- toPlot %>% 
      ungroup() %>%
      mutate(blacklist_cat = fct_reorder(blacklist_cat, noInd, .fun = max, .desc = T))
  }
  levels(toPlot$blacklist_cat)[levels(toPlot$blacklist_cat) =="Stedegne"] <- "Native"
  levels(toPlot$blacklist_cat)[levels(toPlot$blacklist_cat) =="Ikke vurd."] <- "Not yet assessed"
  levels(toPlot$blacklist_cat)[levels(toPlot$blacklist_cat) =="LO"] <- "LO - Low impact"
  levels(toPlot$blacklist_cat)[levels(toPlot$blacklist_cat) =="NR"] <- "NR - Not assessed by definition"
  levels(toPlot$blacklist_cat)[levels(toPlot$blacklist_cat) =="NK"] <- "NK - No known impact"
  levels(toPlot$blacklist_cat)[levels(toPlot$blacklist_cat) =="PH"] <- "PH - Potentially high impact"
  levels(toPlot$blacklist_cat)[levels(toPlot$blacklist_cat) =="HI"] <- "HI - High impact"
  levels(toPlot$blacklist_cat)[levels(toPlot$blacklist_cat) =="SE"] <- "SE - Severe impact"
  

  g <- ggplot(toPlot)
  
  myColors <- c(ninaLogoPalette(), NinaR::ninaPalette())
  names(myColors) <- c("Native", 
                       "Not yet assessed", 
                       "LO - Low impact", 
                       "NR - Not assessed by definition", 
                       "PH - Potentially high impact", 
                       "HI - High impact", 
                       "NK - No known impact", 
                       "SE - Severe impact")
  colScale <- scale_fill_manual(name = "IAS-\ncategory", values = myColors)
  colFill <- scale_color_manual(name = "IAS-\ncategory", values = myColors)
  
  if(type == "Line"){
    
    if(what == "Taxon"){
      g <- g + 
        geom_line(aes(x = noContainer, y = noSpec, color = blacklist_cat), lwd = lwd, ...) +
        ylab("No. Species")
    } else
    {   
      g <- g + 
        geom_line(aes(x = noContainer, y = noInd, color = blacklist_cat), lwd = lwd, ...) +
        ylab("No. Individuals")
    }
  } else
    
    if(what == "Taxon"){
      g <- g + 
        geom_area(aes(x = noContainer, y = noSpec, fill = blacklist_cat), ...) +
        ylab("No. Species")
    } else 
    {
      g <- g +  
        geom_area(aes(x = noContainer, y = noInd, fill = blacklist_cat), ...) +
        ylab("No. Individuals")
    }
  
  if(type == "Line"){
    g <- g +
      colFill +
      xlab("No. sampled containers")
  } else {
    
    g <- g +
      colScale +
      xlab("No. sampled containers")
  }
  
  g <- g + 
    labs(
      title = "Stowaways in imported containers of ornamental plants to Norway",
      subtitle = subheader
    )
  
  g
}
