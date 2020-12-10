introduction_server <- function(input, output, session) {
  
  unaids_api_return <- reactiveValues()
  options <- reactiveValues()
  dat <<- reactiveValues()
  source_files <- reactiveValues()
  
  observeEvent(input$api_key,{
    toggleState("api_button", str_length(input$api_key) == 36)
  })
  
  observeEvent(input$api_button, {
    shinyjs::show("wait")
    disable("api_button")
    
  })
  
  observeEvent(input$api_button, {
    
    res <- GET("https://adr.fjelltopp.org/api/3/action/package_search?q=type:inputs-unaids-estimates&hide_inaccessible_resources=true&rows=100", add_headers(Authorization = input$api_key))
    
    unaids_api_return$res_unaids <- fromJSON(content(res, "text"))$result
    
    options$unaids_geo <- get_unaids_geo_options(unaids_api_return$res_unaids)
    # dhis_options <- get_dhis_options(input$api_key)
    
    # geographic_choice <- unaids_options$geographic
    # art_choice <- bind_rows(unaids_options$art, dhis_options$art)
    # anc_choice <- bind_rows(unaids_options$anc, dhis_options$anc)
    
    update_options(options$unaids_geo, "geo_package", session)
    # update_options(art_choice, "art_package", session)
    # update_options(anc_choice, "anc_package", session)

    shinyjs::hide("wait")
    
  })

  observeEvent(input$geo_package, {

    req(!input$geo_package %in% c("No ADR access key entered", "No packages available", "Choose a package"))
    
    disable("art_package")
    disable("anc_package")
    shinyjs::hide("art_dhis_package")
    shinyjs::hide("anc_dhis_package")
    
    output$shape_check <- NULL
    output$art_check <- NULL
    output$art_version <- NULL
    output$anc_check <- NULL
    output$anc_version <- NULL
    shinyjs::reset("plot_quarter")

    
    shinyjs::show("validation_div")

  })
  
  observeEvent(input$geo_package, {
    
    req(!input$geo_package %in% c("No ADR access key entered", "No packages available", "Choose a package"))
    
    # browser()
    
    dhis_options <- get_dhis_options(input)
    
    geo_choice <- options$unaids_geo %>%
      filter(title == split_title(input$geo_package)[1], organisation == split_title(input$geo_package)[2])
    
    options$unaids_art <- get_unaids_art_options(unaids_api_return$res_unaids) %>%
      filter(area_name == geo_choice$area_name,
             organisation == split_title(input$geo_package)[2])
    
    options$unaids_anc <- get_unaids_anc_options(unaids_api_return$res_unaids) %>%
      filter(area_name == geo_choice$area_name,
             organisation == split_title(input$geo_package)[2])
    
    if(nrow(dhis_options$art)) {
      
      options$full_art <- options$unaids_art %>%
          bind_rows(dhis_options$art %>% filter(area_name == geo_choice$area_name,
                                                organisation == split_title(input$geo_package)[2]))
      
    } else {
    
      options$full_art <- options$unaids_art 
    }
    
    if(nrow(dhis_options$anc)) {
      
      options$full_anc <- options$unaids_anc %>%
        bind_rows(dhis_options$anc %>% filter(area_name == geo_choice$area_name,
                                              organisation == split_title(input$geo_package)[2]))
      
    } else {
      
      options$full_anc <- options$unaids_anc 
    }
    
    update_options(options$full_art, "art_package", session)
    update_options(options$full_anc, "anc_package", session)
    
    shinyjs::enable("art_package")
    shinyjs::enable("anc_package")
    
  })
  
  observeEvent(input$geo_package, {
    
    req(!input$geo_package %in% c("No ADR access key entered", "No packages available", "Choose a package"))
    
    dat$areas_merge <- ""
    
    geo_validation(dat, options, source_files, input, output)
    
  })
  
  observeEvent(input$art_package, {
    
    req(!input$art_package %in% c("No ADR access key entered", "No packages available", "Choose a package"))
    
    dat$art <- ""
    
    if(filter(options$full_art, title == split_title(input$art_package)[1], organisation == split_title(input$art_package)[2])$source == "inputs-unaids-estimates") {
      
      shinyjs::hide("art_dhis_package")
      
    }
  })
  
  observeEvent(input$art_package, {
    
    req(!input$art_package %in% c("No ADR access key entered", "No packages available", "Choose a package"))
    
    if(filter(options$full_art, title == split_title(input$art_package)[1], organisation == split_title(input$art_package)[2])$source == "inputs-unaids-estimates") {
      
      art_validation(dat, options, source_files, input, output)
      
    } else {
      
      options$dhis_art_pull <- options$full_art %>% 
        filter(title == split_title(input$art_package)[1], 
               organisation == split_title(input$art_package)[2])
      
      update_dhis_pull_options(options$dhis_art_pull, "art_dhis_package", session)
      
      shinyjs::show("art_dhis_package")
      
    }
    
    
    
  })
  
  observeEvent(input$anc_package, {
    
    req(!input$anc_package %in% c("No ADR access key entered", "No packages available", "Choose a package"))
    
    dat$anc <- ""
    
    if(filter(options$full_anc, title == split_title(input$anc_package)[1], organisation == split_title(input$anc_package)[2])$source == "inputs-unaids-estimates") {
      
      shinyjs::hide("anc_dhis_package")
      
    }
    
  })
  
  observeEvent(input$anc_package, {
    
    req(!input$anc_package %in% c("No ADR access key entered", "No packages available", "Choose a package"))
    
    if(filter(options$full_anc, title == split_title(input$anc_package)[1], organisation == split_title(input$anc_package)[2])$source == "inputs-unaids-estimates") {
      
      anc_validation(dat, options, source_files, input, output)
      
      
    } else {
      
      options$dhis_anc_pull <- options$full_anc %>% 
        filter(title == split_title(input$anc_package)[1], 
               organisation == split_title(input$anc_package)[2])
      
      update_dhis_pull_options(options$dhis_anc_pull, "anc_dhis_package", session)
      
      shinyjs::show("anc_dhis_package")
      
    }
    
  })
  

  observeEvent(input$art_dhis_package, {
    
    req(!input$art_dhis_package %in% c("No ADR access key entered", "No DHIS datasets available", "Choose a DHIS dataset"))
    
    art_validation(dat, options, source_files, input, output, dhis=TRUE)
    
  })
  
  observeEvent(input$anc_dhis_package, {

    req(!input$anc_dhis_package %in% c("No ADR access key entered", "No DHIS datasets available", "Choose a DHIS dataset"))
    
    anc_validation(dat, options, source_files, input, output, dhis=TRUE)

  })
  

  output$validation <- renderUI({
    
    tagList(
      HTML("<h4>Geographic data</h4>"),
      htmlOutput("shape_check"),
      HTML("<h4>ART data</h4>"),
      htmlOutput("art_check"),
      htmlOutput("art_version"),
      HTML("<h4>ANC data</h4>"),
      htmlOutput("anc_check"),
      htmlOutput("anc_version")
    )


  })
}
