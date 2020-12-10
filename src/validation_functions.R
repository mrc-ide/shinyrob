geo_validation <- function(dat, options, source_files, input, output) {
  
    geo_choice <- options$unaids_geo %>%
      filter(title == split_title(input$geo_package)[1],
             organisation == split_title(input$geo_package)[2])
  
  source_files$areas_file <- ckan_fetch(geo_choice$url, store = "disk", key = input$api_key,
                                        path = tempfile(fileext = ".geojson"))
  
  
  shape_check <- reactive({
    
    validate_shape <- hintr:::do_validate_shape(source_files$areas_file)
    
    if(isTruthy(validate_shape$data)) {
      
      dat$anc <- NULL
      dat$art <- NULL
      
      areas <- naomi::read_area_merged(source_files$areas_file$path)
      
      dat$areas_merge <- areas %>%
        sf::st_drop_geometry() %>%
        dplyr::select(area_id, area_name, area_sort_order) %>%
        dplyr::mutate(
          area_label = paste0(area_name, "\n(", area_id, ")") %>%
            forcats::fct_reorder(area_sort_order)
        )
      
      "<b>Valid</b><br><br>"
    } else {
      safeError(validate_shape)
    }
    
  })
  
  output$shape_check <- renderText({
    shape_check()
  })
  
}

art_validation <- function(dat, options, source_files, input, output, dhis = FALSE) {


  if(dhis) {
    art_choice <- options$full_art %>%
      filter(name == input$art_dhis_package)
  } else {
    art_choice <- options$full_art %>%
      filter(title == split_title(input$art_package)[1],
             organisation == split_title(input$art_package)[2])
  }
  
  
  source_files$art_file <- ckan_fetch(art_choice$url, store = "disk", key = input$api_key,
                                        path = tempfile(fileext = ".csv"))
  
  # validate_shape <- hintr:::do_validate_shape(source_files$areas_file)
  # browser()
  
  # art_check <- reactive({
    
    # browser()
    
    validate_art <- try(hintr:::do_validate_programme(source_files$art_file, source_files$areas_file))
    
    if("try-error" %in% class(validate_art)) {
      # class(validate_art) <- "error"
      dat$art_plot <- ""
      # safeError(hintr:::do_validate_programme(source_files$art_file, source_files$areas_file))
    } else {
      
      art_raw <- naomi::read_art_number(source_files$art_file$path)
      
      quarter_vec <- crossing(year = 2010:2020, lb = "\n", q = paste0("Q", 1:4)) %>%
        mutate(calendar_quarter = paste0(q, lb, year)) %>%
        .$calendar_quarter
      
      dat$art <- art_raw %>%
        left_join(dat$areas_merge, by = "area_id") %>%
        group_by(area_id, area_label, calendar_quarter) %>%
        summarise(
          art_total = sum(art_current, na.rm = TRUE),
          art_adult_f = sum(art_current * as.integer(sex == "female" & age_group == "Y015_999"), na.rm = TRUE),
          art_adult_m = sum(art_current * as.integer(sex == "male" & age_group == "Y015_999"), na.rm = TRUE),
          art_adult = sum(art_current * as.integer(age_group == "Y015_999"), na.rm = TRUE),
          art_child = sum(art_current * as.integer(age_group == "Y000_014"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(year = year_labels(calendar_quarter_to_quarter_id(calendar_quarter))) %>%
        separate(calendar_quarter, into=c(NA, "quarter"), sep=-2) %>%
        mutate(quarter = factor(paste0(quarter, "\n", year), levels = quarter_vec))
      
      dat$art_plot <- dat$art %>%
        group_by(area_id, area_label, year) %>%
        summarise(
          art_total = sum(art_total),
          art_adult_f = sum(art_adult_f),
          art_adult_m = sum(art_adult_m),
          art_adult = sum(art_adult),
          art_child = sum(art_child)
        ) %>%
        mutate(
          art_adult_sex_ratio = art_adult_f / art_adult_m,
          art_adult_child_ratio = art_adult / art_child,
          art_prop_u15 = round(art_child/(art_adult+art_child),3),
          x_axis_variable = year
        )
      
      
     # art_text <- "<b>Valid</b><br>"
    }
    
  # })
  
  output$art_check <- renderUI({
    if("try-error" %in% class(validate_art)) {
      # class(validate_art) <- "error"
      # safeError(hintr:::do_validate_programme(source_files$art_file, source_files$areas_file))
      HTML(paste0("<span style=color:red; font-weight:700>", validate_art[[1]], "</span>"))
    } else {
      HTML("<b>Valid</b>")
    }
  })
  
  output$art_version <- renderUI({
    HTML(paste("<br>Last modified:", ymd_hms(art_choice$last_modified), "<br><br>"))
  })
  
}

anc_validation <- function(dat, options, source_files, input, output, dhis = FALSE) {
  
  if(dhis) {
    anc_choice <- options$full_anc %>%
      filter(name == input$anc_dhis_package)
  } else {
    anc_choice <- options$full_anc %>%
      filter(title == split_title(input$anc_package)[1],
             organisation == split_title(input$anc_package)[2])
  }
  
  # pkg_selected <- pkg_options_unaids$results %>%
  #   # filter(title == pkg_choices_unaids$title)
  #   filter(title == package)
  # 
  # pkg_resources <- pkg_selected$resources[[1]]
  # 
  # areas_url <- pkg_resources$url[pkg_resources$resource_type == "inputs-unaids-ancgraphic"]
  
  source_files$anc_file <- ckan_fetch(anc_choice$url, store = "disk", key = input$api_key,
                                        path = tempfile(fileext = ".csv"))
  
  # validate_shape <- hintr:::do_validate_shape(source_files$areas_file)
  
  validate_anc <- try(hintr:::do_validate_anc(source_files$anc_file, source_files$areas_file))
  
  if("try-error" %in% class(validate_anc)) {
    
    dat$anc <- ""
    
  } else {
    
    anc_raw <- naomi::read_anc_testing(source_files$anc_file$path)
    
    dat$anc <- anc_raw %>%
      left_join(dat$areas_merge, by = "area_id") %>%
      mutate(
        area_label = paste0(area_name, "\n(", area_id, ")") %>%
          fct_reorder(area_sort_order),
        anc_total_pos = anc_known_pos + anc_tested_pos,
        anc_status = anc_known_pos + anc_tested,
        anc_prevalence = anc_total_pos / anc_status,
        anc_art_among_known = anc_already_art / anc_known_pos,
        anc_art_coverage = anc_already_art / anc_total_pos
      ) %>%
      arrange(area_id, year)
    
    "<b>Valid</b>"
  }
  
  output$anc_check <- renderUI({
    if("try-error" %in% class(validate_anc)) {
      HTML(paste0("<span style=color:red; font-weight:700>", validate_anc[[1]], "</span>"))
    } else {
      HTML("Valid</b><br>")
    }
  })
  
  output$anc_version <- renderUI({
    HTML(paste("<br>Last modified:", ymd_hms(anc_choice$last_modified), "<br><br>"))
  })
  
}
