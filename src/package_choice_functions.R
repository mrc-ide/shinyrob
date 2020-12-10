get_unaids_geo_options <- function(res_unaids) {
  
  # res_unaids <- GET("https://adr.fjelltopp.org/api/3/action/package_search?q=type:inputs-unaids-estimates&hide_inaccessible_resources=true&rows=100", add_headers(Authorization = api_key))
  
  # res_unaids <<- fromJSON(content(res_unaids, "text"))$result
  
  
  if(length(res_unaids$results)) {
    
    pkg_options_unaids_geographic <- res_unaids$results %>%
      filter(organization$name != "naomi-development-team") %>%
      group_split(id) %>%
      lapply(function(x) {
        
        
        if(x$num_resources) {
          resources <- x$resources[[1]]
          resources <- resources %>%
            # filter(resource_type  %in% c("naomi-anc", "naomi-art", "naomi-geographic"))
            filter(resource_type== "inputs-unaids-geographic")
          
          
          if(nrow(resources) == 1) {
            data.frame("title" = x$title, "organisation" = x$organization$title, "id" = x$id, "source" = "inputs-unaids-estimates", "type" = "geographic", "area_name" = x$`geo-location`, "url" = resources$url)
          }
        }
        
        
      }) %>% bind_rows
    
  } else {
    pkg_options_unaids_geographic <- NULL
  }
  
  return(pkg_options_unaids_geographic)
  
}

get_unaids_art_options <- function(res_unaids) {

  if(length(res_unaids$results)) {

    pkg_options_unaids_art <- res_unaids$results %>%
      filter(organization$name != "naomi-development-team") %>%
      group_split(id) %>%
      lapply(function(x) {


        if(x$num_resources) {
          resources <- x$resources[[1]]
          resources <- resources %>%
            # filter(resource_type  %in% c("naomi-anc", "naomi-art", "naomi-geographic"))
            filter(resource_type== "inputs-unaids-art")


          if(nrow(resources) == 1) {
            data.frame("title" = x$title, "organisation" = x$organization$title, "id" = x$id, "source" = "inputs-unaids-estimates", "type" = "art", "area_name" = x$`geo-location`, "url" = resources$url, "last_modified" = resources$last_modified)
          }
        }


      }) %>% bind_rows

  } else {
    pkg_options_unaids_art <- NULL
  }
  
}


get_unaids_anc_options <- function(res_unaids) {

    if(length(res_unaids$results)) {

    pkg_options_unaids_anc <- res_unaids$results %>%
      filter(organization$name != "naomi-development-team") %>%
      group_split(id) %>%
      lapply(function(x) {


        if(x$num_resources) {
          resources <- x$resources[[1]]
          resources <- resources %>%
            # filter(resource_type  %in% c("naomi-anc", "naomi-anc", "naomi-geographic"))
            filter(resource_type== "inputs-unaids-anc")


          if(nrow(resources) == 1) {
            data.frame("title" = x$title, "organisation" = x$organization$title, "id" = x$id, "source" = "inputs-unaids-estimates", "type" = "anc", "area_name" = x$`geo-location`, "url" = resources$url, "last_modified" = resources$last_modified)
          }
        }


      }) %>% bind_rows

  } else {
    pkg_options_unaids_anc <- NULL
  }
}

get_dhis_options <- function(input) {
  
  res_dhis_art <- GET("http://adr.unaids.org/api/3/action/package_search?hide_inaccessible_resources=true&fq=tags:dhis2+tags:art&rows=100", add_headers(Authorization = input$api_key))
  
  pkg_options_dhis_art <<- fromJSON(content(res_dhis_art, "text"))$result
  
  if(length(pkg_options_dhis_art$results)) {
    
    pkg_options_dhis_art <- pkg_options_dhis_art$results %>%
      filter(organization$name != "fjelltopp") %>%
      group_split(id) %>%
      lapply(function(x) {
        
        
        if(x$num_resources) {
          resources <- x$resources[[1]]
          resources %>%
            filter(!str_detect(name, "Crosswalk"))
          
          data.frame(title=x$title, "organisation" = x$organization$title, name = resources$name, id = x$id, "source" = "dhis", "type" = "art", "area_name" = x$`geo-location`, "url" = resources$url, "last_modified" = resources$last_modified)
        } else {
          NULL
        }
        
        
      }) %>%
      bind_rows
    
  } else {
    pkg_options_dhis_art <- NULL
  }
  
  res_dhis_anc <- GET("http://adr.unaids.org/api/3/action/package_search?hide_inaccessible_resources=true&fq=tags:dhis2+tags:anc&rows=100", add_headers(Authorization = input$api_key))
  
  pkg_options_dhis_anc <<- fromJSON(content(res_dhis_anc, "text"))$result
  
  if(length(pkg_options_dhis_anc$results)) {
    
    pkg_options_dhis_anc <- pkg_options_dhis_anc$results %>%
      filter(organization$name != "fjelltopp") %>%
      group_split(id) %>%
      lapply(function(x) {
        
        
        if(x$num_resources) {
          resources <- x$resources[[1]]
          resources %>%
            filter(!str_detect(name, "Crosswalk"))
          
          data.frame(title=x$title, "organisation" = x$organization$title, name = resources$name, id = x$id, "source" = "dhis", "type" = "anc", "area_name" = x$`geo-location`, "url" = resources$url, "last_modified" = resources$last_modified)
        } else {
          NULL
        }
        
        
      }) %>%
      bind_rows
    
  } else {
    pkg_options_dhis_anc <- NULL
  }
  
  
  df <- list()
  df$art <- pkg_options_dhis_art
  df$anc <- pkg_options_dhis_anc
  
  df
}

update_options <- function(options, package_type, session) {
  
  # browser()
  
  if (nrow(options)) {
    enable(package_type)
    
    titles <- sort(unique(paste0(options$title, " | ", options$organisation)))
    
    if(package_type != "geo_package" & length(unique(options$source)) != 1) {
      titles <- sort(
        factor(titles, levels = c(grep("Inputs UNAIDS Estimates 2021", titles, value = TRUE), grep("Inputs UNAIDS Estimates 2021", titles, value = TRUE, invert = TRUE)))
      )
    }
    
    updateSelectInput(session, package_type,
                      choices = c("Choose a package", as.character(titles)),
                      selected = "Choose a package"
    )
    
  }
  else {
    options <- data.frame(title="") 
    options$title <-  "No packages available"
    
    updateSelectInput(session, package_type,
                      choices = options$title,
                      selected = head(options$title, 1)
    )
    
    disable(package_type)
  }
}

update_dhis_pull_options <- function(options, package_type, session) {
  
  if (nrow(options)) {
    enable(package_type)
    
    updateSelectInput(session, package_type,
                      choices = c("Choose a DHIS dataset", sort(options$name, decreasing=TRUE)),
                      selected = "Choose a DHIS dataset"
    )
    
  }
  else {
    options <- data.frame(name="") 
    options$name <-  "No DHIS datasets available"
    
    updateSelectInput(session, package_type,
                      choices = options$name,
                      selected = head(options$name, 1)
    )
    
    disable(package_type)
  }
}

split_title <- function(input) {
  
  unlist(str_split(input, " \\| "))
  
}