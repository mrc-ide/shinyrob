download_server <- function(input, output, session) {
  
  plts <<- reactiveValues()
  
  observeEvent(dat$art_plot, {
     
    threshold <- input$art_count_threshold/100
    
    p <- dat$art_plot %>%
      group_by(area_label) %>%
      mutate(x_axis_variable_next = lead(x_axis_variable),
             art_adult_next = lead(art_adult),
             n = cur_group_id(),
             n = floor(n/12)) %>%
      select(area_label, x_axis_variable, art_adult, x_axis_variable_next, art_adult_next, n) %>%
      mutate(art_diff = art_adult_next/art_adult,
             colour_line =
               ifelse(
                 (art_diff >= (1 + threshold) | art_diff <= (1 - threshold)) & art_adult >= 100,
                 1, 0),
             colour_line = ifelse(is.infinite(art_diff) | art_diff > 200, 1, colour_line),
             colour_line = factor(colour_line)
      )  %>%
      ungroup %>%
      group_by(n) %>%
      group_split()
    
    plts$art_count <- lapply(p, function(p) {
      
      plot <- p %>%
        ggplot() +
        geom_segment(aes(x=as.numeric(x_axis_variable), xend = as.numeric(x_axis_variable_next), y = art_adult, yend = art_adult_next, colour = colour_line), size=1.3, show.legend = FALSE) +
        geom_point(aes(x=as.numeric(x_axis_variable), y=art_adult), size=2) +
        scale_colour_manual(values=c("0" = "black", "1" = "red")) +
        facet_rep_wrap(~area_label, ncol = 3, scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
        scale_y_continuous(labels = scales::label_number()) +
        scale_x_continuous(breaks = as.numeric(unique(dat$art_plot$x_axis_variable)), labels = as.character(unique(dat$art_plot$x_axis_variable)))+
        expand_limits(y = 0) +
        theme_minimal() +
        labs(title = "Total receiving ART", x = element_blank(), y = element_blank())
      # theme(strip.text = element_text(face = "bold", size=13),
      #       plot.title = element_text(size=16),
      #       axis.text = element_text(size = 12)
      # )
    })
    
    plts$art_sex_flag <- 0
    
    if(nrow(dat$art_plot %>% filter(!is.nan(art_adult_sex_ratio)))) {
      
      p <- dat$art_plot %>%
        filter(!is.nan(art_adult_sex_ratio)) %>%
        group_by(area_id) %>%
        mutate(area_label = paste0(area_label, "\n15+ Male ART (", max(year), ") = ", sum(art_adult_m[year == max(year)])),
               n = cur_group_id(),
               n = floor(n/12)
               ) %>%
        ungroup %>%
        group_by(n) %>%
        group_split()
        
      plts$art_sex_ratio <- lapply(p, function(p) {
        
        plot <- p %>%
          ggplot(aes(x=x_axis_variable, y=art_adult_sex_ratio, group = 1)) +
          geom_line(size=1.3) +
          geom_point(size=2) +
          facet_rep_wrap(~area_label, ncol = 3, repeat.tick.labels = c("left", "bottom")) +
          scale_y_continuous(labels = scales::label_number()) +
          theme_minimal() +
          labs(title = "Ratio of females-to-males among adults on ART", x = NULL, y = NULL)
      })
      
      plts$art_sex_flag <- 1
  
    }
    
    p <- dat$art_plot %>%
      group_by(area_id) %>%
      mutate(area_label = paste0(area_label, "\nTotal ART (", max(year), ") = ", sum(art_total[year == max(year)])),
             n = cur_group_id(),
             n = floor(n/12)
             ) %>%
      ungroup %>%
      group_by(n) %>%
      group_split()
    
    plts$art_paediatric <- lapply(p, function(p) {
      
      plot <- p %>%
        ggplot(aes(x=x_axis_variable, y=art_prop_u15, group = 1)) +
        geom_line(size=1.3) +
        geom_point(size=2) +
        facet_rep_wrap(~area_label, ncol = 3, repeat.tick.labels = c("left", "bottom")) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        theme_minimal() +
        labs(title = "Proportion of total receiving ART under age 15", x = NULL, y = NULL)
    })
    
  })
  
  observeEvent(dat$anc, {
    
    threshold <- input$anc_count_threshold/100
    
    p <- dat$anc %>%
      group_by(area_label) %>%
      mutate(year_next = lead(year), anc_clients_next = lead(anc_clients)) %>%
      select(area_label, year, anc_clients, year_next, anc_clients_next) %>%
      mutate(anc_diff = anc_clients_next/anc_clients,
             colour_line = 
               ifelse(
                 (anc_diff >= (1 + threshold) | anc_diff <= (1 - threshold)) & anc_clients >= 100,
                 1, 0),
             colour_line = ifelse(is.infinite(anc_diff) | anc_diff > 200, 1, colour_line),
             colour_line = factor(colour_line),
             n = cur_group_id(),
             n = floor(n/12)
      ) %>%
      ungroup %>%
      group_by(n) %>%
      group_split()
    
    plts$anc_count <- lapply(p, function(p) {
      
      plot <- p %>%
        ggplot() +
        geom_segment(aes(x=year, xend = year_next, y = anc_clients, yend = anc_clients_next, colour = colour_line), size=1.3, show.legend = FALSE) +
        geom_point(aes(x=year, y=anc_clients), size=2) +
        scale_colour_manual(values=c("0" = "black", "1" = "red")) +
        facet_rep_wrap(~area_label, ncol = 3, scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
        scale_y_continuous(labels = scales::label_number()) +
        expand_limits(y = 0) +
        theme_minimal() +
        labs(title = "Number of ANC clients", x = element_blank(), y = element_blank())
    })
    
    p <- dat$anc %>%
      group_by(area_id) %>%
      mutate(area_label = paste0(area_label, "\nClients (", max(year), ") = ", anc_clients[year == max(year)]),
             n = cur_group_id(),
             n = floor(n/12)
      ) %>%
      ungroup %>%
      group_by(n) %>%
      group_split()
    
    plts$anc_prev <- lapply(p, function(p) {
      
      plot <- p %>%
        ggplot(aes(year, anc_prevalence)) +
        geom_line(size=1.3) +
        geom_point(size=2) +
        facet_rep_wrap(~area_label, ncol = 3, repeat.tick.labels = c("left", "bottom")) +
        scale_y_continuous(labels = label_percent()) +
        theme_minimal() +
        labs(title = "HIV prevalence among ANC attendees", x = NULL, y = NULL)
    })
    
    p <- dat$anc %>%
      group_by(area_id) %>%
      mutate(area_label = paste0(area_label, "\nKnown positive (", max(year), ") = ", anc_known_pos[year == max(year)]),
             n = cur_group_id(),
             n = floor(n/12)
      ) %>%
      ungroup %>%
      group_by(n) %>%
      group_split()
    
    plts$anc_known <- lapply(p, function(p) {
      
      plot <- p %>%
        ggplot(aes(year, anc_art_among_known)) +
        geom_hline(yintercept = 1.0, color = "grey30", linetype = "dashed") +
        geom_line(size=1.3) +
        geom_point(size=2) +
        facet_rep_wrap(~area_label, ncol = 3, repeat.tick.labels = c("left", "bottom")) +
        scale_y_continuous(labels = label_percent()) +
        theme_minimal() +
        labs(title = "Percentage of known positive already on ART", x = NULL, y = NULL)
    })
    
    p <- dat$anc %>%
      group_by(area_id) %>%
      mutate(area_label = paste0(area_label, "\nTotal positive (", max(year), ") = ", anc_total_pos[year == max(year)]),
             n = cur_group_id(),
             n = floor(n/12)
      ) %>%
      ungroup %>%
      group_by(n) %>%
      group_split()
    
    plts$anc_art <- lapply(p, function(p) {
      
      plot <- p %>%
        ggplot(aes(year, anc_art_coverage)) +
        geom_hline(yintercept = 1.0, color = "grey30", linetype = "dashed") +
        geom_line(size=1.3) +
        geom_point(size=2) +
        facet_rep_wrap(~area_label, ncol = 3, repeat.tick.labels = c("left", "bottom")) +
        scale_y_continuous(labels = label_percent()) +
        theme_minimal() +
        labs(title = "ART coverage prior to first ANC visit", x = NULL, y = NULL)
    })
    
  })
  
  # observeEvent(input$art_report | input$art_report2 | input$art_report3, {
  #   
  #   shinyjs::show("wait_art")
  #   
  # })
  # 
  # observeEvent(input$anc_report | input$anc_report2 | input$anc_report3 | input$anc_report4, {
  #   
  #   shinyjs::show("wait_anc")
  #   
  # })


  output$art_report <- output$art_report2 <- output$art_report3 <- downloadHandler(

    filename = paste0(last_modified$art(), "_naomi_ART.pdf"),
    content = function(file) {
      
      shinyjs::show("wait_art")

      # tempReport <- file.path(tempdir(), "art_report.Rmd")
      # file.copy("art_report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(title = paste("2021", dat$areas_merge$area_name[dat$areas_merge$area_sort_order == 1], "HIV estimates: Naomi ART input data"),
                     version = last_modified$art(),
                     art_count = plts$art_count,
                     art_sex_ratio = plts$art_sex_ratio,
                     art_sex_flag = plts$art_sex_flag,
                     art_paediatric = plts$art_paediatric,
                     threshold = input$art_count_threshold
      )
      
      # rmarkdown::render(tempReport, output_file = file,
      #                   params = params,
      #                   envir = new.env(parent = globalenv())
      # )
      
      rmarkdown::render("art_report.Rmd", output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      

    }
  )
  
  output$anc_report <- output$anc_report2 <- output$anc_report3 <- output$anc_report4 <- downloadHandler(
    
    filename = paste0(last_modified$anc(), "_naomi_ANC.pdf"),
    content = function(file) {
      
      shinyjs::show("wait_anc")
      
      tempReport <- file.path(tempdir(), "anc_report.Rmd")
      file.copy("anc_report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(title = paste("2021", dat$areas_merge$area_name[dat$areas_merge$area_sort_order == 1], "HIV estimates: Naomi ANC input data"),
                     version = last_modified$anc(),
                     anc_count = plts$anc_count,
                     anc_prev = plts$anc_prev,
                     anc_known = plts$anc_known,
                     anc_art = plts$anc_art,
                     threshold = input$anc_count_threshold

      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      

    }
  )
  
}