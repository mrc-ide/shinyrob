viewer_server <- function(input, output, session) {
  
  plot_height <- reactive(250*ceiling(length(unique(dat$areas_merge$area_id))/5))

  output$art_count_threshold <- renderText({ input$art_count_threshold })

  output$anc_count_threshold <- renderText({ input$anc_count_threshold })
  
  observeEvent(input$plot_quarters, {
    
    req(dat$art)
    # browser()
    if(input$plot_quarters) {
      dat$art_plot <- dat$art %>%
        mutate(art_adult_sex_ratio = art_adult_f / art_adult_m,
               art_adult_child_ratio = art_adult / art_child,
               art_prop_u15 = round(art_child/(art_adult+art_child),3)) %>%
        rename(x_axis_variable = quarter)
    } else {
      dat$art_plot <- dat$art %>%
        group_by(area_label, year) %>%
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
    }
    
  })

  output$art_count_plot <- renderPlot({
    
    shiny::validate(
      need(dat$art_plot, "No ART data available")
    )

    threshold <- input$art_count_threshold/100
    
    dat$art_plot %>%
      group_by(area_label) %>%
      mutate(x_axis_variable_next = lead(x_axis_variable), art_adult_next = lead(art_adult)) %>%
      select(area_label, x_axis_variable, art_adult, x_axis_variable_next, art_adult_next) %>%
      mutate(art_diff = art_adult_next/art_adult,
             colour_line = 
               ifelse(
                 (art_diff >= (1 + threshold) | art_diff <= (1 - threshold)) & art_adult >= 100,
                 1, 0),
             colour_line = ifelse(is.infinite(art_diff) | art_diff > 200, 1, colour_line),
             colour_line = factor(colour_line)
      )  %>%
      ggplot() +
      geom_segment(aes(x=as.numeric(x_axis_variable), xend = as.numeric(x_axis_variable_next), y = art_adult, yend = art_adult_next, colour = colour_line), size=1.3, show.legend = FALSE) +
      geom_point(aes(x=as.numeric(x_axis_variable), y=art_adult), size=2) +
      scale_colour_manual(values=c("0" = "black", "1" = "red")) +
      facet_rep_wrap(~area_label, ncol = input$facet_number, scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(labels = scales::label_number()) +
      scale_x_continuous(breaks = as.numeric(unique(dat$art_plot$x_axis_variable)), labels = as.character(unique(dat$art_plot$x_axis_variable)))+
      expand_limits(y = 0) +
      theme_minimal() +
      labs(title = "Total receiving ART", x = element_blank(), y = element_blank()) +
      theme(strip.text = element_text(face = "bold", size=13),
            plot.title = element_text(size=16),
            axis.text = element_text(size = 12)
      )

  }, height = plot_height
  )

  output$art_sex_plot <- renderPlot({

    if(nrow(dat$art_plot %>% filter(!is.nan(art_adult_sex_ratio)))) {

      dat$art_plot %>%
        group_by(area_id) %>%
        mutate(area_label = paste0(area_label, "\n15+ Male ART (", max(year), ") = ", sum(art_adult_m[year == max(year)]))) %>%
        filter(!is.nan(art_adult_sex_ratio)) %>%
        ggplot(aes(x=x_axis_variable, y=art_adult_sex_ratio, group = 1)) +
        geom_line(size=1.3) +
        geom_point(size=2) +
        facet_rep_wrap(~area_label, ncol = input$facet_number, repeat.tick.labels = c("left", "bottom")) +
        scale_y_continuous(labels = scales::label_number()) +
        theme_minimal() +
        labs(title = "Ratio of females-to-males among adults on ART", x = NULL, y = NULL) +
        theme(strip.text = element_text(face = "bold", size=13),
              plot.title = element_text(size=16),
              axis.text = element_text(size = 12)
        )
    }

  }, height = plot_height
  )

  output$art_sex_plot_n <- renderText({

    paste0("Districts with sex disaggregated data: ",
           length(dat$art_plot %>%
                    filter(!is.nan(art_adult_sex_ratio)) %>%
                    .$area_id %>%
                    unique),
           " of ",
           length(dat$art_plot %>%
                    .$area_id %>%
                    unique
           )
    )
  })
  
  output$art_paeds_plot <- renderPlot({

    shiny::validate(
      need(dat$art, "No ART data available")
    )

    dat$art_plot %>%
      group_by(area_id) %>%
      mutate(area_label = paste0(area_label, "\nTotal ART (", max(year), ") = ", sum(art_total[year == max(year)]))) %>%
      ggplot(aes(x=x_axis_variable, y=art_prop_u15, group = 1)) +
      geom_line(size=1.3) +
      geom_point(size=2) +
      facet_rep_wrap(~area_label, ncol = input$facet_number, repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      theme_minimal() +
      labs(title = "Proportion of total receiving ART under age 15", x = NULL, y = NULL) +
      theme(strip.text = element_text(face = "bold", size=13),
            plot.title = element_text(size=16),
            axis.text = element_text(size = 12)
      )

  }, height = plot_height
  )

  output$anc_count_plot <- renderPlot({

    shiny::validate(
      need(dat$anc, "No ANC data available")
    )

    threshold <- input$anc_count_threshold/100
      
    dat$anc %>%
      group_by(area_label) %>%
      mutate(year_next = lead(year), anc_clients_next = lead(anc_clients)) %>%
      select(area_label, year, anc_clients, year_next, anc_clients_next) %>%
      mutate(anc_diff = anc_clients_next/anc_clients,
             colour_line = 
               ifelse(
                 (anc_diff >= (1 + threshold) | anc_diff <= (1 - threshold)) & anc_clients >= 100,
                 1, 0),
             colour_line = ifelse(is.infinite(anc_diff) | anc_diff > 200, 1, colour_line),
             colour_line = factor(colour_line)
      )  %>%
      ggplot() +
      geom_segment(aes(x=year, xend = year_next, y = anc_clients, yend = anc_clients_next, colour = colour_line), size=1.3, show.legend = FALSE) +
      geom_point(aes(x=year, y=anc_clients), size=2) +
      scale_colour_manual(values=c("0" = "black", "1" = "red")) +
      facet_rep_wrap(~area_label, ncol = input$facet_number, scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(labels = scales::label_number()) +
      expand_limits(y = 0) +
      theme_minimal() +
      labs(title = "Number of ANC clients", x = element_blank(), y = element_blank()) +
      theme(strip.text = element_text(face = "bold", size=13),
            plot.title = element_text(size=16),
            axis.text = element_text(size = 12)
      )

  }, height = plot_height
  )

  output$anc_prev_plot <- renderPlot({

    shiny::validate(
      need(dat$anc, "No ANC data available")
    )


    dat$anc %>%
      group_by(area_id) %>%
      mutate(area_label = paste0(area_label, "\nClients (", max(year), ") = ", anc_clients[year == max(year)])) %>%
      ggplot(aes(year, anc_prevalence)) +
      geom_line(size=1.3) +
      geom_point(size=2) +
      facet_rep_wrap(~area_label, ncol = input$facet_number, repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(labels = label_percent()) +
      theme_minimal() +
      labs(title = "HIV prevalence among ANC attendees", x = NULL, y = NULL) +
      theme(strip.text = element_text(face = "bold", size=13),
            plot.title = element_text(size=16),
            axis.text = element_text(size = 12)
      )

  }, height = plot_height
  )

  output$anc_known_plot <- renderPlot({

    shiny::validate(
      need(dat$anc, "No ANC data available")
    )

    dat$anc %>%
      group_by(area_id) %>%
      mutate(area_label = paste0(area_label, "\nKnown positive (", max(year), ") = ", anc_known_pos[year == max(year)])) %>%
      ggplot(aes(year, anc_art_among_known)) +
      geom_hline(yintercept = 1.0, color = "grey30", linetype = "dashed") +
      geom_line(size=1.3) +
      geom_point(size=2) +
      facet_rep_wrap(~area_label, ncol = input$facet_number, repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(labels = label_percent()) +
      theme_minimal() +
      labs(title = "Percentage of known positive already on ART", x = NULL, y = NULL) +
      theme(strip.text = element_text(face = "bold", size=13),
            plot.title = element_text(size=16),
            axis.text = element_text(size = 12)
      )

  }, height = plot_height
  )

  output$anc_art_plot <- renderPlot({

    shiny::validate(
      need(dat$anc, "No ANC data available")
    )

    dat$anc %>%
      group_by(area_id) %>%
      mutate(area_label = paste0(area_label, "\nTotal positive (", max(year), ") = ", anc_total_pos[year == max(year)])) %>%
      ggplot(aes(year, anc_art_coverage)) +
      geom_hline(yintercept = 1.0, color = "grey30", linetype = "dashed") +
      geom_line(size=1.3) +
      geom_point(size=2) +
      facet_rep_wrap(~area_label, ncol = input$facet_number, repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(labels = label_percent()) +
      theme_minimal() +
      labs(title = "ART coverage prior to first ANC visit", x = NULL, y = NULL) +
      theme(strip.text = element_text(face = "bold", size=13),
            plot.title = element_text(size=16),
            axis.text = element_text(size = 12)
      )

  }, height = plot_height
  )
  
}