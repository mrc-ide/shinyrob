viewer <- function() {
  div(style="margin-left:5%; margin-right: 5%",
      h2("Visualise Naomi input data"),
      br(),
      p(style="font-size:14px", "The plots below show visualisations for district-level ART and ANC data used in Naomi"),
      tags$ul(
        tags$li(tags$b("ART count:"), "Number on ART at the end of calendar year"),
        tags$li(tags$b("ART sex ratio:"), "Ratio of females-to-males among adults on ART"),
        tags$li(tags$b("ART paediatric:"), "Proportion of total on-ART under age 15"),
        tags$li(tags$b("ANC count:"), "Number of ANC clients"),
        tags$li(tags$b("ANC prevalence:"), "HIV prevalence among ANC attendees"),
        tags$li(tags$b("ANC known positive:"), "Percentage of known positive already on ART"),
        tags$li(tags$b("ANC ART coverage:"), "ART coverage prior to first ANC visit")
      ),
      fluidRow(style = "display:flex; align-items: center; justify-content: start; margin-left:0px; margin-right:0px",
               p(style="margin-right:15px", "Plot ART data at quarterly resolution"),
               switchInput("plot_quarters", value= FALSE)
               ),
      fluidRow(style = "display:flex; align-items: center; justify-content: start; margin-left:0px; margin-right:0px",
               p(style="margin-right:15px", "Number of plots per row: "),
               numericInput("facet_number", value = 5, min = 1, max = 9, step = 1, width = "50px", label = NULL)
      ),
      # p(textOutput("art_version2")),
      # p(textOutput("anc_version2")),
      tabsetPanel(
        tabPanel(title="ART count",
                 br(),
                 # div(style="display: inline-block; vertical-align:top",
                 #     p(style="display: inline-block; vertical-align:top", "ART warning threshold: "), 
                 #     numericInput("art_count_threshold", label = NULL, value = 10, min=0, max=100, step = 1, width = "60px"), 
                 #     p(style="display: inline-block; vertical-align:top", "%")
                 # ),
                 fluidRow(
                   style = "display:flex; align-items: center; justify-content: start; margin-left:0px; margin-right:0px",
                            p(tags$strong("ART warning threshold (%)")),
                            div(style="margin-top: 5px; margin-left: 20px", 
                                numericInput(inputId= "art_count_threshold", label = NULL, value = 25, min=0, max=100, step = 1, width = "60px")
                            )
                 ),
                 p("Year-on-year changes in the number of adults on ART greater than ", tags$strong("+/-", textOutput("art_count_threshold", inline=TRUE), "%"), " will be highlighted in red. Warnings will not be generated for districts with fewer than 100 people on ART."),
                 shinyjs::hidden(downloadButton("art_report", label="Download ART plots")),
                 shinyjs::hidden(
                   div(id="wait_art",
                       p("Please wait - preparing ART report. This may take some time for countries with many districts")
                   )
                 ),
                 br(),
                 plotOutput("art_count_plot") %>% withSpinner()
                 ),
        tabPanel(title="ART sex ratio",
                 br(),
                 h4(textOutput("art_sex_plot_n")),
                 shinyjs::hidden(downloadButton("art_report2", label="Download ART plots")),
                 shinyjs::hidden(
                   div(id="wait_art",
                       p("Please wait - preparing ART report. This may take some time for countries with many districts")
                   )
                 ),
                 br(),
                 plotOutput("art_sex_plot") %>% withSpinner()
        ),
        tabPanel(title="ART paediatric",
                 br(),
                 shinyjs::hidden(downloadButton("art_report3", label="Download ART plots")),
                 shinyjs::hidden(
                   div(id="wait_art",
                       p("Please wait - preparing ART report. This may take some time for countries with many districts")
                   )
                 ),
                 br(),
                 plotOutput("art_paeds_plot") %>% withSpinner()
        ),
        tabPanel(title="ANC count",
                 br(),
                 fluidRow(
                   style = "display:flex; align-items: center; justify-content: start; margin-left:0px; margin-right:0px",
                   p(tags$strong("ANC warning threshold (%)")),
                   div(style="margin-top: 5px; margin-left: 20px", 
                       numericInput(inputId= "anc_count_threshold", label = NULL, value = 25, min=0, max=100, step = 1, width = "60px")
                   )
                 ),
                 p("Year-on-year changes in the number of ANC clients greater than ", tags$strong("+/-", textOutput("anc_count_threshold", inline=TRUE), "%"), " will be highlighted in red. Warnings will not be generated for districts with fewer than 100 ANC clients."),
                 br(),
                 shinyjs::hidden(downloadButton("anc_report", label="Download ANC plots")),
                 shinyjs::hidden(
                   div(id="wait_anc",
                       p("Please wait - preparing ANC report. This may take some time for countries with many districts")
                   )
                 ),
                 br(),
                 plotOutput("anc_count_plot") %>% withSpinner()
                 ),
        tabPanel(title="ANC prevalence",
                 br(),
                 shinyjs::hidden(downloadButton("anc_report2", label="Download ANC plots")),
                 shinyjs::hidden(
                   div(id="wait_anc",
                       p("Please wait - preparing ANC report. This may take some time for countries with many districts")
                   )
                 ),
                 br(),
                 plotOutput("anc_prev_plot") %>% withSpinner()
        ),
        tabPanel(title="ANC known positive",
                 br(),
                 shinyjs::hidden(downloadButton("anc_report3", label="Download ANC plots")),
                 shinyjs::hidden(
                   div(id="wait_anc",
                       p("Please wait - preparing ANC report. This may take some time for countries with many districts")
                   )
                 ),
                 br(),
                 plotOutput("anc_known_plot") %>% withSpinner()
        ),
        tabPanel(title="ANC ART coverage",
                 br(),
                 shinyjs::hidden(downloadButton("anc_report4", label="Download ANC plots")),
                 shinyjs::hidden(
                   div(id="wait_anc",
                       p("Please wait - preparing ANC report. This may take some time for countries with many districts")
                   )
                 ),
                 br(),
                 plotOutput("anc_art_plot") %>% withSpinner()
        )
      )
  )
  
}