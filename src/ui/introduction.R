introduction <- function() {
  div(style="margin-left:5%; margin-right: 5%",
    br(),
    h1("ShinyRob"),
    br(),
    p("This tool visualises the district-level ART and ANC data used within Naomi. Please enter your ADR access key and select a data package."),
    p("If you do not have your ADR access key, please click the button below and copy it from the green box at the top of the page."),
    p(strong("Do not share your ADR access key with anyone else")),
    br(),
    fluidRow(
      column(2, passwordInput(inputId = "api_key", label = "ADR access key", width="100%")),
      column(1, style="margin-top:25px", disabled(actionButton(inputId = "api_button", label = "Submit"))),
      column(2, style="margin-top:25px", actionButton(inputId = "adr_button", label = "Get ADR key", icon = icon("external-link"), onclick ="window.open('https://adr.unaids.org/me', '_blank')"))
    ),
    hidden(
      div(id="wait",
          p("Please wait - fetching data from ADR...")
          )
    ),
    fluidRow(
      # uiOutput("package")
      disabled(
        column(4, selectInput("geo_package", label="Select geographic data package", choices = "No ADR access key entered", width="100%"))
      ),
      column(4, 
             disabled(selectInput("art_package", label="Select ART data package", choices = "No ADR access key entered", width="100%")),
             hidden(selectInput("art_dhis_package", label="Select DHIS ART dataset", choices = "No ADR access key entered", width="100%"))
      ),
      column(4, 
             disabled(selectInput("anc_package", label="Select ANC data package", choices = "No ADR access key entered", width="100%")),
             hidden(selectInput("anc_dhis_package", label="Select DHIS ANC dataset", choices = "No ADR access key entered", width="100%"))
      ),
    ),
    h4(style="text-decoration:underline", "Data validation check:"),
    fluidRow(
      column(6, 
        hidden(
          div(id="validation_div", style="height:200px",
              uiOutput("validation") %>% withSpinner(proxy.height = "100px")
          )
        )
      )
    ),
    # p(htmlOutput("shape_check")),
    # p(htmlOutput("art_check") %>% withSpinner()),
    # p(textOutput("art_version") %>% withSpinner()),
    # p(htmlOutput("anc_check") %>% withSpinner()),
    # p(textOutput("anc_version") %>% withSpinner()),
    div(id="footer",
      img(id="footer_img", style="height:40%", src="unaids.png"),
      img(id="footer_img", style="height: 40%", src="MRC-GIDA-logo.png"),
      img(id="footer_img", style="height: 50%", src="avenir.png")
    )
  )
  
}