# naomi-viewer
A data visualisation Shiny app for ANC and ART data for Naomi

App can be found at <a href="shinyrob.unaids.org">this link</a>

ANC and ART data are made available in the app through the use of an ADR access key through the AIDS Data Repository - you can find yours <a href="https://adr.unaids.org/me">here</a>

To run the app locally please:

* <a href="https://rstudio.com/products/rstudio/download/" target="_blank">Install R studio</a>
* <a href="https://desktop.github.com/" target="_blank">Download Github Desktop</a>
* On the <a href="https://github.com/mrc-ide/naomi-viewer/tree/master" target="_blank">master branch</a> (or a development branch), click the green “code” button in the middle of the screen, then “open with github desktop”
* Choose where you’d like it downloaded
* In RStudio, open `naomi_viewer/installation_setup.R` from the newly downloaded folder
* Run the code in there to install all the packages you’ll need
* Once that’s done, open `naomi-viewer/app.R` and click “Run App” at the top right of your RStudio code window or use the function `runApp()`
