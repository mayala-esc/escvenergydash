
# load libraries ----
# library(shiny)
# library(shinydashboard)
# library(highcharter)
# library(markdown)
# library(seasonal)
# library(fabletools)
# library(tsibble)
# library(feasts)
# library(scales)
# library(forecast)
# library(tsbox)


# load resources ----
# source("R/utils.R")
# source("R/load_data.R")
# source("R/theme.R")
# source("R/ui.R")
# source("R/page_overview.R")
# source("R/page_disclaimer.R")
# source("R/page_elec_whsl.R")
# source("R/page_elec_dzones.R")
# source("R/page_elec_dzones_size.R")
# source("R/page_elec_n_offers.R")
# source("R/page_elec_market_structure.R")
# source("R/page_elec_size.R")
# source("R/page_gas_whsl.R")
# source("R/page_gas_dzones.R")
# source("R/page_gas_dzones_size.R")
# source("R/page_gas_n_offers.R")
# source("R/page_gas_market_structure.R")
# source("R/page_gas_size.R")
# source("R/page_methodology.R")
# source("R/badges.R")
# source("R/viz_overview.R")
# source("R/viz_gas_dzones.R")
# source("R/viz_gas_size.R")
# source("R/viz_gas_dzones_size.R")
# source("R/viz_gas_n_offers.R")
# source("R/viz_gas_whsl.R")
# source("R/viz_gas_market_structure.R")
# source("R/viz_elec_dzones.R")
# source("R/viz_elec_size.R")
# source("R/viz_elec_dzones_size.R")
# source("R/viz_elec_n_offers.R")
# source("R/viz_elec_whsl.R")
# source("R/viz_elec_market_structure.R")
# source("R/server.R")




# Run the application ----

dashbapp <- function(...){

  shiny::shinyApp(ui = ui, server = server)

}




