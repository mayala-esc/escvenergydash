
# Define UI for application ----
ui <- function() {
  
  custom_header <- shinydashboard::dashboardHeader()
  custom_header$children[[2]] <- NULL
  custom_header <- custom_header %>%
    tagAppendChildren(
      .cssSelector = ".navbar-custom-menu",
      fluidRow(
        div(class = "col", tags$a(href='https://www.esc.vic.gov.au/', tags$img(src='ESC-logo.png', height='40',width='150'))),
        div(class = "col", span(HTML("Energy Pricing</br>Dashboard"), class = "badgeDescript")),
        div(class = "col", badge_vec())
        )
      )
  
  
  shinydashboard::dashboardPage(
    title = "Victorian Energy Pricing Dashboard",
    header = custom_header,
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(id = 'tabs',
                                  shinydashboard::menuItem("Overview", tabName = "overview", selected = TRUE),
                                  shinydashboard::menuItem("Electricity", startExpanded = FALSE,
                                                           shinydashboard::menuSubItem("Retail offers", tabName = "elec_dzones"),
                                                           shinydashboard::menuSubItem("Offers by retailer size", tabName = "elec_size"),
                                                           shinydashboard::menuSubItem("Offers range by retailer size", tabName = "elec_mktsize"),
                                                           shinydashboard::menuSubItem("Number of retail offers", tabName = "elec_noffers"),
                                                           shinydashboard::menuSubItem("Market structure", tabName = "elec_treemap"),
                                                           shinydashboard::menuSubItem("Wholesale prices", tabName = "wholesale")
                                                           ),
                                  shinydashboard::menuItem("Gas", startExpanded = FALSE,
                                                           shinydashboard::menuSubItem("Retail offers", tabName = "gas_dzones"),
                                                           shinydashboard::menuSubItem("Offers by retailer size", tabName = "gas_size"),
                                                           shinydashboard::menuSubItem("Offers range by retailer size", tabName = "gas_mktsize"),
                                                           shinydashboard::menuSubItem("Number of retail offers", tabName = "gas_noffers"),
                                                           shinydashboard::menuSubItem("Market structure", tabName = "gas_treemap"),
                                                           shinydashboard::menuSubItem("Wholesale prices", tabName = "gas_wholesale")
                                  ),
                                  shinydashboard::menuItem("Methodology", tabName = "methodology"),
                                  shinydashboard::menuItem("Disclaimer", tabName = "disclaimer")
                                    #shinydashboard::menuItem("Disclaimer", tabName = "disclaimer") %>%
                                    # shiny::tagAppendAttributes(
                                    #   style = "display:none;"
                                    # )
      ),
      width = "250px"
    ),
    body = shinydashboard::dashboardBody(
      tags$head( #adding styling using css
        # Bootstrap 5 for xl columns
        tags$link(
          href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
          rel = "stylesheet",
          integrity = "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
          crossorigin = "anonymous"
        ),
        # Global vic css
        tags$link(href = "globalvic.css", rel = "stylesheet"),
        # Launchpad links (client side execution)
        #shiny::tags$script("$(document).ready(function(){$('a.launchpadLink').click(function(){$('.sidebar-menu a[data-value=\"launchpad\"]').trigger('click');})});"),
        #shiny::tags$script("$(document).ready(function(){$('a.methodologyLink').click(function(){$('.sidebar-menu a[data-value=\"methodology\"]').trigger('click');})});"),
        #shiny::tags$script("$(document).ready(function(){$('a.disclaimerLink').click(function(){$('.sidebar-menu a[data-value=\"disclaimer\"]').trigger('click');})});"),
        
        # Resize highcharts on sidebar collapse
        shiny::tags$script(src = "sidebar_chart_resize.JS")
      ),
      shiny::tags$script("$('html').attr(\"lang\", \"en\")"),
      shiny::tags$script("$('section.content').attr(\"role\", \"main\")"),
      shinydashboard::tabItems(
        shinydashboard::tabItem("overview", page_overviewUI()),
        shinydashboard::tabItem("gas_dzones", page_gas_dzonesUI()),
        shinydashboard::tabItem("gas_size", page_gas_sizeUI()),
        shinydashboard::tabItem("gas_mktsize", page_gas_mktsizeUI()),
        shinydashboard::tabItem("gas_noffers", page_gas_noffersUI()),
        shinydashboard::tabItem("gas_treemap", page_gas_treemapUI()),
        shinydashboard::tabItem("elec_dzones", page_elec_dzonesUI()),
        shinydashboard::tabItem("elec_size", page_elec_sizeUI()),
        shinydashboard::tabItem("elec_mktsize", page_elec_mktsizeUI()),
        shinydashboard::tabItem("elec_noffers", page_elec_noffersUI()),
        shinydashboard::tabItem("wholesale", page_elec_spotUI()),
        shinydashboard::tabItem("elec_treemap", page_elec_treemapUI()),
        shinydashboard::tabItem("gas_wholesale", page_gas_spotUI()),
        shinydashboard::tabItem("methodology", page_methodology()),
        shinydashboard::tabItem("disclaimer", page_disclaimerUI())
        )
      )
    )
}




