





page_elec_dzonesUI <- function(...){
  
  
  shiny::fluidPage(
    
    # Title
    
    "Retail offer prices" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),
    
    shiny::fluidRow(
      shinydashboard::box(
        width = 8,
        column(
      12,
      div(
        class = "box",
        style = "padding:15px;",
        fluidRow(
          column(
            12,
            shinycssloaders::withSpinner(
              highchartOutput("vec_elec_dzones", height = 500),
              type = 1,
              color = "#023047"
            )
          )
        )
      )
    )
    ),
    
    
    # Box
    
      shinydashboard::box(
        
        title = "Options",
        width = 4,
        collapsible = TRUE,
        
        shiny::fluidRow(
          style = "background-color: #FFFFFF; border-radius: 1rem;margin:1px;padding:10px;",
          shiny::column(
            12,
        
        # chart option selector
        shinyWidgets::radioGroupButtons(
          inputId = "cust_type_elec",
          status = "secondary",
          label = "Customer Type",
          choices = c("Residential" = "res", "Small business" = "sme"),
          selected = "res",
          justified = TRUE,
          direction = "vertical"
        )
        ,
        shinyWidgets::radioGroupButtons(
          inputId = "elec_dist_zones",
          status = "secondary",
          label = "Distribution Zone",
          choices = c(
            "Jemena" = "jemena",
            "AusNet" = "ausnetservices",
            "Powercor" = "powercor",
            "Citipower" = "citipower",
            "United Energy" = "unitedenergy"
          ),
          selected = "jemena",
          justified = TRUE,
          direction = "vertical"
        )
        )
        )
        ) %>%
        tagAppendAttributes(
          style = "background:var(--twilight);overflow-y:auto;overflow-x:crop;",
          .cssSelector = ".box"
        ) %>%
        tagAppendAttributes(
          style = "padding:15px;",
          .cssSelector = ".box-body"
        ) %>%
        tagAppendAttributes(
          style = "font-size:20px;font-weight:bold;",
          .cssSelector = ".box-header h3"
        )
      ),
    
    
    # Latest Offer price - flat tariffs table
    "Price change tables" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),
    
    shiny::fluidRow(
      shinydashboard::box(
        title = "Flat tariff offers Prices - Residential",
        table_elec_dzones(),
        width = 12
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Flat tariff offers Prices - Small Business",
        table_elec_sme_dzones(),
        width = 12
      )
    )
    
    )
  
  
  
}
