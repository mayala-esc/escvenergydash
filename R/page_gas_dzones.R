





page_gas_dzonesUI <- function(...){
  
  
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
              highchartOutput("vec_gas_dzones", height = 500),
              type = 1,
              color = "#023047"
            )
          )
        )
        )
      )
      ),
    
    # Box
    #fluidRow(
      shinydashboard::box(
        
        title = "Options",
        width = 4,
        collapsible = TRUE,
        
        # white background within options box
        shiny::fluidRow(
          style = "background-color: #FFFFFF; border-radius: 1rem;margin:1px;padding:10px;",
          shiny::column(
            12,
        
        # chart option selector
        shinyWidgets::radioGroupButtons(
          inputId = "cust_type_gas",
          status = "secondary",
          label = "Customer Type",
          choices = c("Residential" = "res", "Small business" = "sme"),
          selected = "res",
          justified = TRUE,
          direction = "vertical"
        ),
        shinyWidgets::radioGroupButtons(
          inputId = "gas_dist_zones",
          status = "secondary",
          label = "Distribution Zone",
          choices = c(
            "Aus. Gas Networks" = "australiangasnetworks",
            "AusNet Gas" = "ausnetservices(gas)",
            "Multinet" = "multinet"
          ),
          selected = "australiangasnetworks",
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
    "Latest price changes" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),
    
    shiny::fluidRow(
      shinydashboard::box(
        title = "Market tariff offers Prices - Residential",
        table_gas_dzones(),
        width = 12
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        title = "Market tariff offers Prices - Small Business",
        table_gas_sme_dzones(),
        width = 12
      )
    )
    
    
    )
  
}
