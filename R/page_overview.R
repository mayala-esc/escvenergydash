


page_overviewUI <- function(...){
  
  
  shiny::fluidPage(
    
    # Overview 
    # "Victorian Energy Retail Offers Dashboard" %>%
    #   h2() %>% div(class = "inner") %>%
    #   div(class = "small-box") %>% column(12, .) %>%
    #   fluidRow(),
    # 
    # fluidRow(
    #   column(
    #     12,
    #     # style = "font-size: 16px;",
    #     img(
    #       src = "energy-grid.jpg",
    #       style = "border-radius: 1rem;float: left; margin-right: 1rem; margin-bottom: 1rem;",
    #       width = "402",
    #       height = "268",
    #       alt = "Energy grid"
    #     ),
    #     p(
    #       "The Victorian Energy Retail Offers Dashboard helps the Essential Services Commission explore the latest ",
    #       "Victoria Energy Compare offers data to gauge retail energy market competitiveness and",
    #       " efficiency.",
    #       br(), br(),
    #       "On this page, you can explore retail offers for electricity and gas based",
    #       " on assumed residential and small business consumption profiles. Offers are classified",
    #       " according to their distribution network and retailers market share."
    #     )
    #   )
    # ),
    
    
    ## Medians
    # chart
    "Electricity Retail Offers" %>% 
      h2() %>% 
      div(class = "inner") %>%
      div(class = "small-box") %>% 
      column(12, .) %>%
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
              highchartOutput("vec_overview_elec", height = 500),
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
          #
          # chart option selector
          shinyWidgets::radioGroupButtons(
            inputId = "cust_type_elec", # this name needs to match with server input name
            status = "secondary",
            label = "Customer Type",
            choiceNames = c("Residential", "Small business"),
            choiceValues = c("res", "sme"),
            selected = "res",
            width = "100%",
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
        style = "font-size:20px;font-weight:bold;", # font for title "Options"
        .cssSelector = ".box-header h3"
      )
    ),
    
    
    "Gas Retail Offers" %>% 
      h2() %>% 
      div(class = "inner") %>%
      div(class = "small-box") %>% 
      column(12, .) %>%
      fluidRow(),
    
    
    column(
      8,
      div(
        class = "box",
        style = "padding:15px;",
        fluidRow(
          column(
            12,
            shinycssloaders::withSpinner(
              highchartOutput("vec_overview_gas", height = 500),
              type = 1,
              color = "#023047"
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
          #
          # chart option selector
          shinyWidgets::radioGroupButtons(
            inputId = "cust_type_gas", # this name needs to match with server input name
            status = "secondary",
            label = "Customer Type",
            choiceNames = c("Residential", "Small business"),
            choiceValues = c("res", "sme"),
            selected = "res",
            width = "100%",
            justified = TRUE,
            direction = "vertical"
          )
        )
        
      )
    ) %>% 
      tagAppendAttributes(
        style = "background:var(--twilight);",
        .cssSelector = ".box"
      ) %>%
      tagAppendAttributes(
        style = "padding:15px;",
        .cssSelector = ".box-body"
      ) %>%
      tagAppendAttributes(
        style = "font-size:20px;font-weight:bold;", # font for title "Options"
        .cssSelector = ".box-header h3"
      )#,
    # column(
    #   8,
    #   div(
    #     class = "box",
    #     style = "padding:15px;",
    #     fluidRow(
    #       column(
    #         12,
    #         shinycssloaders::withSpinner(
    #           highchartOutput("vec_overview_gas", height = "auto"),
    #           type = 1,
    #           color = "#023047"
    #         )
    #       )
    #     )
    #   )
    # )
    
    
  )
}
