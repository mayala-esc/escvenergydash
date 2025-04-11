


page_elec_spotUI <- function(...){
  
  
  shiny::fluidPage(
    
    # Medians
    
    "Wholesale Spot Prices" %>% 
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),
    
    # Box

    shiny::fluidRow(
    column(
      11,
      div(
        class = "box",
        style = "padding:15px;",
        fluidRow(
          column(
            12,
            shinycssloaders::withSpinner(
              highchartOutput("vec_elec_spot", height = 500),
              type = 1,
              color = "#023047"
            )
          )
        )
      )
    )
    ),
    "Wholesale Futures Prices" %>% 
      h2() %>% 
      div(class = "inner") %>%
      div(class = "small-box") %>% 
      column(12, .) %>%
      fluidRow(),
    
    shiny::fluidRow(
      column(
        11,
        div(
          class = "box",
          style = "padding:15px;",
          fluidRow(
            column(
              12,
              shinycssloaders::withSpinner(
                highchartOutput("vec_elec_futures", height = 500),
                type = 1,
                color = "#023047"
              )
            )
          )
        )
      )
    )
    )
  
}
