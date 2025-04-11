
page_elec_treemapUI <- function(...){
  
  
  shiny::fluidPage(
    
    # Title
    
    "Electricity retail market share" %>% 
      h2() %>% 
      div(class = "inner") %>%
      div(class = "small-box") %>% 
      column(12, .) %>%
      fluidRow(),
    
    shiny::fluidRow(
      
      #Treemap chart
      shinydashboard::box(
        width = 8,
        column(
          12,
          div(
            class = "chart-box",
            style = "margin:2px;",
            fluidRow(
              column(
                12,
                shinycssloaders::withSpinner(
                  highchartOutput("elec_treemap", height = 700),
                  type = 1,
                  color = "#023047"
                )
              )
            )
          )
        )
      ),
      
      # Options Box
      
      shinydashboard::box(
        
        title = "Options",
        width = 4,
        collapsible = TRUE,
        
        shiny::fluidRow(
          style = "background-color: #FFFFFF; border-radius: 1rem;margin:2px;padding-bottom:25px;",
          shiny::column(
            12,
            
            # chart option selector
            shinyWidgets::radioGroupButtons(
              inputId = "cust_type_elec_treemap",
              status = "secondary",
              label = "Customer Type",
              choices = c("Residential" = "res", "Small business" = "sme"),
              selected = "res",
              justified = TRUE,
              direction = "horizontal"
            ),
            
            # slider theme
            tagList(
              tags$link(
                href = "slider.css", 
                rel = "stylesheet",
                type = 'text/css'),
              div(id = 'irs',
                  class = "irs--shiny",
                  shiny::div(
                    class = "year-slider"),
            
            # slider for year
            shinyWidgets::sliderTextInput(
              inputId = "date_elec_treemap",
              label = "Select a year",
              choices = format_choice(start = "2014-12-01", end = "2024-12-01"),
              selected = format_choice(start = "2024-12-01", end = "2024-12-01"),
              animate = FALSE,
              grid = TRUE
            )
              )#div close
            )#taglist close
          )
        )
      ) %>%
        tagAppendAttributes(
          style = "background:var(--twilight);overflow-y:auto;overflow-x:crop;",
          .cssSelector = ".box"
        ) %>%
        tagAppendAttributes(
          style = "padding:10px;",
          .cssSelector = ".box-body"
        ) %>%
        tagAppendAttributes(
          style = "font-size:20px;font-weight:bold;",
          .cssSelector = ".box-header h3"
        )
    ),
    
    # Title
    
    "Electricity retail market transfers flows" %>% 
      h2() %>% 
      div(class = "inner") %>%
      div(class = "small-box") %>% 
      column(12, .) %>%
      fluidRow(),
    
    shiny::fluidRow(
      
      #Treemap chart
      shinydashboard::box(
        width = 8,
        column(
          12,
          div(
            class = "chart-box",
            style = "margin:2px;",
            fluidRow(
              column(
                12,
                shinycssloaders::withSpinner(
                  highchartOutput("elec_sankey", height = 700),
                  type = 1,
                  color = "#023047"
                )
              )
            )
          )
        )
      ),
      
      # Options Box
      
      shinydashboard::box(
        
        title = "Options",
        width = 4,
        collapsible = TRUE,
        
        shiny::fluidRow(
          style = "background-color: #FFFFFF; border-radius: 1rem;margin:2px;padding-bottom:25px;",
          shiny::column(
            12,
            
            # slider theme
            tagList(
              tags$link(
                href = "slider.css", 
                rel = "stylesheet",
                type = 'text/css'),
              div(id = 'irs',
                  class = "irs--shiny",
                  shiny::div(
                    class = "year-slider"),
                  
                  # slider for year
                  shinyWidgets::sliderTextInput(
                    inputId = "date_elec_sankey",
                    label = "Select a period",
                    choices = format_choice(start = "2018-12-01", end = "2024-12-01"),
                    selected = c(format_choice(start = "2023-12-01", end = "2023-12-01"), 
                                 format_choice(start = "2024-12-01", end = "2024-12-01")),
                    animate = FALSE,
                    grid = TRUE
                  )
              )#div close
            ), #taglist close
          )
        )
      ) %>%
        tagAppendAttributes(
          style = "background:var(--twilight);overflow-y:auto;overflow-x:crop;",
          .cssSelector = ".box"
        ) %>%
        tagAppendAttributes(
          style = "padding:10px;",
          .cssSelector = ".box-body"
        ) %>%
        tagAppendAttributes(
          style = "font-size:20px;font-weight:bold;",
          .cssSelector = ".box-header h3"
        )
    ),
    
    "Electricity retail net flows and switching rate" %>% 
      h2() %>% 
      div(class = "inner") %>%
      div(class = "small-box") %>% 
      column(12, .) %>%
      fluidRow(),
    
    shiny::fluidRow(
      
      #Column chart
      shinydashboard::box(
        width = 6,
        column(
          12,
          div(
            class = "chart-box",
            style = "margin:2px;",
            fluidRow(
              column(
                12,
                shinycssloaders::withSpinner(
                  highchartOutput("elec_column_transfers", height = 700),
                  type = 1,
                  color = "#023047"
                )
              )
            )
          )
        )
      ),
      
      #Switching rate chart
      shinydashboard::box(
        width = 6,
        column(
          12,
          div(
            class = "chart-box",
            style = "margin:2px;",
            fluidRow(
              column(
                12,
                shinycssloaders::withSpinner(
                  highchartOutput("elec_switching_rate", height = 680),
                  type = 1,
                  color = "#023047"
                )
              )
            )
          )
        )
      )
      
    )
    
  )
  
  
}
