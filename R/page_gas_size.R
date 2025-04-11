







page_gas_sizeUI <- function(...){
  
  
  shiny::fluidPage(
    
    # Title
    
    "Market offers for residential customers" %>%
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
                  highchartOutput("vec_gas_size_res", height = 500),
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
              inputId = "gas_dist_zones_size_res",
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
    
    # Header 2
    "Market offers for small business" %>%
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
                  highchartOutput("vec_gas_size_sme", height = 500),
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
              inputId = "gas_dist_zones_size_sme",
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
    )
  )
  
  
  
}



