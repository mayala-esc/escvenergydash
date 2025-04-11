page_disclaimerUI <- function(...){
  fluidPage(
    "Disclaimer" %>%
      h2() %>% 
      div(class = "inner") %>%
      div(class = "small-box") %>% 
      column(12, .) %>%
      fluidRow(),
    
    fluidRow(
      shinydashboard::box(
        width = 12,
        p(
          "The Economic Pricing Dashboard (EP) data is not and should not be regarded as legal, business or other professional advice.",
          "It should not be relied on and is not a substitute for obtaining idependent legal, business or other professional advice relevant to your particular cirmcumstances.",
          br(), br(),
          "EP data is based on information supplied to the Essential Services Commission (commission) by third parties",
          " for information purposes only. No claim is made as to the accuracy",
          "or currency of any of the content on this dashboard at any time. The ",
          "commission do not accept any liability to any person",
          "for the information (or the use of such information) which is",
          "provided on this dashboard, including information sourced or derived",
          "from third parties or incorporated into the dashboard by reference."
        )
      )
    )
  )
}
