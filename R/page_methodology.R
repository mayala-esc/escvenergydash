


page_methodology <- function(...) {
  shiny::fluidPage(
    
    "Methodology" %>%
      h2() %>% div(class = "inner") %>%
      div(class = "small-box") %>% column(12, .) %>%
      fluidRow(),
    
    # Q1
    shinydashboard::box(
      title = "What does the Victorian Energy Prices Dashboard show?",
      p(
        "The Economic Pricing Dashboard shows detailed retail and wholeseale data for Victorian",
        " electricity and gas. It shows retail market offer prices (i.e.  prices retailers are willing to offer in the electricity and gas market to new or switching costumers)",
        " and wholesale prices for electricity and gas. Retail market offers are estimated ",
        " by implementing usage assumptions to allow plans with differing usage charge",
        " block structures to be compared on a like-for-like basis.",
        br(), br(),
        "These assumptions are required because pricing information is at a plan level with no individual customer level usage information.",
        " The following annual consumptions are used:",
        br(),
        " Electricity: 4,000 kWh (Residential) and 20,000 kWh (SME).",
        br(),
        " Gas: 54,400 MJ (Residential) and 500,000 MJ (SME).",
        br(),
        br(),
        "We exclude electricity standing offers from the dashboard as they are priced by the Victorian Default Offer.",
        "",
        ""
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      pagemd_attrb() %>% 
      fluidRow(),
    
    # Q2
    shinydashboard::box(
      title = "Where does the data come from?",
      p(
        "Data is sourced from the Department of Energy, Environment and Climate Action",
        " (DEECA), the Australia Energy Market Operator (AEMO), and",
        " ASX Energy."
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      pagemd_attrb() %>% 
      fluidRow(),
    
    # Q3
    shinydashboard::box(
      title = "How is the annual bill calculated?",
      p(
        "Annual bill is calculated as: ",
        br(),
        br(),
        "Annual Total Bill = supply charge + usage charge – (all conditional + unconditional discounts) + (signup fee + annual fee)",
        br(),
        br(),
        "Annual bill excludes solar feed-in tariff."
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      pagemd_attrb() %>% 
      fluidRow(),
    
    # Q4
    shinydashboard::box(
      title = "How often is the dashboard updated?",
      p(
        "Monthly"
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      pagemd_attrb() %>% 
      fluidRow(),
    
    # Q5
    shinydashboard::box(
      title = "Can I download data or charts?",
      p(
        "All diagrams and charts have a context menu in the upper right-hand corner which allows users to: print the chart; download the chart as an image; download the underlying data as a CSV or XLS file."
      ),
      width = 12,
      collapsed = TRUE,
    ) %>%
      pagemd_attrb() %>% 
      fluidRow(),
    
    # Q6
    shinydashboard::box(
      title = "How do you classify retailers size?",
      p(
        "
        We classify retailers size based on their customer share by customer type"
      ),
      shiny::includeMarkdown("R/retailers.md"),
      retailers_tb(),
      width = 12,
      collapsed = TRUE,
    ) %>%
      pagemd_attrb() %>% 
      
      fluidRow(),
    
    
    
    "Glossary" %>%
      h2() %>% 
      div(class = "inner") %>%
      div(class = "small-box") %>% 
      column(12, .) %>%
      fluidRow(),
    
    fluidRow(
      column(
        12,
        div(style = "background-color: #e5e4e2;border-radius: 1rem;padding: 15px;",
        shiny::includeMarkdown("R/glossary.md")
        )
      )
    )
    
  )
  
}

