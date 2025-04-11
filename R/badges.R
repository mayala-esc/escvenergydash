

# badges on top right corner of dash

badge_vec <- function(...){
  
  
  d <- loadtabs()
  
  m <- d$offers_elec_vdo_median %>% pull(identifier_date) %>% max() %>% lubridate::month(label = TRUE, abbr = FALSE)
  y <- d$offers_elec_vdo_median %>% pull(identifier_date) %>% max() %>% lubridate::year()
  
  
  tags$button(
    #onclick="window.open('https://compare.energy.vic.gov.au/','mywindow');",
    class = "sourceBadge",
    #style = "cursor: pointer;",
    role = "button",
    div(
      style = "display: table-cell;",
      "Updated"
    ),
    div(
      style = "display: table-cell;",
      format(paste0(m," ", y)) %>%
        stringr::str_replace_all("[:blank:]", "&nbsp;") %>%
        HTML(),
      br(),
      span(style = "font-size:8px", "Monthly")
    )
  )
}
