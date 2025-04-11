



library(highcharter)

# VIZ ----

highcharts_gas_size <- function(
    data = loadtabs()$offers_gas_mktsize_summaries,
    customer = "res",
    dist_zone = "australiangasnetworks"
    
) {
  
  df <- data %>% filter(cust_type==customer & distribution_zone==dist_zone)
  
  month <- lubridate::month(max(data$identifier_date), label = TRUE, abbr = FALSE) %>% as.character()
  year <- lubridate::year(max(data$identifier_date))
  
  # calculate lowest for subtitle
  l <- df %>%
    filter(identifier_date == max(identifier_date)) %>% 
    mutate(lowest = min(median)) %>% 
    filter(median == lowest)
  
  size <- l %>% pull(cprg_retailer_size)
  
  
  title <-  paste0("Median Gas Offers Annual Bill - ", stringr::str_to_sentence(customer))
  
  subtitle <-  paste0(stringr::str_to_sentence(size), " size retailers have the lowest median offers in ", stringr::str_to_sentence(dist_zone), " in ", month," ", year)
  
  caption <- "Source: Victorian Energy Compare."
  
  min <- df %>% pull(median) %>% min() %>% round(digits = -2) *.90
  max <- df %>% pull(median) %>% max() %>% round(digits = -2) *1.1
  interval <- if_else(customer=="res", 200, 1000)
  
  
  # Make Highchart
  highchart(type = "stock") %>%
    
    
    highcharter::hc_yAxis(
      title = list(text = "Annual Bill"),
      labels = list(format = "${text}"),
      tickinterval = interval,
      min = min,
      max = max
    ) %>% 
    highcharter::hc_xAxis(
      type = "datetime",
      dateTimeLabelFormats = list(month = '%b %Y')
    ) %>% 
    
    
    hc_add_series(
      df,
      "line",
      name = c("Large", "Medium", "Small"),
      hcaes(x = identifier_date, y = median, group = cprg_retailer_size),
      color = c("#ed8b00", "#ce0058", "#236192"),
      id = "median") %>%
    
    highcharter::hc_plotOptions(
      series = list(label = list(enabled = TRUE))
    ) %>%
    
    # custom exporting
    format_export(
      filename = paste("ESC_gas_offers", customer, dist_zone, sep = "_")
    ) %>%
    
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_caption(
      text = caption
    ) %>% 
    thm_highcharts() %>% 
    
    highcharter::hc_rangeSelector(
      inputEnabled = T,
      floating = FALSE,
      buttonPosition = list(align = "left"),
      selected = 2,
      buttons = list(
        list(
          type  = 'all',
          text  =  'All',
          title =  'View all'
        ),
        list(
          type  = 'year',
          count = 2,
          text  = '2y',
          title = 'View two years'
        )
      )
    ) %>%
    highcharter::hc_navigator(series = list(label = list(enabled = FALSE))) %>%
    highcharter::hc_tooltip(
      valuePrefix = "$"
    ) %>%
    hc_scrollbar(enabled = FALSE)
  
  
}



#highcharts_gas_size()
