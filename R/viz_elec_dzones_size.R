



library(highcharter)

# VIZ ----

highcharts_elec_mktsize <- function(
    data = loadtabs()$offers_elec_vdo_mktsize_summaries,
    customer = "res",
    dist_zone = "jemena",
    size = "large"
    
) {
  
  df <- data %>% 
    filter(cust_type==customer, distribution_zone==dist_zone, cprg_retailer_size == size)
  
  month <- lubridate::month(max(data$identifier_date), label = TRUE, abbr = FALSE) %>% as.character()
  year <- lubridate::year(max(data$identifier_date))
  
  
  # calculate diff for subtitle
  l <- df %>%
    mutate(diff = median - lag(median), sign = if_else(diff>1, "increased", "decreased")) %>%
    filter(identifier_date == max(identifier_date))
  
  s <- l %>% pull(diff) %>% round(.,0) %>% abs()
  sign <- l %>% pull(sign)
  
  
  title <-  paste0("Electricity Offers Annual Bill - ", stringr::str_to_sentence(size), " retailers")
  
  subtitle <-  paste0("Median offers in ", stringr::str_to_sentence(dist_zone)," ", sign, " by $", s, " in ", month," ", year)
  
  caption <- "Source: Victorian Energy Compare. Note: light colour range denotes offers annual bills in the 10th to 90th percentile."
  
  min <- df %>% pull(p10) %>% min() %>% round(digits = -2) *.90
  max <- df %>% pull(p90) %>% max() %>% round(digits = -2) *1.1
  interval <- if_else(customer=="res", 200, 1000)
  m_color <- case_when(
    size == "large" ~ "#ed8b00",
    size == "medium" ~ "#ce0058",
    size == "small" ~ "#236192",
  )
  r_color <- case_when(
    size == "large" ~ "#F1AA44",
    size == "medium" ~ "#DB4484",
    size == "small" ~ "#5D8BAF",
  )
  
  
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
      name = "Median",
      hcaes(x = identifier_date, y = median),
      color = m_color,
      id = "median") %>%
    
    hc_add_series(
      df,
      "line",
      name = "VDO",
      hcaes(x = identifier_date, y = vdo),
      color = "#293241",
      id = "vdo") %>% 
    
    hc_add_series(
      df,
      name = "10th-90th",
      hcaes(x = identifier_date, low = p10, high = p90),
      fillOpacity = 0.1,
      id = "10th-90th", 
      type = "arearange",
      color = r_color) %>% 
    
    highcharter::hc_plotOptions(
      series = list(label = list(enabled = TRUE))
    ) %>%
    
    # custom exporting
    format_export(
      filename = paste("ESC_electricity_offers", customer, dist_zone, size, sep = "_")
    ) %>%
    
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_caption(
      text = caption
    ) %>% 
    thm_highcharts() %>% 
    
    highcharter::hc_rangeSelector(
      inputEnabled = FALSE,
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



#highcharts_elec_mktsize(size = "medium")
