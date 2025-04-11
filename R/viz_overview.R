
# CREATE DATA VIZ FOR VEC OVERVIEW

library(highcharter)


# ELEC CHART ----
highcharts_overview_elec <- function(data = loadtabs()$offers_elec_vdo_median,
                                     customer = "res") {
  
  options(dplyr.summarise.inform = FALSE)
  
  df <- data %>% filter(cust_type==customer)
  
  # calculate diff for subtitle
  l <- df %>%
    dplyr::rename(value = annual_bill_median) %>% 
    mutate(diff = value - lag(value), sign = if_else(diff>1, "increased", "decreased")) %>%
    filter(identifier_date == max(identifier_date))
  
  s <- l %>% pull(diff) %>% round(.,0) %>% abs()
  sign <- l %>% pull(sign)
  
  month <- lubridate::month(max(df$identifier_date), label = TRUE, abbr = FALSE) %>% as.character()
  year <- lubridate::year(max(df$identifier_date))
  
  # prepare title, subtitle and caption
  title <-  "Retail electricity offers in Victoria"
  subtitle <- paste0(
    "The median market offer in Victoria ", sign, " by $", s, " in ", month," ", year
    )
  caption <- "Source: Victorian Energy Compare"
  
  # restrict x and y axis
  min <- df %>% dplyr::pull(annual_bill_median) %>% min() %>% round(digits = -2) *.90
  max <- df %>% dplyr::pull(annual_bill_median) %>% max(na.rm = TRUE) %>% round(digits = -2) *1.1
  interval <- dplyr::if_else(customer=="res", 200, 1000)
  
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
      name = "VDO",
      dashStyle = "Dash",
      hcaes(x = identifier_date, y = vdo_avg)
      ) %>% 
    
    hc_add_series(
      df, 
      "line",
      name = "Market offer",
      hcaes(x=identifier_date, y = annual_bill_median)
      ) %>%
    
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    
    # custom chart exporting
    format_export(
      filename = paste("ESC_electricity_offers_vic", customer, sep = "_")
    ) %>%
    
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_caption(
      text = caption
      ) %>% 
    thm_highcharts() %>% 
    
    # selecting ranges to display within the chart
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
    
    # format tooltip
    highcharter::hc_tooltip(
      dateTimeLabelFormats = list(day = "%b %Y"),
      valuePrefix = "$"
    ) %>%
    hc_scrollbar(enabled = FALSE)
  
  
}



# GAS CHART ----

highcharts_overview_gas <- function(data = loadtabs()$offers_gas_median,
                                    customer = "res"
                                    ) {
  
  options(dplyr.summarise.inform = FALSE)
  
  df <- data %>% filter(cust_type==customer)
  
  # dynamic subtitle
  # calculate diff for subtitle
  l <- df %>%
    filter(offer_type=="market offer") %>% 
    dplyr::rename(value = annual_bill_median) %>% 
    mutate(diff = value - lag(value), sign = if_else(diff>1, "increased", "decreased")) %>%
    filter(identifier_date == max(identifier_date))
  
  s <- l %>% pull(diff) %>% round(.,0) %>% abs()
  sign <- l %>% pull(sign)
  
  month <- lubridate::month(max(df$identifier_date), label = TRUE, abbr = FALSE) %>% as.character()
  year <- lubridate::year(max(df$identifier_date))
  
  # prepare title, subtitle and caption
  title <-  "Retail gas offers in Victoria"
  subtitle <- paste0("The median market offer in Victoria ", sign, " by $", s, " in ", month," ", year)
  caption <- "Source: Victorian Energy Compare"
  
  
  # Make Highchart
  highchart(type = "stock") %>%
    
    highcharter::hc_yAxis(
      title = list(text = "Annual Bill"),
      labels = list(format = "${text}")
    ) %>% 
    
    highcharter::hc_xAxis(
      type = "datetime",
      dateTimeLabelFormats = list(month = '%b %Y')
    ) %>% 
    
    hc_add_series(
      df, 
      "line",
      hcaes(x = identifier_date, y = annual_bill_median, group = offer_type),
      color = c("#ce0058", "#236192"),
      name = c("Market offer", "Standing offer")
      ) %>%
    
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    
    # custom chart exporting
    format_export(
      filename = paste("ESC_gas_offers_vic", customer, sep = "_")
    ) %>%
    
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_caption(text = caption) %>% 
    
    # custom theme
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
      dateTimeLabelFormats = list(day = "%b %Y"),
      valuePrefix = "$"
    ) %>%
    hc_navigator(enabled = FALSE) %>%
    hc_scrollbar(enabled = FALSE)
  
  
}

