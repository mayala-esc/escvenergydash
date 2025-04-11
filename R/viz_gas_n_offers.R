
# CREATE DATA VIZ FOR VEC GAS VOLUME OFFERS

library(highcharter)


highcharts_dzones_gas_noffers <- function(
    data = loadtabs()$offers_count_gas_mktsize,
    customer = "res",
    dist_zone = "australiangasnetworks",
    transform = "n_obs") {
  

  n_offers <- data
  
  # get all dates
  n_dates <- tibble(
    identifier_date = unique(n_offers$identifier_date)
  )
  
  n_df1 <- n_offers %>% 
    filter(cust_type==customer & distribution_zone==dist_zone) %>% 
    tidyr::complete(identifier_date, cprg_retailer_size, cust_type) %>% 
    tidyr::fill(cust_type, distribution_zone) %>% 
    pivot_longer(cols = c("n_obs", "pcent")) %>% 
    filter(name==transform)
  
  
  n_df <- n_dates %>% left_join(n_df1, by = "identifier_date") %>% 
    tidyr::fill(cust_type, distribution_zone) %>% 
    tidyr::complete(identifier_date, cprg_retailer_size) %>% 
    filter(!is.na(cprg_retailer_size))
  
  title <-  if_else(transform == "n_obs", "Volume of offers by distribution zone", "Percent of offers by distribution zone")
  subtitle <-  paste0(if_else(transform == "n_obs", "Number", "Pecent")," of offers in ",stringr::str_to_sentence(dist_zone))
  caption <- "Source: Victorian Energy Compare"
  
  
  # Make Highchart
  
  if (transform=="pcent") {
    
    highchart() %>%
      
      hc_chart(zoomType = 'x') %>%
      
      highcharter::hc_yAxis(
        #title = list(text = "Percent"),
        labels = list(format = "{text}%"),
        tickinterval = interval,
        min = 0,
        max = 100
      ) %>%
      
      hc_xAxis(
        categories = format(unique(n_df$identifier_date), "%b %Y"),
        labels = list(
          step = 12
        )
      ) %>% 
      
      hc_plotOptions(
        column = list(
          stacking = "normal",  # Enable stacking
          dataLabels = list(
            format = '{point.y}%',
            enabled = FALSE  # Enable data labels
          )
        )) %>% 
      
      hc_add_series(
        n_df,
        "column",
        hcaes(y = value, group = cprg_retailer_size),
      ) %>%
      
      highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
      
      highcharter::hc_exporting(enabled = TRUE) %>%
      highcharter::hc_title(text = title) %>%
      highcharter::hc_subtitle(text = subtitle) %>%
      highcharter::hc_caption(text = caption) %>% 
      thm_highcharts() %>% 
      
      highcharter::hc_rangeSelector(
        inputEnabled = F,
        selected = 0,
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
      highcharter::hc_tooltip(
        headerFormat = '<b>{point.x}</b><br/>',
        pointFormat = '{series.name}: {point.y}%',
        dateTimeLabelFormats = list(day = "%b %Y")
      )
    
    
  }else{
    
    highchart() %>%
      
      hc_chart(zoomType = 'x') %>%
      
      hc_xAxis(
        categories = format(unique(n_df$identifier_date), "%b %Y"),
        labels = list(
          step = 12
          )
      ) %>% 
      
      hc_plotOptions(
        column = list(
          stacking = "normal"  # Enable stacking
        )) %>% 
      
      hc_add_series(
        n_df,
        "column",
        hcaes(y = value, group = cprg_retailer_size),
      ) %>%
      
      highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
      
      highcharter::hc_exporting(enabled = TRUE) %>%
      highcharter::hc_title(text = title) %>%
      highcharter::hc_subtitle(text = subtitle) %>%
      highcharter::hc_caption(text = caption) %>% 
      thm_highcharts() %>% 
      
      highcharter::hc_rangeSelector(
        inputEnabled = F,
        #floating = FALSE,
        #buttonPosition = list(align = "left"),
        selected = 0,
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
      highcharter::hc_tooltip(
        headerFormat = '<b>{point.x}</b><br/>',
        pointFormat = '{series.name}: {point.y}<br/>Total: {point.stackTotal}',
        dateTimeLabelFormats = list(day = "%b %Y")
      )
    
  }
  
}


