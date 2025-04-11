
# CREATE DATA VIZ FOR VEC ELEC VOLUME OFFERS 

library(highcharter)


highcharts_dzones_elec_noffers <- function(
    data = loadtabs()$offers_count_elec_mktsize,
    customer = "res",
    dist_zone = "jemena",
    transform = "n_obs") {
  
  
  options(dplyr.summarise.inform = FALSE)
  
  n_offers <- data
  
  n_df <- n_offers %>% 
    filter(cust_type==customer & distribution_zone==dist_zone) %>% 
    tidyr::complete(identifier_date, cprg_retailer_size) %>% 
    pivot_longer(cols = c("n_obs", "pcent")) %>% 
    filter(name==transform)
  
  title <-  if_else(transform == "n_obs", "Volume of offers by distribution zone", "Percent of offers by distribution zone")
  subtitle <-  paste0(if_else(transform == "n_obs", "Number", "Pecent")," of offers in ",stringr::str_to_sentence(dist_zone))
  caption <- "Source: Victorian Energy Compare"
  
  
  # Make Highchart

  if (transform=="pcent") {
    
    highchart() %>%
      
      hc_chart(
        zoomType = 'x'
        ) %>%
      
      highcharter::hc_yAxis(
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
        color = c("#ed8b00", "#ce0058","#236192")
      ) %>%
      
      highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
      
      # custom exporting
      highcharter::hc_exporting(
        enabled = TRUE,
        filename = paste("ESC_percent_electricity_offers", customer, dist_zone, sep = "_"),
        chartOptions = list(
          chart = list(
            width = 1600,
            height = 500
          ),
          xAxis = list(
            dateTimeLabelFormats = list(month = '%b %Y')
          )
        )
        ) %>%
      
      
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
      
      hc_chart(
        zoomType = 'x'
        ) %>%
      
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
        color = c("#ed8b00", "#ce0058","#236192")
      ) %>%
      
      highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
      
      highcharter::hc_exporting(
        enabled = TRUE,
        filename = paste("ESC_number_electricity_offers", customer, dist_zone, sep = "_"),
        chartOptions = list(
          chart = list(
            width = 1600,
            height = 500
          ),
          xAxis = list(
            dateTimeLabelFormats = list(month = '%b %Y')
          )
        )
        ) %>%
      
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
        pointFormat = '{series.name}: {point.y}<br/>Total: {point.stackTotal}',
        dateTimeLabelFormats = list(day = "%b %Y")
      )
    
  }
  
  
}

