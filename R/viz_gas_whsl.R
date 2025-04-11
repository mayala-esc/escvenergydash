
# CREATE DATA VIZ FOR VEC ELECTRICITY WHOLESALE SPOT PRICE


library(highcharter)


highcharts_spot_gas <- function(data = loadtabs()$whsl_gas
                               ) {
  
  gas_spot <- data
    
  month <- lubridate::month(max(gas_spot$Dates), label = TRUE, abbr = FALSE) %>% as.character()
  year <- lubridate::year(max(gas_spot$Dates))
  
  l <- gas_spot %>%
    filter(variable=="Spot") %>%
    mutate(diff = value - lag(value), sign = if_else(diff > 0, "increased", "decreased")) %>%
    filter(Dates == max(gas_spot$Dates)) 
  
  s <- l %>% pull(diff) %>% round(.,0) %>% abs()
  sign <- l %>% pull(sign)
  
  
  title <-  "Monthly Average Wholesale Gas Price"
  subtitle <-  paste0("Spot price ", sign, " by $", s, " per GJ in ", month," ", year)
  caption <- "Source: AEMO"
  
  
  # Make Highchart
  highcharter::hchart(
    gas_spot,
    "line",
    highcharter::hcaes(x=Dates, y = round(value,0), group = variable),
    marker = list(enabled = FALSE)
  ) %>% 
    hc_chart(
      zoomType = 'x'
    ) %>% 
    
    # Annotations
    highcharter::hc_annotations(
      list(
        labelOptions = list(
          backgroundColor = 'rgba(255,255,255,0.5)'
        ),
        draggable = "",
        labels = list(
          list(point = list(x = datetime_to_timestamp(as_date("2022-06-01")), 
                            y = 40, 
                            xAxis = 0, 
                            yAxis = 0), 
               text = "War in Ukraine",
               x = 40)
        )
      )
    ) %>% 
    
    highcharter::hc_plotOptions(
      series = list(label = list(enabled = TRUE))
    ) %>%
    
    highcharter::hc_yAxis(
      title = list(text = "$/GJ"),
      labels = list(format = "${text}"),
      tickAmount = 4,
      accessibility = list(description = "Gas Spot per GJ")
    ) %>% 
    highcharter::hc_xAxis(
      title = list(text = NULL)
    ) %>%
    
    # custom exporting
    highcharter::hc_exporting(
      enabled = TRUE, 
      filename = "ESC_victorian_gas_spot_price",
      chartOptions = list(
        chart = list(
          width = 1600,
          height = 500
        )
      )
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







