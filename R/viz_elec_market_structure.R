
library(highcharter)

# Market share treemap-------------
highcharts_elec_treemap <- function(
    data = loadtabs()$retail_mktshare,
    date_chosen = format(as_date("2024-12-01"), format = "%b %Y"),
    fuel_type = "electricity",
    customer_type = "res"){
  
  mkt_share <- data %>% 
    dplyr::mutate(
      date_full = format(as_date(paste0(date,"-01")), format = "%b %Y"),
      size = if_else(retailer %in% c("EnergyAustralia", "AGL", "Origin Energy"),
                     "Big 3",
                     size)
      ) %>%
    filter(fuel == fuel_type,
           date_full == date_chosen,
           customer == customer_type
           ) 
  
  market_share <- mkt_share %>% 
    data_to_hierarchical(c(size, retailer), 
                         market_share, 
                         colors = c("#ed8b00","#8CB8CB","#ce0058","#236192")
                         )
  
  total_ret <- mkt_share %>% filter(market_share>0) %>% slice_max(date_full) %>% pull(retailer) %>% unique() %>% length()
  
  highcharter::hchart(
    market_share,
    dataLabels = list(style = list(fontSize = '15px')),
    type = "treemap",
    colorByPoint = FALSE,
    allowTraversingTree = T,
    levelIsConstant = F,
    levels = list(
      list(
        level = 1,
        dataLabels = list(enabled = TRUE,
                          format = "{point.name}<br>{point.value:.1f}%"),
        borderColor = "#FFFFFF",
        borderWidth = 2
      ),
      list(
        level = 2,
        dataLabels = list(enabled = TRUE,
                          format = ""),
        borderColor = "#FFFFFF",
        borderWidth = 2
      )
    )
  ) %>%
    hc_caption(
      text = "Source: Compliance & Performance Regulatory Guidelines"
      ) %>% 
    hc_title(
      text = paste0("There are ", total_ret, " retailers with ", if_else(customer_type=="res", "residential", "SME"), " customers")
      ) %>%
    hc_tooltip(
      pointFormat = '<b>{point.name}</b>: {point.value:.2f}%'
      ) %>% 
    
    thm_highcharts()
  
}  

# Retail transfers sankey ------------------------------------------------
highcharts_elec_sankey <- function(
    data = loadtabs()$msats_retail_transfers,
    end_date = format(as_date("2024-09-01"), format = "%b %Y"),
    start_date = format(as_date("2024-08-01"), format = "%b %Y")
    ){
  
  # date range for dynamic title
  year <- as.numeric(format(dmy(paste("01", end_date)),"%Y"))
  month <- as.numeric(format(dmy(paste("01", end_date)),"%m"))
  year_prev <- as.numeric(format(dmy(paste("01", start_date)),"%Y"))
  month_prev <- as.numeric(format(dmy(paste("01", start_date)),"%m"))
  
  # make date range into date format
  start_chosen <- dmy(paste("01", start_date))
  end_chosen <- dmy(paste("01", end_date))
  
  # data filtered by user
  sankey_data <- data %>% 
    dplyr::filter(STAT_DATE >= start_chosen & STAT_DATE <= end_chosen) %>% 
    dplyr::group_by(size_FRMP, size_NEWFRMP) %>% 
    dplyr::summarise(total_flows = sum(STAT_VALUE)) %>% 
    ungroup()
  
  # period customer flows
  sankey_groups <- sankey_data %>% 
    filter(size_FRMP != size_NEWFRMP) %>% 
    dplyr::mutate(
      big3_flows = case_when(
        size_FRMP == "big 3" ~ -total_flows,
        size_NEWFRMP == "big 3" ~ total_flows,
        TRUE ~ 0
      ),
      large_flows = case_when(
        size_FRMP == "large" ~ -total_flows,
        size_NEWFRMP == "large" ~ total_flows,
        TRUE ~ 0
      ),
      medium_flows = case_when(
        size_FRMP == "medium" ~ -total_flows,
        size_NEWFRMP == "medium" ~ total_flows,
        TRUE ~ 0
      ),
      small_flows = case_when(
        size_FRMP == "small" ~ -total_flows,
        size_NEWFRMP == "small" ~ total_flows,
        TRUE ~ 0
      )
    ) %>% 
    select(-total_flows)
  
  # get the net flows
  total_flows <- sankey_groups %>% 
    dplyr::summarise(across(ends_with("_flows"), ~sum(.x, na.rm = TRUE))) %>% 
    tidyr::pivot_longer(everything(), names_to = "type", values_to = "value") %>% 
    separate_wider_delim(type, delim = "_", names = c("type", "var")) %>% 
    dplyr::mutate(value_char = comma(value))
  
  
  # dynamic title inputs
  ttl_df <- total_flows %>% slice_max(value)
  ttl_size <- if_else(ttl_df$type=="big3", "Big 3", stringr::str_to_sentence(ttl_df$type))
  ttl_change <- "gained"
  ttl_value <- comma(round(ttl_df$value,-2))
  
  # dynamic title
  ttl <- paste(ttl_size,"retailers", ttl_change, "around", 
                ttl_value, "customers from",month.name[month_prev], year_prev,
                "to", month.name[month], year,sep = " ")
  
  
  net_big3 <- total_flows[total_flows$type == "big3", "value_char"]$value_char
  net_large <- total_flows[total_flows$type == "large", "value_char"]$value_char
  net_medium <- total_flows[total_flows$type == "medium", "value_char"]$value_char
  net_small <- total_flows[total_flows$type == "small", "value_char"]$value_char
  
  
  # paste the net flows into the end node
  sankey_data <- sankey_data %>% 
    mutate(
      weight = round(100*total_flows/sum(sankey_data$total_flows),1),
      tosize_NEWFRMP = case_when(
        size_NEWFRMP == "big 3" ~ paste("Big 3", net_big3, sep = ": "),
        size_NEWFRMP == "large" ~ paste("Large", net_large, sep = ": "),
        size_NEWFRMP == "medium" ~ paste("Medium", net_medium, sep = ": "),
        size_NEWFRMP == "small" ~ paste("Small", net_small, sep = ": "),
        TRUE ~ NA_character_
      ),
      size_FRMP = str_to_title(size_FRMP)
      )
  
  destinations <- sort(unique(sankey_data$tosize_NEWFRMP))
  
  # make highchart
  highcharter::hchart(
    sankey_data,
    "sankey",
    hcaes(from = size_FRMP, to = tosize_NEWFRMP, weight = total_flows)
  ) %>% 
    hc_title(text = ttl) %>%
    
    hc_caption(text = "Source: AEMO") %>%
    
    hc_add_series(
      colorByPoint = TRUE
      ) %>%
    hc_xAxis(
      visible = FALSE
      ) %>%
    hc_plotOptions(
      sankey = list(
        dataLabels = list(
          enabled = TRUE,
          style = list(fontSize = '15px'),
          nodeformat = '{point.name}: {point.sum:.1f}'
          ),
        nodeWidth = 30,
        nodePadding = 50,
        nodes = list(
          list(id = "Big 3", color = "#ed8b00"),
          list(id = "Large", color = "#8CB8CB"),
          list(id = "Medium", color = "#ce0058"),
          list(id = "Small", color = "#236192"),
          list(id = destinations[1], color = "#ed8b00"),
          list(id = destinations[2], color = "#8CB8CB"),
          list(id = destinations[3], color = "#ce0058"),
          list(id = destinations[4], color = "#236192"),
          list(id = "New Connections", color = "#75787b")
        )
      )
      ) %>%
    hc_tooltip(
      headerFormat = NULL,
      pointFormat = 'Customers: {point.weight:,.0f}',
      nodeFormat = '{point.name}'
      ) %>%
    thm_highcharts()
  
}

# Retail transfers column graph ------

highcharts_elec_column_transfers <- function(
    data = loadtabs()$msats_retail_transfers_net
    ){
  
  df_net_flows <- data %>% 
    dplyr::group_by(date, size_group) %>% 
    dplyr::summarise(net_flows = sum(qty_net)) %>% 
    ungroup() %>% 
    mutate(date = as.Date(format(dmy(paste("01", date)),"%Y-%m-%d"))) %>% 
    filter(!size_group == "New Connections") %>% 
    arrange(date)
  
  
  title <- "Net customer transfers, by retailer size"
  subtitle <- "Victorian residential and small business customers"
  caption <- "Source: AEMO"
  
  # make highchart
  highchart() %>%
    hc_chart(
      zoomType = 'x'
      ) %>% 
    hc_xAxis(
      categories = format(unique(df_net_flows$date), "%b %Y"),
      labels = list(
        step = 12
        )
    ) %>%
    highcharter::hc_yAxis(
      gridLineWidth = 0,
      plotLines  = list(list(
        zIndex = 4,
        value = 0
      ))
    ) %>%
    hc_plotOptions(
      column = list(
        stacking = "normal"  # Enable stacking
      )) %>% 
    
    hc_legend(
      x = -10,
      y = 50,
      verticalAlign = "top",
      floating = TRUE
    ) %>% 
    
    hc_add_series(
      df_net_flows,
      "column",
      hcaes(y = net_flows, group = size_group),
      color = c("#ed8b00", "#8CB8CB", "#ce0058", "#236192")
    ) %>%
    
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "ESC_net_electricity_transfers",
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
    highcharter::hc_tooltip(
      headerFormat = '<b>{point.x}</b><br/>',
      pointFormat = '{series.name}: {point.y}<br/>Total: {point.stackTotal:,.0f}',
      dateTimeLabelFormats = list(day = "%b %Y")
    )
  
}  


# Retail switching rate--------------
highcharts_elec_switchingrate <- function(
    data = loadtabs()$msats_retail_switch_totals
    ) {
  
  df_total_switches <- data
  
  # time series conversion for moving average
  ts_endyear <- as.numeric(format(as.Date(max(df_total_switches$date, 
                                              format = "%Y-%m-%d")),"%Y"))
  ts_endmonth <- as.numeric(format(as.Date(max(df_total_switches$date, 
                                               format = "%Y-%m-%d")),"%m"))
  
  ts_switching <- ts(data = df_total_switches$switching_rate, 
                     start = c(2018,1), 
                     end = c(ts_endyear,ts_endmonth), 
                     frequency = 12)
  
  ts_switching_decomp <- decompose(ts_switching, type = "additive")
  ts_switching_trend <- ts_switching_decomp$trend
  
  df_switching_trend <- as.data.frame(ts_switching_trend) %>% 
    rename(trend = x) %>% 
    bind_cols(df_total_switches) %>% 
    mutate(trend = round(trend,2),
           switching_rate = round(switching_rate,2))
  
  
  subttl <- df_switching_trend %>% slice_max(date)
  
  # prepare title, subtitle and caption
  title <-  "Electricity retail switching rate"
  subtitle <- paste(subttl$switching_rate,
                    "% of electricity customers switched retailer in ",
                    format(subttl$date, "%B %Y"), sep="")
  
  caption <- "Source: AEMO"
  
  
  # Make Highchart
  highchart(type = "stock") %>%
    
    highcharter::hc_yAxis(
      labels = list(format = "{text}%"),
      tickinterval = interval,
      min = min(df_total_switches$switching_rate),
      max = max(df_total_switches$switching_rate)
    ) %>%
    highcharter::hc_xAxis(
      type = "datetime",
      dateTimeLabelFormats = list(month = '%b %Y')
    ) %>%
    
    
    hc_add_series(
      df_switching_trend,
      "line",
      name = "Trend",
      dashStyle = "Dash",
      color = "#236192",
      hcaes(x = date, y = trend)
    ) %>% 
    
    hc_add_series(
      df_switching_trend, 
      "line",
      name = "Switching rate",
      color = "#75787b",
      hcaes(x = date, y = switching_rate)
    ) %>%
    
    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))) %>%
    
    # custom chart exporting
    format_export(
      filename = "ESC_electricity_switching_rate"
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
      valueSuffix = "%"
    ) %>%
    hc_scrollbar(enabled = FALSE)
  
  
}
