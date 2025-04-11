
library(highcharter)

# Gas-------------
highcharts_gas_treemap <- function(
    data = loadtabs()$retail_mktshare,
    date_chosen = format(as_date("2024-09-01"), format = "%b %Y"),
    fuel_type = "gas",
    customer_type = "res"){
  
  
  mkt_share <- data %>% 
    dplyr::mutate(
      date_full = format(as_date(paste0(date,"-01")), format = "%b %Y")
    ) %>% 
    dplyr::filter(fuel == fuel_type,
           date_full == date_chosen,
           customer == customer_type
    ) 
  
  
  market_share <- mkt_share %>% 
    dplyr::mutate(size = ifelse(retailer %in% c("EnergyAustralia", "AGL", "Origin Energy"),
                         "Big 3", size)) %>% 
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
                          format = "{point.name}<br>
                          {point.value:.1f}%"),
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

