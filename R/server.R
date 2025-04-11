

# server ----
server <- function(input, output, session) {
  
  # overview section ----
  output$vec_overview_elec <- renderHighchart({
    highcharts_overview_elec(
      customer = input$cust_type_elec
    )
  }) %>% bindCache(input$cust_type_elec)
  
  output$vec_overview_gas <- renderHighchart({
    highcharts_overview_gas(
      customer = input$cust_type_gas
    )
  }) %>% bindCache(input$cust_type_gas)
  
  # elec - dist. zones section ----
  output$vec_elec_dzones <- renderHighchart({
    highcharts_dzones_elec(
      customer = input$cust_type_elec,
      dist_zone = input$elec_dist_zones
    )
  }) %>% bindCache(input$cust_type_elec,input$elec_dist_zones)
  
  
  # elec - overview size section ----
  output$vec_elec_size_res <- renderHighchart({
    highcharts_elec_size(
      dist_zone = input$elec_dist_zones_size_res,
      customer = "res"
    )
  }) %>% bindCache(input$elec_dist_zones_size_res)
  
  
  output$vec_elec_size_sme <- renderHighchart({
    highcharts_elec_size(
      dist_zone = input$elec_dist_zones_size_sme,
      customer = "sme"
    )
  }) %>% bindCache(input$elec_dist_zones_size_sme)
  
  
  # elec - retail size section ----
  output$vec_elec_mktsize_lrg <- renderHighchart({
    highcharts_elec_mktsize(
      customer = input$cust_type_elec_mktsize_lrg,
      dist_zone = input$elec_dist_zones_mktsize_lrg,
      size = "large"
    )
  }) %>% bindCache(input$cust_type_elec_mktsize_lrg,input$elec_dist_zones_mktsize_lrg)
  
  
  output$vec_elec_mktsize_med <- renderHighchart({
    highcharts_elec_mktsize(
      customer = input$cust_type_elec_mktsize_med,
      dist_zone = input$elec_dist_zones_mktsize_med,
      size = "medium"
    )
  }) %>% bindCache(input$cust_type_elec_mktsize_med, input$elec_dist_zones_mktsize_med)
  
  output$vec_elec_mktsize_sml <- renderHighchart({
    highcharts_elec_mktsize(
      customer = input$cust_type_elec_mktsize_sml,
      dist_zone = input$elec_dist_zones_mktsize_sml,
      size = "small"
    )
  }) %>% bindCache(input$cust_type_elec_mktsize_sml, input$elec_dist_zones_mktsize_sml)
  
  
  # elec - volume section ----
  output$vec_elec_noffers <- renderHighchart({
    highcharts_dzones_elec_noffers(
      customer = input$cust_type_elec,
      dist_zone = input$elec_dist_zones
    )
  }) %>% bindCache(input$cust_type_elec,input$elec_dist_zones)
  
  output$vec_elec_noffers_pcent <- renderHighchart({
    highcharts_dzones_elec_noffers(
      customer = input$cust_type_elec_pcent,
      dist_zone = input$elec_dist_zones_pcent,
      transform = "pcent"
    )
  }) %>% bindCache(input$cust_type_elec_pcent,input$elec_dist_zones_pcent)
  
  
  # elec - wholesale section ----
  output$vec_elec_spot <- renderHighchart({
    highcharts_spot_elec()
  })
  
  output$vec_elec_futures <- renderHighchart({
    highcharts_futures_elec()
  })
  
  
  # elec - treemap section ----
  output$elec_treemap <- renderHighchart({
    highcharts_elec_treemap(date_chosen = input$date_elec_treemap,
                            fuel_type = "electricity", 
                            customer_type = input$cust_type_elec_treemap)
  }) %>% bindCache(input$date_elec_treemap,
                   input$cust_type_elec_treemap)
  
  # elec - sankey section ----
  output$elec_sankey <- renderHighchart({
    highcharts_elec_sankey(start_date = input$date_elec_sankey[1],
                           end_date = input$date_elec_sankey[2])
  }) %>% bindCache(input$date_elec_sankey[1], input$date_elec_sankey[2])
  
  
  # elec - column chart section ----
  output$elec_column_transfers <- renderHighchart({
    highcharts_elec_column_transfers()
  })
  
  # elec - switching rate section ----
  output$elec_switching_rate <- renderHighchart({
    highcharts_elec_switchingrate()
  })
  
  # gas - dist. zones section ----
  output$vec_gas_dzones <- renderHighchart({
    highcharts_dzones_gas(
      customer = input$cust_type_gas,
      dist_zone = input$gas_dist_zones
    )
  }) %>% bindCache(input$cust_type_gas,input$gas_dist_zones)
  
  # gas - overview size section ----
  output$vec_gas_size_res <- renderHighchart({
    highcharts_gas_size(
      dist_zone = input$gas_dist_zones_size_res,
      customer = "res"
    )
  }) %>% bindCache(input$gas_dist_zones_size_res)
  
  output$vec_gas_size_sme <- renderHighchart({
    highcharts_gas_size(
      dist_zone = input$gas_dist_zones_size_sme,
      customer = "sme"
    )
  }) %>% bindCache(input$gas_dist_zones_size_sme)
  
  # GAS - retail size section ----
  output$vec_gas_mktsize_lrg <- renderHighchart({
    highcharts_gas_mktsize(
      customer = input$cust_type_gas_mktsize_lrg,
      dist_zone = input$gas_dist_zones_mktsize_lrg,
      size = "large"
    )
  }) %>% bindCache(input$cust_type_gas_mktsize_lrg,
                   input$gas_dist_zones_mktsize_lrg)
  
  
  output$vec_gas_mktsize_med <- renderHighchart({
    highcharts_gas_mktsize(
      customer = input$cust_type_gas_mktsize_med,
      dist_zone = input$gas_dist_zones_mktsize_med,
      size = "medium"
    )
  }) %>% bindCache(input$cust_type_gas_mktsize_med,
                   input$gas_dist_zones_mktsize_med)
  
  
  output$vec_gas_mktsize_sml <- renderHighchart({
    highcharts_gas_mktsize(
      customer = input$cust_type_gas_mktsize_sml,
      dist_zone = input$gas_dist_zones_mktsize_sml,
      size = "small"
    )
  }) %>% bindCache(input$cust_type_gas_mktsize_sml,
                   input$gas_dist_zones_mktsize_sml)
  
  
  
  # gas - volume section
  output$vec_gas_noffers <- renderHighchart({
    highcharts_dzones_gas_noffers(
      customer = input$cust_type_gas,
      dist_zone = input$gas_dist_zones
    )
  }) %>% bindCache(input$cust_type_gas,
                   input$gas_dist_zones)
  
  
  output$vec_gas_noffers_pcent <- renderHighchart({
    highcharts_dzones_gas_noffers(
      customer = input$cust_type_gas_pcent,
      dist_zone = input$gas_dist_zones_pcent,
      transform = "pcent"
    )
  }) %>% bindCache(input$cust_type_gas_pcent,
                   input$gas_dist_zones_pcent)
  
  
  # gas - wholesale section ----
  output$vec_gas_spot <- renderHighchart({
    highcharts_spot_gas()
  })
  
  
  # gas - treemap section ----
  output$gas_treemap <- renderHighchart({
    
    highcharts_gas_treemap(
      date_chosen = input$date_gas_treemap,
      fuel_type = "gas",
      customer_type = input$cust_type_gas_treemap
    )
  }) %>% bindCache(input$date_gas_treemap,
                   input$cust_type_gas_treemap)
  
  
}
