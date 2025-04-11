
# CREATE DATA VIZ FOR VEC ELECTRICITY WHOLESALE SPOT PRICE


highcharts_spot_elec <- function(
    data = loadtabs()$whsl_elec
    ) {

  elec_spot <- data

  month <- lubridate::month(max(elec_spot$Dates), label = TRUE, abbr = FALSE) %>% as.character()
  year <- lubridate::year(max(elec_spot$Dates))

  l <- elec_spot %>%
    filter(variable=="Spot") %>%
    mutate(diff = value - lag(value), sign = if_else(diff > 0, "increased", "decreased")) %>%
    filter(Dates == max(elec_spot$Dates))

  s <- l %>% pull(diff) %>% round(.,0) %>% abs()
  sign <- l %>% pull(sign)

  title <- "Wholesale Spot Price in Victoria"
  subtitle <- paste0("Spot Price ", sign, " by $", s, " per MWh in ", month," ", year)
  caption <- "Source: AEMO"


  # Make Highchart
  highcharter::hchart(
    elec_spot,
    "line",
    highcharter::hcaes(x = Dates, y = round(value,0), group = variable),
    marker = list(enabled = FALSE)
    ) %>%

    # Annotations
    highcharter::hc_annotations(
      list(
        labelOptions = list(
          backgroundColor = 'rgba(255,255,255,0.5)'
        ),
        draggable = "",
        labels = list(
          list(
            point = list(x = datetime_to_timestamp(as_date("2017-04-01")), y = 150, xAxis = 0, yAxis = 0),
            text = "Hazelwood<br>decommissioned"),
          list(
            point = list(x = datetime_to_timestamp(as_date("2019-01-01")), y = 330, xAxis = 0, yAxis = 0),
            text = "Load shedding in VIC",
            x = -10,
            verticalAlign = 'middle', align = "right"),
          list(
            point = list(x = datetime_to_timestamp(as_date("2022-07-01")), y = 340, xAxis = 0, yAxis = 0),
            text = "Coal price surge &<br>war in Ukraine",
            x = -10,
            verticalAlign = 'middle', align = "right"),
          list(
            point = list(x = datetime_to_timestamp(as_date("2023-05-01")), y = 170, xAxis = 0, yAxis = 0),
            text = "AEMO scheduling<br>error post−Liddell<br>shut down"),
          list(
            point = list(x = datetime_to_timestamp(as_date("2024-07-01")), y = 230, xAxis = 0, yAxis = 0),
            text = "Wind<br>drought")
        )
      )
    ) %>%

    highcharter::hc_plotOptions(series = list(label = list(enabled = TRUE))
                                ) %>%
    highcharter::hc_yAxis(
      title = list(text = "$/MWh"),
      labels = list(format = "${text}"),
      tickAmount = 6,
      accessibility = list(description = "Elec. Spot per MWh")
    ) %>%

    highcharter::hc_xAxis(
      title = list(text = NULL)
    ) %>%

    # custom exporting
    highcharter::hc_exporting(
      enabled = TRUE,
      filename = "ESC_victorian_electricity_spot_price",
      chartOptions = list(
        chart = list(
          width = 1600,
          height = 500
          )
      )
      ) %>%

    highcharter::hc_title(text = title) %>%

    highcharter::hc_subtitle(text = subtitle) %>%

    highcharter::hc_caption(text = caption) %>%

    thm_highcharts() %>%

    highcharter::hc_tooltip(
      valuePrefix = "$"
    ) %>%
    hc_navigator(enabled = FALSE) %>%
    hc_scrollbar(enabled = FALSE)


}


#highcharts_spot_elec()


highcharts_futures_elec <- function(data = loadtabs()$whsl_elec_futures
                                    ) {


  futures <- data %>%
    # tidyr::complete(observed_date, quarter) %>%
    # arrange(quarter) %>%
    # tidyr::fill(Dates, .direction = "up") %>%
    arrange(Dates, observed_date)


  title <- "Volume−Weighted Futures Price in Victoria"
  caption <- "Source: ASX Energy"
  stitle <- "Quarterly based products in the last 3 months"

  #Note: volume weighted approach, based on trades that took place during the month. When volumes are zero, there is no price to report.

  # Make Highchart
    highcharter::hchart(
      futures,
      "line",
      highcharter::hcaes(x=quarter, y = round(VWP,0), group = observed_date)
    ) %>%

      highcharter::hc_yAxis(
        title = list(text = "$/MWh"),
        labels = list(format = "${text}"),
        tickAmount = 6,
        min = 0,
        accessibility = list(description = "Elec. Futures per MWh")
      ) %>%

      highcharter::hc_xAxis(
        title = list(text = "Delivered in"),
        labels = list(step = 2)
      ) %>%

      highcharter::hc_plotOptions(
        series = list(label = list(enabled = TRUE))
      ) %>%

      # custom exporting
      highcharter::hc_exporting(
        enabled = TRUE,
        filename = "ESC_indicator_victorian_electricity_futures",
        chartOptions = list(
          chart = list(
            width = 1600,
            height = 500
          )
        )
        ) %>%

      highcharter::hc_title(text = title) %>%

      highcharter::hc_subtitle(text = stitle) %>%

      highcharter::hc_caption(text = caption) %>%

      highcharter::hc_legend(enabled = FALSE) %>%

      thm_highcharts() %>%

      highcharter::hc_navigator(series = list(label = list(enabled = TRUE))) %>%

      highcharter::hc_tooltip(
        useHTML = TRUE,
        headerFormat = '<table><tr><th colspan="2">Delivered in: {point.key}</th></tr>',

        pointFormat = paste(
          '<tr><td style="color: {series.color}"> Purchased in {series.name}: ',
          '</td>',
          '<td style="text-align: right"><b>{point.y}</b></td></tr>'
        ),

        valuePrefix = "$"
      ) %>%
      hc_navigator(enabled = FALSE) %>%
      hc_scrollbar(enabled = FALSE)

}

#highcharts_futures_elec()



