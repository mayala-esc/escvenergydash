
# CREATE DATA VIZ FOR VEC ELECTRICITY DISTRIBUTION ZONES


library(highcharter)

# VIZ ----

highcharts_dzones_elec <- function(
    data = loadtabs()$offers_elec_vdo_dzones_summaries,
    customer = "res",
    dist_zone = "jemena"
    ) {
  
  options(dplyr.summarise.inform = FALSE)
  
  df <- data %>% filter(cust_type==customer & distribution_zone==dist_zone)
  
  month <- lubridate::month(max(data$identifier_date), label = TRUE, abbr = FALSE) %>% as.character()
  year <- lubridate::year(max(data$identifier_date))
  
  # calculate diff for subtitle
  l <- df %>%
    mutate(diff = median - lag(median), sign = if_else(diff>1, "increased", "decreased")) %>%
    filter(identifier_date == max(identifier_date))
  
  s <- l %>% pull(diff) %>% round(.,0) %>% abs()
  sign <- l %>% pull(sign)
  
  
  title <-  paste0("Electricity Offers Annual Bill")
  
  subtitle <-  paste0("Median offers in ", stringr::str_to_sentence(dist_zone)," ", sign, " by $", s, " in ", month," ", year)
  
  caption <- "Source: Victorian Energy Compare. Note: light blue range denotes annual bills in the 10th to 90th percentile."
  
  min <- df %>% pull(p10) %>% min() %>% round(digits = -2) *.90
  max <- df %>% pull(p90) %>% max() %>% round(digits = -2) *1.1
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
      name = "Median",
      hcaes(x = identifier_date, y = median),
      id = "median") %>%
    
    hc_add_series(
      df,
      "line",
      name = "VDO",
      hcaes(x = identifier_date, y = vdo),
      id = "vdo") %>% 
    
    hc_add_series(
      df,
      name = "10th-90th",
      hcaes(x = identifier_date, low = p10, high = p90),
      fillOpacity = 0.1,
      id = "10th-90th", 
      type = "arearange",
      color = "#4f80a8") %>% 
    
    highcharter::hc_plotOptions(
      series = list(label = list(enabled = TRUE))
    ) %>%

    # remove zoom from exporting
    format_export(
      filename = paste("ESC_victorian_electricity_offers", customer, dist_zone, sep = "_")
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
  highcharter::hc_navigator(
    series = list(label = list(enabled = FALSE))
    ) %>%
    highcharter::hc_tooltip(
      valuePrefix = "$"
    ) %>%
    hc_scrollbar(enabled = FALSE)
  
  
}


#highcharts_dzones_elec()

# TABLES ----

# RESIDENTIAL

table_elec_dzones <- function(
    flow = "vec_elec",
    n = 5,
    data_elec = loadtabs()$offers_elec_vdo_dzones_summaries
){
  
  df_all <- data_elec %>% select(-p10, -p90, -vdo)
  
  # Get distribution zones
  dzones <- df_all %>%
    filter(
      cust_type == "res",
      identifier_date == max(identifier_date)
    ) %>%
    slice_max(median, n = n) %>%
    pull(distribution_zone)
  
  # Create date vector
  date_select <- c(
    max(df_all$identifier_date),
    max(df_all$identifier_date) - months(1),
    max(df_all$identifier_date) - months(3),
    max(df_all$identifier_date) - months(12)
  )
  
  names(date_select) <- c(
    paste(format(date_select[1], "%B"), "median"),
    "Monthly change",
    "Quarterly change",
    "Yearly change"
  )
  
  # Collect data
  data <- df_all %>%
    filter(
      identifier_date %in% !!date_select,
      cust_type == "res"
    ) %>%
    dplyr::rename(date = identifier_date,
                  value = median)
  
  
  # Tidy & generate features
  summary <- data %>%
    arrange(desc(date), desc(value)) %>%
    dplyr::group_by(distribution_zone) %>%
    dplyr::mutate(
      distribution_zone = stringr::str_to_sentence(distribution_zone),
      abs_diff = first(value) - value,
      rel_diff = round((first(value) - value) / value, 2)
    ) %>%
    ungroup() %>%
    mutate(
      abs_diff = format_table_num(abs_diff, suffix = ""),
      rel_diff = format_table_pc(rel_diff),
      value    = format_table_num(value, suffix = "", plus_neg = F),
      date = names(date_select)[match(date, date_select)]
    )
  
  
  # Generate summary table
  summary_current_cols <- summary %>%
    filter(date == names(date_select)[1]) %>%
    dplyr::mutate(distribution_zone = stringr::str_to_sentence(distribution_zone)) %>% 
    select(distribution_zone, value)
  
  col_order <- paste0(
    rep(names(date_select)[2:4], each = 2),
    "_",
    rep(c("abs_diff", "rel_diff"), 3)
  )
  
  summary_change_cols <- summary %>%
    select(-value, -cust_type) %>%
    filter(date != names(date_select)[1]) %>%
    tidyr::pivot_wider(
      names_from = date,
      values_from = c("abs_diff", "rel_diff"),
      names_glue = "{date}_{.value}"
    ) %>%
    relocate(all_of(col_order), .after = distribution_zone)
  
  summary <- summary_current_cols %>%
    full_join(summary_change_cols, by = "distribution_zone")
  
  
  
  # Create header
  header <- tags$thead(
    tags$tr(
      tags$th("Distribution zone", rowspan = "2", scope = "col"),
      tags$th(names(date_select)[1], rowspan = "2", scope = "col"),
      lapply(
        names(date_select)[2:4],
        shiny::tags$th,
        scope = "col",
        colspan = "2"
      )
    ),
    tags$tr(
      lapply(
        rep(c("$", "%"), 3),
        shiny::tags$th,
        scope = "col"
      )
    )
  )
  
  
  # Create body
  body <- shiny::tags$tbody(
    apply(summary, 1, function(x) {
      shiny::tags$tr(
        c(
          list(shiny::tags$th(scope = "row", x[[1]])),
          lapply(x[2:length(x)], function(y) shiny::tags$td(y))
        )
      )
    }
    )
  )
  
  # Return
  return(
    tags$table(class = "escTable", header, body)
  )
  
}



# SME table ----

table_elec_sme_dzones <- function(
    n = 5,
    data_elec = loadtabs()$offers_elec_vdo_dzones_summaries
    
){
  
  df_all <- data_elec %>% select(-p10, -p90, -vdo)
  
  # Get current top trading partners
  dzones <- df_all %>%
    filter(
      cust_type == "sme",
      identifier_date == max(identifier_date)
    ) %>%
    slice_max(median, n = n) %>%
    pull(distribution_zone) %>% stringr::str_to_sentence()
  
  # Create date vector
  date_select <- c(
    max(df_all$identifier_date),
    max(df_all$identifier_date) - months(1),
    max(df_all$identifier_date) - months(3),
    max(df_all$identifier_date) - months(12)
  )
  
  names(date_select) <- c(
    paste(format(date_select[1], "%B"), "median"),
    "Monthly change",
    "Quarterly change",
    "Yearly change"
  )
  
  # Collect data
  data <- df_all %>%
    filter(
      identifier_date %in% !!date_select,
      cust_type == "sme"
    ) %>%
    dplyr::rename(date = identifier_date,
                  value = median)
  
  
  # Tidy & generate features
  summary <- data %>%
    arrange(desc(date), desc(value)) %>%
    dplyr::group_by(distribution_zone) %>%
    dplyr::mutate(
      distribution_zone = stringr::str_to_sentence(distribution_zone),
      abs_diff = first(value) - value,
      rel_diff = round((first(value) - value) / value, 2)
    ) %>%
    ungroup() %>%
    mutate(
      abs_diff = format_table_num(abs_diff, suffix = ""),
      rel_diff = format_table_pc(rel_diff),
      value    = format_table_num(value, suffix = "", plus_neg = F),
      date = names(date_select)[match(date, date_select)]
    )
  
  
  # Generate summary table
  summary_current_cols <- summary %>%
    filter(date == names(date_select)[1]) %>%
    dplyr::mutate(distribution_zone = stringr::str_to_sentence(distribution_zone)) %>% 
    select(distribution_zone, value)
  
  col_order <- paste0(
    rep(names(date_select)[2:4], each = 2),
    "_",
    rep(c("abs_diff", "rel_diff"), 3)
  )
  
  summary_change_cols <- summary %>%
    select(-value, -cust_type) %>%
    filter(date != names(date_select)[1]) %>%
    tidyr::pivot_wider(
      names_from = date,
      values_from = c("abs_diff", "rel_diff"),
      names_glue = "{date}_{.value}"
    ) %>%
    relocate(all_of(col_order), .after = distribution_zone)
  
  summary <- summary_current_cols %>%
    full_join(summary_change_cols, by = "distribution_zone")
  
  
  
  # Create header
  header <- tags$thead(
    tags$tr(
      tags$th("Distribution zone", rowspan = "2", scope = "col"),
      tags$th(names(date_select)[1], rowspan = "2", scope = "col"),
      lapply(
        names(date_select)[2:4],
        shiny::tags$th,
        scope = "col",
        colspan = "2"
      )
    ),
    tags$tr(
      lapply(
        rep(c("$", "%"), 3),
        shiny::tags$th,
        scope = "col"
      )
    )
  )
  
  
  # Create body
  body <- shiny::tags$tbody(
    apply(summary, 1, function(x) {
      shiny::tags$tr(
        c(
          list(shiny::tags$th(scope = "row", x[[1]])),
          lapply(x[2:length(x)], function(y) shiny::tags$td(y))
        )
      )
    }
    )
  )
  
  # Return
  return(
    tags$table(class = "escTable", header, body)
  )
  
}
