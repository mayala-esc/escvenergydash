# Highchart theme
thm_highcharts <- function(hc){

  
  highcharter::hc_add_theme(
    hc,
    highcharter::hc_theme(
      
      colors = c("#236192", "#ce0058", "#ed8b00", "#808080", "#fdda24", "#D7D7D7", "#07393C", "#4f80a8", "#2C6E49"),
      
      chart = list(
        backgroundColor = "#ffffff",
        style = list(
          fontFamily = "VIC-Regular",
          `font-size` = "14px",
          color = "#000000"
        )
      ),
      title = list(
        align = "left",
        # x = 75,
        style = list(
          color = "#000000",
          fontFamily = "VIC-Regular",
          `font-weight` =  "bold",
          `font-size` = "22px",
          `background-color` = "#000000"
        )
      ),
      subtitle = list(
        align = "left",
        style = list(
          color = "#000000",
          fontFamily = "VIC-Regular",
          `font-size` = "16px"
        )
      ),
      xAxis = list(
        labels = list(
          style = list(
            color = "#595959",
            `font-weight` =  "bold",
            `font-size` = "12px"
          )
        ),
        title = list(style = list(color = "#595959", `font-weight` =  "bold")),
        lineWidth = 3,
        lineColor = "#D7D7D8",
        tickPosition = "inside",
        tickColor = "#D7D7D8",
        tickWidth = 3,
        tickLength = 7
      ),
      yAxis = list(
        labels = list(
          style = list(
            color = "#595959",
            `font-weight` =  "bold",
            `font-size` = "12px"
          )
        ),
        title = list(style = list(color = "#595959", `font-weight` =  "bold")),
        minorGridLineWidth = 0,
        lineWidth = 3,
        lineColor = "#D7D7D8",
        gridLineColor = "transparent",
        opposite = FALSE
      ),
      navigator = list(
        enabled = FALSE
      ),
      scrollbar = list(
        enabled = FALSE
      ),
      caption = list(
        # x = 75,
        style = list(
          color = "#000000"
        )
      ),
      rangeSelector = list(
        labelStyle = list(color = "#000000"),
        floating = TRUE,
        buttonPosition = list(
          align = "right",
          x = -10,
          y = 10
        )
      ),
      series = list(
        line = list(lineWidth = 4),
        spline = list(lineWidth = 4)
      ),
      exporting = list(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = c(
                          'viewFullscreen',
                          "printChart",
                          "separator",
                          'downloadPNG',
                          "downloadSVG",
                          'separator',
                          'downloadXLS',
                          'downloadCSV')
            )
          )
        )
    )
  )
  
}



# Generic table generation fun
esc_table <- function(df, first_col_header = TRUE){
  # Table container
  shiny::tags$table(
    class = "escTable",
    # Header row
    shiny::tags$thead(
      shiny::tags$tr(lapply(colnames(df), function(x) shiny::tags$th(scope = "col", x)))
    ),
    # Table body
    shiny::tags$tbody(
      apply(df, 1, function(x) {
        shiny::tags$tr(
          c(
            list(
              if(first_col_header) {
                shiny::tags$th(scope = "row", x[[1]])
              } else {
                shiny::tags$td(x[[1]])
              }
            ),
            lapply(x[2:length(x)], function(y) shiny::tags$td(y))
          )
        )
      }
      )
    )
  )
}




format_table_num <- function(x, suffix = "", round = 0, plus_neg = TRUE){
  signs <- if(plus_neg){ifelse(x < 0, "-", "+")}else{""}
  x <- x %>%
    abs() %>%
    base::format(base::round(round), big.mark = ",") %>%
    as.character()
  x <- paste0(signs, "$", x, suffix)
  return(x)
}

format_table_pc <- function(x, suffix = "%", round = 0, plus_neg = TRUE){
  signs <- if(plus_neg){ifelse(x < 0, "-", "+")}else{""}
  x <- x %>%
    `*`(100) %>%
    abs() %>%
    base::round(round) %>%
    as.character()
  x <- paste0(signs, x, suffix)
  return(x)
}

