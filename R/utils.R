
# custom formatting of exporting charts

format_export <- function(hc, filename = "ESC"){
  
  hc %>% 
  highcharter::hc_exporting(
    enabled = TRUE,
    filename = filename,
    chartOptions = list(
    chart = list(
      width = 1600,
      height = 500
      ),
    xAxis = list(
      dateTimeLabelFormats = list(month = '%b %Y')
    ),
    rangeSelector = list(
      enabled = FALSE
    )
  )
  )
  
}

# customise markdown attributes

pagemd_attrb <- function(box){
  
  box <- box %>%
    tagAppendAttributes(
      .cssSelector = ".box-header",
      `data-widget` = "collapse",
      cursor = "pointer",
      `aria-expanded` = "false",
      role = "button"
    ) %>%
    tagAppendAttributes(
      .cssSelector = ".box-header h3",
      class = "fa-plus",
      style = "font-weight: bold;"
    ) %>%
    tagAppendAttributes(
      .cssSelector = ".box-body",
      class = "collapse"
    ) %>%
    tagAppendAttributes(
      .cssSelector = ".box",
      class = "collapsed-box"
    )
  
  return(box)
  
}



# faq table retailers

retailers_tb <- function(){
  
  tble <- tidyr::tibble(
    `Retailer size` = c("Large", "Medium", "Small"),
    `Retailers` = c("AGL, Origin Energy, Energy Australia, ENGIE, Red Energy, Lumo Energy",
                    "Alinta Energy, Tango Energy, Globird Energy, Momentum, Powershop, Dodo, OVO Energy, Sumo Power",
                    "1st Energy, Amber Electric, CovaU, Diamond Energy, Energy Locals, Maximum Energy Retail, Nectr, Next Business Energy, Progressive Green Energy, Real Utilities"),
    `Number of retailers` = c(6,8,10)
  )
  
  # Create header
  head <- tags$thead(
    tags$tr(
      tags$th("Retailer size", rowspan = "2", scope = "col"),
      tags$th("Retailers", rowspan = "2", scope = "col"),
      tags$th("Number of retailers", rowspan = "2", scope = "col")
    )
  )
  
  
  # Create body
  body <- shiny::tags$tbody(
    apply(tble, 1, function(x) {
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
    tags$table(class = "escTable", head, body)
  )
  
  
}


format_choice <- function(start, end){
  
  choice <-  format(seq.Date(as.Date(start), as.Date(end), by = "year"), format = "%b %Y")
  
  return(choice)
  
}




