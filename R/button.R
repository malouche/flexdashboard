#' Create a value box component for a dashboard.
#'
#' A value box displays a value (usually a number) in large text, with a smaller
#' caption beneath, and a large icon on the right side.
#'
#' @param value the text in the button
#' @param color Background color for the button. This can be one of the built-in
#'   background colors ("primary", "info", "success", "warning", "danger") or
#'   any valid CSS color value.
#' @param href An optional URL to link to. Note that this can be an anchor of
#'   another dashboard page (e.g. "#details").
#'
#' @details See the flexdashboard website for additional documentation:
#'  \href{http://rmarkdown.rstudio.com/flexdashboard/using.html#value_boxes}{http://rmarkdown.rstudio.com/flexdashboard/using.html#value_boxes}
#'
#' @examples
#' library(flexdashboard)
#'
#' button('Click here')
#' @export
button <- function(value, color = NULL, href = NULL) {

  # resolve background color
  if (!is.null(color) && color %in% c("primary", "info", "success", "warning", "danger"))
    color <- paste0("bg-", color)

  # build the value output
  buttonValue <- tags$span(class="button",
                           `data-color` = color,
                           `data-href` = href,
                           value
  )

  # attach font dependency if necessary
  hasPrefix <- function(x, prefix) {
    if (!is.null(x))
      grepl(paste0('^', prefix), x)
    else
      FALSE
  }


  # return output
  buttonValue
}


#' Shiny bindings for valueBox
#'
#' Output and render functions for using valueBox within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a gauge
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name valueBox-shiny
#'
#' @export
buttonBoxOutput <- function(outputId, width = '100%', height = '160px') {
  shiny::uiOutput(outputId, class = 'shiny-html-output shiny-valuebox-output',
                  width = width, height = height)
}


#' @rdname valueBox-shiny
#' @export
renderButtonBox <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  renderFunc <- shiny::renderUI(expr, env, quoted = TRUE)
  attr(renderFunc, "outputFunc") <- buttonBoxOutput
  renderFunc
}

