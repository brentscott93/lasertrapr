
#' Theme for app
#' @noRd
theme_lasertrapr <- function(){
  fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#141313",
    aqua = "#ff41c8"
  ),
   fresh::adminlte_sidebar(
  #   width = "400px",
      dark_bg = "#141313"
  #   dark_hover_bg = "#81A1C1",
  #   dark_color = "#2E3440"
   ),
  fresh::adminlte_global(
    content_bg = "#3b3939"
    #box_bg = "#e0cee3"#,
    #info_box_bg = "#D8DEE9"
  )
)
}

  