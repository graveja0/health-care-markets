theme_tufte_revised <- function(base_size = 11, base_family = "Gill Sans", ticks = TRUE) {
  
  ret <- ggplot2::theme_bw(base_family = base_family, base_size = base_size) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = 'black'),
      axis.title.x = ggplot2::element_text(vjust = -0.3),
      axis.title.y = ggplot2::element_text(vjust = 0.8),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face="plain"),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )
  
  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_blank())
  }
  
  ret
}