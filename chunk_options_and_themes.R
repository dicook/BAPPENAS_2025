knitr::opts_chunk$set(
  fig.width = 6,
  fig.height = 4,
  fig.align = "center",
  out.width = "100%",
  code.line.numbers = FALSE,
  fig.retina = 4,
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  cache = FALSE,
  dev.args = list(pointsize = 11)
)
options(
  digits = 2,
  width = 60,
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
)
theme_set(theme_bw(base_size = 14) +
            theme(
              #aspect.ratio = 1,
              plot.background = element_rect(fill = 'transparent', colour = NA),
              plot.title.position = "plot",
              plot.title = element_text(size = 18),
              panel.background = element_rect(fill = 'transparent', colour = NA),
              legend.background = element_rect(fill = 'transparent', colour = NA),
              legend.key = element_rect(fill = 'transparent', colour = NA)
            )
)