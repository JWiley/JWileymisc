#' Tufte Range
#'
#' Make axis lines informative by making them show the \textbf{observed range} of the data.
#' Inspired from the excellent ggthemes package: https://github.com/jrnold/ggthemes
#'
#' @inheritParams ggplot2::geom_point
#' @export
#' @importFrom ggplot2 layer
geom_tufterange <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", ..., show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = TufteRange,
        position = position, show.legend = show.legend,  inherit.aes = inherit.aes,
        params = list(...))
}

#' @rdname geom_tufterange
#' @usage NULL
#' @format NULL
#' @export
#' @importFrom ggplot2 Geom draw_key_path aes 
#' @importFrom scales alpha
#' @importFrom grid gpar grobName segmentsGrob gTree gList
TufteRange <- ggplot2::ggproto(
  "TufteRange", ggplot2::Geom, optional_aes = c("x", "y"),
  draw_panel = function(data, panel_scales, coord) {
    ggname <- \(prefix, grob) {grob$name <- grobName(grob, prefix); return(grob)}
    a <- list()
    data <- coord[["transform"]](data, panel_scales)
    gp <- gpar(col = alpha(data[["colour"]], data[["alpha"]]),
               lty = data[["linetype"]],
               lwd = data[["size"]] * ggplot2::.pt)
    if (!is.null(data[["x"]])) {
      rx <- range(data[["x"]], na.rm = TRUE)
      a[["bottom"]] <- ggname("bottom_range", segmentsGrob(
        x0 = unit(rx[1], "native"), x1 = unit(rx[2], "native"),
        y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gp))
    }

    if (!is.null(data[["y"]])) {
      ry <- range(data[["y"]], na.rm = TRUE)
      a[["left"]] <- ggname("left_range", segmentsGrob(
        x0 = unit(0, "npc"), x1 = unit(0, "npc"),
        y0 = unit(ry[1], "native"), y1 = unit(ry[2], "native"), gp = gp))
    }

    ggname("geom_tufterange", gTree(children = do.call("gList", a)))
  },
  default_aes = ggplot2::aes(colour = "black", size = 0.5,
                             linetype = 1, alpha = NA),
  draw_key = ggplot2::draw_key_path
)

#' Tufte Inspired Theme
#'
#' This is a barebones theme. It turns many aspects of plot background lines, etc. off completely.
#' Inspired from the excellent ggthemes package: https://github.com/jrnold/ggthemes
#'
#' @inheritParams ggplot2::theme_grey
#' @export
#' @importFrom ggplot2 theme_minimal
theme_tufte <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_family = base_family, base_size = base_size) +
    theme(axis.line = element_blank(), panel.grid = element_blank(),
          axis.ticks = element_line(colour = "white", linewidth = 2))
}
