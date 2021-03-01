#' Plot the output value of 'myseq_n'
#'
#' @param input_df a data frame with 4 columns
#'
#' @return plot
#' @export myseq_plot
#'
#' @examples myseq_plot(data.frame(x = 2, y = 4, z = 3, n = 3:9))
myseq_plot <- function(input_df) {
# Error checking
  if (!is.data.frame(input_df)) {
    stop('input_df must be a data frame')
  }
  else if (ncol(input_df) != 4) {
    stop('input_df must have 4 columns')
  }

  rows_num <- nrow(input_df)

  plot_df = data.frame(
    n = vector('integer', rows_num),
    output = vector('double', rows_num)
  )
  for (i in seq_len(rows_num)) {
    plot_df$n[[i]] <- input_df[[4]][[i]]

    plot_df$output[[i]] <- myseq_n(c(input_df[[1]][[i]],
                                     input_df[[2]][[i]],
                                     input_df[[3]][[i]]
                                     ),
                                     input_df[[4]][[i]]
                                   )
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(plot_df$n, plot_df$output)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = 'n', y = 'output', title = paste('My Sequence: c(', toString(round(plot_df$output, 3)), ')',
                                                       sep = ''))


}


# Testing

myseq_plot(data.frame(2, 4, 3, 3:9))
