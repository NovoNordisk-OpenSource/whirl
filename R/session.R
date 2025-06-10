#' @noRd
insert_at_intervals_df <- function(df, column_name, char_to_insert, interval) {
  df[[column_name]] <- sapply(df[[column_name]], function(input_string) {
    if (nchar(input_string) < interval) {
      return(input_string)
    } else {
      result <- input_string
      insert_positions <- seq(interval, nchar(input_string), by = interval)
      for (i in rev(seq_along(insert_positions))) {
        result <- paste(
          substr(result, 1, insert_positions[i] - 1),
          char_to_insert,
          substr(result, insert_positions[i], nchar(result)),
          sep = ""
        )
      }
      return(result)
    }
  })
  return(df)
}
