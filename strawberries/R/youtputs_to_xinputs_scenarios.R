youtputs_to_xinputs_scenarios <- function(df, output_names) {
  # finde alle Spalten, die mit einem der output_names + "_" anfangen
  pattern <- paste0("^(", paste(output_names, collapse = "|"), ")_")
  move_cols <- grep(pattern, colnames(df$y), value = TRUE)
  
  # in x verschieben
  for (col in move_cols) {
    df$x[[col]] <- df$y[[col]]
  }
  
  # aus y löschen
  df$y <- df$y[ , setdiff(colnames(df$y), move_cols), drop = FALSE]
  
  return(df)
}