#various_functions which are needed for output data or something

youtputs_to_xinputs <- function(df, output_names) {
  for (name in output_names) {
    df$x[[name]] <- df$y[[name]]
  }
  drop_col <-  which(colnames(df$y) %in% output_names)
  df$y<-subset(df$y, select = -drop_col)
  return(df)
}
