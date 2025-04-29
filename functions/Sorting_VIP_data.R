#Function to reformat the pls-analysis results from the format "large mvr" to "data.frame"

VIP_table<-function (plsrResults, input_table = NULL, cut_off_line = 1, 
          threshold = 0.8, x_axis_name = "Variable Importance in Projection", 
          y_axis_name = NULL, legend_name = "Coefficient", legend_labels = c("Positive", 
                                                                             "Negative"), pos_color = "cadetblue", neg_color = "firebrick", 
          base_size = 11, ...) 
{
  assertthat::assert_that(class(plsrResults) == "mvr", 
                          msg = "plsrResults is not class 'mvr', please provide a valid object. This does not appear to have been generated with the 'plsr.mcSimulation' function.")
  VIP <- function(object) {
    if (object$method != "oscorespls") 
      stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
    if (nrow(object$Yloadings) > 1) 
      stop("Only implemented for single-response models")
    SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
    Wnorm2 <- colSums(object$loading.weights^2)
    SSW <- sweep(object$loading.weights^2, 2, SS/Wnorm2, 
                 "*")
    sqrt(nrow(SSW) * apply(SSW, 1, cumsum)/cumsum(SS))
  }
  if (plsrResults$ncomp == 1) 
    vipResult <- VIP(plsrResults)
  else vipResult <- VIP(plsrResults)["Comp 1", ]
  coef <- plsrResults$coefficients[, , 1]
  pls_outputs <- data.frame(Variable = names(vipResult), VIP = vipResult, 
                            Coefficient = coef)
  rownames(pls_outputs) <- NULL
  if (!(is.null(input_table))) 
    combined_table <- dplyr::left_join(pls_outputs, input_table, 
                                       by = c(Variable = "variable"))
  else combined_table <- pls_outputs
  filtered_table <- dplyr::filter(combined_table, VIP > threshold)
  if ("label" %in% colnames(filtered_table)) 
    ordered_vars <- stats::reorder(filtered_table$label, 
                                   filtered_table$VIP)
  else ordered_vars <- stats::reorder(filtered_table$Variable, 
                                      filtered_table$VIP)
  
  return(VIP_table_results=filtered_table)
  
}

