#' box.payment
#'
#' This function produces a box plot of payment by DRG code.
#' The user can customize which payment type to draw in the plot
#'
#' @param df Default dataset to use for the plot
#' @param P_type String indicating which column in the DRG data to plot. "AMP" plots
#' the Average Medicare Payment; "ATP" plots the Average Total Payments; "ACC" plots
#' the Average Covered Charges. The default value is "AMP".
#'
#' @return a box plot of the column user chose
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom stringi stri_trans_totitle
#'
#' @examples
#' box.payment(DRG_data, "ATP")
#'
#'

box.payment <- function(df, P_type="AMP") {
  #------------------------------#
  # set correct values for input
  #------------------------------#
  if (P_type == "AMP"){
    P_type <- "Average.Medicare.Payments"
  }
  else if (P_type == "ATP"){
    P_type <- "Average.Total.Payments"
  }
  else if(P_type == "ACC"){
    P_type <- "Average.Covered.Charges"
  }
  else {
    return("The input does not match the options.")
  }
  # extract the DRG Code
  newData <- df %>% mutate(DRGcode = substr(DRG.Definition, 1,3),
                           # mutate code to numeric to sort
                           Sort = as.numeric(substr(DRG.Definition, 1,3)))

  # set the label of x axis
  temp_name <- gsub("\\.", " ", P_type) %>% stri_trans_totitle()

  # plot corresponding data by ascending order
  ggplot(newData, aes(x=reorder(DRGcode, desc(Sort)), y= .data[[P_type]])) +
    # remove ouliers to make the graph clear
    geom_boxplot(outlier.shape = NA)+
    # set limits to graph
    ylim(0, 50000)+
    # add labels to the graph
    ggtitle(paste("Boxplot of Payments", temp_name))+
    labs(x = 'DRG Code', y = "Payments($)")+
    # flip x and y coordinates
    coord_flip()+
    # change the text size of labels
    theme(
      text = element_text(size=6)
    )
}
