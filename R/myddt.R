library(dplyr)
library(ggplot2)
library(Intro2R)


#' @title myddt
#'
#' @param df data file
#' @param SPECIES Species of fish
#'
#' @return Plots length vs. weight of species of fish. This function includes my name on top, is colored according to the river variable, has a quadratic curve placed over it, prints the subset data into a csv file in the working directory, and returns a list of the DDT data from before subsetting, after subsetting, and relative frequency.
#' @export
#'
#' @examples
#' \dontrun{myddt(df = ddt, SPECIES = "CCATFISH")}
myddt = function(df, SPECIES){
  # 1 - 4 --------------------
  filter = df %>% filter(SPECIES == {{SPECIES}})
  graph = ggplot(filter, aes_string(x = "LENGTH", y = "WEIGHT")) +
    geom_point(aes_string(color = "RIVER")) +
    geom_smooth(formula = y~x + I(x^2), method = "lm") +
    ggtitle("Robby Frost")
  print(graph)

  # 5  --------------------
  if (SPECIES == "CCATFISH"){
    write.csv(x = filter, "LvsWforCCATFISH.csv", row.names = TRUE)}
  if (SPECIES == "SMBUFFALO"){
    write.csv(x = filter, "LvsWforSMBUFFALO.csv", row.names = TRUE)}
  if (SPECIES == "LMBASS"){
    write.csv(x = filter, "LvsWforLMBASS.csv", row.names = TRUE)}

  # 6  --------------------
  print(df)
  print(filter)
  tab = table(df$RIVER) / length(df$RIVER)
  print(tab)
}
