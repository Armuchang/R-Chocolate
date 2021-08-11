#' Year difference calculation between two dates
#'
#' @param startDate A first date as %d/%m/%Y format
#' @param endDate A last date as %d/%m/%Y format
#'
#' @return A number of year difference between a starting date and a final date
#' @author J.Chanthercrob
#' @export
#' @examples 
#' yearDiff(d1, d2)
yearDiff <- function(startDate, endDate){
  day1 <- as.numeric(format(startDate, "%d"))
  day2 <- as.numeric(format(endDate, "%d"))
  month1 <- as.numeric(format(startDate, "%m"))
  month2 <- as.numeric(format(endDate, "%m"))
  year1 <- as.numeric(format(startDate, "%Y"))
  year2 <- as.numeric(format(endDate, "%Y"))
  
  if (month2 > month1){
    yDiff <- year2 - year1
  }
  if (month2 == month1){
    if (day2 > day1 || day2 == day1){
      yDiff <- year2 - year1
    }else{
      yDiff <- year2 - year1 - 1
    }
  }
  if (month2 < month1){
    yDiff <- year2 - year1 - 1
  }
  return(yDiff)
}