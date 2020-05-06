#' A Sailing Function
#'
#' This function allows you to express your love of sailing.
#' @param love Do you love sailing? Defaults to TRUE.
#' @keywords sailing
#' @export
#' @examples
#' sailing_function()

sailing_function <- function(love=TRUE) {
  if (love) {
    print('It is time to hoist!!')
  }
  else {
    print('You can stay at dock')
  }
}
