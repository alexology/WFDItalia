#' shannon
#'
#' Calculate shannon.
#' @param data.frame as specified in details.
#' @keywords data preparation
#' @export
#' @examples
#' shannon()

shannon <- function(x){
			return(round(diversity(x[[1]]),3))
}
