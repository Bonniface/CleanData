
Drop_if <- function(data,percent = NULL){
  data[colMeans(is.na(data)) <= percent]
}