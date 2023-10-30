# packs
library(dplyr);library(tidyr)

############################################ New plot with updated ER split 

# data
data <- readr::read_csv("pilot_data_long.csv")

distance <- function(x, method = "euclidean", func = func){
  if(method != "custom") {
    dist.vec <- parallelDist::parDist(x, method = method)
  } else{
    dist.vec <- parallelDist::parDist(x, method = method, func = func)
  }
  dist <- as.matrix(dist.vec)
  
  if(method == "custom") {
    dist <- dist / ncol(x)
    diag(dist) <- 1
  }
  
  return(dist)
}
matchingFuncPtr <- RcppXPtrUtils::cppXPtr(
  'double customDist(const arma::mat &A, const arma::mat &B) {
  return arma::accu(A == B);
}', depends = c("RcppArmadillo"))

data_mat <- data %>%
  # rename subject
  rename(subject = 1) %>%
  # remove dude with NA
  na.omit %>%
  # to longer
  pivot_longer(
    cols = IR1:IM8,
    names_to = "item",
    values_to = "resp"
  ) %>%
  mutate(
    construct = stringr::str_sub(item, start = 1, end = 2),
    item_number = as.numeric(stringr::str_trim(stringr::str_sub(item, start = 4, end = nchar(item))))
  ) %>%
  arrange(subject, construct, item_number) %>%
  # filter by construct if desired %>%
  #filter(construct == "AM") %>%
  dplyr::select(-construct, -item_number) %>%
  pivot_wider(id_cols = subject, names_from = "item", values_from = "resp") %>%
  dplyr::select(-subject) %>%
  as.matrix
dist_mat <- distance(t(data_mat), method = "custom", func = matchingFuncPtr)

# create plot
dist_mat[upper.tri(dist_mat)] <- NA
fields::image.plot(
  1:nrow(dist_mat), 
  1:ncol(dist_mat), 
  dist_mat,
  col = gray.colors(n=12,start=1,end=0),
  breaks = seq(0,1,length=13),
  # col = rainbow(n=12), 
  # breaks = seq(0,1,length=13),
  axes = F,
  xlab = "Items",
  ylab = "",
  main = "Within Participant Matches Across Items"
)
box()
axis(
  1, 
  at = 1:ncol(data_mat), 
  labels = colnames(data_mat),
  cex.axis = .7,
  las = 2
)
axis(
  2, 
  at = 1:ncol(data_mat), 
  labels = colnames(data_mat),
  cex.axis = .7,
  las = 1
)








  
  
  

  









































