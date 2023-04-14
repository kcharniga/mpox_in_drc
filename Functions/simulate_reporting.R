obs_process <- function(my_list, p_report){
  
  epidemic1 <- my_list
  
  # Sample observed cases according to reporting probability
  obs <- runif(epidemic1$n) < p_report
  
  # Filter out cases that were not observed
  locations <- data.frame(matrix(unlist(lapply(epidemic1$cases, function(e) e$location)),
                                 ncol = 2, byrow = TRUE))
  
  dates <- data.frame(matrix(unlist(lapply(epidemic1$cases, function(e) e$date)),
                             ncol = 1, byrow = TRUE))
  colnames(dates) <- "date_onset"
  
  ances <- data.frame(epidemic1$ances)
  colnames(ances) <- "from"
  
  linelist <- data.frame(id = 1:epidemic1$n, 
                         date = dates,
                         Lat = locations$X1,
                         Lon = locations$X2)
  
  contacts <- data.frame(to = 1:epidemic1$n,
                         from = ances)
  
  # Remove NAs?
  contacts <- contacts[!(is.na(contacts$from)),]
  
  # Make epicontacts object
  x <- make_epicontacts(linelist = linelist,
                        contacts = contacts,
                        directed = TRUE)
  
  # Assign cluster membership based on ancestor case
  clusters <- get_clusters(x)
  
  linelist_new <- clusters$linelist
  
  linelist_new <- linelist_new[which(obs),]
  
  return(linelist_new)
  
}