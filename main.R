source("config.R")

#Grab the IDs
ids <- read_lines("ids.tsv")
ids <- ids[-1]
ids <- split(ids, seq(1,length(ids), length(ids)/4))

#Define a query-maker
random_item <- function(id_list){
  
  id_list <- paste(id_list, collapse = "|")
  results <- content(GET(paste0("http://www.wikidata.org/w/api.php?action=query&prop=revisions&format=json&rvprop=content&titles=",
                                id_list)))
  cat(".")
  return(lapply(results$query$pages, function(x){
    if(is.null(x$missing)){
      return(fromJSON(x$revisions[[1]]$`*`))
    }
    return(NULL)
  }))
}

#Multicore apply random_item to chunks of ids
wd_items <- mclapply(ids, function(x){
  x <- split(x, seq(1, length(x), 50))
  return(lapply(x, random_item))
}, mc.preschedule = FALSE, mc.cores = 4)