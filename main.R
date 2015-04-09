source("config.R")

#Grab the IDs
ids <- read_lines("ids.tsv")
ids <- ids[-1]
ids <- split(ids, seq(1,length(ids), length(ids)/4))

#Define a query-maker
random_item <- function(id_list){
  
  id_list <- paste(id_list, collapse = "|")
  results <- content(GET(paste0("http://www.wikidata.org/w/api.php?action=query&prop=revisions&format=json&rvprop=content&titles=",
                                id_list), user_agent("Minify your JSON or the API gets it")))
  cat(".")
  return(lapply(results$query$pages, function(x){
    if(is.null(x$missing)){
      return(fromJSON(x$revisions[[1]]$`*`))
    }
    return(NULL)
  }))
}

#Multicore apply random_item to chunks of ids
wd_items <- unlist(mclapply(ids, function(x){
  x <- split(x, seq(1, length(x), 50))
  return(unlist(lapply(x, random_item), recursive = FALSE))
}, mc.preschedule = FALSE, mc.cores = 4), recursive = FALSE)
save(wd_items, file = "items_raw.RData")

#Okay; from each one we want:
#1. Number of languages it has without labels
#2. Number of languages it has without descriptions
#3. Number of labels it has without languages
#4. Number of labels it has without descriptions
#5. Number of properties it has with values
results <- lapply(wd_items, function(x){
  out <- list()
  out$sitelinks <- gsub(x = names(x$sitelinks), pattern = "wiki.*", replacement = "")
  out$labels <- names(x$labels)
  out$descriptions <- names(x$descriptions)
  out$properties <- names(x$claims)
  return(out)
})

#Define retrievers for sets and offsets
set_retriever <- function(list, name, compare_name, offset = FALSE){
  out <- lapply(list, function(x, name, compare_name){
    if(offset){
      return(x[[name]][!x[[name]] %in% x[[compare_name]]])
    }
    return(x[[name]][x[[name]] %in% x[[compare_name]]])
  }, name, compare_name)
  out <- as.data.frame(table(unname(unlist(out))), stringsAsFactors = FALSE)
  return(out[!out$Var1 %in% c("commons","simple"),])
}

labels_without_sites <- set_retriever(results, "labels", "sitelinks", TRUE)
labels_with_sites <- set_retriever(results, "labels", "sitelinks", FALSE)
labels <- merge(labels_without_sites, labels_with_sites, by = "Var1", all.x = TRUE)
labels$Freq.y[is.na(labels$Freq.y)] <- 0
labels$labels_lacking_sites <- (labels$Freq.x/(labels$Freq.x + labels$Freq.y))*100
ggsave(filename = "labels_without_sites_density.svg",
       plot = ggplot(labels, aes(labels_lacking_sites)) + 
         geom_density(fill = "royalblue3") +
         labs(title = "Proportion of localised Wikidata labels lacking an equivalent site link",
              x = "Proportion without links",
              y = "Density"))
ggsave(filename = "labels_without_sites_point.svg",
       plot = ggplot(labels, aes(labels_lacking_sites, Freq.x, label = Var1)) + 
         geom_text() +
         labs(title = "Items with localised Wikidata labels lacking an equivalent site link",
              x = "Percentage of (labels without sitelinks/labels with sitelinks)",
              y = "Raw count of labels without sitelinks"))