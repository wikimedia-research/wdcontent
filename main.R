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

sites_without_labels <- set_retriever(results, "sitelinks", "labels", TRUE)
sites_with_labels <- set_retriever(results, "sitelinks", "labels", FALSE)
sites <- merge(sites_without_labels, sites_with_labels, by = "Var1", all.x = TRUE)
sites$Freq.y[is.na(sites$Freq.y)] <- 0
sites$sites_lacking_labels <- (sites$Freq.x/(sites$Freq.x + sites$Freq.y))*100
ggsave(filename = "sites_without_labels_density.svg",
       plot = ggplot(sites, aes(sites_lacking_labels)) + 
         geom_density(fill = "royalblue3") +
         labs(title = "Proportion of Wikidata-linked pages lacking an equivalent language label",
              x = "Proportion without labels",
              y = "Density"))
ggsave(filename = "sites_without_labels_point.svg",
       plot = ggplot(sites, aes(sites_lacking_labels, Freq.x, label = Var1)) + 
         geom_text() +
         labs(title = "Items linked to Wikidata that lack an equivalent language label",
              x = "Percentage of (sitelinks without labels/sitelinks with labels)",
              y = "Raw count of sitelinks without labels"))

descriptions_without_sites <- set_retriever(results, "descriptions", "sitelinks", TRUE)
descriptions_with_sites <- set_retriever(results, "descriptions", "sitelinks", FALSE)
descriptions <- merge(descriptions_without_sites, descriptions_with_sites, by = "Var1", all.x = TRUE)
descriptions$Freq.y[is.na(descriptions$Freq.y)] <- 0
descriptions$descriptions_without_sites <- (descriptions$Freq.x/(descriptions$Freq.x + descriptions$Freq.y))*100
ggsave(filename = "descriptions_without_sites_density.svg",
       plot = ggplot(descriptions, aes(descriptions_without_sites)) + 
         geom_density(fill = "royalblue3") +
         labs(title = "Proportion of descriptions without same-language Wikidata-linked pages",
              x = "Proportion without pages",
              y = "Density"))
ggsave(filename = "descriptions_without_sites_point.svg",
       plot = ggplot(descriptions, aes(descriptions_without_sites, Freq.x, label = Var1)) + 
         geom_text() +
         labs(title = "Descriptions on Wikidata that lack an equivalent sitelink",
              x = "Percentage of (descriptions without sitelinks/sitelinks with descriptions)",
              y = "Raw count of descriptions without sitelinks"))
sites_without_descriptions <- set_retriever(results, "sitelinks", "descriptions", TRUE)
sites_with_descriptions <- set_retriever(results, "sitelinks", "descriptions", FALSE)
sites <- merge(sites_without_descriptions, sites_with_descriptions, by = "Var1", all.x = TRUE)
sites$Freq.y[is.na(sites$Freq.y)] <- 0
sites$sites_lacking_descriptions <- (sites$Freq.x/(sites$Freq.x + sites$Freq.y))*100
ggsave(filename = "sites_without_descriptions_density.svg",
       plot = ggplot(sites, aes(sites_lacking_descriptions)) + 
         geom_density(fill = "royalblue3") +
         labs(title = "Proportion of links to sites without same-language descriptions",
              x = "Proportion without desscriptions",
              y = "Density"))
ggsave(filename = "sites_without_descriptions_point.svg",
       plot = ggplot(sites, aes(sites_lacking_descriptions, Freq.x, label = Var1)) + 
         geom_text() +
         labs(title = "Sitelinks on Wikidata that lack an equivalent description",
              x = "Percentage of (sitelinks without descriptions/sitelinks with descriptions)",
              y = "Raw count of sitelinks without descriptions"))

image_props <- c("P368","P18","P1442","P94","P109","P181","P692","P117","P154",
                 "P242","P1621","P41","P158","P1766","P15","P14","P207","P367",
                 "P491")
#References and images
images <- unlist(lapply(wd_items, function(x){
  if(any(names(x$claims) %in% image_props)){
    return(TRUE)
  }
  return(FALSE)
}))
print(sum(images)/length(images))