options(scipen = 500)
Sys.setenv("http_proxy" = "http://webproxy.eqiad.wmnet:8080")
Sys.setenv("https_proxy" = "http://webproxy.eqiad.wmnet:8080")

library(parallel)
library(httr)
library(jsonlite)
library(readr)
library(ggplot2)