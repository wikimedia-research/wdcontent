mysql -h analytics-store.eqiad.wmnet wikidatawiki < "random_ids.sql" > "ids.tsv"
R CMD BATCH main.R