SELECT * FROM (
  SELECT page_title FROM page
  WHERE page_namespace = 0
  ORDER BY rand()
) alias1 LIMIT 1000000;