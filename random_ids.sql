SELECT * FROM (
  SELECT page_title FROM page
  WHERE page_namespace = 0
  ORDER BY page_random
) LIMIT 1000000;