is_valid_connection <- function(conn) {
  return(class(conn) == "MariaDBConnection")
}

conn <- ageR::open_conn_mysql("RPD-latest", "root")
out <- ageR::select_query_mysql(conn, "SELECT * FROM `RPD-latest`.date_info LIMIT 10;")
query <- paste0(
"SELECT `RPD-latest`.date_info.ID_ENTITY as ID_ENTITY,
       age_C14,
       error,
       avg_depth,
       `RPD-latest`.entity.ID_ENTITY AS ID_ENTITY,
       entity_name AS ENTITY,
       `RPD-latest`.site.ID_SITE AS ID_SITE,
       site_name AS SITE
FROM
	(`RPD-latest`.date_info INNER JOIN `RPD-latest`.entity
		ON `RPD-latest`.date_info.ID_ENTITY = `RPD-latest`.entity.ID_ENTITY)
INNER JOIN `RPD-latest`.site
			ON `RPD-latest`.entity.ID_SITE = `RPD-latest`.site.ID_SITE")
out <- ageR::select_query_mysql(conn, query)
charcoal <- select_all(conn, "charcoal")
ageR::close_conn(conn)


