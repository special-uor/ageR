is_valid_connection <- function(conn) {
  return(class(conn) == "MariaDBConnection")
}

conn <- open_conn_mysql("RPD-latest", "root")
out <- select_query_mysql(conn, "SELECT * FROM `RPD-latest`.date_info LIMIT 10;")
close_conn_mysql(conn)
