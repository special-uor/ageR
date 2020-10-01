#' Connect to database
#'
#' Uses \code{RMariaDB} to open a connection to a MySQL database
#'
#' @param dbname database name
#' @param user username of database owner
#' @param password password [default: \code{NULL}]
#' @param host database host, it can be local (default) or remote
#' @param port database port
#'
#' @return \code{MariaDBConnection} connection object
#' @export
#'
#' @examples
#' \dontrun{
#'     conn <- conn_mysql("sys", "root")
#' }
open_conn_mysql <- function(dbname,
                            user,
                            password = NULL,
                            host = "localhost",
                            port = 3306) {
  conn <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                              user = user,
                              password = password,
                              dbname = dbname,
                              host = host,
                              port = port)
  # Create connection
  # convert <- DBI::dbConnect(odbc::odbc(),
  #                           driver   = "/usr/local/lib/psqlodbcw.so",
  #                           database = "idep96v2",
  #                           UID      = rstudioapi::askForPassword("Database user"),
  #                           PWD      = rstudioapi::askForPassword("Database password"),
  #                           server   = "localhost",
  #                           Port     = 5432)
  return(conn)
}

