wd <- here::here("runs")
prefix <- "rpd001"
`%>%` <- dplyr::`%>%`
dir.create(file.path(wd, 'Bacon_runs/'),
           showWarnings = FALSE,
           recursive = TRUE)
setwd(file.path(wd, 'Bacon_runs/'))
conn <- ageR::open_conn_mysql("RPD-latest", "root")
# out <- ageR::select_query_mysql(conn, "SELECT * FROM `RPD-latest`.date_info LIMIT 10;")
# query <- paste0(
# "SELECT `RPD-latest`.date_info.ID_ENTITY as ID_ENTITY,
#        age_C14,
#        error,
#        avg_depth,
#        `RPD-latest`.entity.ID_ENTITY AS ID_ENTITY,
#        entity_name AS ENTITY,
#        `RPD-latest`.site.ID_SITE AS ID_SITE,
#        site_name AS SITE
# FROM
# 	(`RPD-latest`.date_info INNER JOIN `RPD-latest`.entity
# 		ON `RPD-latest`.date_info.ID_ENTITY = `RPD-latest`.entity.ID_ENTITY)
# INNER JOIN `RPD-latest`.site
# 			ON `RPD-latest`.entity.ID_SITE = `RPD-latest`.site.ID_SITE")
# query <- paste0(
# "SELECT lab_number AS labID,
#        age_C14 AS age,
#        error,
#        avg_depth AS depth
# FROM date_info
# WHERE material_dated = 'charcoal'")
query <- paste0(
"SELECT entity_name,
       entity.ID_ENTITY as entity_id,
	     lab_number AS labID,
       age_C14 AS age,
       error,
       avg_depth AS depth
FROM date_info INNER JOIN entity
    ON date_info.ID_ENTITY = entity.ID_ENTITY")
bacon_input <- ageR::select_query_mysql(conn, query)
bacon_input <- bacon_input %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::filter(!is.na(error)) %>%
  dplyr::filter(!is.na(depth)) %>%
  dplyr::mutate(error =  ifelse(error <= 0, 1, error)) %>%
  dplyr::arrange(depth)

# query <- "SELECT ID_SAMPLE AS id, sample_depth AS depth FROM sample"
# sample_tb <- ageR::select_query_mysql(conn, query)
# sample_tb <- sample_tb %>%
#   dplyr::filter(depth != -9999)

entities <- sort(unique(bacon_input$entity_name))
entities_id <- c()
entities_clean_names <- c()
pb <- progress::progress_bar$new(
  format = "(:spin) [:bar] :percent",
  total = length(entities), clear = FALSE, width = 60)
for (e in entities) {
  pb$tick()
  # print(paste0("Processing: ", e))
  nent <- iconv(nent, to = 'ASCII//TRANSLIT') # Remove accented characters
  nent <- gsub("[[:punct:]]", "", nent)
  nent <- gsub(" ", "-", nent)
  entities_clean_names <- c(entities_clean_names, nent)
  dir.create(file.path(wd, '/Bacon_runs/', nent),
             showWarnings = FALSE,
             recursive = TRUE)
  setwd(file.path(wd, '/Bacon_runs/', nent))
  idx <- bacon_input$entity_name == e
  entities_id <- c(entities_id, bacon_input[idx, 2][1])
  write.csv(bacon_input[idx, -c(1:2)], paste0(nent, ".csv"), row.names = FALSE)

  query <- paste0("SELECT ID_SAMPLE AS id, sample_depth AS depth
                   FROM sample
                   WHERE ID_ENTITY = ", bacon_input[idx, 2][1])
  sample_tb <- ageR::select_query_mysql(conn, query, quiet = TRUE)
  sample_tb <- sample_tb %>%
    dplyr::filter(depth != -9999)
  write.table(sample_tb$depth,
              paste0(nent, "_depths.txt"),
              row.names = FALSE,
              col.names = FALSE)
  write.csv(sample_tb$id,
            paste0(nent, "_sample_ids.csv"),
            row.names = FALSE)
  # query <- paste0("SELECT sample_depth AS depth
  #                  FROM sample
  #                  WHERE ID_ENTITY = ", bacon_input[idx, 2][1])
  # depths <- ageR::select_query_mysql(conn, query, quiet = TRUE)
  # write.table(depths, paste0(nent, "_depths.txt"),
  #             row.names = FALSE,
  #             col.names = FALSE)
#   write.table(sample_tb$depth, paste0(nent, "_depths.txt"),
#               row.names = FALSE,
#               col.names = FALSE)
#   write.csv(sample_tb$id, paste0(nent, "_sample_ids.csv"), row.names = FALSE)
}
setwd(file.path(wd, "Bacon_runs/"))
entities <- data.frame(id = entities_id,
                       original = entities,
                       clean = entities_clean_names)
write.csv(entities, "entities.csv", row.names = FALSE)

# query <- "SELECT sample_depth AS depth FROM sample"
# depths <- ageR::select_query_mysql(conn, query)
# write.csv(depths, paste0(prefix, "_depths.csv"), row.names = FALSE)

# query <- "SELECT ID_SAMPLE AS id FROM sample"
# ids <- ageR::select_query_mysql(conn, query)
# write.csv(ids, paste0(prefix, "_sample_ids.csv"), row.names = FALSE)

# Empty data frame for Hiatus
hiatus_tb <- data.frame(sample_id = NA, depth_sample_bacon = NA)[-1, ]
write.csv(hiatus_tb, "hiatus.csv", row.names = FALSE)

# Empty data frame for "not used dates"
setwd(file.path(wd))
not_used_dates <- data.frame(depth = NA, error = NA)[-1, ]
write.csv(not_used_dates, "not_used_dates.csv", row.names = FALSE)

# charcoal <- ageR::select_all(conn, table = "charcoal")
ageR::close_conn(conn)


runBacon(wdir, entities_clean_names[1])
runBacon(wdir, entities_clean_names[2])
