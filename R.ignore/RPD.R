wdir <- "/path/to/working/directory"
`%>%` <- dplyr::`%>%`
dir.create(wdir, showWarnings = FALSE, recursive = FALSE)

conn <- dabr::open_conn_mysql("RPD-latest",
                              password = rstudioapi::askForPassword(prompt = "Password"))
query <- paste0(
"SELECT entity_name,
       entity.ID_ENTITY as entity_id,
	     lab_number AS labID,
       age_C14 AS age,
       error,
       avg_depth*100 AS depth,
       date_type,
       thickness
FROM date_info INNER JOIN entity
    ON date_info.ID_ENTITY = entity.ID_ENTITY
WHERE latitude >= 45")
rpd <- dabr::select(conn, query)
rpd <- rpd %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::filter(!is.na(error)) %>%
  dplyr::filter(!is.na(depth)) %>%
  dplyr::mutate(error =  ifelse(error <= 0, 1, error)) %>%
  dplyr::arrange(depth) %>%
  dplyr::mutate(cc = ifelse(grepl("*carbon", date_type), 1, 0))

entities <- sort(unique(rpd$entity_name))
entities_id <- c()
entities_clean_names <- c()
message(paste0(length(entities), " were found."))

pb <- progress::progress_bar$new(
  format = "(:current/:total) [:bar] :percent",
  total = length(entities), clear = FALSE, width = 60)
for (entity in entities) {
  pb$tick()
  nent <- ageR:::cln_str(entity) # Clean entity name
  entities_clean_names <- c(entities_clean_names, nent)
  idx <- rpd$entity_name == entity
  entities_id <- c(entities_id, rpd[idx, 2][1])

  query <- paste0("SELECT ID_SAMPLE AS id, sample_depth*100 AS depth
                   FROM sample
                   WHERE ID_ENTITY = ", rpd[idx, 2][1])
  sample_tb <- dabr::select(conn, query, quiet = TRUE)
  sample_tb <- sample_tb %>%
    dplyr::filter(depth != -9999)

  ageR::create_input(data = list(core = rpd[idx, c("labID", "age", "error", "depth", "cc")],
                                 sample_depths = sample_tb),
                     wdir = wdir,
                     entity = nent)
}

entities <- data.frame(id = entities_id,
                       original = entities,
                       clean = entities_clean_names)
write.csv(entities, "entities.csv", row.names = FALSE)
dabr::close_conn(conn)
