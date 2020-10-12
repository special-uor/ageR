wdir <- here::here("runs/006")
tictoc::tic("Retrive data from DB")
`%>%` <- dplyr::`%>%`
dir.create(wdir, showWarnings = FALSE, recursive = FALSE)

conn <- ageR::open_conn_mysql("RPD-latest", "root")
query <- paste0(
"SELECT entity_name,
       entity.ID_ENTITY as entity_id,
	     lab_number AS labID,
       age_C14 AS age,
       error,
       avg_depth*100 AS depth,
       latitude,
       date_type,
       thickness
FROM date_info INNER JOIN entity
    ON date_info.ID_ENTITY = entity.ID_ENTITY
WHERE latitude >= 45")
rpd <- ageR::select_query_mysql(conn, query)
rpd <- rpd %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::filter(!is.na(error)) %>%
  dplyr::filter(!is.na(depth)) %>%
  dplyr::mutate(error =  ifelse(error <= 0, 1, error)) %>%
  dplyr::arrange(depth) %>%
  dplyr::mutate(cc = ifelse(grepl("*carbon", date_type), 1, 0))
rpd$latitude <- NULL

entities <- sort(unique(rpd$entity_name))
entities_id <- c()
entities_clean_names <- c()
message(paste0(length(entities), " were found."))
pb <- progress::progress_bar$new(
  format = "(:current/:total) [:bar] :percent",
  total = length(entities), clear = FALSE, width = 60)
for (entity in entities) {
  pb$tick()
  nent <- cln_str(entity)
  entities_clean_names <- c(entities_clean_names, nent)
  idx <- rpd$entity_name == entity
  entities_id <- c(entities_id, rpd[idx, 2][1])

  query <- paste0("SELECT ID_SAMPLE AS id, sample_depth*100 AS depth
                   FROM sample
                   WHERE ID_ENTITY = ", rpd[idx, 2][1])
  sample_tb <- ageR::select_query_mysql(conn, query, quiet = TRUE)
  sample_tb <- sample_tb %>%
    dplyr::filter(depth != -9999)

  # Write core data
  dir.create(file.path(wdir, nent, "core"),
             showWarnings = FALSE,
             recursive = TRUE)
  setwd(file.path(wdir, nent, "core"))
  # write.csv(rpd[idx, -c(1:2)], paste0(nent, ".csv"), row.names = FALSE)
  write.csv(sample_tb, paste0(nent, "_depths.csv"), row.names = FALSE)
  write.table(sample_tb$depth,
              paste0(nent, "_depths.txt"),
              row.names = FALSE,
              col.names = FALSE)
  write.csv(sample_tb$id,
            paste0(nent, "_sample_ids.csv"),
            row.names = FALSE)

  # Data for the Bacon model
  dir.create(file.path(wdir, nent, 'Bacon_runs', nent),
             showWarnings = FALSE,
             recursive = TRUE)
  setwd(file.path(wdir, nent, 'Bacon_runs', nent))
  # c("labID", "age", "error", "depth", "cc")
  write.csv(rpd[idx, c("labID", "age", "error", "depth", "cc")],
            paste0(nent, ".csv"), row.names = FALSE)
  . <- file.symlink(from = paste0("../../core/", nent, "_depths.txt"),
                    to = paste0("."))
  . <- file.symlink(from = paste0("../../core/", nent, "_sample_ids.csv"),
                    to = paste0("."))
  # Empty data frame for "not used dates"
  setwd(file.path(wdir, nent))
  not_used_dates <- data.frame(depth = NA, error = NA)[-1, ]
  write.csv(not_used_dates, "not_used_dates.csv", row.names = FALSE)

  # Empty data frame for Hiatus
  hiatus_tb <- data.frame(sample_id = NA, depth_sample = NA)[-1, ]
  write.csv(hiatus_tb, "hiatus.csv", row.names = FALSE)

  # Data for the Bchron model
  dir.create(file.path(wdir, nent, 'Bchron'),
             showWarnings = FALSE,
             recursive = TRUE)
  setwd(file.path(wdir, nent, 'Bchron'))
  # c("labID", "age", "error", "depth", "thickness", "calib_used")
  # mutate(calib_curve_new = case_when(
  #   calib_used == 'NULL' ~ 'normal',
  #   calib_used == 'unknown' ~ 'unknown',
  #   calib_used == "not calibrated" ~ 'ask again',
  #   calib_used == "INTCAL13 NH" ~ 'intcal13'))
  write.csv(rpd[idx, c("labID", "age", "error", "depth", "thickness")],
            paste0(nent, ".csv"), row.names = FALSE)
  . <- file.symlink(from = paste0("../core/", nent, "_depths.csv"),
                    to = paste0("."))


  # Data for the Linear Interpolation model
  dir.create(file.path(wdir, nent, 'linInterp'),
             showWarnings = FALSE,
             recursive = TRUE)
  setwd(file.path(wdir, nent, 'linInterp'))
  write.csv(rpd[idx, c("labID", "age", "error", "depth", "date_type")],
            paste0(nent, ".csv"), row.names = FALSE)
  . <- file.symlink(from = paste0("../core/", nent, "_depths.csv"),
                    to = paste0("."))

  # Data for the Linear Regression model
  dir.create(file.path(wdir, nent, 'linReg'),
             showWarnings = FALSE,
             recursive = TRUE)
  setwd(file.path(wdir, nent, 'linReg'))
  write.csv(rpd[idx, c("labID", "age", "error", "depth", "date_type")],
            paste0(nent, ".csv"), row.names = FALSE)
  . <- file.symlink(from = paste0("../core/", nent, "_depths.csv"),
                    to = paste0("."))
  . <- file.symlink(from = paste0("../core/", nent, "_depths.csv"),
                    to = paste0("./", nent, "_ids.csv"))

  # Data for the StalAge model
  dir.create(file.path(wdir, nent, 'StalAge'),
             showWarnings = FALSE,
             recursive = TRUE)
  setwd(file.path(wdir, nent, 'StalAge'))
  write.csv(rpd[idx, c("labID", "age", "error", "depth")],
            paste0(nent, ".csv"), row.names = FALSE)
  . <- file.symlink(from = paste0("../core/", nent, "_depths.csv"),
                    to = paste0("."))
}
setwd(file.path(wdir))
entities <- data.frame(id = entities_id,
                       original = entities,
                       clean = entities_clean_names)
write.csv(entities, "entities.csv", row.names = FALSE)

# charcoal <- ageR::select_all(conn, table = "charcoal")
ageR::close_conn(conn)
tictoc::toc()

# wdir <- here::here("runs/004")
# setwd(wdir)
entities <- read.csv("entities.csv")
AM <- function(i) {
  tictoc::tic(paste0(entities$original[i]))
  setwd(wdir)
  ageR::runBacon(wdir, entities$clean[i])
  # ageR::runLinReg(wdir, entities$clean[i])
  tictoc::toc()
}
AM(1)
