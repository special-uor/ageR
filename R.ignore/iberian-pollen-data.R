# Iberian Pollen data location
wdir <- "/path/to/iberian-pollen-data"

# Import pipe operator from dplyr
`%>%` <- dplyr::`%>%`

# Load pollen data
all_sites_pollen <- read.csv(file.path(wdir, "iberia-all-sites-pollen.csv"))
all_sites_pollen <- all_sites_pollen %>%
  dplyr::mutate(BACON_INTCAL13_age =
                  suppressWarnings(as.numeric(BACON_INTCAL13_age))) %>%
  dplyr::filter(., !is.na(BACON_INTCAL13_age))

pollen_summary <- read.csv(file.path(wdir, "iberia-pollen-summary.csv"))
pollen_summary <- pollen_summary %>%
  dplyr::filter(., BACON_INTCAL13_age == "YES")

# Load all sites dates
all_sites_dates <- read.csv(file.path(wdir, "iberia-all-sites-dates.csv"))

sites_of_interest <- c("El Tiemblo",
                       "Arroyo de las Cárcavas",
                       "El Brezosa",
                       "El Perro mire",
                       "Laguna Guallar",
                       "Las Vinuelas",
                       "Siles Lake")

# Create Bacon input for each site
for (site in sites_of_interest) {
  core <- all_sites_dates %>%
    dplyr::filter(Site.name == site) %>%
    dplyr::mutate(labID = ifelse(Date.code == "", "UNK", Date.code)) %>%
    dplyr::mutate(age = as.numeric(ifelse(Radiocarbon.Age == "",
                                          Calibrated.age,
                                          Radiocarbon.Age))) %>%
    dplyr::mutate(error = ifelse(is.na(Error), 1, Error)) %>%
    dplyr::mutate(depth = Depth..cm.) %>%
    dplyr::mutate(cc = ifelse(is.na(Error) | depth == 0, 0, 1)) %>%
    dplyr::select(labID, age, error, depth, cc)

  idx <- c()
  not_used_dates <- NULL
  # for (i in seq_len(nrow(core))[-1]) {
  #   # Previous
  #   ## Drop younger and deeper
  #   if (core$age[i] < core$age[i - 1]) {
  #     idx <- c(idx, i)
  #   }
  #   # ## Drop older and shallow
  #   # if (any(core$age[i] < core$age[1:i - 1])) {
  #   #   idx <- c(idx, which(core$age[i] < core$age[1:i - 1]))
  #   # }
  #   idx <- unique(idx)
  # }
  #
  # if (length(idx) > 0) {
  #   print(core[idx, ])
  #   not_used_dates <- core[idx, ]
  #   core <- core[-idx, ]
  # }

  depths0 <- all_sites_pollen %>%
    dplyr::filter(Site.Name == site) %>%
    dplyr::select(Depth..cm.)
  depths0 <- depths0[, 1]
  # Find corresponding depths file: depths/<ENTITY>_depths.txt
  depths_filepath <- paste0(file.path(wdir, "depths/"),
                            toupper(gsub(" ", "_", ageR:::cln_str(site))),
                            "_depths.txt")

  # if (file.exists(depths_filepath)) {
  if (length(depths0) > 0) {
    message(paste0("Processing: ", site))
    depths <- depths0
    # print(cbind(depths0, depths))
    depths <- data.frame(id = seq_len(length(depths)),
                         depth = as.numeric(depths))
    hiatus <- NULL

    if (site == "Laguna Guallar") {
      core <- rbind(c("TOP", -57, 1, 0, 0),
                    core,
                    c("UNK", 9888, 80, 120, 1),
                    c("UNK", 10529, 80, 170, 1))
      hiatus <- data.frame(id = c(1, 2), depth = c(49, 90))
    }
    ageR::create_input(list(sample_depths = depths,
                            core = core,
                            hiatus = hiatus,
                            not_used = not_used_dates),
                       wdir = file.path(wdir, "runs"),
                       entity = ageR:::cln_str(site))
    depths_C_filepath <- paste0(file.path(wdir, "depths/"),
                                toupper(gsub(" ", "_", ageR:::cln_str(site))),
                                "_depths_C.txt")

    if (file.exists(depths_C_filepath)) {
      message(paste0("Creating entity with Charcoal record depths: ",
                     depths_C_filepath))
      depths <- matrix(read.table(depths_C_filepath,
                                  col.names = ""))[[1]]
      depths <- data.frame(id = seq_len(length(depths)),
                           depth = as.numeric(depths))

      entity <- ageR:::cln_str(site)
      path <- file.path(wdir, "runs", entity, 'Bacon _runs', entity)
      write.table(depths$depth,
                  file.path(path, paste0(entity, "_Carbon_depths.alt.txt")),
                  row.names = FALSE,
                  col.names = FALSE)
    }
  } else {
    warning(paste0("Depths file not found: \n", depths_filepath))
  }
}

# Run Bacon for each site of interest
for (site in sites_of_interest) {
  out <- ageR::Bacon(file.path(wdir, "runs"), ageR:::cln_str(site), cpus = 8, cc = 1, dry_run = F)
  # test18 <- ageR::Bacon(wdir, # file.path(wdir0, "runs"),
  #                       ageR:::cln_str("Arroyo de las Carcavas"),
  #                       cpus = 4,
  #                       cc = 1,
  #                       quiet = F,
  #                       dry_run = F,
  #                       seed = 2020)
}
