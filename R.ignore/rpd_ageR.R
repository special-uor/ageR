WDIR <- "~/Downloads/ageR-training/tests"
sample_depths <- data.frame(id = 1:100,
                            depth = seq(0, 500, length.out = 100))
core <- data.frame(labID = paste0("X", sprintf("%03d", 1:5)),
                   age = c(50, 200, 1150, 2060, 4050),
                   error = c(10, 15, 5, 1, 70),
                   depth = c(5, 100, 230, 300, 450),
                   cc = 1)
hiatus <- data.frame(id = c(1, 2),
                     depth = c(50, 150))
ageR::create_input(data = list(sample_depths = sample_depths,
                               core = core,
                               # Optional
                               hiatus = NULL),
                   wdir = WDIR,
                   entity = "X",
                   am = "bacon")
out <- ageR::Bacon2(wdir = WDIR, entity = "X", cpus = 8, verbose = FALSE) %>%
  ageR::pb()


run_bacon_RPD("Glenmire", wdir = WDIR, dry_run = F, cpus = 8)

run_bacon_RPD <- function(entity_name, wdir = getwd(), ...) {
  entity_tb <- rpdata::entity %>%
    dplyr::filter(stringr::str_detect(entity_name, !!entity_name))
  if (nrow(entity_tb) == 0)
    stop("Zero entities found, `", entity_name, "`", call. = FALSE)
  if (nrow(entity_tb) > 1)
    stop("Multiple entities found: ",
         paste0(entity_tb$entity_name, collapse = ", "),
         call. = FALSE)
  cc <- ifelse(entity_tb$latitude[1] > 0, 1, 2)
  ### Date info ----
  date_info_tb <- rpdata::date_info %>%
    dplyr::filter(date_type %in% c("Radiocarbon date",
                                   # "Tephra",
                                   # "Other",
                                   "Publication date used as estimate of top of core age (1950 AD = 0)",
                                   "AMS",
                                   "Top of core known",
                                   # "Pollen correlation",
                                   # "Pb",
                                   # "-777777",
                                   # "OSL",
                                   # "Liquid Scintillation Counting",
                                   # "Stratigraphic correlation",
                                   # "Hiatus",
                                   # "Annual laminations",
                                   # "Archaeological dating",
                                   # "TL",
                                   # "-999999",
                                   # "Cs",
                                   # "Uranium Thorium dating",
                                   # "IRSL",
                                   # "Palaeomagnetic date",
                                   # "hiatus",
                                   "Known Fire"
                                   # "tephra",
                                   # "top of core known",
                                   # "pollen correlation"
    )) %>%
    dplyr::filter(ID_ENTITY %in% entity_tb$ID_ENTITY) %>%
    dplyr::mutate(error = ifelse(is.na(error) | error <= 0, 1, error),
                  age_C14 = ifelse(age_C14 < -777777, NA, age_C14),
                  age_calib = ifelse(age_calib < -777777, NA, age_calib),
                  age = dplyr::coalesce(age_C14, age_calib),
                  avg_depth = avg_depth * 100,
                  cc = cc) %>%
    dplyr::select(labID = lab_number,
                  age,
                  error,
                  depth = avg_depth,
                  cc)
  ### Hiatuses ----
  hiatus_tb <- rpdata::date_info %>%
    dplyr::filter(ID_ENTITY %in% entity_tb$ID_ENTITY) %>%
    dplyr::filter(date_type %in% c("Hiatus|hiatus")) %>%
    dplyr::mutate(avg_depth = avg_depth * 100) %>%
    dplyr::select(id = ID_DATE_INFO,
                  depth = avg_depth)

  ## Samples ----
  sample_tb <- rpdata::sample %>%
    dplyr::filter(ID_ENTITY %in% entity_tb$ID_ENTITY) %>%
    dplyr::mutate(avg_depth = avg_depth * 100) %>%
    dplyr::select(id = ID_SAMPLE,
                  depth = avg_depth)
  # list(entity_tb, date_info_tb, sample_tb)
  entity <- unique(entity_tb$entity_name)
  entity_cln <- ageR:::cln_str(entity)
  if (entity != entity_cln)
    ageR:::msg(paste0("Using alias for the entity folder, ",
                      "as special characters were found: `",
                      entity, "` ----> `", entity_cln, "`"))
  ageR::create_input(data = list(core = date_info_tb,
                                 sample_depths = sample_tb,
                                 hiatus = hiatus_tb),
                     wdir = wdir,
                     entity = entity_cln)
  ageR::Bacon2(wdir = wdir, entity = entity_cln, ...)
}

