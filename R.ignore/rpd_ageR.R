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



run_bacon_RPD <- function(entity_name, wdir = getwd(), ...) {
  entity_tb <- rpdata::entity %>%
    dplyr::filter(stringr::str_detect(entity_name, !!entity_name))
  if (nrow(entity_tb) == 0)
    stop("Zero entities found, `", entity_name, "`", call. = FALSE)
  if (nrow(entity_tb) > 1)
    stop("Multiple entities found: ",
         paste0(entity_tb$entity_name, collapse = ", "),
         call. = FALSE)
  # Calibration curves
  cc <- ifelse(entity_tb$latitude[1] > 0,
               1, # Northern hemisphere
               3) # Southern hemisphere
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
    dplyr::filter(age_used == "yes") %>%
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

# Test cases ----
## Aguas Frias ----
run_bacon_RPD("Aguas Frias", wdir = WDIR, dry_run = F, cpus = 2, thick = c(5,6)) %>%
  ageR::pb()

## Bermu mire ----
run_bacon_RPD("Bermu Mire core_large", wdir = WDIR, dry_run = F, cpus = 4) %>%
  ageR::pb()

## El Brezosa core_macro ---
run_bacon_RPD("El Brezosa core_macro", wdir = WDIR, dry_run = F, cpus = 4, acc = c(5, 10, 20)) %>%
  ageR::pb()

# bacon_plots <- purrr::map(out, ~.x$BACON)
# bacon_plots_labels <- scenarios %>%
#   dplyr::mutate(n = seq_along(acc.mean),
#                 label = sprintf("S%03d-AR%03d-T%d", n, acc.mean, thick)) %>%
#   .$label
# ggplot2::ggsave(filename = paste0(prefix, "_BACON.pdf"),
#                 plot = cowplot::plot_grid(plotlist = bacon_plots,
#                                           nrow = length(thickness),
#                                           labels = bacon_plots_labels,
#                                           label_size = 12,
#                                           label_x = 0, label_y = 0,
#                                           hjust = -0.1, vjust = -0.7),
#                 # plot = cowplot::ggdraw(bacon_plots[[1]]),
#                 device = "pdf",
#                 path = wdir,
#                 width = 7 * length(accMean),
#                 height = 5 * length(thickness),
#                 limitsize = FALSE)

## Glenmire ----
run_bacon_RPD("Glenmire", wdir = WDIR, dry_run = F, cpus = 8)

