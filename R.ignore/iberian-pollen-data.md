Iberian Pollen Data
================

## Load data

### Setup

``` r
# Iberian Pollen data location
wdir <- "/path/to/iberian-pollen-data/"
# Import pipe operator from dplyr
`%>%` <- dplyr::`%>%`
```

### Pollen

``` r
all_sites_pollen <- read.csv(file.path(wdir, "iberia-all-sites-pollen.csv"))
all_sites_pollen <- all_sites_pollen %>%
  dplyr::mutate(BACON_INTCAL13_age =
                  suppressWarnings(as.numeric(BACON_INTCAL13_age))) %>%
  dplyr::filter(., !is.na(BACON_INTCAL13_age))

pollen_summary <- read.csv(file.path(wdir, "iberia-pollen-summary.csv"))
pollen_summary <- pollen_summary %>%
  dplyr::filter(., BACON_INTCAL13_age == "YES")
```

### Sites dates

``` r
all_sites_dates <- read.csv(file.path(wdir, "iberia-all-sites-dates.csv"))
```

## Sites of interest

``` r
sites_of_interest <- c("El Tiemblo",
                       "Arroyo de las Cárcavas",
                       "El Brezosa",
                       "El Perro mire",
                       "Laguna Guallar",
                       "Las Vinuelas",
                       "Siles Lake")
```

## Bacon AM

### Create input

``` r
for (site in sites_of_interest) {
  core <- all_sites_dates %>%
    dplyr::filter(., Site.name == site) %>%
    dplyr::mutate(labID = ifelse(Date.code == "", "UNK", Date.code)) %>%
    dplyr::mutate(age = as.numeric(ifelse(Radiocarbon.Age == "",
                                          Calibrated.age,
                                          Radiocarbon.Age))) %>%
    dplyr::mutate(error = ifelse(is.na(Error), 1, Error)) %>%
    dplyr::mutate(depth = Depth..cm.) %>%
    dplyr::mutate(cc = ifelse(is.na(Error) | depth == 0, 0, 1)) %>%
    dplyr::select(., labID, age, error, depth, cc)
  
  # Find corresponding depths file: depths/<ENTITY>_depths.txt
  depths_filepath <- paste0(file.path(wdir, "depths/"),
                            toupper(gsub(" ", "_", ageR:::cln_str(site))),
                            "_depths.txt")
  
  if (file.exists(depths_filepath)) {
    message(paste0("Processing: ", site))
    depths <- matrix(read.table(depths_filepath,
                                col.names = ""))[[1]]
    depths <- data.frame(id = seq_len(length(depths)),
                         depth = as.numeric(depths))
    ageR::create_input(list(sample = depths, core = core),
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
      ageR::create_input(list(sample = depths, core = core),
                         wdir = file.path(wdir, "runs"),
                         entity = paste0(ageR:::cln_str(site), "_C"))
      sites_of_interest <- c(sites_of_interest,
                             paste0(ageR:::cln_str(site), "_C"))
    }
  } else {
    warning(paste0("Depths file not found: \n", depths_filepath))
  }
}
```

### Run model

``` r
for (site in sites_of_interest) {
  ageR::runBacon(file.path(wdir, "runs"), ageR:::cln_str(site), cc = 1)
}
```
