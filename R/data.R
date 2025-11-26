#' Use PUF files for SILC example
#'
#' @returns nothing, but creation of csv datasets
#' @param vec_country Vector of country to import
#' @param path_out Path to export csv
#' @export
#'
#' @examples
#' # create_fake_silc()
create_fake_silc <- function(
    vec_country = c("BE", "RO"),
    path_out = "inst/shiny-examples/complete/data/SILC/") {
  vec_country %>% map(~ {
    country_code <- .x

    temp_dir <- tempdir()
    temp_file <- tempfile()
    path <- paste0(
      "https://ec.europa.eu/eurostat/documents/203647/16979414/",
      country_code, "_PUF_EUSILC.zip"
    )
    download.file(path, temp_file)
    unzip(temp_file, exdir = temp_dir)
    unlink(temp_file)

    vec_year <- unique(str_extract(list.files(temp_dir, country_code), "[0-9]{4}"))

    vec_year %>% map(~ {
      year <- .x

      df_d <- readr::read_csv(paste0(temp_dir, "/", country_code, "_", year, "d_EUSILC.csv"))
      df_d <- df_d %>% mutate(NR_ITW = paste(DB040, round(runif(nrow(.), 1, 5)), sep = "-"))

      df_h <- readr::read_csv(paste0(temp_dir, "/", country_code, "_", year, "h_EUSILC.csv")) %>%
        dplyr::left_join(
          df_d %>% dplyr::select(DB010, DB020, DB030, DB040, NR_ITW),
          dplyr::join_by(HB010 == DB010, HB020 == DB020, HB030 == DB030)
        )

      df_p <- readr::read_csv(paste0(temp_dir, "/", country_code, "_", year, "p_EUSILC.csv")) %>%
        dplyr::left_join(
          df_d %>% dplyr::select(DB010, DB020, DB030, DB040, NR_ITW),
          dplyr::join_by(PB010 == DB010, PB020 == DB020, PX030 == DB030)
        )

      path_out_P <- paste0(path_out, "PFILE/", country_code, "_", year)
      path_out_H <- paste0(path_out, "HFILE/", country_code, "_", year)

      readr::write_csv(df_p, file = paste0(path_out_P, "p_EUSILC.csv"), col_names = T)
      readr::write_csv(df_h, file = paste0(path_out_H, "h_EUSILC.csv"), col_names = T)
    })
  })
  return(NULL)
}


#' Simulate EU-SILC dataset with injected errors
#'
#' @returns data.frame from laeken::eusilc with ids and errors
#' @export
#'
#' @examples
#' # create_eusilc_sim()
create_eusilc_sim <- function() {

  # Charger les données
  data("eusilc", package = "laeken")

  # Create interview variable
  set.seed(123)
  eusilc_sim <- eusilc %>%
    mutate(
      nr_itw = paste(db040,
                     sample(1:5, n(), replace = TRUE),
                     sep = "-")
    )

  # Create year wave
  set.seed(123)
  eusilc_sim <- eusilc_sim %>%
    dplyr::mutate(db010 = sample(2018:2020, n(), replace = TRUE))

  # Generate errors
  eusilc_sim <- eusilc_sim %>%
    dplyr::mutate(
      age = dplyr::if_else(nr_itw == "Vienna-2",age/10,age),
      eqIncome = dplyr::if_else(nr_itw == "Vienna-2",eqIncome / 100,eqIncome),
      pl030 = replace(pl030,nr_itw == "Vienna-2" & pl030 == "5",NA),
      pb220a = replace(pb220a,db010 == 2020,NA),
      hsize = replace(hsize,db010 == 2020 & hsize == 4,NA),
      eqIncome = dplyr::if_else(db010 == 2020,eqIncome / 100,eqIncome)
    )

  return(eusilc_sim)
}
