load_psn <- function() {
  
  list.files("data/virridy_verification/verified_directory/", full.names = TRUE) %>%
    map_dfr(~readRDS(.) %>% dplyr::mutate(verification_status = as.character(verification_status))) %>%
    data.table::data.table() %>%
    dplyr::mutate(date = as_date(DT_round),
                  clean_mean = case_when(is.na(flag) & verification_status == "PASS" ~ mean,
                                         is.na(flag) & verification_status == "FAIL" ~ NA,
                                         !is.na(flag) & verification_status == "PASS" ~ NA,
                                         !is.na(flag) & verification_status == "FAIL" ~ mean),
                  clean_flag = case_when(is.na(flag) & verification_status == "PASS" ~ NA,
                                         is.na(flag) & verification_status == "FAIL" ~ "needs a flag",
                                         !is.na(flag) & verification_status == "PASS" ~ flag,
                                         !is.na(flag) & verification_status == "FAIL" ~ NA)) %>%
    dplyr::select(DT_join, site, parameter, clean_mean, clean_flag)
  
  
}
