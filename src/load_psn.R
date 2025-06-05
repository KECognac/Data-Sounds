load_psn <- function() {
  
  list.files("data/virridy_verification/verified_directory/", full.names = TRUE) %>%
    purrr::map_dfr(~readRDS(.) %>% dplyr::mutate(verification_status = as.character(verification_status))) %>%
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



#combine all 2023 and 2024 data!

compile_psn <- function(){
  library(here)
  
  #pull in 2023 data (post verification)
  post_ver_pwqn_2023 <- list.files(here("data", "pwqn_data",  "2023", "post_verified_directory"), full.names = TRUE)%>%
    purrr::map_dfr(~read_rds(.) %>% dplyr::mutate(verification_status = as.character(verification_status)))
  
  #create each combo
  all_names <- crossing(site = unique(post_ver_pwqn_2023$site), parameter = unique(post_ver_pwqn_2023$parameter)) %>%
    dplyr::mutate(name = paste0(site, "-", parameter))%>%
    pull(name)
  
  verified_psn_2023 <- tibble(files = list.files(here("data", "pwqn_data",  "2023", "verified_directory"), full.names = TRUE)) %>%
    dplyr::filter(!str_detect(files, paste(all_names, collapse = "|")))%>%
    pull(files) %>%
  purrr::map_dfr(~readRDS(.) %>% dplyr::mutate(verification_status = as.character(verification_status)))%>%
    data.table::data.table()
  
  #combine into single dataframe
  pwqn_2023 <- bind_rows(post_ver_pwqn_2023, verified_psn_2023) %>%
    dplyr::filter(DT_round>= as.Date("2023-01-01") & DT_round < as.Date("2024-01-01"))%>%
    dplyr::mutate(date = as_date(DT_round),
                  clean_mean = case_when(is.na(flag) & verification_status == "PASS"  ~ mean,
                                         is.na(flag) & verification_status == "FAIL" ~ NA,
                                         !is.na(flag) & verification_status == "PASS" ~ NA,
                                         !is.na(flag) & verification_status == "FAIL" ~ mean),
                  clean_flag = case_when(is.na(flag) & verification_status == "PASS" ~ NA,
                                         is.na(flag) & verification_status == "FAIL" ~ "needs a flag",
                                         !is.na(flag) & verification_status == "PASS" ~ flag,
                                         !is.na(flag) & verification_status == "FAIL" ~ NA)) %>%
    dplyr::select(DT_round, site, parameter, clean_mean, clean_flag)%>%
    dplyr::mutate(site = tolower(site)) %>%
    # renaming all the sites, just in case
    dplyr::mutate(site = case_when(
      grepl("tamasag", site, ignore.case = TRUE) ~ str_replace(site, "tamasag", "bellvue"),
      grepl("legacy", site, ignore.case = TRUE) ~ str_replace(site, "legacy", "salyer"),
      grepl("lincoln", site, ignore.case = TRUE) ~ str_replace(site, "lincoln", "udall"),
      grepl("timberline", site, ignore.case = TRUE) ~ str_replace(site, "timberline", "riverbend"),
      grepl("prospect", site, ignore.case = TRUE) ~ str_replace(site, "prospect", "cottonwood"),
      grepl("boxelder", site, ignore.case = TRUE) ~ str_replace(site, "boxelder", "elc"),
      grepl("archery", site, ignore.case = TRUE) ~ str_replace(site, "archery", "archery"),
      grepl("river bluffs", site, ignore.case = TRUE) ~ str_replace(site, "river bluffs", "riverbluffs"),
      TRUE ~ site)
    )%>%
    dplyr::mutate(site = case_when(grepl(" virridy", site) ~ str_replace(site, " virridy", "_virridy"),
                                   TRUE ~ site))
  #pull in 2024 data
  # since these are not done being verified, we need to pick out the correct file
    
    pre_verification_path = here("data", "pwqn_data",  "2024", "pre_verification_directory")
    intermediary_path = here("data", "pwqn_data",  "2024", "intermediary_directory")
    verified_path = here("data", "pwqn_data",  "2024", "verified_directory")

  pre_verification_data = set_names( map(list.files(pre_verification_path, full.names = TRUE), read_rds),
                                         list.files(pre_verification_path))
    
  intermediary_data = set_names(
        map(list.files(intermediary_path, full.names = TRUE), read_rds),
        list.files(intermediary_path))
      
  verified_data = set_names(
        map(list.files(verified_path, full.names = TRUE), read_rds),
        list.files(verified_path))
  
  #create each combo
  all_names <- crossing(site = c(unique(pwqn_2023$site), "pfal", "sfm", "penn", "lbea"), parameter = unique(pwqn_2023$parameter)) %>%
    dplyr::mutate(name = paste0(site, "-", parameter)) %>%
    dplyr::pull(name)

  
  retrieve_relevant_data_name <- function(df_name_arg) {
    
    # Check verified_data
    matches <- str_detect(names(verified_data), fixed(df_name_arg, ignore_case = TRUE))
    if (any(matches)) {
      matched_name <- names(verified_data)[matches][1]
      return(list(source = "verified_data", matched_name = matched_name))
    }
    
    # Check intermediary_data
    matches <- str_detect(names(intermediary_data), fixed(df_name_arg, ignore_case = TRUE))
    if (any(matches)) {
      matched_name <- names(intermediary_data)[matches][1]
      return(list(source = "intermediary_data", matched_name = matched_name))
    }
    
    # Check pre_verification_data
    matches <- str_detect(names(pre_verification_data), fixed(df_name_arg, ignore_case = TRUE))
    if (any(matches)) {
      matched_name <- names(pre_verification_data)[matches][1]
      return(list(source = "pre_verification_data", matched_name = matched_name))
    }
    
    # Return NULL if no matches found
    return(NULL)
  }
  
  #get the data for each name
  #parameter_loc <- map(all_names, retrieve_relevant_data_name)
  relevant_data <- map(all_names, ~ {
    sonde_name <- .x
    data_source <- NULL
    sonde_df <- NULL
    
    # Determine which directory to pull data from
    tryCatch({
      data_source <- retrieve_relevant_data_name(sonde_name)
      # cat("Data for",sonde_name,"will be pulled from",data_source,"\n")
    }, error = function(err) {
      cat("Data for",sonde_name,"not found.\n")
      return(NULL)  # Return NULL if data source can't be determined
    })
    
    # Only try to pull in the data if data_source was successfully determined
    if (!is.null(data_source)) {
      tryCatch({
        sonde_df <- get(data_source$source)[[data_source$matched_name]]
      }, error = function(err) {
        #cat("Sonde", sonde_name, "not found.\n")
        return(NULL)  # Return NULL if sonde data can't be retrieved
      })
    }
    # Only return a list if both data_source and sonde_df are available
    if (!is.null(data_source) & !is.null(sonde_df)) {
      return(list(sonde_df = sonde_df, data_source = data_source$source, 
                  matched_name = data_source$matched_name))
    } else {
      return(NULL)  # Return NULL if either data_source or sonde_df is NULL
    }
  })
  
pwqn_2024 <- relevant_data %>%
    purrr::compact() %>%  # Remove NULL entries
    purrr::map_dfr(~ .x$sonde_df)%>%
  dplyr::mutate(date = as_date(DT_round),
                clean_mean = case_when(is_verified & verification_status %in% c( "PASS", "FLAGGED") ~ mean, 
                                       is_verified & verification_status == "OMIT" ~ NA,
                                       !is_verified & is.na(flag) ~ mean,
                                       !is_verified & !is.na(flag) ~ NA),
                clean_flag = case_when(is_verified & verification_status %in% c( "PASS") ~ NA, 
                                       is_verified & verification_status == "OMIT" ~ flag,
                                       is_verified & verification_status == "FLAGGED" ~ user_flag,
                                       !is_verified & is.na(flag) ~ NA,
                                       !is_verified & !is.na(flag) ~ flag))%>%
  dplyr::select(DT_round, site, parameter, clean_mean, clean_flag)
  

  #write to single file for DS use

all_pwqn_data <- bind_rows(pwqn_2023, pwqn_2024)%>%
  dplyr::filter(parameter %in% c("Temperature", "pH", "DO", "Turbidity", "Specific Conductivity", "Depth")) %>%
  dplyr::filter(!grepl("_virridy", site, ignore.case = TRUE), 
         !grepl("boxcreek", site, ignore.case = TRUE))%>%
  dplyr::filter(!is.na(clean_mean))

  
arrow::write_parquet(all_pwqn_data, here("data", "all_pwqn_data.parquet"))
  
}

#compile_psn()
