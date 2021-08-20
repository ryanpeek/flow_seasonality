##' @title data_raw_combine
##' @param ffc_version
##' @param returndata
##' @return combined_dat
##' @author Ryan Peek
##' @export
#'compile raw data
data_raw_combine <- function(ffc_version) {

  # set basefolder name
  basedir <- fs::dir_ls(path = "data/",
                        type = "directory",
                        regexp = glue::glue("{ffc_version}"))

  # list all files
  csv_files <- dir(path = basedir, pattern='*.csv$', recursive = T)

  ## * Read in and convert to long format -------------------------
  if(!fs::file_exists(glue("output/{fs::path_file(basedir)}_tidy_df.fst")) | !fs::file_exists(glue("output/{fs::path_file(basedir)}_tidy_df.fst"))){

    # create a dataframe to map data into
    dat_long <- tibble(filename = csv_files, version=ffc_version) %>%
      mutate(file_contents = map(filename,
                                 ~ read_csv(file.path(basedir, .x))),
             dat_long = map(file_contents,
                            ~pivot_longer(.x, cols=!Year, names_to="year", values_to="value"))
      ) %>%
      # here we drop the raw data and unlist everything
      unnest(cols=c(dat_long)) %>% select(-file_contents) %>%
      rename(ffm=Year) # fix funky matrix remnant name

    # quick tidying here to pull out class and gage as independent cols
    df_tidy <- dat_long %>%
      separate(filename, into=c("class","gage"), sep="/", remove=F) %>%
      # clean up last bit of filename from gage
      separate(gage, into=c("gageID"), sep="_", remove=TRUE)

    ## * Tidy FFM Name and Components ----------------------------

    # get 24 flow component names
    ff_defs <- readxl::read_xlsx("data/Functional_Flow_Metrics_List_and_Definitions_final.xlsx", range = "A1:F25", .name_repair = "universal", trim_ws = TRUE)

    # check names that match, should be 24
    sum(unique(df_tidy$ffm) %in% ff_defs$Flow.Metric.Code)

    # join with the data
    df_trim <- inner_join(df_tidy, ff_defs, by=c("ffm"="Flow.Metric.Code")) %>%
      # fix names
      janitor::clean_names() %>%
      mutate(flow_component = factor(flow_component,
                                     levels = c("Fall pulse flow", "Wet-season baseflow",
                                                "Peak flow", "Spring recession flow",
                                                "Dry-season baseflow")),
             version = as.character(version))

    ## * Save it out ---------------------------------------------

    # write a compressed .fst version
    fst::write_fst(x=df_trim, path = glue("{here::here()}/output/{fs::path_file(basedir)}_tidy_df.fst"), compress=100)

    print("Data loaded into local environment")
    return(df_trim)
    #assign(x = glue("v{ffc_version}"), value = df_trim, envir = .GlobalEnv)
  }
  else{
    print("Already updated")
    # with fst
    dat <- fst::read_fst(path = glue("{here::here()}/output/{fs::path_file(basedir)}_tidy_df.fst"))
    print("Data loaded into local environment")
    #assign(x = glue("v{ffc_version}"), value = dat, envir = .GlobalEnv)
    return(dat)
    }
}

