##' @title data_merge
##' @param ffc_v1
##' @param ffc_v2
##' @return df_all
##' @author Ryan Peek
##' @export
data_merge <- function(ffc_v1, ffc_v2){

  if(!fs::file_exists(glue("{here()}/output/ffm_ref_combined_tidy.fst"))){
    # get paths
    base_data_dir <- glue("{here()}/output/")

    #fst_files <- dir(path = base_data_dir, pattern='*.fst$', recursive = T, full.names = T)
    # filter to files listed above
    #fst_files <- fst_files[grep(fst_files, pattern=glue("{ffc_v1}*|{ffc_v2}*"))]

    # load data
    #v19 <- fst::read_fst(fst_files[grep(fst_files, pattern=glue("{ffc_v1}*"))]) %>%
    #  mutate(version=as.character(version))
    #v20 <- fst::read_fst(fst_files[grep(fst_files, pattern=glue("{ffc_v2}*"))]) %>%
     # mutate(version=as.character(version))

    # makes plotting easier
    df_all <- bind_rows(ffc_v1, ffc_v2) %>%
      mutate(ffc_version=factor(version), .after=filename) %>% select(-version)

    # save out
    write_rds(x = df_all, compress = "gz",
              file = glue("{base_data_dir}ffm_ref_combined_tidy.rds"))
    write_fst(x = df_all, path = glue("{base_data_dir}/ffm_ref_combined_tidy.fst"), compress = 100)
    return(df_all)
  }
  else{
    print("Already exists!")
    df_all <- read_rds(file = glue("{here::here()}/output/ffm_ref_combined_tidy.rds"))
    print("Data loaded into local environment")
    return(df_all)
  }
}
