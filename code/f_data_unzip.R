# function to unzip data

data_unzip <- function(ffc_zip){
  ffc_zip_dirs <- fs::dir_ls("data", type="file", regexp = glue("{ffc_zip}"))
  basefolder <- path_ext_remove(ffc_zip_dirs)
  # unzip
  if(!fs::dir_exists(basefolder)){
    unzip(zipfile = glue("{basefolder}.zip"), exdir = "data", overwrite = FALSE)
    return(basefolder)
  } else{
    print("Already unzipped")
    return(basefolder)}
}

