## 01. data download  =========================================================

# download longformat file on Medium variant estimates and projections
# for single years (https://esa.un.org/unpd/wpp/Download/Standard/CSV/)
pop.df <- download.file(paste0("https://esa.un.org/unpd/wpp/DVD/Files/",
                               "1_Indicators%20(Standard)/CSV_FILES/",
                               "WPP2017_PopulationBySingleAgeSex.csv"), 
                        "data/raw/WPP2017_PBSAS.csv")

# when changing this file run touch data/raw/WPP2017_PBSAS.csv to stop 
# from running this first download again. 