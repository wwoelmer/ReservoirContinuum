# download published data from EDI

eems  <-   "https://pasta.lternet.edu/package/data/eml/edi/841/1/f63272976bcd151f8e879cbd14d9a9ce"  # URL from EDI: https://portal.edirepository.org/nis/codeGeneration?packageId=edi.841.1&statisticalFileType=r
try(download.file(eems, destfile = paste0("./Data/raw_data/eems_edi.csv"), 
                  method = "curl"))

flow <- "https://pasta.lternet.edu/package/data/eml/edi/454/4/a18421fd2e95c15d6f97009d5fef3e59" 
try(download.file(flow, destfile = paste0("./Data/raw_data/flow_edi.csv"), 
                  method = "curl"))

chl_all <- "https://pasta.lternet.edu/package/data/eml/edi/555/1/93e2c69f314809705ea21d9244eb368d" 
try(download.file(chl_all, destfile = paste0("./Data/raw_data/chl_all_edi.csv"), 
                  method = "curl"))

chem_all <- "https://pasta.lternet.edu/package/data/eml/edi/199/8/da174082a3d924e989d3151924f9ef98" 
try(download.file(chem_all, destfile = paste0("./Data/raw_data/chem_all_edi.csv"), 
                  method = "curl"))

spcond_all <-  "https://pasta.lternet.edu/package/data/eml/edi/198/10/b3bd353312f9e37ca392e2a5315cc9da" 
try(download.file(spcond_all, destfile = paste0("./Data/raw_data/spcond_edi.csv"), 
                  method = "curl"))

met <-  "https://pasta.lternet.edu/package/data/eml/edi/389/6/a5524c686e2154ec0fd0459d46a7d1eb" 
try(download.file(met, destfile = paste0("./Data/raw_data/met_edi.csv"), 
                  method = "curl"))

ctd <- "https://pasta.lternet.edu/package/data/eml/edi/200/12/0a62d1946e8d9a511bc1404e69e59b8c" 
try(download.file(ctd, destfile = paste0("./Data/raw_data/ctd_edi.csv"), 
                  method = "curl"))

cat <- "https://pasta.lternet.edu/package/data/eml/edi/271/6/23a191c1870a5b18cbc17f2779f719cf" 
try(download.file(cat, destfile = paste0("./Data/raw_data/catwalk_edi.csv"), 
                  method = "curl"))

bvr_vol <- "https://raw.githubusercontent.com/CareyLabVT/BVR-GLM/master/Data_Output/09Apr20_BVR_WaterLevelDailyVol.csv"
try(download.file(bvr_vol, destfile = paste0("./Data/raw_data/bvr_vol_git.csv"), 
                  method = "curl"))

bvr_flow <- "https://raw.githubusercontent.com/CareyLabVT/BVR-GLM/master/inputs/BVR_flow_calcs_new.csv"
try(download.file(bvr_flow, destfile = paste0("./Data/raw_data/bvr_flow_calcs_git.csv"), 
                  method = "curl"))

