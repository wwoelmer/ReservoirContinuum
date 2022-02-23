# script to download published data from EDI

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

spcond_all <- "https://pasta.lternet.edu/package/data/eml/edi/198/9/b3bd353312f9e37ca392e2a5315cc9da"
try(download.file(spcond_all, destfile = paste0("./Data/raw_data/spcond_edi.csv"), 
                  method = "curl"))

met <-  "https://pasta.lternet.edu/package/data/eml/edi/389/6/a5524c686e2154ec0fd0459d46a7d1eb" 
try(download.file(met, destfile = paste0("./Data/raw_data/met_edi.csv"), 
                  method = "curl"))
