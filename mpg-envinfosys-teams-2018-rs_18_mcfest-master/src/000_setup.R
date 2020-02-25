root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                 alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

#Set librarys
libs = c("lidR", "link2GI", "mapview", "raster", "rgdal", "ForestTools",
         "sp", "corrplot", "RStoolbox", "glcm", "TileManager", "factoextra", "smoothie")
lapply(libs, require, character.only = TRUE)

# Set project specific subfolders
project_folders = c("data/",                                 # data folders
                    "data/aerial/org/", "data/lidar/org/", "data/lidar/org/extend/", "data/lidar/segtest/", "data/lidar/segtest/laubtest/", "data/lidar/segtest/nadeltest/",
                    "data/lidar/segtest/nadel_laub_test/", "data/lidar/org/corrected/", "data/lidar/norm/", 
                    "data/lidar/processed/", "data/lidar/processed/shannon/", "data/lidar/processed/zstats/", "data/lidar/processed/nreturns/", "data/lidar/processed/nreturns/level1/", "data/lidar/processed/nreturns/level2/",
                    "data/lidar/processed/nreturns/level3/", "data/lidar/processed/nreturns/level4/",
                    "data/lidar/processed/density/", "data/lidar/processed/density/level1/", "data/lidar/processed/density/level2/", "data/lidar/processed/density/level3/",
                    "data/lidar/processed/density/level4/", "data/lidar/processed/density/level5/", "data/lidar/processed/density/level6/", "data/lidar/processed/density/level7/",
                    "data/lidar/height/level1/", "data/lidar/height/level2/", "data/lidar/height/level3/",
                    "data/lidar/height/level4/", "data/lidar/height/level5/", "data/lidar/height/level6/", "data/lidar/height/level7/",
                    "data/lidar/height/level8/", "data/lidar/height/level9/",
                    "data/grass/", "data/training/", 
                    "data/mof/", "data/tmp/", "data/aerial_processed/", "data/aerial_processed/selection/", "data/validation/", "data/validation/valtif/", "data/validation/valshp/",
                    "data/validation/valres/",
                    "run/", "log/", "data/lidar/", "data/lidar/csf/", "data/plots/",                  # bins and logging
                    "mpg-envinfosys-teams-2018-rs_18_mcfest/src/",   # source code
                    "mpg-envinfosys-teams-2018-rs_18_mcfest/doc/",
                    "mpg-envinfosys-teams-2018-rs_18_mcfest/Val_Tree_pos_Group/")   # markdown etc. 

envrmt = link2GI::initProj(projRootDir = root_folder, GRASSlocation = "data/grass",
                           projFolders = project_folders, path_prefix = "path_", 
                           global = FALSE)

rasterOptions(tmpdir = envrmt$path_data_tmp)
source(paste0(envrmt$`path_mpg-envinfosys-teams-2018-rs_18_mcfest_src`, "002_functions.R"))
