######################################################## Title and Information ##################################################################
# This script contains all code used in Ethan Heidtman's University of Maryland Department of Geology Senior Thesis Project
# Script owned and written by Ethan Heidtman, created in the fall of 2023
# This project is investigating how changing snowpack patterns over the last 40 years in the western United States have influenced snowmelt hydrology

#### Table of Contents ###
# Functions and Packages
# Metadata and Map
# Read all SNOTEL Data
# Gather Streamflow Data
# Gather Air Temperature Data
# Filter the stations and HUC8s: Cross matching
# Calculate Pertinent Characteristics
# Some Preliminary Results
# Plotting

############################################################### Functions and Packages ################################################################

          # Where the SWE data are stored
          path1 <- '/Users/ethanheidtman/Library/CloudStorage/GoogleDrive-eheidtma@terpmail.umd.edu/My Drive/Senior Thesis/All_SWE_Data/'
          # Where the Streamflow Data are Stored
          path2 <- '/Users/ethanheidtman/Library/CloudStorage/GoogleDrive-eheidtma@terpmail.umd.edu/My Drive/Senior Thesis/Streamflow_Data/'
          # Where the Air Temperature Data are Stored
          path3 <- '/Users/ethanheidtman/Library/CloudStorage/GoogleDrive-eheidtma@terpmail.umd.edu/My Drive/Senior Thesis/AirTemp/'
          # Where the Precipitation Data are Stored
          path4 <- '/Users/ethanheidtman/Library/CloudStorage/GoogleDrive-eheidtma@terpmail.umd.edu/My Drive/Senior Thesis/Precip/'
          # Where any figures will be saved to
          path5 <- '/Users/ethanheidtman/Library/CloudStorage/GoogleDrive-eheidtma@terpmail.umd.edu/My Drive/Senior Thesis/Figures/'
          # Where the PDO index is stored
          path6 <- '/Users/ethanheidtman/Library/CloudStorage/GoogleDrive-eheidtma@terpmail.umd.edu/My Drive/Senior Thesis/Indices/'
          # Where the HUC2 Shapefiles are located
          path7 <- '/Users/ethanheidtman/Library/CloudStorage/GoogleDrive-eheidtma@terpmail.umd.edu/.shortcut-targets-by-id/1GX8mj4pqw1SoB9JTetKChQp2Y0mgEAss/Ethan_Heidtman_Summer_2023/HUC2_Shapefiles'
          # Where the snowmelt onset date file is located
          path8 <- '/Users/ethanheidtman/Library/CloudStorage/GoogleDrive-eheidtma@terpmail.umd.edu/My Drive/Senior Thesis/Picked_Data/'
          
          # Names of the HUC2 shapefiles
          shapes_path <- c('WBD_09_HU2_Shape/Shape/', 'WBD_10_HU2_Shape/Shape/', 'WBD_11_HU2_Shape/Shape/', 'WBD_13_HU2_Shape/Shape/', 'WBD_14_HU2_Shape/Shape/',
                           'WBD_15_HU2_Shape/Shape/', 'WBD_16_HU2_Shape/Shape/', 'WBD_17_HU2_Shape/Shape/', 'WBD_18_HU2_Shape/Shape/')
          
          setwd(path1)
          
          # Source our functions 
          source('~/Library/CloudStorage/GoogleDrive-eheidtma@terpmail.umd.edu/.shortcut-targets-by-id/1GX8mj4pqw1SoB9JTetKChQp2Y0mgEAss/Ethan_Heidtman_Summer_2023/SWE/Functions.R')
          
          # Load lots of packages 
          library(easypackages)
          libraries('tidyverse', 'sf', 'ggplot2', 'dplyr', 'lubridate', 'dataRetrieval', 'data.table', 'stringr', 'usmap', 'broom', 'ggthemes', 'viridis',
                    'maptools', 'maps', 'mapdata', 'anytime', 'reshape2', 'scales', 'ggpubr', 'ggbreak', 'gganimate', 'raster', 'rgdal', 'ncdf4',
                    'ggridges', 'grwat', 'WaveletComp', 'multispatialCCM', 'rEDM', 'trend', 'FlowScreen', 'zoo', 'changepoint', 'mcp', 'segmented',
                    'strucchange', 'ggpmisc', 'moments', 'Kendall', 'Rbeast', 'gridExtra', 'cowplot', 'ggtext', 'htmlTable')

################################################################ Metadata and Map #############################################################
          
          
          # Geologic info 
           # https://mrdata.usgs.gov/geology/state/map-us.html#home
          
          # Map of the US 
          us <- map_data('state')
          us <- subset(us, region %in% c("california", "oregon", 'nevada', 'colorado', 'washington', 'wyoming', 'idaho', 'new mexico', 'montana', 'texas',
                                         'arizona', 'north dakota', 'south dakota', 'nebraska', 'kansas', 'oklahoma'))
          
          # Read in the Full SNOTEL SWE Metadata: 897 Stations
          swe_meta <- read.csv('SNOTEL.csv', skip = 916)
          swe_meta$Elevation <- swe_meta$Elevation / 3.281 # Convert from feet to meters
          swe_meta$HUC8[465] <- '09040001' # Add an extra 0 to the front of this HUC8
          swe_meta <- swe_meta %>%
            mutate_at('Elevation', ~ round(., 2)) %>%
            dplyr::select(-c('EndDate', 'StartDate')) %>%
            mutate(HUC2 = substr(HUC8, 1, 2)) %>%
            relocate(HUC2, .after = HUC8)
          
          # Gather just the Alaskan Stations so they can be removed later
          alaska <- swe_meta[swe_meta$StateName == 'ALASKA', ] # Alaskan Stations 
          
          # Read in the HUC2 shapefiles
          for (i in 1:length(shapes_path)) {
            new_path <- paste(path7, shapes_path[i], sep = '/')
            setwd(new_path)
            if (i == 1) { huc9 <- st_read('WBDHU2.shp') }
            else if (i == 2) { huc10 <- st_read('WBDHU2.shp') }
            else if (i == 3) { huc11 <- st_read('WBDHU2.shp') }
            else if (i == 4) { huc13 <- st_read('WBDHU2.shp') }
            else if (i == 5) { huc14 <- st_read('WBDHU2.shp') }
            else if (i == 6) { huc15 <- st_read('WBDHU2.shp') }
            else if (i == 7) { huc16 <- st_read('WBDHU2.shp') }
            else if (i == 8) { huc17 <- st_read('WBDHU2.shp') }
            else if (i == 9) { huc18 <- st_read('WBDHU2.shp') }
            rm(new_path)
          }
          rm(i)
          labels <- data.frame(longitude = c(-97, -103, -99, -106, -109, -112, -116, -119, -119.5),
                               latitude = c(48, 44, 36, 34, 39.5, 33.5, 40, 47, 36),
                               label = c('09', '10', '11', '13', '14', '15', '16', '17', '18'))

############################ Read all SNOTEL Data ##############################
          # https://wcc.sc.egov.usda.gov/reportGenerator/
          # https://www.nrcs.usda.gov/wps/portal/wcc/home/aboutUs/monitoringPrograms/automatedSnowMonitoring/
          
          # Precision of SWE: +/- 0.1 inches, 2.54 millimeters  
          # Read all the SNOTEL Data
          setwd(path1)
          swe <- read_snotel(path1)
          colnames(swe)[2:length(swe)] <- gsub("[^0-9-]{3,4}", "", colnames(swe)[2:length(swe)]) # Make column names just the station ID
          colnames(swe)[2:length(swe)] <- str_extract(colnames(swe)[2:length(swe)], "[[:digit:]]+(?!.)") # Make column names just the station ID
          
          # Remove station 1317 from SWE so all columns match
          swe <- swe %>%
            dplyr::select(-c('1317'))
          
          # Work the data a bit for usability
          swe$Year <- year(ymd(swe$Date))
          swe$Month <- month(ymd(swe$Date))
          swe$Day <- day(ymd(swe$Date))
          swe <- swe %>%
            relocate(c('Year', 'Month', 'Day')) %>%
            relocate('Date') %>%
            mutate(Water_Year = wtr_yr(Date)) %>% # Calls the water year function
            relocate('Water_Year') %>% 
            mutate(across(c('301' : '1033'), ~ .x * 25.4)) %>% # Inches to millimeters
            mutate_if(is.numeric, round, digits = 1) %>% # Round to 2 sig figs
            mutate(Decade = Year - Year %% 10) %>%
            relocate(Decade, .after = Water_Year)
          swe$Decade[swe$Decade == 1970] <- 1980
          swe$Date <- as.Date(swe$Date)
          
          # Select only the stations that are in our collected metadata
          swe <- swe %>%
            dplyr::select(c(1:6, colnames(swe[colnames(swe)[7:length(swe)] %in% swe_meta$StationId])))
          
          # Clip the data to the correct time frame
          swe <- swe %>%
            filter(Date > '1979-09-30') %>%
            filter(Date < '2022-10-01')
          
          # Remove the alaskan stations from SWE
          swe <- swe %>%
            dplyr::select(-c(colnames(swe[colnames(swe) %in% alaska$StationId]))) 


######################### Gather Streamflow Data ###############################

          # Create list of HUC8s
          hucs <- unique(swe_meta$HUC8)
          bad_hucs <- vector(mode = 'list')
          
          # Write Streamflow files and create a streamflow metadata data frame (Only need to do once)
          # Utilizes the dataRetrieval package that actually fetches data from the USGS Data Dashboard
          # q_meta <- HUC_streamflow(unique_huc = hucs, bad_hucs = bad_hucs, startDate = '1979-10-01', endDate = '2022-12-31')
          # write.csv(q_meta, file.path(path2, 'q_meta.csv'))
          
          # Read Streamflow Data and Metadata
          q_meta <- read.csv('/Users/ethanheidtman/Library/CloudStorage/GoogleDrive-eheidtma@terpmail.umd.edu/My Drive/Senior Thesis/Streamflow_Data/q_meta.csv')
          q_meta$Elevation <- q_meta$Elevation / 3.281 # Convert from feet to meters 
          q_meta <- q_meta %>%
            dplyr::select(-1)  %>%
            mutate(HUC2 = substr(HUC, 1, 2)) %>%
            relocate(HUC2, .after = HUC)
          q_meta$HUC[214] <- '09040001' # Add a 0 to this HUC8 so it is 8 digits (a valid HUC)
          q_meta$Drainage <- q_meta$Drainage * (2.788*10^7) # Convert drainage from square miles to square feet
          
          
          # Read and tidy streamflow from their folder (written by the code lines 124-127)
          q <- read_streamflow(path = path2, file_pattern = '*_.csv')
          q$Year <- year(ymd(q$Date))
          q$Month <- month(ymd(q$Date))
          q$Day <- day(ymd(q$Date))
          q <- q %>%
            relocate(c('Year', 'Month', 'Day')) %>%
            relocate('Date') %>%
            mutate(Water_Year = wtr_yr(Date)) %>% # Calls the water year function
            relocate('Water_Year') %>%
            mutate(Decade = Year - Year %% 10) %>%
            relocate(Decade, .after = Water_Year) 

          q$Decade[q$Decade == 1970] <- 1980
          q$Date <- as.Date(q$Date)
          
          # Clip the data to the correct time frame
          q <- q %>%
            filter(Date > '1979-09-30') %>%
            filter(Date < '2022-10-01')
          
          rm(bad_hucs)

###################### Gather Air Temperature Data #############################
          setwd(path3)

          # Read each file that has the temperature data and bind them into one dataset
          one <- read.csv('80to90.csv', skip = 952)
          two <- read.csv('91to01.csv', skip = 952)
          three <- read.csv('02to11.csv', skip = 952)
          four <- read.csv('12to22.csv', skip = 952)
          temp <- rbind(one, two, three, four)
          rm(one, two, three, four)

          # Work with the column names so they just have the SNOTEL ID
          colnames(temp)[2:length(temp)] <- gsub("[^0-9-]{3,4}", "", colnames(temp)[2:length(temp)])
          colnames(temp)[2:length(temp)] <- str_extract(colnames(temp)[2:length(temp)], "[[:digit:]]+(?=s)")

          # Select just stations that are CONUS and in the selected metadata (i.e., minus Alaska and any stations that are excluded from one of the datasets)
          temp <- temp %>%
            dplyr::select(-c(colnames(temp[colnames(temp) %in% alaska$StationId])))
          temp <- temp %>%
            dplyr::select(-c(setdiff(colnames(temp)[2:length(temp)], swe_meta$StationId)))

          # Tidy and work with data for usefulness
          temp$Year <- year(ymd(temp$Date)) # Make year column
          temp$Month <- month(ymd(temp$Date)) # Make date column
          temp$Day <- day(ymd(temp$Date)) # Make day column
          temp <- temp %>%
            relocate(c('Year', 'Month', 'Day')) %>%
            relocate('Date') %>%
            mutate(Water_Year = wtr_yr(Date)) %>% # Calls the water year function
            relocate('Water_Year') %>%
            mutate(across(c('301' : '1033'), ~ ((.x - 32)*(5/9)))) %>% # Fahrenheit to celsius
            mutate_if(is.numeric, round, digits = 2) %>% # Round to 2 sig figs
            filter(Date > '1979-09-30') %>%
            filter(Date < '2022-10-01') %>%
            mutate(Decade = Year - Year %% 10) %>% # Make decade column
            relocate(Decade, .after = Water_Year)
          temp$Decade[temp$Decade == 1970] <- 1980
          temp$Date <- as.Date(temp$Date)

          # Remove station 1106 from all sets
          swe_meta <- swe_meta %>%
            filter(StationId != 1106)
          swe <- swe %>%
            dplyr::select(-c('1106'))
          temp <- temp %>%
            dplyr::select(-c('1106'))

          # Remove alaskan stations from metadata
          swe_meta <- swe_meta %>%
            filter(StateName != 'ALASKA')

          # Remove alaska dataframe to clean up the workspace
          rm(alaska)

################## Filter the stations and HUC8s: Crossmatching ################
          # # Select stations above 2000 meters to remove the impacts of coastal precipitation
          # swe_meta <- swe_meta %>%
          #   filter(Elevation > 2000)
          # swe <- swe %>%
          #   dplyr::select(c(1:6, which(colnames(swe) %in% swe_meta$StationId)))
          # temp <- temp %>%
          #   dplyr::select(c(1:6, which(colnames(temp) %in% swe_meta$StationId)))
          # 
          # # Update the streamflow sites to match the >2000m SNOTEl sites (although there will still be USGS gauges below 2000m)
          # q_meta <- q_meta %>%
          #   filter(HUC %in% unique(swe_meta$HUC8))
          # q <- q %>%
          #   dplyr::select(c(1:6, which(colnames(q) %in% q_meta$HUC)))
          # swe_meta <- swe_meta %>%
          #   filter(HUC8 %ni% setdiff(swe_meta$HUC8, q_meta$HUC))
          # swe <- swe %>%
          #   dplyr::select(c(1:6, which(colnames(swe) %in% swe_meta$StationId)))
          # temp <- temp %>%
          #   dplyr::select(c(1:6, which(colnames(temp) %in% swe_meta$StationId)))
          # 
          # # Filter for the stations that have most (14706/15706) of the SWE observations
          # # Leaves with 266 SNOTEL sites in 99 different HUC8s
          # nas <- swe %>%
          #   summarise(across(c('907' : '1033'),  ~ sum(is.na(.x)))) %>%
          #   dplyr::select_if(~ any(. < 1000))
          # swe <- swe %>%
          #   dplyr::select(c(1:6), colnames(nas))
          # swe_meta <- swe_meta %>%
          #   filter(StationId %in% colnames(swe))
          # q_meta <- q_meta %>%
          #   filter(HUC %in% unique(swe_meta$HUC8))
          # q <- q %>%
          #   dplyr::select(c(1:6), which(colnames(q) %in% q_meta$HUC))
          # 
          # # Select HUCs that have only 1000 or fewer missing observations and update the SNOTEL data to match
          # nas2 <- q %>%
          #   summarise(across(c('15040004' : '10030201'), ~sum(is.na(.x)))) %>% # '10030201'
          #   dplyr::select_if(~any(. < 1000))
          # q_meta <- q_meta %>%
          #   filter(HUC %in% colnames(nas2))
          # q <- q %>%
          #   dplyr::select(c(1:6), q_meta$HUC)
          # swe_meta <- swe_meta %>%
          #   filter(HUC8 %in% q_meta$HUC)
          # swe <- swe %>%
          #   dplyr::select(c(1:6, which(colnames(swe) %in% swe_meta$StationId)))
          # temp <- temp %>%
          #   dplyr::select(c(1:6, which(colnames(temp) %in% swe_meta$StationId)))
          # 
          # rm(nas, nas2)
          # 
          # # One USGS station does not have an elevation: retrieved manually
          # q_meta$Elevation[74] <- 2255 # 74
          # q_meta$Drainage <- q_meta$Drainage / 1.076e+7
          # q_meta <- q_meta %>%
          #   mutate(Elevation = round(Elevation, digits = 1),
          #          Drainage = round(Drainage, digits = 1))

          # Select stations above 2500 meters to remove the impacts of coastal precipitation
          swe_meta <- swe_meta %>%
            filter(Elevation > 2500)
          swe <- swe %>%
            dplyr::select(c(1:6, which(colnames(swe) %in% swe_meta$StationId)))
          temp <- temp %>%
            dplyr::select(c(1:6, which(colnames(temp) %in% swe_meta$StationId)))
          
          # Update the streamflow sites to match the >2000m SNOTEl sites (although there will still be USGS gauges below 2000m)
          q_meta <- q_meta %>%
            filter(HUC %in% unique(swe_meta$HUC8))
          q <- q %>%
            dplyr::select(c(1:6, which(colnames(q) %in% q_meta$HUC)))
          swe_meta <- swe_meta %>%
            filter(HUC8 %ni% setdiff(swe_meta$HUC8, q_meta$HUC))
          swe <- swe %>%
            dplyr::select(c(1:6, which(colnames(swe) %in% swe_meta$StationId)))
          temp <- temp %>%
            dplyr::select(c(1:6, which(colnames(temp) %in% swe_meta$StationId)))
          
          # Filter for the stations that have most (14706/15706) of the SWE observations
          # Leaves with 266 SNOTEL sites in 99 different HUC8s
          nas <- swe %>%
            summarise(across(c('907' : '1033'),  ~ sum(is.na(.x)))) %>%
            dplyr::select_if(~ any(. < 1000))
          swe <- swe %>%
            dplyr::select(c(1:6), colnames(nas))
          swe_meta <- swe_meta %>%
            filter(StationId %in% colnames(swe))
          q_meta <- q_meta %>%
            filter(HUC %in% unique(swe_meta$HUC8))
          q <- q %>%
            dplyr::select(c(1:6), which(colnames(q) %in% q_meta$HUC))
          
          # Select HUCs that have only 1000 or fewer missing observations and update the SNOTEL data to match
          nas2 <- q %>%
            summarise(across(c('15040004' : '17010201'), ~sum(is.na(.x)))) %>% # '10030201'
            dplyr::select_if(~any(. < 1000))
          q_meta <- q_meta %>%
            filter(HUC %in% colnames(nas2))
          q <- q %>%
            dplyr::select(c(1:6), q_meta$HUC)
          swe_meta <- swe_meta %>%
            filter(HUC8 %in% q_meta$HUC)
          swe <- swe %>%
            dplyr::select(c(1:6, which(colnames(swe) %in% swe_meta$StationId)))
          temp <- temp %>%
            dplyr::select(c(1:6, which(colnames(temp) %in% swe_meta$StationId)))
          
          rm(nas, nas2)
          
          # One USGS station does not have an elevation: retrieved manually
          q_meta$Elevation[52] <- 2255 # 74
          
          q_meta$Drainage <- q_meta$Drainage / 1.076e+7
          q_meta <- q_meta %>%
            mutate(Elevation = round(Elevation, digits = 1),
                   Drainage = round(Drainage, digits = 1))
          
          # Leaves with 207 SNOTEL sites in 99 different HUC8s
          # 121 SNOTEL sites in 69 different HUC8s
          
###################### Calculate Pertinent Characteristics #####################
          
          # 1. MAGNITUDE OF PEAK SWE (one column for each SNOTEL station)  2500 m: c('309' : '869'), 2000 m: c('307' : '877')
              peak_swe <- swe %>%
                group_by(Water_Year) %>%
                summarise(across(c('309' : '869'), ~max(., na.rm = TRUE))) # Collect the max SWE for each column for each water year
              peak_swe[peak_swe == '-Inf'] <- NA
          
          # 2. DATE OF PEAK SWE: # of Days from September 1 (One column for each SNOTEL station)
              date_peak_swe <- as.data.frame(peak_swe$Water_Year) # Initialize empty data frame
              colnames(date_peak_swe) <- 'Water_Year'
              for (i in 2:length(peak_swe)) { # For each SNOTEL station
                df <- swe %>%
                  dplyr::select(c(1,3, colnames(swe)[i + 5])) # Select SNOTEL station i
                local <- data.frame(matrix(ncol = 1, nrow = 0))
                colnames(local) <- colnames(peak_swe)[i]
                for (x in date_peak_swe$Water_Year) { # For each year 
                  data <- df %>%
                    filter(Water_Year == x) # Filter for water year x
                  index <- which.max(data[, 3]) # Find the index that is the max value of SWE
                  if (is_empty(index)) { # if there is no max index (i.e., this year is all NA obs for SWE)
                    date <- NA
                    local <- rbind(local, date)
                    next
                  }
                  if (max(data[, 3], na.rm = TRUE) == 0) { # If the max SWE is 0 for this year
                    date <- NA
                    local <- rbind(local, date)
                    next
                  }
                  date <- data$Date[index] # Collect the date at that index
                  local <- rbind(local, as.character(date)) # bind to local data frame
                  rm(data, date, index)
                }
                colnames(local) <- colnames(peak_swe)[i]
                local[, 1] <- as.Date(local[, 1]) # Make class date
                date_peak_swe <- cbind(date_peak_swe, as.data.frame(local)) # bind to product dataframe
                rm(df, local)
              }
   
          # 3. # of Days from April 1 until the date of Peak SWE (one column for each SNOTEL station)
              diff_april <- date_peak_swe %>%
                mutate_if(is.Date, as.character)
              for (i in 2:length(date_peak_swe)) {
                for (x in 1:length(date_peak_swe$Water_Year)) {
                  # Compute number of days that have elapsed since the start of the water year for each observation
                  ref <- as.Date(paste0(as.numeric(date_peak_swe$Water_Year[x]), '-04-01'))
                  diff <- as.integer(difftime(date_peak_swe[x, i], ref)) 
                  diff_april[x, i] <- diff
                  rm(ref, dif)
                }
              }
              
              # Change from date to # of days from start of water year
              date_peak_swe <- date_peak_swe %>%
                mutate_if(is.Date, as.character)
              for (i in 2:length(date_peak_swe)) {
                for (x in 1:length(date_peak_swe$Water_Year)) {
                  # Compute number of days that have elapsed since the start of the water year for each observation
                  ref <- as.Date(paste0(as.numeric(date_peak_swe$Water_Year[x]) - 1, '-10-01'))
                  diff <- as.integer(difftime(date_peak_swe[x, i], ref)) 
                  date_peak_swe[x, i] <- diff
                  rm(ref, dif)
                }
              }

          # 4. DATE OF SNOWMELT ONSET (one column for each HUC8)
              # Picked manually by eye and compiled for each water year in individual watersheds.
              # Dates of Onset were recorded in an Excel file to be read in here
              setwd(path8)
              onsets <- read.csv('Snowmelt_Onsets.csv', check.names = FALSE)
              onsets <- onsets %>%
                mutate_if(is.character, as.Date) # Make all columns class date
              
              onsets <- onsets %>%
                dplyr::select(c(1, which(colnames(onsets) %in% q_meta$HUC)))
              
          # 5. Pick the date and value of Peak Q, then create a separate df for peak Q
              date_peak_q <- as.data.frame(onsets$Water_Year) # Initialize empty data frame
              colnames(date_peak_q) <- 'Water_Year'
              for (i in 2:length(onsets)) { # For each SNOTEL station
                df <- q %>%
                  dplyr::select(c(1:6, colnames(onsets)[i])) %>% # Select USGS watershed i
                  rename(Q = 7) %>%
                  mutate(Roll = zoo::rollapply(Q, 3, mean, align = 'right', fill = NA))
                local <- data.frame(matrix(ncol = 2, nrow = 0))
                for (x in 1:length(onsets$Water_Year)) { # For each year 
                  onset_date <- as.Date(onsets[x,i]) # grab the computed date of snowmelt onset
                  if (is.na(onset_date)) {
                    date <- NA
                    val <- NA
                    new <- data.frame(date, val)
                    local <- rbind(local, new)
                    rm(date, val, new)
                    next
                  }
                    data <- df %>%
                      filter(Water_Year == onsets$Water_Year[x]) %>% # filter for the correct water year
                      filter(Date >= onset_date) # filter for after the computed snowmelt onset date
                    if (data$Month[1] < 4) { # If the snowmelt onset was in Feb/March
                      data <- data %>%
                        filter(Date < onset_date + 100) # look no later than 100 days after onset
                      date <- data$Date[which.max(data$Roll)] # grab the date of peak Q
                      val <- as.numeric(max(data$Roll)) # grab the value of peak Q
                      new <- data.frame(date, val) # create a little df to bind to local
                      local <- rbind(local, new)
                      rm(date, val, new)
                      next
                    }
                    if (data$Month[1] == 4) { # if the snowmelt onset was in april
                      data <- data %>%
                        filter(Date < onset_date + 90) # look no later than 90 days after onset
                      date <- data$Date[which.max(data$Roll)] # grab the date of peak Q
                      val <- as.numeric(max(data$Roll)) # grab the value of peak Q
                      new <- data.frame(date, val) # create a little df to bind to local
                      local <- rbind(local, new)
                      rm(date, val, new)
                      next
                    }
                    if (data$Month[1] > 4) { # if the snowmelt onset was in may or june
                      data <- data %>%
                        filter(Date < onset_date + 75) # look no later than 75 days after onset
                      date <- data$Date[which.max(data$Roll)] # grab the date of peak Q
                      val <- as.numeric(max(data$Roll)) # grab the value of peak Q
                      new <- data.frame(date, val) # create a little df to bind to local
                      local <- rbind(local, new)
                      rm(date, val, new)
                      next
                    }
                }
                colnames(local) <- c(colnames(onsets)[i], paste0(colnames(onsets)[i], '.X')) # Set the names of our columns
                local[,1] <- as.Date(local[,1]) # make the date pick class date
                local[,2] <- as.numeric(local[,2]) # make the value pick class numeric
                date_peak_q <- cbind(date_peak_q, as.data.frame(local)) # bind to final df
                rm(df, local, onset_date, data)
              }
              
              peak_q <- date_peak_q %>%
                dplyr::select(c(1, ends_with('.X'))) %>%
                rename_with(~str_remove(., '.X')) %>% # remove the .X suffix on all columns
                mutate_if(is.numeric, round, digits = 1) %>% # Round to the tenths place
                mutate(Water_Year = as.factor(Water_Year))
              
              date_peak_q <- date_peak_q %>%
                dplyr::select(-c(ends_with('.X')))
                
              # Change from date to # of days from start of water year
              onsets <- onsets %>%
                mutate_if(is.Date, as.character) # Make date columns class character 
              for (i in 2:length(onsets)) {
                for (x in 1:length(onsets$Water_Year)) {
                  # Compute number of days that have elapsed since the start of the water year for each observation
                  ref <- as.Date(paste0(as.numeric(onsets$Water_Year[x]) - 1, '-10-01')) # The start of the water year
                  diff <- as.integer(difftime(onsets[x, i], ref)) # The number of days
                  onsets[x, i] <- diff
                  rm(ref, dif)
                }
              }
              # Make the Onset dates numeric
              onsets <- onsets %>%
                mutate_if(is.character, as.numeric)
  
              # 9. April 1 SWE 
              april1 <- swe %>%
                filter(Month == 4 & Day == 1) 
              
              # Change from date to # of days from start of water year
              date_peak_q <- date_peak_q %>%
                mutate_if(is.Date, as.character)
              for (i in 2:length(date_peak_q)) {
                for (x in 1:length(date_peak_q$Water_Year)) {
                  # Compute number of days that have elapsed since the start of the water year for each observation
                  ref <- as.Date(paste0(as.numeric(date_peak_q$Water_Year[x]) - 1, '-10-01'))
                  diff <- as.integer(difftime(date_peak_q[x, i], ref)) 
                  date_peak_q[x, i] <- diff
                  rm(ref, dif)
                }
              }
              
################################################# Mann-Kendall Tests for Trend ####################################################
          
          # Peak SWE 
              MK_peak <- peak_swe %>%
                summarise(across(-1,  ~matrix(MannKendall(.)))) %>%
                as.data.frame() %>%
                mutate_all(as.numeric)
              rownames(MK_peak) <- c('tau', 'p', 'score', 'denominator', 'varianceS')
              MK_peak <- MK_peak %>%
                transpose(keep.names = 'StationId')
              colnames(MK_peak) <- c('StationId', 'tau', 'p', 'score', 'denominator', 'varianceS')
              MK_peak <- cbind(swe_meta, MK_peak[, 2:6])

          # Date Peak SWE
              MK_date_peak <- date_peak_swe %>%
                summarise(across(-1,  ~matrix(MannKendall(.)))) %>%
                as.data.frame() %>%
                mutate_all(as.numeric)
              rownames(MK_date_peak) <- c('tau', 'p', 'score', 'denominator', 'varianceS')
              MK_date_peak <- MK_date_peak %>%
                transpose(keep.names = 'StationId')
              colnames(MK_date_peak) <- c('StationId', 'tau', 'p', 'score', 'denominator', 'varianceS')
              MK_date_peak <- cbind(swe_meta, MK_date_peak[, 2:6])
              
          # Snowmelt Onset
              MK_onset <- onsets %>%
                summarise(across(-1,  ~matrix(MannKendall(.)))) %>%
                as.data.frame() %>%
                mutate_all(as.numeric)
              rownames(MK_onset) <- c('tau', 'p', 'score', 'denominator', 'varianceS')
              MK_onset <- MK_onset %>%
                transpose(keep.names = 'StationId')
              colnames(MK_onset) <- c('StationId', 'tau', 'p', 'score', 'denominator', 'varianceS')
              MK_onset <- cbind(q_meta, MK_onset[, 2:6])
              
          # April 1 SWE
              MK_april1 <- april1 %>%
                dplyr::select(-c(3:6)) %>%
                summarise(across(-c(1,2), ~ matrix(MannKendall(.)))) %>%
                as.data.frame() %>%
                mutate_all(as.numeric)
              rownames(MK_april1) <- c('tau', 'p', 'score', 'denominator', 'varianceS')
              MK_april1 <- MK_april1 %>%
                transpose(keep.names = 'StationId')
              colnames(MK_april1) <- c('StationId', 'tau', 'p', 'score', 'denominator', 'varianceS')
              MK_april1 <- cbind(swe_meta, MK_april1[, 2:6])

############################################ Mann-Kendall Maps/Plots ############################              
              
          # Peak SWE 
              station_map <- ggplot(us, aes(long, lat, group = group)) +
                geom_polygon(fill = 'white', color = 'grey50') +
                geom_sf(data = huc9, color  = '#E41A1C', fill = '#E41A1C', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc10, color  = '#377EB8', fill = '#377EB8', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc11, color  = '#4DAF4A', fill = '#4DAF4A', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc13, color  = '#984EA3', fill = '#984EA3', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc14, color  = '#FF7F00',fill = '#FF7F00',  alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc15, color  = '#FFFF33', fill = '#FFFF33', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc16, color  = '#A65628', fill = '#A65628', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc17, color  = '#F781BF', fill = '#F781BF', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc18, color = '#999999', fill = '#999999', alpha = 0.6, inherit.aes = FALSE) +
                coord_sf() +
                # geom_point(data = MK_peak %>% filter(p <= 0.05 & tau > 0), 
                #            mapping = aes(x = Longitude, y = Latitude, fill = 'blue'), 
                #            inherit.aes = FALSE, shape = 21, size = 4) + 
                geom_point(data = MK_peak %>% filter(p <= 0.05 & tau <= 0), 
                           mapping = aes(x = Longitude, y = Latitude, fill = 'red'), 
                           inherit.aes = FALSE, shape = 21, size = 4) +
                geom_point(data = MK_peak %>% filter(p > 0.05), 
                           mapping = aes(x = Longitude, y = Latitude, fill = 'grey'), 
                           inherit.aes = FALSE, shape = 21, size = 3) +
                labs(x = 'Longitude', y = 'Latitude', title = 'Mann-Kendall Test for Trend in Peak SWE Magnitude: 1980-2022', 
                     subtitle = '**Elevation Threshold: 2500 meters**',
                     caption = '**Figure 12** Non-Parametric Mann-Kendall Test for Trend of the Peak SWE Magnitudes at all 121<br>SNOTEL stations. Mann-Kendall Test performed at the 95% Confidence Level. Significance<br>determined when p<0.05. Blue dots represent stations that have a significant increasing trend,<br>red dots significant and decreasing. Grey dots represent stations with no significant trend.') +
                theme(plot.title = element_text(size = 20),
                      plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                      plot.subtitle = ggtext::element_markdown(size = 14),
                      legend.title = element_text(size = 14), 
                      legend.text = element_text(size = 11), 
                      legend.position = 'bottom', 
                      legend.key.size = unit(1, 'cm')) +
                geom_label(data = labels, aes(x = longitude, y = latitude, label = label), inherit.aes = FALSE) + 
                scale_fill_identity(name = 'Trend', guide = 'legend', 
                                    labels = c('No Significant Trend', 'Significant and Decreasing')) + 
                guides(fill = guide_legend(override.aes = list(size = 6, stroke = 0)))
              ggsave(filename = 'MK_Peak_2500.png', width = 10, height = 11, station_map, path = path5)
              
          # Date of Peak SWE
              station_map <- ggplot(us, aes(long, lat, group = group)) +
                geom_polygon(fill = 'white', color = 'grey50') +
                geom_sf(data = huc9, color  = '#E41A1C', fill = '#E41A1C', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc10, color  = '#377EB8', fill = '#377EB8', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc11, color  = '#4DAF4A', fill = '#4DAF4A', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc13, color  = '#984EA3', fill = '#984EA3', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc14, color  = '#FF7F00',fill = '#FF7F00',  alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc15, color  = '#FFFF33', fill = '#FFFF33', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc16, color  = '#A65628', fill = '#A65628', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc17, color  = '#F781BF', fill = '#F781BF', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc18, color = '#999999', fill = '#999999', alpha = 0.6, inherit.aes = FALSE) +
                coord_sf() +
                # geom_point(data = MK_date_peak %>% filter(p <= 0.05 & tau > 0), 
                #            mapping = aes(x = Longitude, y = Latitude, fill = 'blue'), 
                #            inherit.aes = FALSE, shape = 21, size = 4) + 
                geom_point(data = MK_date_peak %>% filter(p <= 0.05 & tau <= 0), 
                           mapping = aes(x = Longitude, y = Latitude, fill = 'red'), 
                           inherit.aes = FALSE, shape = 21, size = 4) +
                geom_point(data = MK_date_peak %>% filter(p > 0.05), 
                           mapping = aes(x = Longitude, y = Latitude, fill = 'grey'), 
                           inherit.aes = FALSE, shape = 21, size = 3) +
                labs(x = 'Longitude', y = 'Latitude', title = 'Mann-Kendall Test for Trend in Peak SWE Date: 1980-2022', 
                     caption = '**Figure BLANK** Non-Parametric Mann-Kendall Test for Trend of the Peak SWE Dates at all 121<br>SNOTEL stations. Mann-Kendall Test performed at the 95% Confidence Level. Significance<br>determined when p<0.05. Blue dots represent stations that have a significant increasing trend,<br>red dots significant and decreasing. Grey dots represent stations with no significant trend.') +
                theme(plot.title = element_text(size = 20),
                      plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                      legend.title = element_text(size = 14), 
                      legend.text = element_text(size = 11), 
                      legend.position = 'bottom', 
                      legend.key.size = unit(1, 'cm')) +
                geom_label(data = labels, aes(x = longitude, y = latitude, label = label), inherit.aes = FALSE) + 
                scale_fill_identity(name = 'Trend', guide = 'legend', 
                                    labels = c('No Significant Trend', 'Significant and Decreasing')) + 
                guides(fill = guide_legend(override.aes = list(size = 6, stroke = 0)))
              ggsave(filename = 'MK_date_peak.png', width = 11, height = 11, station_map, path = path5)
          
          # Snowmelt Onset
              station_map <- ggplot(us, aes(long, lat, group = group)) +
                geom_polygon(fill = 'white', color = 'grey50') +
                geom_sf(data = huc9, color  = '#E41A1C', fill = '#E41A1C', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc10, color  = '#377EB8', fill = '#377EB8', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc11, color  = '#4DAF4A', fill = '#4DAF4A', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc13, color  = '#984EA3', fill = '#984EA3', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc14, color  = '#FF7F00',fill = '#FF7F00',  alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc15, color  = '#FFFF33', fill = '#FFFF33', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc16, color  = '#A65628', fill = '#A65628', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc17, color  = '#F781BF', fill = '#F781BF', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc18, color = '#999999', fill = '#999999', alpha = 0.6, inherit.aes = FALSE) +
                coord_sf() +
                geom_point(data = MK_onset %>% filter(p <= 0.05 & tau > 0), 
                           mapping = aes(x = Longitude, y = Latitude, fill = 'blue'), 
                           inherit.aes = FALSE, shape = 21, size = 4) + 
                geom_point(data = MK_onset %>% filter(p <= 0.05 & tau <= 0), 
                           mapping = aes(x = Longitude, y = Latitude, fill = 'red'), 
                           inherit.aes = FALSE, shape = 21, size = 4) +
                geom_point(data = MK_onset %>% filter(p > 0.05), 
                           mapping = aes(x = Longitude, y = Latitude, fill = 'grey'), 
                           inherit.aes = FALSE, shape = 21, size = 3) +
                labs(x = 'Longitude', y = 'Latitude', title = 'Mann-Kendall Test for Trend in Snowmelt Onset Date: 1980-2022', 
                     caption = '**Figure BLANK** Non-Parametric Mann-Kendall Test for Trend of the Snowmelt Onset Dates at all 69<br>USGS Streamflow Gauges. Mann-Kendall Test performed at the 95% Confidence Interval. Significance<br>determined when p<0.05. Blue dots represent stations that have a significant increasing trend, red dots<br>significant and decreasing. Grey dots represent stations with no significant trend.') +
                theme(plot.title = element_text(size = 20),
                      plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                      legend.title = element_text(size = 14), 
                      legend.text = element_text(size = 11), 
                      legend.position = 'bottom', 
                      legend.key.size = unit(1, 'cm')) +
                geom_label(data = labels, aes(x = longitude, y = latitude, label = label), inherit.aes = FALSE) + 
                scale_fill_identity(name = 'Trend', guide = 'legend', 
                                    labels = c('Significant & Increasing', 'No Significant Trend', 'Significant and Decreasing')) + 
                guides(fill = guide_legend(override.aes = list(size = 6, stroke = 0)))
              ggsave(filename = 'MK_onset.png', width = 11, height = 11, station_map, path = path5)
              
          # April 1 SWE
              station_map <- ggplot(us, aes(long, lat, group = group)) +
                geom_polygon(fill = 'white', color = 'grey50') +
                geom_sf(data = huc9, color  = '#E41A1C', fill = '#E41A1C', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc10, color  = '#377EB8', fill = '#377EB8', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc11, color  = '#4DAF4A', fill = '#4DAF4A', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc13, color  = '#984EA3', fill = '#984EA3', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc14, color  = '#FF7F00',fill = '#FF7F00',  alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc15, color  = '#FFFF33', fill = '#FFFF33', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc16, color  = '#A65628', fill = '#A65628', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc17, color  = '#F781BF', fill = '#F781BF', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc18, color = '#999999', fill = '#999999', alpha = 0.6, inherit.aes = FALSE) +
                coord_sf() +
                # geom_point(data = MK_april1 %>% filter(p <= 0.05 & tau > 0), 
                #            mapping = aes(x = Longitude, y = Latitude, fill = 'blue'), 
                #            inherit.aes = FALSE, shape = 21, size = 4) + 
                geom_point(data = MK_april1 %>% filter(p <= 0.05 & tau <= 0), 
                           mapping = aes(x = Longitude, y = Latitude, fill = 'red'), 
                           inherit.aes = FALSE, shape = 21, size = 4) +
                geom_point(data = MK_april1 %>% filter(p > 0.05), 
                           mapping = aes(x = Longitude, y = Latitude, fill = 'grey'), 
                           inherit.aes = FALSE, shape = 21, size = 3) +
                labs(x = 'Longitude', y = 'Latitude', title = 'Mann-Kendall Test for Trend in April 1 SWE: 1980-2022', 
                     subtitle = '**Elevation Threshold: 2500 meters**',
                     caption = '**Figure 13** Non-Parametric Mann-Kendall Test for Trend of the Peak SWE Magnitudes at all 121<br>SNOTEL stations. Mann-Kendall Test performed at the 95% Confidence Level. Significance<br>determined when p<0.05. Blue dots represent stations that have a significant increasing trend,<br>red dots significant and decreasing. Grey dots represent stations with no significant trend.') +
                theme(plot.title = element_text(size = 20),
                      plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                      plot.subtitle = ggtext::element_markdown(size = 14),
                      legend.title = element_text(size = 14), 
                      legend.text = element_text(size = 11), 
                      legend.position = 'bottom', 
                      legend.key.size = unit(1, 'cm')) +
                geom_label(data = labels, aes(x = longitude, y = latitude, label = label), inherit.aes = FALSE) + 
                scale_fill_identity(name = 'Trend', guide = 'legend', 
                                    labels = c('No Significant Trend', 'Significant and Decreasing')) + 
                guides(fill = guide_legend(override.aes = list(size = 6, stroke = 0)))
              ggsave(filename = 'april1_2500.png', width = 10, height = 11, station_map, path = path5)
              
#################################### Create Massive Dataframe with All Data for Easy Plotting #######################
              
          # Pivot each df longer, and bind. Have HUC8s, HUC2s, Elevation, Lat/Lon, Station Id
              
              
          # Peak SWE
              peak_swe <- peak_swe %>%
                mutate(Decade = Water_Year - Water_Year %% 10) %>%
                relocate(Decade, .after = Water_Year)
              peak_swe$Decade[41:43] <- 2010
              peak_swe <- peak_swe %>%
                mutate(Decade = as.factor(Decade)) %>%
                pivot_longer(-c(Water_Year, Decade), names_to = 'Station', values_to = 'PeakSWE')
              
          # Date of Peak SWE
              date_peak_swe <- date_peak_swe %>%
                mutate(Decade = Water_Year - Water_Year %% 10) %>%
                relocate(Decade, .after = Water_Year)
              date_peak_swe$Decade[41:43] <- 2010
              date_peak_swe <- date_peak_swe %>%
                mutate(Decade = as.factor(Decade)) %>%
                pivot_longer(-c(Water_Year, Decade), names_to = 'Station', values_to = 'DatePeakSWE')
              
          # Date of Peak SWE compared to April 1
              diff_april <- diff_april %>%
                mutate(Decade = Water_Year - Water_Year %% 10) %>%
                relocate(Decade, .after = Water_Year)
              diff_april$Decade[41:43] <- 2010
              
              diff_april <- diff_april %>%
                pivot_longer(-c(Water_Year, Decade), names_to = 'Station', values_to = 'DiffApril1')
              diff_april$DiffApril1 <- as.numeric(diff_april$DiffApril1)
              diff_april$Decade <- as.factor(diff_april$Decade)
              
          # Date of Snowmelt Onset
              # create a df that matches snowmelt onsets to the SNOTEL stations in that huc
              df <- data.frame(matrix(nrow = 5203, ncol = 1)) # 8901 when using the threshold of 2000m
              colnames(df) <- 'Onset'
              for (i in 1:length(peak_swe$Station)) {
                station <- peak_swe$Station[i]
                index <- which(swe_meta$StationId == station) # index for the station in question 
                huc <- swe_meta$HUC8[index] # the huc that corresponds to that station
                col <- which(colnames(onsets) == huc) # the column in our onsets df that is for our huc 
                year <- peak_swe$Water_Year[i] # the year for that row
                val <- as.numeric(onsets[onsets$Water_Year == year, col]) # 
                df[i, 1] <- val # assign the value
                rm(station, index, huc, col, year, val)
              }
              onsets <- cbind(peak_swe[,1:3], df)
              rm(df)
              
          # Date of Peak Q
              df <- data.frame(matrix(nrow = 5203, ncol = 1)) # 8729, 5203
              colnames(df) <- 'DatePeakQ'
              for (i in 1:length(peak_swe$Station)) {
                station <- peak_swe$Station[i]
                index <- which(swe_meta$StationId == station) # index for the station in question 
                huc <- swe_meta$HUC8[index] # the huc that corresponds to that station
                col <- which(colnames(date_peak_q) == huc) # the column in our onsets df that is for our huc 
                year <- peak_swe$Water_Year[i] # the year for that row
                val <- as.numeric(date_peak_q[date_peak_q$Water_Year == year, col]) # 
                df[i, 1] <- val # assign the value
                rm(station, index, huc, col, year, val)
              }
              date_peak_q <- cbind(peak_swe[,1:3], df)
              rm(df)
              
          # Peak Q
              df <- data.frame(matrix(nrow = 5203, ncol = 1)) # 8901
              colnames(df) <- 'Peak_Q'
              for (i in 1:length(peak_swe$Station)) {
                station <- peak_swe$Station[i]
                index <- which(swe_meta$StationId == station) # index for the station in question 
                huc <- swe_meta$HUC8[index] # the huc that corresponds to that station
                col <- which(colnames(peak_q) == huc) # the column in our peak q df that is for our huc 
                year <- peak_swe$Water_Year[i] # the year for that row
                val <- as.numeric(peak_q[peak_q$Water_Year == year, col]) # 
                df[i, 1] <- val # assign the value
                rm(station, index, huc, col, year, x, val)
              }
              peak_q <- cbind(peak_swe[,1:3], df)
              rm(df)
              
          # All Metadata (SWE and Q)
              data <- data.frame(matrix(nrow = 5203, ncol = 6)) # 8901, 5203
              colnames(data) <- c('Longitude', 'Latitude', 'Elevation', 'HUC8', 'HUC2', 'Drainage')
              for (i in 1:length(peak_swe$Station)) {
                station <- peak_swe$Station[i]
                index <- which(swe_meta$StationId == station) # index for the station in question 
                  huc8 <- swe_meta$HUC8[index] # the huc that corresponds to that station
                  huc2 <- swe_meta$HUC2[index] # the HUC2 that corresponds to that station 
                  elev <- swe_meta$Elevation[index] # the elevation that corresponds to that station
                  lon <- swe_meta$Longitude[index]
                  lat <- swe_meta$Latitude[index]
                huc_index <- which(q_meta$HUC == huc8) # the index of our huc in the streamflow metadata df
                drainage <- q_meta$Drainage[huc_index]
                data[i, 1] <- lon
                data[i, 2] <- lat
                data[i, 3] <- elev
                data[i, 4] <- huc8 
                data[i, 5] <- huc2
                data[i, 6] <- drainage
                rm(station, index, huc8, huc2, elev, lon, lat, huc_index, drainage)
              }
              
              # DrainageMiles = Drainage / (2.788*10^7),
              # DrainageKm = DrainageMiles / (2.59)
              
          # Join them all together
              master <- cbind(peak_swe, data)
              master <- master %>%
                relocate(c(5:10), .after = Station)
              master <- cbind(master, date_peak_swe$DatePeakSWE, diff_april$DiffApril1, onsets$Onset, date_peak_q$DatePeakQ, peak_q$Peak_Q)
              colnames(master) <- c('Water_Year', 'Decade', 'Station', 'Longitude', 'Latitude', 'Elevation', 'HUC8', 'HUC2', 'Drainage',
                                    'PeakSWE', 'DatePeakSWE', 'DiffApril1', 'Onset', 'DatePeakQ', 'PeakQ')
              master <- master %>%
                mutate(Water_Year = as.factor(Water_Year),
                       Station = as.factor(Station),
                       HUC8 = as.factor(HUC8),
                       HUC2 = as.factor(HUC2),
                       DatePeakSWE = as.numeric(DatePeakSWE)) %>% # Change the classes of the variables appropriately
                mutate(RiseTime = DatePeakQ - Onset,
                       MeltLag = Onset - DatePeakSWE,
                       PeakSWEtoPeakQ = DatePeakQ - DatePeakSWE,
                       PeakQ = PeakQ / 35.315,
                       DrainageMiles = Drainage / (2.788*10^7),
                       DrainageKm = Drainage / (1.076e+7)) %>% # Compute more characteristics and metadata 2.59
                relocate(DrainageMiles, .after = Drainage) %>%
                relocate(DrainageKm, .after = DrainageMiles) %>%
                mutate(DrainageKm = round(DrainageKm, digits = 1)) %>%
                mutate(NormPeakQ = (PeakQ) / DrainageKm) %>% # normalized peak streamflow, cubic meters/sec
                mutate(NormPeakQ = round(NormPeakQ, digits = 2)) %>%
                mutate(DrainageBinned = findInterval(DrainageKm, c(0, 500, 1000, 5000, 10000, 20000, 70000))) %>% #c(0, 100, 200, 500, 1000, 5000, 10000)
                relocate(DrainageBinned, .after = Drainage)  %>% # Bin the HUC8 drainage areas
                mutate(DrainageBinned = factor(DrainageBinned, labels = c('<500km^2', '500-1000km^2', '1000-5000km^2', '5000-10000km^2', '10000-20000km^2', '20000-70000km^2')))
              # c('<100km^2', '100-200km^2', '200-500km^2', '500-1000km^2', '1000-5000km^2', '5000-10000km^2')
              
              # Peak Q vs Peak SWE
              plot <- ggplot(master[!(is.na(master$DrainageBinned)),], aes(x = PeakSWE, y = PeakQ, color = Latitude)) + 
                geom_point(na.rm = TRUE, alpha = 0.7) +
                # stat_poly_line(data = subset(master[!(is.na(master$DrainageBinned)),], Latitude > 42.5), method = 'lm', se = FALSE, color = 'blue') +
                # stat_poly_eq(label.y.npc = 0.9,use_label(c("R2")), color = 'blue') +
                # stat_poly_line(data = subset(master[!(is.na(master$DrainageBinned)),], Latitude < 42.5), method = 'lm', se = FALSE, color = 'red') +
                # stat_poly_eq(label.y.npc = 0.85, use_label(c("R2")), color = 'red') +
                facet_wrap(~DrainageBinned, drop = TRUE, scales = 'free') +
                scale_color_viridis() +
                labs(x = 'Peak SWE (mm)', y = 'Peak Mean Daily Discharge (m^3/s)', title = 'Peak Q vs Peak SWE: 1980-2022',
                     caption = '**Figure 16** The peak Q value for each of the 121 SNOTEL stations vs the magnitude of peak SWE in each year.<br>The dots are colored by their latitude, with brighter colors representing more northerly stations. Each panel<br>represents a particular drainage area, with the y-axis corresponding to the peak Q value measured in<br>cubic meters per second.',
                     color = 'Latitude (°N)') +
                theme_clean() + 
                theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
                      plot.title = element_text(size = 14),
                      strip.text = element_text(size = 12),
                      plot.caption = ggtext::element_markdown(size = 15, hjust = 0),
                      legend.title = element_text(size = 11),
                      legend.text = element_text(size = 11),
                      legend.position = 'right',
                      axis.title = element_text(size = 16),
                      axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1),
                      axis.text.y = element_text(size = 14))
              ggsave(filename = 'PeakQvsPeakSWE.png', width = 12, height = 10, plot, path = path5)
              
              
              options(scipen=999)
              plot <- ggplot(master[!(is.na(master$DrainageBinned)),], aes(x = DrainageBinned, y = PeakQ, fill = DrainageBinned)) + 
                geom_boxplot(na.rm = TRUE, alpha = 0.7) +
                labs(x = 'Drainage Basin Area (km^2)', y = 'Peak Mean Daily Discharge (m^3/s)', title = 'Peak Q vs Basin Area: 1980-2022',
                     caption = '**Figure 17** The peak Q value for each of the 121 SNOTEL stations vs the basin area of each HUC8.<br>Values are plotted on a logarithmic scale') + 
                scale_y_continuous(trans = 'log10') + 
                theme_clean() + 
                theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
                      strip.text = element_text(size = 12),
                      plot.title = element_text(size = 14),
                      plot.caption = ggtext::element_markdown(size = 15, hjust = 0),
                      legend.title = element_text(size = 11),
                      legend.text = element_text(size = 11),
                      legend.position = 'none',
                      axis.title = element_text(size = 16),
                      axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1),
                      axis.text.y = element_text(size = 14))
              ggsave(filename = 'PeakQBoxplot.png', width = 12, height = 10, plot, path = path5)
              
              
             bl <- swe %>%
               dplyr::select(1:6, '353') %>%
               mutate(Roll = zoo::rollapply(`353`, 3, mean, align = 'right', fill = NA)) %>%
               filter(Water_Year == 2020)
             bl <- q %>%
               dplyr::select(1:6, '14040101') %>%
               mutate(Roll = zoo::rollapply(`14040101`, 3, mean, align = 'right', fill = NA)) %>%
               filter(Water_Year == 2020)
             
             
             plot <- ggplot(bl, aes(x = Date, y = Roll)) + 
               geom_line(color = 'green') + 
               labs(x = 'Date', y = 'Mean Daily Discharge (cubic ft/sec)', title = 'Example Streamflow Hydrograph: HUC 14040101, Water Year 2020',
                    caption = '**Figure 7** A sample streamflow hydrograph for the outflow USGS gauge in HUC 14040101, located at 43.01908 °N,<br>-110.1189 °W, for water year 2020. The vertical black line represents the date of snowmelt onset, April 20, 2020.<br>The vertical red line represents the date of peak mean daily discharge, June 4 2020.') +
               geom_vline(aes(xintercept = as.Date('2020-04-20'))) +
               geom_vline(aes(xintercept = as.Date('2020-06-04')), color = 'red') +
               scale_x_date(breaks = '3 weeks') +
               theme_clean() + 
               theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
                     plot.title = element_text(size = 14),
                     plot.caption = ggtext::element_markdown(size = 15, hjust = 0),
                     legend.title = element_text(size = 11),
                     legend.text = element_text(size = 11),
                     legend.position = 'right',
                     axis.title = element_text(size = 16),
                     axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1),
                     axis.text.y = element_text(size = 14))  
             ggsave(filename = 'sample_streamflow_hydrograph.png', width = 12, height = 7, plot, path = path5)
               
              
              
              
################################### Snowpack Characteristic Plotting #############################              
              
             # Peak SWE Magnitude vs Date of Peak SWE
             # Facet by Drainage, color by Latitude
             plot <- ggplot(master[!(is.na(master$DrainageBinned)),], aes(x = DatePeakSWE, y = PeakSWE, color = Latitude)) + 
               geom_point(na.rm = TRUE, alpha = 0.7) +
               facet_wrap(~DrainageBinned, drop = TRUE) +
               geom_vline(xintercept = 250, color = 'lightblue') +
               scale_color_viridis() +
               labs(x = 'Date of Peak SWE (# of Days from October 1)', y = 'Magnitude of Peak SWE (mm)', title = 'Peak SWE Magnitude vs Date of Peak SWE: 1980-2022',
                    caption = '**Figure 14** The magnitude of peak SWE for each of the 121 SNOTEL stations vs the date of peak SWE in each year.<br>The dots are colored by their latitude, with brighter colors representing more northerly stations. Each panel represents<br>a different range of HUC8 Basin size (square kilometers). The light-blue line represents June 8th.',
                    color = 'Latitude (°N)') +
               theme_clean() + 
               theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
                     strip.text = element_text(size = 12),
                     plot.title = element_text(size = 14),
                     plot.caption = ggtext::element_markdown(size = 15, hjust = 0),
                     legend.title = element_text(size = 11),
                     legend.text = element_text(size = 11),
                     legend.position = 'bottom',
                     axis.title = element_text(size = 16),
                     axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1),
                     axis.text.y = element_text(size = 14))
             ggsave(filename = 'PeakSWEvsDatePeakSWE.png', width = 12, height = 10, plot, path = path5)
             
             # Date of Snowmelt Onset Density Distribution
             plot <- ggplot(master[!(is.na(master$DrainageBinned)),], aes(x = DrainageBinned, y = Onset, fill = DrainageBinned)) + 
               geom_boxplot(na.rm = TRUE) +
               labs(x = 'HUC8 Drainage Area ', y = 'Date of Snowmelt Onset (# of Days from October 1)', title = 'Date of Snowmelt Onset by Drainage Area: 1980-2022',
                    caption = '**Figure 15** The date of snowmelt onset  for each of the 121 SNOTEL stations (69 watersheds) vs the basin drainage<br>area. On the y-axis, 200 days from October 1 corresponds to April 19. From left to right, the number of SNOTEL<br>stations in each bin is: 24, 11, 47, 19, 10, 9.') +
               theme_clean() + 
               theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
                     strip.text = element_text(size = 12),
                     plot.title = element_text(size = 14),
                     plot.caption = ggtext::element_markdown(size = 15, hjust = 0),
                     legend.title = element_text(size = 11),
                     legend.text = element_text(size = 11),
                     legend.position = 'none',
                     axis.title = element_text(size = 16),
                     axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1),
                     axis.text.y = element_text(size = 14))
             ggsave(filename = 'OnsetBoxplot.png', width = 12, height = 10, plot, path = path5)
             
############################################# Other Plots #################################################
              
        
                
          
        
            
            
            blah <- data.frame(Decade = c('1980', '1990', '2000', '2010'),
                               NDays = 0.02,
                               label = c('Mean = 10.846\nStd. Dev. = 23.734\nSkew = -0.418\nKurtosis = 6.703', 
                                         'Mean = 14.724\nStd. Dev. = 23.033\nSkew = -0.795\nKurtosis = 4.147',
                                         'Mean = 6.818\nStd. Dev. = 22.264\nSkew = -0.661\nKurtosis = 4.166',
                                         'Mean = 4.864\nStd. Dev. = 26.800\nSkew = -0.723\nKurtosis = 4.804'))
            
            
            # blah_2000 <- data.frame(Decade = c('1980', '1990', '2000', '2010'),
            #                    NDays = 0.02,
            #                    label = c('Mean = 7.134\nStd. Dev. = 24.269\nSkew = -0.666\nKurtosis = 6.529', 
            #                              'Mean = 9.491\nStd. Dev. = 23.724\nSkew = -0.460\nKurtosis = 3.104',
            #                              'Mean = 3.602\nStd. Dev. = 22.674\nSkew = -0.518\nKurtosis = 4.047',
            #                              'Mean = 1.479\nStd. Dev. = 27.132\nSkew = -0.799\nKurtosis = 4.695'))
            
            
            
            plot <- ggplot(data = master, aes(x = DiffApril1, fill = Decade, group = Decade)) + # DiffApril1
              geom_density(na.rm = TRUE) + 
              scale_color_identity(name = 'Decade', guide = 'legend', 
                                   labels = c('1980s', '1990s', '2000s', '2010s')) +
              facet_wrap(~Decade) +
              scale_x_continuous(breaks = c(-125, -100, -75, -50, -25, 0, 25, 50, 75, 100, 125), limits = c(-125, 125)) +
              guides(color = guide_legend(override.aes = list(alpha = 1, stroke = 0, shape = NA))) + 
              geom_vline(aes(xintercept = 0), color = 'red') + 
              theme_clean() + 
              labs(x = 'Number of Days from April 1', y = 'Density', title = 'Density Distribution of Peak SWE Date by Decade',
                   subtitle = '**Elevation Threshold: 2500 meters**',
                   caption = '**Figure 11** Density Distribution of the date of peak SWE, where 0 is April 1, at all 121 SNOTEL stations.<br>Results are separated by decade, where the 1980s ranges from 10-01-1979 through 09-30-1990, and<br>2021 + 2022 are included in the 2010s. The vertical red line represents April 1, and the sample mean for<br>all stations in all years is 8.978 days later than April 1.') + # 5.122
              theme(plot.title = element_text(size = 20),
                    plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                    plot.subtitle = ggtext::element_markdown(size = 14),
                    legend.title = element_text(size = 14), 
                    legend.text = element_text(size = 11), 
                    legend.position = 'bottom', 
                    legend.key.size = unit(1, 'cm'), 
                    axis.title = element_text(size = 18),
                    axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1), 
                    axis.text.y = element_text(size = 14), 
                    strip.text.x = element_text(size = 14)) +
              geom_text(data = blah, mapping = aes(x = 90, y = 0.015, label = label))
            ggsave(filename = 'density_peak_date_2500.png', width = 10, height = 9.5, plot, path = path5)
            
            
            
            
            # 5. Peak SWE Magnitude Statistics
            
            
            
            ggplot(data = master, aes(x = PeakSWE, fill = Decade, group = Decade)) +
              geom_density(na.rm = TRUE) + 
              scale_color_identity(name = 'Decade', guide = 'legend', 
                                   labels = c('1980s', '1990s', '2000s', '2010s')) +
              facet_wrap(~Decade) +
              # scale_x_continuous(breaks = c(-125, -100, -75, -50, -25, 0, 25, 50, 75, 100, 125), limits = c(-125, 150)) +
              guides(color = guide_legend(override.aes = list(alpha = 1, stroke = 0, shape = NA))) + 
              geom_vline(aes(xintercept = mean(PeakSWE, na.rm = TRUE)), color = 'red') + 
              labs(x = 'Snow Water Equivalent (mm)', y = 'Density', title = 'Density Distribution of Peak SWE Magnitude',
                   caption = 'Density Distribution of the magnitude of peak SWE at all 207 SNOTEL stations.<br>Results are separated by decade, where the 1980s ranges from 10-01-1979 through 09-30-1990, and<br>2021 + 2022 are included in the 2010s. The vertical red line represents the sample mean for all stations<br>in all years, with an average magnitude of 489.694 mm') +
              theme_clean() + 
              theme(plot.title = element_text(size = 20),
                    plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                    legend.title = element_text(size = 14), 
                    legend.text = element_text(size = 11), 
                    legend.position = 'bottom', 
                    legend.key.size = unit(1, 'cm'), 
                    axis.title = element_text(size = 18),
                    axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1), 
                    axis.text.y = element_text(size = 14), 
                    strip.text.x = element_text(size = 14)) +
              geom_text(data = blah, mapping = aes(x = 2000, y = 0.0015, label = label))
            
            # 6. Peak to Snowmelt onset statistics
 
            blah <- data.frame(Decade = c('1980', '1990', '2000', '2010'),
                               NDays = 0.015,
                               label = c('Mean = 5.061<br>Skew = -0.019<br>Kurtosis = 5.411', 
                                         'Mean = 6.477<br>Skew = 0.470<br>Kurtosis = 6.911',
                                         'Mean = 10.950<br>Skew = 0.936<br>Kurtosis = 6.561',
                                         'Mean = 11.900<br>Skew = 1.154<br>Kurtosis = 7.859'))
            
           
            
           plot <- ggplot(master, aes(x = PeakSWE, y = PeakQ, color = HUC2)) +
             geom_point(na.rm = TRUE, alpha = 0.5) +
             scale_y_continuous(trans = 'log10') +
             scale_x_continuous(trans = 'log10') +
             geom_abline() +
             labs(x = 'Magnitude of Peak SWE (mm)', y = 'Peak Streamflow per HUC8 Area (ft/sec)', title = 'Peak Streamflow vs Peak Snow Water Equivalent', caption = 'Plot of peak discharge per basin area versus the peak snow water equivalent for that year. Here, basin area is the<br>area of the HUC8 that the USGS streamflow gauge corresponds to. Both the x and y axis are on a log10<br>scale, and the colors correspond to the large USGS HUC2 watersheds.') + 
             theme_clean() + 
             theme(plot.title = element_text(size = 20),
                   plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                   legend.title = element_text(size = 14), 
                   legend.text = element_text(size = 11), 
                   legend.position = 'bottom', 
                   legend.key.size = unit(1, 'cm'), 
                   axis.title = element_text(size = 18),
                   axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1), 
                   axis.text.y = element_text(size = 14), 
                   strip.text.x = element_text(size = 14)) +
             guides(color = guide_legend(override.aes = list(size = 3, alpha = 1, stroke = 1, shape = 'circle'))) 
           ggsave(filename = 'peakQ_vs_peakSWE.png', width = 11, height = 8.5, plot, path = path5)

################################################# SNOTEL 363 Box Canyon ########################################

        # 1. PLOT SWE WITH NUMBER OF DAYS BETWEEN PEAK SWE AND DATE OF SNOWMELT ONSET
              test <- peak_swe[, c('Water_Year', '363')] # Select our station, SNOTEL 363 at Box Canyon, Montana
              test <- cbind(test, peak_to_onset[colnames(peak_to_onset) == '363' ])
              colnames(test) <- c('Water_Year', 'SWE', 'Duration')
              test <- test %>%
                mutate(across(c('SWE'), ~./10)) %>%# Convert to centimeters so the two y axes line up nicely in the plot
                mutate(Decade = Water_Year - Water_Year %% 10) %>%
                relocate(Decade, .after = Water_Year)
              test$Decade[test$Decade == 1970] <- 1980
              
              plot <- ggplot(test) + 
                geom_bar(mapping = aes(x = Water_Year, y = Duration), stat = 'identity', na.rm = TRUE, fill = 'pink') + 
                geom_line(mapping = aes(x = Water_Year, y = SWE), stat = 'identity', na.rm = TRUE, color = 'blue', linewidth = 2) + 
                scale_y_continuous(name = 'Peak SWE (centimeters)', 
                                   sec.axis = sec_axis(~., name = 'Days from Peak SWE to Snowmelt Onset')) + 
                labs(x = 'Water Year', title = 'Number of Days from Peak SWE to Snowmelt Onset, Peak SWE Magnitude: 1980-2022',
                     caption = 'Figure 10: SNOTEL 363, Box Canyon, Montana. Pink bars are the number of days that elapsed each year from the date of the peak SWE <br>to the date of snowmelt onset. Blue line is the magnitude, in centimeters, of peak SWE for each year. <br>Time series comprises each water year from 1980 through 2022.') + 
                theme(plot.title = element_text(size = 20),
                      plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                      legend.title = element_text(size = 14), 
                      legend.text = element_text(size = 11), 
                      legend.position = 'right', 
                      legend.key.size = unit(1, 'cm'), 
                      axis.title = element_text(size = 18),
                      axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1), 
                      axis.text.y = element_text(size = 14),
                      axis.line.y.right = element_line(color = "pink", size = 2), 
                      axis.ticks.y.right = element_line(color = "pink"),
                      axis.line.y.left = element_line(color = "blue", size = 2), 
                      axis.ticks.y.left = element_line(color = "blue"))
              ggsave(filename = 'Box_Canyon_bars_and_line.png', width = 13, height = 8.5, plot, path = path5)
              
              
        # 2: PLOT PEAK SWE VS DAYS BETWEEN PEAK SWE AND DATE OF SNOWMELT ONSET
              plot <- ggplot(test, aes(x = SWE, y = Duration)) + 
                stat_poly_line(se = FALSE) +
                stat_poly_eq(use_label(c("eq", "R2")), label.x = 'right') +
                geom_point(aes(color = as.factor(Decade)), na.rm = TRUE, size = 3) + 
                labs(x = 'Peak Snow Water Equivalent (cm)', y = '# of Days from Peak SWE to Snowmelt Onset',
                     title = 'Duration from Peak SWE to Snowmelt vs Magnitude of Peak SWE: SNOTEL 363', 
                     color = 'Decade',
                     caption = 'Figure 13: Box Canyon, Montana. Duration (in days) between the date of peak SWE and the date of snowmelt onset vs the magnitude of <br>peak SWE. The dots are colored by the decade they represent (each represents a single year), but the linear regression is for the whole <br>dataset. Note that the r-squared of 0.14 suggests a weak correlation between the duration and the peak SWE.') +
                theme_clean() + 
                theme(plot.title = element_text(size = 20),
                      plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                      legend.title = element_text(size = 14), 
                      legend.text = element_text(size = 11), 
                      legend.position = 'bottom', 
                      legend.key.size = unit(1, 'cm'), 
                      axis.title = element_text(size = 18),
                      axis.text.x = element_text(size = 14, vjust = 0.9, hjust = 1), 
                      axis.text.y = element_text(size = 14)) 
              ggsave(filename = 'Box_Canyon_duration_vs_swe.png', width = 13, height = 8.5, plot, path = path5)
                


        # 2. PLOT DATES OF 20, 50, AND 80% BASEFLOW/SNOWMELT FOR SNOTEL 363
              
              # Just replace 'snow' with 'base' to plot the baseflow instead of the streamflow (Command F)
              test <- snow_20[, c('Water_Year', '10070002')] # Select the HUC that SNOTEL 363 is in
              test <- cbind(test, snow_50[colnames(snow_50) == '10070002'])
              test <- cbind(test, snow_80[colnames(snow_80) == '10070002'])
              colnames(test) <- c('Water_Year', '20', '50', '80')
              test <- test %>%
                mutate_if(is.character, as.numeric) # Convert to class numeric
              mean1 <- mean(test$`20`) # Compute the means of each of the percentiles so it can be plotted as a horizontal line
              mean2 <- mean(test$`50`)
              mean3 <- mean(test$`80`)
              test <- test %>%
                pivot_longer('20' : '80', names_to = 'Percentile', values_to = 'NDays') # Pivot longer so all three can be plotted at once
              test$Water_Year <- as.factor(test$Water_Year)
              test$NDays <- as.numeric(test$NDays)
              
              plot <- ggplot(test, aes(x = Water_Year, y = NDays, group = Percentile, color = Percentile)) + 
                geom_line(linewidth = 2) + 
                geom_hline(yintercept = mean1, color = '#F8766D') + 
                geom_hline(yintercept = mean2, color = '#00BA38') + 
                geom_hline(yintercept = mean3, color = '#619CFF') + 
                annotate('text', x = c('2003', '2003', '2003'), 
                         y = c(210, 250, 275), 
                         label = c('May 12th', 'May 31st', 'June 25th'), 
                         color = c('#F8766D', '#00BA38', '#619CFF'),
                         size = 12) + 
                labs(x = 'Water Year', y = 'Number of Days since start of Water Year (October 1)', 
                     title = 'Snowmelt Threshold Dates for HUC 10070002 (Yellowstone River, Montana)',
                     caption = 'Figure 12: Number of days that elapsed before 20%, 50%, and 80% of the snowmelt passed each year on the Yellowstone River, Montana. <br>Colored horizontal lines and text represent the mean for each category. Y-axis is represented as the number of days since the beginning <br>of the water year, October 1.', 
                     color = 'Percentage of Total Snowmelt for Each Water Year') + 
                theme(plot.title = element_text(size = 20),
                      plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                      legend.title = element_text(size = 14), 
                      legend.text = element_text(size = 11), 
                      legend.position = 'bottom', 
                      legend.key.size = unit(1, 'cm'), 
                      axis.title = element_text(size = 18),
                      axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1), 
                      axis.text.y = element_text(size = 14))
              ggsave(filename = 'Snowmelt_Box_Canyon.png', width = 13, height = 8.5, plot, path = path5)

        # 3. MAP PLOTTING
              # Just change the set of stations to be plotted

              
              station_map <- ggplot(us, aes(long, lat, group = group)) +
                geom_polygon(fill = 'white', color = 'grey50') +
                geom_sf(data = huc9, color  = '#E41A1C', fill = '#E41A1C', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc10, color  = '#377EB8', fill = '#377EB8', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc11, color  = '#4DAF4A', fill = '#4DAF4A', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc13, color  = '#984EA3', fill = '#984EA3', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc14, color  = '#FF7F00',fill = '#FF7F00',  alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc15, color  = '#FFFF33', fill = '#FFFF33', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc16, color  = '#A65628', fill = '#A65628', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc17, color  = '#F781BF', fill = '#F781BF', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc18, color = '#999999', fill = '#999999', alpha = 0.6, inherit.aes = FALSE) +
                coord_sf() +
                geom_point(q_meta, mapping = aes(x = Longitude, y = Latitude, fill = Elevation), inherit.aes = FALSE, size = 3, shape = 25) +
                labs(x = 'Longitude', y = 'Latitude', title = 'Selected USGS Streamflow Gauges', 
                     caption = '**Figure 4** Map of final set of USGS streamflow gauges following the application of a 2500-meter<br>elevation filter. SOme gauges are below 2500 meters because the threshold was applied to the<br>SNOTEL stations whose melt can be drained below 2500 meters. Total number of gauges is 69,<br>colored by elevation. Shaded regions are large river basins (HUC2s) that are numbered here.') +
                theme(plot.title = element_text(size = 20),
                      plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                      legend.title = element_text(size = 14), 
                      legend.text = element_text(size = 11), 
                      legend.position = 'right', 
                      legend.key.size = unit(1, 'cm')) +
                geom_label(data = labels, aes(x = longitude, y = latitude, label = label), inherit.aes = FALSE) +
                scale_fill_viridis('Elevation (m)', option = 'plasma')
              ggsave(filename = 'final_gauges_2500.png', width = 10, height = 9.5, station_map, path = path5)
              
              station_map <- ggplot(us, aes(long, lat, group = group)) +
                geom_polygon(fill = 'white', color = 'grey50') +
                geom_sf(data = huc9, color  = '#E41A1C', fill = '#E41A1C', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc10, color  = '#377EB8', fill = '#377EB8', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc11, color  = '#4DAF4A', fill = '#4DAF4A', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc13, color  = '#984EA3', fill = '#984EA3', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc14, color  = '#FF7F00',fill = '#FF7F00',  alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc15, color  = '#FFFF33', fill = '#FFFF33', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc16, color  = '#A65628', fill = '#A65628', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc17, color  = '#F781BF', fill = '#F781BF', alpha = 0.6, inherit.aes = FALSE) +
                geom_sf(data = huc18, color = '#999999', fill = '#999999', alpha = 0.6, inherit.aes = FALSE) +
                coord_sf() +
                geom_point(select_stations, mapping = aes(x = Longitude, y = Latitude, fill = Elevation), inherit.aes = FALSE, size = 5, shape = 25) +
                labs(x = 'Longitude', y = 'Latitude', title = '6 Select SWE Monitoring Stations', 
                     caption = 'Figure 7: Map of 6 selected SNOTEL stations following the application of a <br>2000 meter elevation threshold. Colored by elevation. <br>Shaded regions are large river basins (HUC2s) that are numbered here.') +
                theme(plot.title = element_text(size = 20),
                      plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                      legend.title = element_text(size = 14), 
                      legend.text = element_text(size = 11), 
                      legend.position = 'right', 
                      legend.key.size = unit(1, 'cm')) +
                geom_label(data = labels, aes(x = longitude, y = latitude, label = label), inherit.aes = FALSE) +
                scale_fill_viridis('Elevation (m)', option = 'magma')
              ggsave(filename = 'select_stations.png', width = 10, height = 9.5, station_map, path = path5)
              
              rm(huc9, huc10, huc11, huc13, huc14, huc15, huc16, huc17, huc18)
              
              
              
              
              
              
              
              
              
              
              
              
              
              
                      
                
################################################ Some Preliminary Results ####################################################

          # Select a few stations that are representative of the whole dataset
          # 363 Montana 10, 624 Colorado 13, 432 Utah 14, 617 Arizona 15, 534 Idaho 17, 356 California 18
          select_stations <- swe_meta %>%
            filter(StationId == 363 | StationId == 624 | StationId == 432 | StationId == 617 | StationId == 534 | StationId == 356)

          # Compute rolling mean and plot with raw discharge for one single USGS gauge
          test_q <- q %>%
            dplyr::select(c(1:6, '10080001')) %>%
            mutate(Rolling = zoo::rollapply(`10080001`, 3, mean, align = 'right', fill = NA)) %>% # Compute three day rolling average
            pivot_longer('10080001' : 'Rolling', names_to = 'HUC8', values_to = 'Discharge') 
          
          test_q <- q %>%
            dplyr::select(c(1:6, '16050302')) %>%
            mutate(Rolling = zoo::rollapply(`16050302`, 3, mean, align = 'right', fill = NA)) 
          test_q$Decade[test_q$Decade == 1970] <- 1980
          test_q$Date <- as.Date(test_q$Date)
          plot <- ggplot(test_q %>% filter(Water_Year == 2017), aes(x = Date, y = Rolling)) + 
            geom_line(linewidth = 1, na.rm = TRUE) +
            scale_x_date(date_breaks = '6 months', limits = c(ymd(20101001), ymd(20130930))) + 
            theme(aspect.ratio = 2/10) + 
            ylim(0, 30000) +
            labs(x = 'Date', y = 'Discharge (cubic ft/sec)', title = 'Raw and Smoothed Mean Daily Streamflow', 
                 caption = 'Figure 4: Sample streamflow hydrograph for HUC 17040209 at Yellowstone River, Montana. <br>Dates cover water years 2011 through 2013. Red curve is raw data, blue is <br>smoothed using a 3-day rolling average.') + 
            theme(plot.title = element_text(size = 20),
                  plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                  legend.title = element_text(size = 14), 
                  legend.text = element_text(size = 11), 
                  legend.position = 'right', 
                  legend.key.size = unit(1, 'cm'), 
                  axis.title = element_text(size = 18),
                  axis.text.x = element_text(size = 14, angle = 30), 
                  axis.text.y = element_text(size = 14))
          ggsave(filename = 'sample_hydrograph.png', width = 10, height = 5, plot, path = path5)
          
          # Plotting Cumulative Frequency Distributions for Snowmelt onset picks
          test_q <- q %>%
            dplyr::select(c(1:6, '10070002')) %>%
            mutate(Rolling = zoo::rollapply(`10070002`, 3, mean, align = 'right', fill = NA)) %>% # Compute three-day rolling average
            mutate_if(is.numeric, round, digits = 2) %>%
            filter(Water_Year == 2022) %>%
            mutate(cumsum = cumsum(replace_na(Rolling, 0)), # Compute the cumulative percent by day
                   cumpercent = (cumsum / tail(cumsum, n = 1)) * 100) %>%
            mutate(across('cumpercent', ~round(., digits = 2))) 
          t <- temp %>%
            dplyr::select(c(1:5, '363')) %>%
            rename(t = '363') %>%
            filter(Water_Year == 2022) %>%
            dplyr::select(last_col())
          test_q <- cbind(test_q, t)
          s <- swe %>%
            dplyr::select(c(1:5, '363')) %>%
            rename(s = '363') %>%
            filter(Water_Year == 2022) %>%
            dplyr::select(last_col())
          test_q <- cbind(test_q, s)
          plot <- ggplot(test_q, aes(x = Date)) + 
            geom_line(mapping = aes(y = cumpercent), color = 'black') + 
            # geom_line(mapping = aes(y = t), color = 'blue', na.rm = TRUE) + 
            # scale_y_continuous(name = 'Cumulative Percent',
            #                    sec.axis = sec_axis(~., name="Degrees Celsius", breaks = seq(-35, 30, by = 5))) + 
            labs(x = 'Date', title = 'Cumulative Frequency Distribution of Streamflow vs Time: Water Year 2022', y = 'Cumulative Percent',
                 caption = 'Figure 6: Example cumulative frequency distribution of streamflow as measured in HUC8 10070002 and SNOTEL 363. Station and gauge <br>located near the Yellowstone River in Montana. Note the sharp increase that occurs in late May of 2022. <br>This can be identified as the onset of snowmelt for this year.' ) + 
            scale_x_date(breaks = '2 weeks') + 
            theme_clean() +
            theme(plot.title = element_text(size = 20),
                  plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                  legend.title = element_text(size = 14), 
                  legend.text = element_text(size = 11), 
                  legend.position = 'right', 
                  legend.key.size = unit(1, 'cm'), 
                  axis.title = element_text(size = 18),
                  axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1), 
                  axis.text.y = element_text(size = 14))  
          #   geom_vline(xintercept = as.Date('2022-04-21'), color = 'red') + 
          # geom_hline(yintercept = 0, color = 'red') 
          ggsave(filename = 'cum_freq_ex.png', width = 13, height = 8.5, plot, path = path5)
          
          plot <- ggplot(test_q, aes(x = Date, y = s)) + 
            geom_area(fill = 'grey') + 
            labs(x = 'Date', y = 'Snow Water Equivalent (millimeters)', title = 'SNOTEL 363 Snowpack (Box Canyon, Montana) for Water Year 2011',
                 caption = 'Figure 9: Snowpack Hydrograph at Box Canyon, Montana (SNOTEL 363). Note that SWE is measured cumulatively each day, <br>resulting in this gradual increase. The red vertical line indicates the date of peak SWE, identified by eye. <br>Note the steep dropoff in SWE once the melt period begins.') + 
            theme_clean() + 
            scale_x_date(breaks = '2 weeks') +
            theme(plot.title = element_text(size = 20),
                  plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                  legend.title = element_text(size = 14), 
                  legend.text = element_text(size = 11), 
                  legend.position = 'right', 
                  legend.key.size = unit(1, 'cm'), 
                  axis.title = element_text(size = 18),
                  axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1), 
                  axis.text.y = element_text(size = 14)) +
            geom_vline(xintercept = as.Date('2013-03-25'), color = 'red') 
            # geom_label(mapping = aes(x = as.Date('2011-03-01'), y = 75), label = 'Date of Peak SWE: 2011-04-12')
            ggsave(filename = 'snowpack_hydro.png', width = 14, height = 8.5, plot, path = path5)
            
        
            
            blah <- q %>%
              dplyr::select(c(1:6, '10070002')) %>%
              rename(Q = 7) %>%
              mutate(Base = gr_baseflow(Q, method = 'lynehollick', a = 0.925, passes = 3)) %>%
              mutate(Roll1 = zoo::rollapply(Base, 3, mean, align = 'right', fill = NA),
                     Roll2 = zoo::rollapply(Q, 3, mean, align = 'right', fill = NA))
            
        
            plot <- ggplot(blah) +
              geom_area(mapping = aes(x = Date, y = Roll2, fill = 'blue')) + 
              geom_area(mapping = aes(x = Date, y = Roll1, fill = 'pink')) + 
              scale_x_date(breaks = '2 months', limits = c(ymd(20101001), ymd(20130930))) + 
              theme(plot.title = element_text(size = 20),
                    plot.caption = ggtext::element_markdown(size = 14, hjust = 0), 
                    legend.title = element_text(size = 14), 
                    legend.text = element_text(size = 11), 
                    legend.position = 'bottom', 
                    legend.key.size = unit(1, 'cm'), 
                    axis.title = element_text(size = 18),
                    axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1), 
                    axis.text.y = element_text(size = 14)) + 
              scale_fill_identity(name = 'Streamflow Type', guide = 'legend', labels = c('Total Streamflow', 'Lyne-Hollick Baseflow')) +
              labs(x = 'Date', y = 'Daily Mean Streamflow (cubic ft/sec)', title = 'Separated Hydrograph: Baseflow and Snowmelt Flow, Water Years 2011-2013',
                   caption = 'Separated streamflow hydrograph for HUC10070002, Yellowstone River, Montana. Pink is the baseflow separated using the Lyne-Hollick Method <br>using filtering parameter a = 0.925, with three consecutive passes. Blue is the total daily mean streamflow. <br>Both curves are smoothed with a 3-day rolling average. Note that the baseflow increases to a lesser magnitude as the total streamflow does.') 
            ggsave(filename = 'sep_streamflow_10070002.png', width = 14, height = 8.5, plot, path = path5)
          
          # HUC8 = 10070002, SNOTEL Station 363, Box Canyon, Montana
          # HUC8 = 13010001, SNOTEL Station 624, Middle Creek, Colorado
          # HUC8 = 14060004, SNOTEL Station 432, Currant Creek, Utah: TERRIBLE STATION, VERY LITTLE SNOWMELT
          # HUC8 = 15060101, SNOTEL Station 617, Maverick Fork, Arizona
          # HUC8 = 17040209, SNOTEL Station 534, Howell Canyon, Idaho
          # HUC8 = 18040012, SNOTEL Station 356, Blue Lakes, California  
            
############################## Discarded Stuff ###############################
            # # Diff April Onsets for each SNOTEL station that snowmelt onset dates have been picked for
            # diff_april_onsets <- date_peak_swe %>%
            #   mutate_if(is.Date, as.character)
            # for (i in 2:length(date_peak_swe)) {
            #   index = which(swe_meta$StationId == colnames(date_peak_swe)[i]) # The index in our metadata df that has the correct station
            #   huc = swe_meta$HUC8[index] # The HUC8 of this selected station
            #   for (x in 1:length(onsets$Water_Year)) {
            #     # Compute number of days that have elapsed since the start of the water year for each observation
            #     col <- which(colnames(onsets) == huc)
            #     ref <- as.Date(paste0(as.numeric(onsets$Water_Year[x]), '-04-01'))
            #     diff <- as.integer(difftime(onsets[x, col], ref)) 
            #     diff_april_onsets[x, i] <- diff
            #     rm(ref, dif, col)
            #   }
            #   rm(index, huc)
            # }
            # 
                
  
############################### Visual Snowmelt Onset Picks Code ########################
            stream <- q %>%
              dplyr::select(1:6, '14040101') %>%
              rename(Q = 7) %>%
              mutate(RollQ = zoo::rollapply(Q, 3, mean, align = 'right', fill = NA)) %>%
              mutate(Base = gr_baseflow(Q, method = 'lynehollick', a = 0.925, passes = 3)) 
            snowpack <- swe %>%
              dplyr::select(c(as.character(swe_meta$StationId[swe_meta$HUC8 == 14040101]))) %>%
              dplyr::select(1) %>%
              rename(SWE = 1) %>%
              mutate(RollSWE = zoo::rollapply(SWE, 3, mean, align = 'right', fill = NA)) %>%
              mutate_if(is.numeric, round, digits = 1)
            t <- temp %>%
              dplyr::select(c(as.character(swe_meta$StationId[swe_meta$HUC8 == 14040101]))) %>% 
              dplyr::select(1) %>%
              rename(Temp = 1)
            stream <- cbind(stream, snowpack, t)
            rm(snowpack, t)
            station <- as.character(swe_meta$StationId[swe_meta$HUC8 == 14040101])[1]
            huc <- 14040101
            
    
              x = 2020
              df <- stream %>%
                filter(Water_Year == x) %>%
                mutate(cumsum = cumsum(replace_na(RollQ, 0)), # Compute the cumulative percent by day
                       cumpercent = (cumsum / tail(cumsum, n = 1)) * 100) %>%
                mutate(across('cumpercent', ~round(., digits = 2)))
              p1 <- ggplot(df, aes(x = Date, y = cumpercent), color = 'black') + 
                geom_line() +
                labs(y = 'Cumulative Percent', title = paste0('Daily Cumulative Discharge Percent: SNOTEL', station, ' HUC', huc)) +
                scale_x_date(date_breaks = '2 weeks') +
                theme_clean() +
                theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
                      plot.title = element_text(size = 14),
                      plot.caption = ggtext::element_markdown(size = 14, hjust = 0),
                      legend.title = element_text(size = 14),
                      legend.text = element_text(size = 11),
                      legend.position = 'right',
                      legend.key.size = unit(1, 'cm'),
                      axis.title = element_text(size = 14),
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_text(size = 13))
              p2 <- ggplot(df, aes(x = Date)) +
                geom_line(mapping = aes(y = RollSWE), color = 'blue') +
                labs(y = 'Snow Water Equivalent (mm)', title = paste0('3-Day Running Mean SWE: SNOTEL', station, ' HUC', huc)) +
                geom_vline(aes(xintercept = as.Date('2020-05-09'))) +
                scale_x_date(date_breaks = '2 weeks') +
                theme_clean() +
                theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
                      plot.title = element_text(size = 14),
                      plot.caption = ggtext::element_markdown(size = 14, hjust = 0),
                      legend.title = element_text(size = 14),
                      legend.text = element_text(size = 11),
                      legend.position = 'right',
                      legend.key.size = unit(1, 'cm'),
                      axis.title = element_text(size = 14),
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_text(size = 13))
              p3 <- ggplot(df, aes(x = Date)) +
                geom_line(mapping = aes(y = RollQ), color = 'green') +
                labs(y = 'Daily Discharge (cubic ft/sec)', title = paste0('3-Day Running Mean Discharge: SNOTEL', station, ' HUC', huc)) + 
                scale_x_date(date_breaks = '2 weeks') +
                theme_clean() + 
                theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
                      plot.title = element_text(size = 14),
                      plot.caption = ggtext::element_markdown(size = 14, hjust = 0),
                      legend.title = element_text(size = 14),
                      legend.text = element_text(size = 11),
                      legend.position = 'right',
                      legend.key.size = unit(1, 'cm'),
                      axis.title = element_text(size = 14),
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_text(size = 13)) +
                geom_vline(aes(xintercept = as.Date('2020-04-20')))
              p4 <- ggplot(df, aes(x = Date)) +
                geom_line(mapping = aes(y = Temp), color = 'red') + 
                geom_hline(aes(yintercept = 0)) + 
                labs(y = 'Degrees Celsius', title = paste0('Surface Air Temperature: SNOTEL', station, ' HUC', huc),
                     caption = paste0('**Figure 6** Water Year  ', x, ' plots of snow water equivalent, streamflow, and temperature for SNOTEL ', station, ' located in HUC ', huc, '. (A) depicts a three<br>day running mean of snow water equivalent. (B) shows a 3-day smoothed discharge hydrograph in green, (C) is a cumulative frequency distribution of<br>streamflow over theduration of the water year, and (D) is the air temperature curve with a horizontal line depicting 0 degrees Celsius.')) + 
                scale_x_date(date_breaks = '2 weeks') +
                theme_clean() + 
                theme(panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
                      plot.title = element_text(size = 14),
                      plot.caption = ggtext::element_markdown(size = 15, hjust = 0),
                      legend.title = element_text(size = 1),
                      legend.text = element_text(size = 11),
                      legend.position = 'right',
                      legend.key.size = unit(1, 'cm'),
                      axis.title = element_text(size = 16),
                      axis.text.x = element_text(size = 12, angle = 30, vjust = 0.9, hjust = 1),
                      axis.text.y = element_text(size = 14))
              p <- plot_grid(p2, p3, p1, p4, ncol = 1, nrow = 4, labels = 'AUTO', rel_heights = c(3,3,2.5,4))
              ggsave(filename = 'panel_plot.png', width = 15, height = 15, p, path = path5)
    
            
            

              