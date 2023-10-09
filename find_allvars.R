# This script is for extracting GLEAM and MSWEP data by coordinates

library(readxl)
library(openxlsx)
library(ncdf4)
library(tools)
library(tidyverse)
library(zoo)
library(lubridate)
library(dplyr)

Mywd <- readline(prompt = "Enter the route you put the result folder:") #D:/Master4_TUD/TA_karst
setwd(Mywd)

#################### 1. Read the coordinate data ----
my_coordinate <- "./SISAL3_site/SISALv3_sites.xlsx" #A file with "latitude" and "longitude" first
cave_location <- read_excel(my_coordinate)
cave_number = length(cave_location$site_name) #check the names(cave_location)
my_coordinatename <- file_path_sans_ext(basename(my_coordinate))

##################### 2. Read GLEAM data -------
# make sure the variable input
whichvar_list <- c("E","Ep")
# make sure the route for your files (In same folder)
GLEAM_dir_path <- paste0("D:/Master4_TUD/TA_karst/GLEAM/v3.6a/daily")

##### 2-1. Run first year and see if there is NA value ----
GLEAM_day <- data.frame() # create DF for storing the GLEAM data
for(var in head(whichvar_list,2)) {
  var #E and then Ep
  GLEAM_file_list <- list.files(path = GLEAM_dir_path, full.names = TRUE, recursive = TRUE
                                , pattern = paste0("^",var,"_.*")) # Get a list of all files in the directory
  for (file in head(GLEAM_file_list,41)) {
    print(file)
    data <- file
    ncin <- nc_open(data)
    lat = ncvar_get(ncin, "lat")
    lon = ncvar_get(ncin, "lon")
    year <- sub('.*_(\\d{4}).*', '\\1', basename(file)) # extract the year from the file name
    tm =  ncvar_get(ncin, "time")
    tm_date = as.Date(tm, origin = paste0(year, "-01-01"), tz = "UTC")

    # Find the grid order of the caves by their coordinate
    for (cave_no in 5:cave_number){ #should change back to cave_number
      print(c("NO.",cave_no))
      cave_name = cave_location$site_name[cave_no]
      print(c("Now we are searching for data in", cave_name))
      Cave <- rep(cave_name, length(tm_date))
      need_lat = cave_location$latitude[cave_no]
      lat_range = c(need_lat-0.125,need_lat+0.12499) #if the point is on the boarder of the grid, we can only choose one grid
      lat_index = which(lat >=lat_range[1] & lat <= lat_range[2])
      print(lat_index)
      need_lon = cave_location$longitude[cave_no]
      lon_range = c(need_lon-0.125,need_lon+0.12499)
      lon_index = which(lon >=lon_range[1] & lon <= lon_range[2])
      print(lon_index)

      var_value = ncvar_get(ncin, var, start = c(lon_index[1],lat_index[1],1), count = c(length(lon_index),length(lat_index),-1)) #find the variable in that point #-1 means all
      print("ok")
      result <- data.frame(tm_date, var_value, Cave, var)
      print("ok")
      GLEAM_day <- rbind(result,GLEAM_day)
      }
  }
}

##### 2-2. Select the sites that have NA value ----
df_wide <- pivot_wider(GLEAM_day, names_from = var, values_from = var_value, names_prefix = "value_")
GLEAM_mon <- df_wide %>%
  mutate(year_month = as.yearmon(tm_date)) %>%
  group_by(Cave, year_month) %>%
  summarise(PET = sum(value_Ep),
            AET = sum(value_E)) #unit: mm/mon

##### For Chinese system users ------
GLEAM_mon$year <- year(GLEAM_mon$year_month)
GLEAM_mon$month <- month(GLEAM_mon$year_month)
GLEAM_mon$year_month <- as.Date(paste(GLEAM_mon$year, GLEAM_mon$month, "01", sep = "-"))
write.xlsx(GLEAM_mon,paste0("./","_GLEAM.xlsx"), rowNames=FALSE)

GLEAM_needtobefix <- subset(GLEAM_mon, year == "1980" & month == "1" 
                            &  is.na(PET))

if (nrow(GLEAM_needtobefix) == 0) {
  # No NA values in the dataset
  print("There are no NA values in this dataset. You can go to 2-4 to have all the data")
} else {
    # NA values present in the dataset
  print("Please go to 2-3 to fix the coordinate")
  GLEAM_needtobefix <- merge(GLEAM_needtobefix, cave_location[, c("site_name", "latitude", "longitude")], by = "site_name", all.x = TRUE)
  write.xlsx(GLEAM_needtobefix,paste0("./SISAL3_site/","needtofindtheETagain.xlsx"), rowNames=FALSE)
}

#### 2-3. use GridonMap_GLEAMandMSWEPandSin.R ------
# -> see where the point is and fix the coordinate manually (0.25*0.25)
# here i add columns called new_latitude, new_longitude
# when you fix the data, input the new coordinate
GLEAM_fixedlocation <- read_excel("./SISAL3_site/needtofindtheETagain_fix.xlsx") 

# Merge the data frames based on the common 'site' column
merged_df <- merge(cave_location, GLEAM_fixedlocation, by = "site_name", all = TRUE)

# Select the desired columns in the merged data frame
cave_ET_fixlocation <- merged_df[, c("site_name", "site_id", "elevation", "monitoring","latitude.x", "longitude.x", "lat_GLEAM", "lon_GLEAM")]
cave_ET_fixlocation$lat_GLEAM  <- ifelse(is.na(merged_df$lat_GLEAM), merged_df$latitude.x, merged_df$lat_GLEAM)
cave_ET_fixlocation$lon_GLEAM <- ifelse(is.na(merged_df$lon_GLEAM), merged_df$longitude.x, merged_df$lon_GLEAM)

write.csv(cave_ET_fixlocation,paste0("./SISAL3_site/","cave_redo_ET_fixall.csv"), row.names=FALSE)

##################### 3. Read MSWEP Monthly data (all files) ----
# set the directory path where the files are stored
MSWEP_dir_path <- paste0(Mywd,"/MSWEP/Monthly/") #this is from Past (downloaded from GOOGLE)
MSWEP_file_list <- list.files(MSWEP_dir_path,full.names = TRUE, recursive = TRUE)

##################### LOOP the year: Read all the MSWEP data -------
##### 3-1. Check the missing data ----
all_months <- data.frame(date = seq(as.Date("1980-01-01"), as.Date("2019-12-31"), by = "month"))
all_months$year <- format(all_months$date, "%Y")
all_months$month <- format(all_months$date, "%m")
MSWEP_file_words <- basename(MSWEP_file_list)
# Extract the year and month using indexing
MSWEP_years <- substr(MSWEP_file_words, 1,4)
MSWEP_months <- substr(MSWEP_file_words,5,6)

# create a data frame with precipitation data
precipitation <- data.frame(file = MSWEP_file_list)
precipitation$year <- MSWEP_years
precipitation$month <- MSWEP_months
# merge the two data frames
merged_data <- merge(all_months, precipitation, by = c("year", "month"), all.x = TRUE)

##### 3-2. Extract MSWEP Monthly data----
MSWEP_result_bind <- data.frame()
for (MSWEP_data in MSWEP_file_list) {
  MSWEP_ncin <- nc_open(MSWEP_data)
  MSWEP_lat = ncvar_get(MSWEP_ncin, "lat")
  MSWEP_lon = ncvar_get(MSWEP_ncin, "lon")
  MSWEP_yr <- substr(basename(MSWEP_data), 1, 4) # extract the year from the file name
  print(MSWEP_yr)
  MSWEP_mon <- substr(basename(MSWEP_data), 5, 6) # extract the year from the file name
  MSWEP_tm =  ncvar_get(MSWEP_ncin, "time")
  MSWEP_tm_date = as.Date(MSWEP_tm, origin = paste0("1900-01-01"), tz = "UTC")

  for (cave_no in 1:cave_number){
    # print(c("NO.",cave_no))
    cave_name = cave_location$site_name[cave_no] #check the names(cave_location)
    # print(c("Now we are searching for data in", cave_name))
    name_col <- rep(cave_name, length(MSWEP_tm_date))
    # should i use the original or the new LAT?
    need_lat = cave_location$latitude[cave_no]  - 0.001 #if the point is on the boarder of the grid, we only choose one grid
    lat_index_min <- which.min(abs(MSWEP_lat - need_lat))
    # cat("Closest latitude grid:", lat[lat_index_min], "at index", lat_index_min)
    need_lon = cave_location$longitude[cave_no] - 0.001 #if the point is on the boarder of the grid, we only choose one grid
    lon_index_min <- which.min(abs(MSWEP_lon - need_lon))
    # print(" ")
    # cat("Closest latitude grid:", lon[lon_index_min], "at index ", lon_index_min)
    #find the Precipitation in that grid
    MSWEP_value = ncvar_get(MSWEP_ncin, "precipitation", start = c(lon_index_min[1],lat_index_min[1],1), count = c(length(lon_index_min),length(lat_index_min),-1)) #-1 means ALL
    MSWEP_result <- data.frame(MSWEP_tm_date, MSWEP_value, name_col)
    print("ok")
    MSWEP_result_bind <- rbind(MSWEP_result,MSWEP_result_bind)
  }
}

##### 3-3. Fill NA into the missing date ----
df_MSWEP_wide <- pivot_wider(MSWEP_result_bind,
                       names_from = name_col, values_from = MSWEP_value,
                       values_fill = NA) #long to wide
 
##### 3-4. Reshape to long format & Rename the colnames----
df_MSWEP_long <- df_MSWEP_wide %>%
  pivot_longer(cols = -MSWEP_tm_date, names_to = "site_name", values_to = "value")
df_MSWEP_long <- df_MSWEP_long %>%
  rename(year_month = MSWEP_tm_date, P_MSWEP = value)

write.csv(df_MSWEP_wide,paste0("./result/","MSWEPwide",my_coordinatename,".csv"), row.names=FALSE)
write.csv(df_MSWEP_long,paste0("./result/","MSWEPlong",my_coordinatename,".csv"), row.names=FALSE)


##################### 4. READ TIF (sincurve-Supplement 3) and extract the values by coordinates -------
library(sp)
library(raster)

rasters_list <- c("./method-precipitation isotope/O18Amp.tif", "./method-precipitation isotope/O18Offset.tif", "./method-precipitation isotope/O18Phase.tif",
                  "./method-precipitation isotope/H2Amp.tif", "./method-precipitation isotope/H2Offset.tif", "./method-precipitation isotope/H2Phase.tif",
                  "./method-precipitation isotope/PAmp.tif", "./method-precipitation isotope/POffset.tif", "./method-precipitation isotope/PPhase.tif")

###### 4-1. Loop over the raster files and extract values for each one ----
cave_location_sp <- SpatialPointsDataFrame(coords = cave_location[, c("longitude", "latitude")],
                                           data = cave_location)
coords <- coordinates(cave_location_sp)
extracted_values <- list() # create an empty list to store the extracted values
coordinates(cave_location) <- ~longitude+latitude
proj4string(cave_location) <- CRS("+proj=longlat +datum=WGS84")

for (r_file in rasters_list) {
  raster_obj <- raster(r_file) # load the raster file
  values <- extract(raster_obj, cave_location)  # extract values for the points
  extracted_values[[r_file]] <- values # add the extracted values to the list
}

extracted_values_df <- do.call(cbind, extracted_values) #convert list to df
col_names <- colnames(extracted_values_df) # extracted_values_df
# Remove leading "./" and trailing ".tif"
new_col_names <- sub("^\\./", "", col_names)
new_col_names <- sub("\\.tif$", "", new_col_names)

colnames(extracted_values_df) <- new_col_names # Assign the new column names
sin_APO <- cbind(cave_location, extracted_values_df)
sin_APO_df <- as.data.frame(sin_APO)
sin_APO_df[sin_APO_df == -9999] <- NA

# write.csv(sin_APO_df,paste0("./result/","sincurve_APO.csv"), row.names=FALSE)

###### 4-2. Select the sites that have NA value -------
sin_needtobefix <- subset(sin_APO_df, is.na(method.precipitation.isotope.O18Amp))
if (nrow(sin_needtobefix) == 0) {
  # No NA values in the dataset
  print("There are no NA values in this dataset. You can go to 4-4 to have all the data")
} else {
  # NA values present in the dataset
  print("Please go to 4-3 to fix the coordinate")
}
write.xlsx(sin_needtobefix,paste0("./SISAL3_site/","needtofindtheAPOagain.xlsx"), rowNames=FALSE)

###### 4-3. use GridonMap_GLEAMandMSWEPandSin.R ------
# -> see where the point is and fix the coordinate manually (GLEAM: 0.25*0.25, SIN: 0.1*0.1)
# here i add columns called lat/lon_GLEAM, lat/lon_sin
# when you fix the data, input the new coordinate
sin_fixedlocation <- read_excel("./SISAL3_site/needtofindtheAPOagain_fix.xlsx") 

# Merge the data frames based on the common 'site' column
merged_df2 <- merge(merged_df, sin_fixedlocation, by = "site_name", all = TRUE)
cave_fixlocation <- merged_df2[, c("site_name","lat_GLEAM", "lon_GLEAM", "lat_sin.y", "lon_sin.y")]
cave_location <- read_excel(my_coordinate)

# write.csv(cave_fixlocation,paste0("./SISAL3_site/","cave_location_fixall.csv"), row.names=FALSE)

##### 5. Combine all variables (df_MSWEP_long, GLEAM_mon, sin_isotope_mon) by date and caves... -----
GLEAM_mon <- read.csv("./result/EandEp_monwide_SISALv3_sites.csv")
df_MSWEP_long <- read.csv("./result/MSWEPlongSISALv3_sites.csv")
sin_APO <- read.csv("./result/sincurve_APO_SISALv3_sites.csv")
sin_isotope_mon <- read.csv("./result/sin_isotope_mon_SISAL3.csv")

# Left join MSWEP_result_bind and GLEAM_mon data frames by cave name and year_month columns
GLEAM_mon$year_month <- as.Date(GLEAM_mon$year_month)
df_MSWEP_long$year_month <- as.Date(df_MSWEP_long$year_month)
 
df_combined <- data.frame()
df_combined <- left_join(df_MSWEP_long, GLEAM_mon, by = c("site_name" = "site_name", "year_month" = "year_month"))
df_combined <- left_join(df_combined,sin_isotope_mon,  by = c("site_name" = "site_name", "month" = "month"), multiple = "all") 
   
df_filtered <- df_combined %>%
  filter(year_month >= as.Date("1980-01-01") & year_month <= as.Date("2019-12-01"))
# delete column "c"
df_filtered <- df_filtered[, c("site_id", "site_name", "year", "month","P_MSWEP", "AET","PET", "precip_sin_d18O","precip_sin_d2H")]
write.csv(df_filtered,paste0("./result/allin_SISAL3.csv"), row.names=FALSE)
 
##### 6. Split the df by the caves... -----
df_list <- split(df_filtered, df_filtered$site_name)
 # Loop over each data frame and save to a separate file
for (i in seq_along(df_list)) {
  # Construct the file name based on the cave name
  print(i)
  file_name <- paste0(gsub("/", "-", df_list[[i]]$site_name[1]), ".csv")
  print(file_name)
  write.csv(df_list[[i]],paste0("./SISAL3_site/",file_name), row.names = FALSE)
}

 

##################### Close the netCDF file ----
nc_close(nc)


##################### For separate ----
sin_fixedlocation <- read_excel("./SISAL3_site/needtofindtheAPOagain_fix.xlsx") 
merged_df <- merge(sin_fixedlocation, GLEAM_fixedlocation, by = "site_name", all = TRUE)
cave_number = length(merged_df$site_name)
fixsite_name_list <- merged_df$site_name
extracted_coord <- cave_location[cave_location$site_name %in% fixsite_name_list, c("site_name","site_id", "latitude", "longitude")]
merged_df_all <- merge(extracted_coord, merged_df, by = "site_name", all = TRUE)

# Select the desired columns in the merged data frame
cave_fixlocation <- merged_df_all[, c("site_name", "site_id.x","latitude", "longitude", "lat_GLEAM", "lon_GLEAM", "lat_sin", "lon_sin")]
cave_fixlocation$lat_GLEAM  <- ifelse(is.na(cave_fixlocation$lat_GLEAM), cave_fixlocation$latitude, cave_fixlocation$lat_GLEAM)
cave_fixlocation$lon_GLEAM <- ifelse(is.na(cave_fixlocation$lon_GLEAM), cave_fixlocation$longitude, cave_fixlocation$lon_GLEAM)
cave_fixlocation$lat_sin  <- ifelse(is.na(cave_fixlocation$lat_sin), cave_fixlocation$latitude, cave_fixlocation$lat_sin)
cave_fixlocation$lon_sin <- ifelse(is.na(cave_fixlocation$lon_sin), cave_fixlocation$longitude, cave_fixlocation$lon_sin)

#### 2-4. RUN the 2-1, but now LOOP all the years: Read all the GLEAM data -----
GLEAM_day <- data.frame() # create DF for storing the GLEAM data
result <- data.frame()
# var_value <- 0
for(var in whichvar_list){
  print(var)
  GLEAM_file_list <- list.files(path = GLEAM_dir_path, full.names = TRUE, recursive = TRUE
                                , pattern = paste0("^",var,"_.*")) # Get a list of all files in the directory
  # for (file in GLEAM_file_list) {
  for (file in GLEAM_file_list) {    
    print(file)
    data <- file
    ncin <- nc_open(data)
    lat = ncvar_get(ncin, "lat")
    lon = ncvar_get(ncin, "lon")
    year <- sub('.*_(\\d{4}).*', '\\1', basename(file)) # extract the year from the file name
    tm =  ncvar_get(ncin, "time")
    tm_date = as.Date(tm, origin = paste0(year, "-01-01"), tz = "UTC")
    
    for (cave_no in 1:cave_number){ #change back to cave_number
      cave_name = cave_fixlocation$site_name[cave_no]
      print(c("Now searching for: NO.",cave_no, cave_name))
      Cave <- rep(cave_name, length(tm_date))
      need_lat = cave_fixlocation$lat_GLEAM[cave_no]
      lat_range = c(need_lat-0.125,need_lat+0.12499) #if the point is on the boarder of the grid, we can only choose one grid
      lat_index = which(lat >=lat_range[1] & lat <= lat_range[2])
      need_lon = cave_fixlocation$lon_GLEAM[cave_no]
      lon_range = c(need_lon-0.125,need_lon+0.12499)
      lon_index = which(lon >=lon_range[1] & lon <= lon_range[2])
      print(c(need_lat, need_lon))
      print(c(lat_index, lon_index))
      
      var_value = ncvar_get(ncin, var, start = c(lon_index,lat_index,1), 
                            count = c(1,1,-1)) #find the variable in that point #-1 means all
      
      print("ok")
      result <- data.frame(tm_date, var_value, site_name, var)
      print("ok")
      GLEAM_day <- rbind(result,GLEAM_day)
    }
    print("this year is done!")
  }
  print("ET extract is done!")
}
print("check GLEAM_day")
# write.csv(GLEAM_day,paste0("./SISAL3_site/","EandEp_daylong_fixed.csv"), row.names=FALSE)

##### 2-5. Reshape data frame from long to wide format ----
df_wide <- pivot_wider(GLEAM_day, names_from = var, values_from = var_value, names_prefix = "value_")
# write.csv(df_wide,paste0("./result/","EandEp_daywide_",my_coordinatename,".csv"), row.names=FALSE)

##### 2-6. Aggregate the daily GLEAM value to monthly value and reshape them ------
GLEAM_mon <- df_wide %>%
  mutate(year_month = as.yearmon(tm_date)) %>%
  group_by(site_name, year_month) %>%
  summarise(PET = sum(value_Ep),
            AET = sum(value_E)) #unit: mm/mon


##### For Chinese system users ------
GLEAM_mon$year <- year(GLEAM_mon$year_month)
GLEAM_mon$month <- month(GLEAM_mon$year_month)
GLEAM_mon$year_month <- as.Date(paste(GLEAM_mon$year, GLEAM_mon$month, "01", sep = "-"))

write.xlsx(GLEAM_mon,paste0("./SISAL3_site/","EandEp_monlong_fixed.xlsx"), rowNames=FALSE)

# GLEAM_mon <- GLEAM_mon %>%
#   rename(site_name = Cave, year_month = year_month)

##### 3-2. Extract MSWEP Monthly data----
MSWEP_result_bind <- data.frame()
for (MSWEP_data in MSWEP_file_list) {
  MSWEP_ncin <- nc_open(MSWEP_data)
  MSWEP_lat = ncvar_get(MSWEP_ncin, "lat")
  MSWEP_lon = ncvar_get(MSWEP_ncin, "lon")
  MSWEP_yr <- substr(basename(MSWEP_data), 1, 4) # extract the year from the file name
  print(MSWEP_yr)
  MSWEP_mon <- substr(basename(MSWEP_data), 5, 6) # extract the year from the file name
  MSWEP_tm =  ncvar_get(MSWEP_ncin, "time")
  MSWEP_tm_date = as.Date(MSWEP_tm, origin = paste0("1900-01-01"), tz = "UTC")
  
  for (cave_no in 1:cave_number){
    # print(c("NO.",cave_no))
    cave_name = cave_fixlocation$site_name[cave_no] #check the names(cave_location)
    # print(c("Now we are searching for data in", cave_name))
    name_col <- rep(cave_name, length(MSWEP_tm_date)) 
    need_lat = cave_fixlocation$latitude[cave_no]  - 0.001 #if the point is on the boarder of the grid, we only choose one grid
    lat_index_min <- which.min(abs(MSWEP_lat - need_lat))
    # cat("Closest latitude grid:", lat[lat_index_min], "at index", lat_index_min)
    need_lon = cave_fixlocation$longitude[cave_no] - 0.001 #if the point is on the boarder of the grid, we only choose one grid
    lon_index_min <- which.min(abs(MSWEP_lon - need_lon))
     #find the Precipitation in that grid
    MSWEP_value = ncvar_get(MSWEP_ncin, "precipitation", start = c(lon_index_min[1],lat_index_min[1],1), count = c(length(lon_index_min),length(lat_index_min),-1)) #-1 means ALL
    MSWEP_result <- data.frame(MSWEP_tm_date, MSWEP_value, name_col)
    print("ok")
    MSWEP_result_bind <- rbind(MSWEP_result,MSWEP_result_bind)
  }
  print("check df MSWEP_result_bind")
}

##### 3-3. Fill NA into the missing date ----
df_MSWEP_wide <- pivot_wider(MSWEP_result_bind,
                             names_from = name_col, values_from = MSWEP_value,
                             values_fill = NA) #long to wide

##### 3-4. Reshape to long format & Rename the colnames----
df_MSWEP_long <- df_MSWEP_wide %>%
  pivot_longer(cols = -MSWEP_tm_date, names_to = "site_name", values_to = "value")
df_MSWEP_long <- df_MSWEP_long %>%
  rename(year_month = MSWEP_tm_date, P_MSWEP = value)

write.xlsx(df_MSWEP_wide,paste0("./SISAL3_site/","MSWEPwide_fix.xlsx"), rowNames=FALSE)
write.xlsx(df_MSWEP_long,paste0("./SISAL3_site/","MSWEPlong_SISAL_fix.xlsx"), rowNames=FALSE)


###### 4-4. Loop over the raster files and extract values for each one ----
cave_location_sp2 <- SpatialPointsDataFrame(coords = cave_fixlocation[, c("lon_sin", "lat_sin")],
                                            data = cave_fixlocation)
coords2 <- coordinates(cave_location_sp2)
extracted_values2 <- list() # create an empty list to store the extracted values
coordinates(cave_fixlocation) <- ~lon_sin + lat_sin
proj4string(cave_fixlocation) <- CRS("+proj=longlat +datum=WGS84")

for (r_file in rasters_list) {
  raster_obj <- raster(r_file) # load the raster file
  values2 <- extract(raster_obj, cave_fixlocation)  # extract values for the points
  extracted_values2[[r_file]] <- values2 # add the extracted values to the list
}

extracted_values_df2 <- do.call(cbind, extracted_values2) #convert list to df
col_names2 <- colnames(extracted_values_df2)
# Remove leading "./" and trailing ".tif"
new_col_names2 <- sub("^\\./", "", col_names)
new_col_names2 <- sub("\\.tif$", "", new_col_names)

colnames(extracted_values_df2) <- new_col_names # Assign the new column names
sin_APO <- cbind(cave_fixlocation, extracted_values_df2)
sin_APO_df <- as.data.frame(sin_APO)
# sin_APO_df[sin_APO_df == -9999] <- NA # should not have -9999

###### 4-4. Create an empty data frame for the fractional time -------
dates <- seq(as.Date("1980-01-01"), as.Date("1980-12-31"), by = "day") #date sequence
days <- as.numeric(format(dates, "%j")) #1 to 366
t <- days/ 365 #fractional time
df_fractTime <- data.frame(date = dates, day = days, t = t)

sin_isotope <- merge(df_fractTime, sin_APO, by = NULL)
sin_isotope$precip_sin_d18O <- with(sin_isotope, method.precipitation.isotope.O18Amp *
                                      sin(2*pi*t - method.precipitation.isotope.O18Phase) + method.precipitation.isotope.O18Offset)
sin_isotope$precip_sin_d2H <-with(sin_isotope, method.precipitation.isotope.H2Amp *
                                    sin(2*pi*t - method.precipitation.isotope.H2Phase) + method.precipitation.isotope.H2Offset)

###### 4.2 Aggregate to monthly value -------
sin_isotope$month <-  month(sin_isotope$date)
sin_isotope_mon <- sin_isotope %>%
  group_by(site_id.x,site_name, month, longitude, latitude) %>%
  summarise(precip_sin_d18O = mean(precip_sin_d18O),
            precip_sin_d2H = mean(precip_sin_d2H),
            .groups = "drop") #unit: mm/mon

write.xlsx(sin_isotope_mon,paste0("./SISAL3_site/","sin_isotope_mon_SISAL_fix.xlsx"), rowNames=FALSE)

##### 5. Combine all variables (df_MSWEP_long, GLEAM_mon, sin_isotope_mon) by date and caves... -----
# GLEAM_mon <- read.csv("./result/EandEp_monwide_SISALv3_sites.csv")
# df_MSWEP_long <- read.csv("./result/MSWEPlongSISALv3_sites.csv")
# sin_APO <- read.csv("./result/sincurve_APO_SISALv3_sites.csv")
# sin_isotope_mon <- read.csv("./result/sin_isotope_mon_SISAL3.csv")

# Left join MSWEP_result_bind and GLEAM_mon data frames by cave name and year_month columns
GLEAM_mon$year_month <- as.Date(GLEAM_mon$year_month)
df_MSWEP_long$year_month <- as.Date(df_MSWEP_long$year_month)
# names(GLEAM_mon)[1] <- "site_name"

df_combined <- data.frame()
df_combined <- left_join(df_MSWEP_long, GLEAM_mon, by = c("site_name" = "site_name", "year_month" = "year_month"))
df_combined <- left_join(df_combined,sin_isotope_mon,  by = c("site_name" = "site_name", "month" = "month"), multiple = "all") 

df_filtered <- df_combined %>%
  filter(year_month >= as.Date("1980-01-01") & year_month <= as.Date("2019-12-31"))
# delete column "c"
df_filtered <- df_filtered[, c("site_id.x", "site_name", "year", "month","P_MSWEP", "AET","PET", "precip_sin_d18O","precip_sin_d2H")]
names(df_filtered)[1] <- "site_id"
write.xlsx(df_filtered,paste0("./SISAL3_site/fixin_SISAL3.xlsx"), rowNames=FALSE)

##### 6. Split the df by the caves... -----
df_list <- split(df_filtered, df_filtered$site_name)
# Loop over each data frame and save to a separate file
for (i in seq_along(df_list)) {
  # Construct the file name based on the cave name
  print(i)
  file_name <- paste0(gsub("/", "-", df_list[[i]]$site_name[1]), ".csv")
  print(file_name)
  write.csv(df_list[[i]],paste0("./SISAL3_site/",file_name), row.names = FALSE)
}
