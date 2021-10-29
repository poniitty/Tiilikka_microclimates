##########################################################################
# THIS CODE MAKES SOME QUALITY CHECKS IF THE SITE AND TOMST IDs LOOKS FINE
#

library(tidyverse)

# List binary and command files to be removed from repository if also data file exists

f <- c(list.files("data", pattern = "binary_", recursive = T, full.names = T),
       list.files("data", pattern = "command_", recursive = T, full.names = T))

for(i in f){ if(file.exists(gsub("binary_","data_",i)) | file.exists(gsub("command_","data_",i))){
  unlink(i)
} else {
  print(paste0("DATA FILE MISSING!!! ", i))
} 
}
# If no printed messages then no problems

# Haxo

f <- list.files("data", pattern = "-2021.ltd$", recursive = T, full.names = T)

for(i in f){ if(file.exists(gsub("-2021.ltd","-2021.csv",i))){
  unlink(i)
} else {
  print(paste0("DATA FILE MISSING!!! ", i))
} 
}

###########################################################################
# Check Tomst ID-numbers from last year data
maxdt <- read_csv("data/reading_times_2020.csv") %>% 
  mutate(site = site)

f <- list.files("data", pattern = "data_9", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- as.numeric(toupper(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2]))))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

# Check if more than one data file in a folder
fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none


#######################################################################
# Check if missing sites in 2021 data
all <- full_join(fi, maxdt %>% rename(tomst_id_20 = tomst_id))

# Check for duplicate sites
all %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
all %>% filter(site %in% doubled_sites) # No, Good!

# Non-matching sites
all %>% filter(!complete.cases(.))

# No sites that occur only in 2021 data




# All together 17 sites in 2020 data but not in 2021
all %>% filter(tomst_id == 94194253) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94190042) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194260) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94190041) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194394) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194402) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194404) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194405) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194255) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194408) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194410) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194254) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194256) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194252) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194258) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194409) # No such tomst_id in 2021 data, so it is fine
all %>% filter(tomst_id == 94194407) # No such tomst_id in 2021 data, so it is fine


# For these 17 sites find 2020 data and copy to repository
f2 <- list.files("C:/Users/OMISTAJA/OneDrive - University of Helsinki/R_Projects/microclim_suomi/raw_field_data",
                 pattern = "data_", recursive = T, full.names = T)

# Copy site 6 data from last year data
f2[grepl("94194253", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/6"))
file.copy(f2[grepl("94194253", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/6/data_94194253_0.csv"))

# Copy site 8 data from last year data
f2[grepl("94190042", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/8"))
file.copy(f2[grepl("94190042", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/8/data_94190042_1.csv"))

# Copy site 11 data from last year data
f2[grepl("94194260", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/11"))
file.copy(f2[grepl("94194260", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/11/data_94194260_0.csv"))

# Copy site 15 data from last year data
f2[grepl("94190041", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/15"))
file.copy(f2[grepl("94190041", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/15/data_94190041_0.csv"))

# Copy site 16 data from last year data
f2[grepl("94194394", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/16"))
file.copy(f2[grepl("94194394", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/16/data_94194394_0.csv"))

# Copy site 22 data from last year data
f2[grepl("94194402", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/22"))
file.copy(f2[grepl("94194402", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/22/data_94194402_0.csv"))

# Copy site 24 data from last year data
f2[grepl("94194404", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/24"))
file.copy(f2[grepl("94194404", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/24/data_94194404_0.csv"))

# Copy site 25 data from last year data
f2[grepl("94194405", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/25"))
file.copy(f2[grepl("94194405", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/25/data_94194405_0.csv"))

# Copy site 26 data from last year data
f2[grepl("94194255", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/26"))
file.copy(f2[grepl("94194255", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/26/data_94194255_0.csv"))

# Copy site 30 data from last year data
f2[grepl("94194408", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/30"))
file.copy(f2[grepl("94194408", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/30/data_94194408_0.csv"))

# Copy site 32 data from last year data
f2[grepl("94194410", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/32"))
file.copy(f2[grepl("94194410", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/32/data_94194410_0.csv"))

# Copy site 38 data from last year data
f2[grepl("94194254", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/38"))
file.copy(f2[grepl("94194254", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/38/data_94194254_0.csv"))

# Copy site 39 data from last year data
f2[grepl("94194256", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/39"))
file.copy(f2[grepl("94194256", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/39/data_94194256_0.csv"))

# Copy site 40 data from last year data
f2[grepl("94194252", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/40"))
file.copy(f2[grepl("94194252", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/40/data_94194252_0.csv"))

# Copy site 43 data from last year data
f2[grepl("94194258", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/43"))
file.copy(f2[grepl("94194258", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/43/data_94194258_0.csv"))

# Copy site 44 data from last year data
f2[grepl("94194409", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/44"))
file.copy(f2[grepl("94194409", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/44/data_94194409_0.csv"))

# Copy site 55 data from last year data
f2[grepl("94194407", f2)]
dir.create(paste0(getwd(), "/data/Tiilikka_2020/55"))
file.copy(f2[grepl("94194407", f2)],
          paste0(getwd(), "/data/Tiilikka_2020/55/data_94194407_0.csv"))

########################################################################################
# Update file list

f <- list.files("data", pattern = "data_9", recursive = T, full.names = T)

fi <- data.frame(file = f)

fi$site <- as.numeric(toupper(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2]))))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

fi %>% group_by(tomst_id) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(tomst_id) -> doubled_ids
fi %>% filter(tomst_id %in% doubled_ids) # check for weird things!!! Good if none

fi %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
fi %>% filter(site %in% doubled_sites) # check for weird things!!! Good if none
# Looks good still!!!

#######################################################################
# Check if Tomst ids match between years
all <- full_join(fi, maxdt %>% rename(tomst_id_20 = tomst_id))

# Check for duplicate sites
all %>% group_by(site) %>% summarise(n = n()) %>% filter(n > 1) %>% pull(site) -> doubled_sites
all %>% filter(site %in% doubled_sites) # These are fine

all %>% filter(tomst_id == tomst_id_20)
all %>% filter(tomst_id != tomst_id_20)
# All seems to match nicely!!!!!!!!!!


# Good to go and read the data


