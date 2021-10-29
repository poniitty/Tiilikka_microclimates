
#############################################################################
library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(zoo)

d <- fread("output/haxo_data.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2"))

d %>% filter(haxo_probl == 0) %>% 
  group_by(datetime) %>% 
  summarise(md = median(at, na.rm = T),
            md_arh = median(arh, na.rm = T)) %>% 
  ungroup() -> md

# pdf("visuals/Haxo_correction_diagnoses.pdf", 12, 10)
# for(i in unique(d$site)){
#   print(i)
#   
#   d %>% filter(site == i) -> temp
#   
#   temp <- full_join(temp, md) %>% as.data.table()
#   
#   temp$corr24 <- rollapply(temp, 12 ,function(x) cor(as.numeric(x[,"at"]),
#                                                      as.numeric(x[,"md"])),
#                            by.column=FALSE, fill = NA)
#   
#   temp$corr48 <- rollapply(temp, 24 ,function(x) cor(as.numeric(x[,"at"]),
#                                                      as.numeric(x[,"md"])),
#                            by.column=FALSE, fill = NA)
#   
#   temp$corr168 <- rollapply(temp, 84 ,function(x) cor(as.numeric(x[,"at"]),
#                                                       as.numeric(x[,"md"])),
#                             by.column=FALSE, fill = NA)
#   
#   temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
#                                                          as.numeric(x[,"md_arh"])),
#                                by.column=FALSE, fill = NA)
#   
#   temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
#                                                          as.numeric(x[,"md_arh"])),
#                                by.column=FALSE, fill = NA)
#   
#   temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
#                                                           as.numeric(x[,"md_arh"])),
#                                 by.column=FALSE, fill = NA)
#   
#   
#   temp %>% 
#     mutate(lead_probl = rollapply(haxo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
#   
#   temp %>% mutate(tdiff = abs(md-at)) %>% 
#     mutate(corr24 = ifelse(corr24<0 | haxo_probl != 0 | lead_probl != 0,0,corr24),
#            corr48 = ifelse(corr48<0 | haxo_probl != 0 | lead_probl != 0,0,corr48),
#            corr168 = ifelse(corr168<0 | haxo_probl != 0 | lead_probl != 0,0,corr168),
#            corr24_arh = ifelse(corr24_arh<0 | haxo_probl != 0 | lead_probl != 0,0,corr24_arh),
#            corr48_arh = ifelse(corr48_arh<0 | haxo_probl != 0 | lead_probl != 0,0,corr48_arh),
#            corr168_arh = ifelse(corr168_arh<0 | haxo_probl != 0 | lead_probl != 0,0,corr168_arh)) -> temp
#   
#   temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
#     mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
#     mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) %>% 
#     mutate(comb = ifelse(haxo_probl != 0, 0, comb)) %>% 
#     mutate(comb = ifelse(lead_probl != 0, 0, comb)) -> temp
#   
#   temp %>% ggplot(aes_string(x="datetime")) +
#     geom_line(aes_string(y = "md"), col = "black") +
#     geom_line(aes_string(y = "at"), col = "cornflowerblue") +
#     theme_minimal() +
#     ylab("Temperature") + xlab("Date")+
#     scale_y_continuous(limits = c(-30, 35))+
#     ggtitle(i) -> GG1
#   
#   temp %>% ggplot(aes_string(x="datetime")) +
#     geom_line(aes_string(y = "corr24"), col = "cornflowerblue") +
#     geom_line(aes_string(y = "corr48"), col = "red") +
#     geom_line(aes_string(y = "corr168"), col = "black") +
#     theme_minimal() +
#     ylab("Rolling correlation") +
#     scale_y_continuous(limits = c(0, 1)) -> GG2
#   
#   temp %>% ggplot(aes_string(x="datetime")) +
#     geom_line(aes_string(y = "tdiff"), col = "cornflowerblue") +
#     theme_minimal() +
#     ylab("T difference") -> GG3
#   
#   temp %>% ggplot(aes_string(x="datetime")) +
#     geom_line(aes_string(y = "comb"), col = "cornflowerblue") +
#     theme_minimal() +
#     ylab("T difference") -> GG4
#   
#   print(plot_grid(plotlist = list(GG1,GG2,GG3,GG4), nrow = 4))
#   
# }
# dev.off()

df <- data.frame()
for(i in unique(d$site)){
  # i <- 9
  
  d %>% filter(site == i) -> temp
  
  temp <- full_join(temp, md) %>% 
    arrange(datetime) %>% as.data.table()
  
  firstd <- temp %>% filter(!is.na(at)) %>% pull(datetime) %>% max() - days(28)
  lastd_data <- temp %>% filter(!is.na(at)) %>% pull(datetime) %>% max()
  lastd <- temp %>% pull(datetime) %>% max() + days(2)
  
  temp %>% filter(datetime >= firstd) -> move_data
  
  max_move <- length(seq(from = lastd_data, to = lastd, by = "2 hours"))-1
  
  moves <- 0:max_move
  
  cors <- rep(NA, length(moves))
  names(cors) <- moves
  # summary(temp)
  for(mi in moves){
    #mi <- 1
    move_data %>% filter(!is.na(at)) %>% 
      select(datetime, at, arh) %>% 
      mutate(datetime = datetime + hours(2*mi)) %>% 
      full_join(move_data %>% select(datetime, md, md_arh)) %>% 
      arrange(datetime) %>% 
      filter(complete.cases(.)) -> temp2
    
    cors[[paste0(mi)]] <- cor(temp2$at, temp2$md)+cor(temp2$arh, temp2$md_arh)
  }
  
  move_n <- as.numeric(names(which.max(cors)))
  
  if(move_n  != 0){
    
    print(i)
    
    d %>% filter(site == i) -> temp
    
    temp <- full_join(temp, md) %>% 
      arrange(datetime) %>% as.data.table()
    
    temp$corr24 <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"md"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr48 <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"md"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr168 <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"at"]),
                                                        as.numeric(x[,"md"])),
                              by.column=FALSE, fill = NA)
    
    temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
                                                       as.numeric(x[,"md_arh"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
                                                       as.numeric(x[,"md_arh"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
                                                        as.numeric(x[,"md_arh"])),
                              by.column=FALSE, fill = NA)
    
    temp %>% 
      mutate(lead_probl = rollapply(haxo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
    
    temp %>% mutate(tdiff = abs(md-at)) %>% 
      mutate(corr24 = ifelse(corr24<0 | haxo_probl != 0 | lead_probl != 0,0,corr24),
             corr48 = ifelse(corr48<0 | haxo_probl != 0 | lead_probl != 0,0,corr48),
             corr168 = ifelse(corr168<0 | haxo_probl != 0 | lead_probl != 0,0,corr168),
             corr24_arh = ifelse(corr24_arh<0 | haxo_probl != 0 | lead_probl != 0,0,corr24_arh),
             corr48_arh = ifelse(corr48_arh<0 | haxo_probl != 0 | lead_probl != 0,0,corr48_arh),
             corr168_arh = ifelse(corr168_arh<0 | haxo_probl != 0 | lead_probl != 0,0,corr168_arh)) -> temp
    
    temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
      mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
      mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) %>% 
      mutate(comb = ifelse(haxo_probl != 0, 0, comb)) %>% 
      mutate(comb = ifelse(lead_probl != 0, 0, comb)) -> temp
    
    firstd <- temp$datetime[min(which(temp$comb >= 10))]
    if(is.na(firstd)){
      d %>% filter(site == i) -> temp
      
      temp <- full_join(temp, md) %>% 
        arrange(datetime) %>% as.data.table()
      
      temp$corr24 <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"at"]),
                                                         as.numeric(x[,"md"])),
                               by.column=FALSE, fill = NA)
      
      temp$corr48 <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"at"]),
                                                         as.numeric(x[,"md"])),
                               by.column=FALSE, fill = NA)
      
      temp$corr168 <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"at"]),
                                                          as.numeric(x[,"md"])),
                                by.column=FALSE, fill = NA)
      
      temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
                                                             as.numeric(x[,"md_arh"])),
                                   by.column=FALSE, fill = NA)
      
      temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
                                                             as.numeric(x[,"md_arh"])),
                                   by.column=FALSE, fill = NA)
      
      temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
                                                              as.numeric(x[,"md_arh"])),
                                    by.column=FALSE, fill = NA)
      
      temp %>% 
        mutate(lead_probl = rollapply(haxo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
      
      temp %>% mutate(tdiff = abs(md-at)) %>% 
        mutate(corr24 = ifelse(corr24<0,0,corr24),
               corr48 = ifelse(corr48<0,0,corr48),
               corr168 = ifelse(corr168<0,0,corr168),
               corr24_arh = ifelse(corr24_arh<0,0,corr24_arh),
               corr48_arh = ifelse(corr48_arh<0,0,corr48_arh),
               corr168_arh = ifelse(corr168_arh<0,0,corr168_arh)) -> temp
      
      temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
        mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
        mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) -> temp
      
      firstd <- temp$datetime[min(which(temp$comb >= 10))]
    }
    
    if(!is.na(firstd)){
      lastd_data <- temp %>% filter(!is.na(at)) %>% pull(datetime) %>% max()
      lastd <- temp %>% pull(datetime) %>% max() + days(2)
      
      temp %>% filter(datetime >= firstd) -> move_data
      
      max_move <- length(seq(from = lastd_data, to = lastd, by = "2 hours"))-1
      
      moves <- 0:max_move
      
      cors <- rep(NA, length(moves))
      names(cors) <- moves
      
      for(mi in moves){
        #mi <- 1
        move_data %>% filter(!is.na(at)) %>% 
          select(datetime, at, arh) %>% 
          mutate(datetime = datetime + hours(2*mi)) %>% 
          full_join(move_data %>% select(datetime, md, md_arh)) %>% 
          arrange(datetime) %>% 
          filter(complete.cases(.)) -> temp2
        
        cors[[paste0(mi)]] <- cor(temp2$at, temp2$md)+cor(temp2$arh, temp2$md_arh)
      }
      
      move_n <- as.numeric(names(which.max(cors)))
      print(move_n)
      
      move_data %>% mutate(datetime = datetime + hours(2*move_n)) %>% 
        select(datetime, at, arh, haxo_probl) %>% 
        bind_rows(temp %>% filter(datetime < firstd) %>% select(datetime, at, arh, haxo_probl)) %>% 
        full_join(md) %>% arrange(datetime) %>% 
        filter(!is.na(md)) %>% 
        mutate(site = i) -> temp
    }
    
  }
  
  df <- bind_rows(df, temp %>% select(site, datetime, at, arh, haxo_probl))
  
}

df %>% filter(!is.na(site)) -> df


# ROUND 2

df2 <- data.frame()
for(i in unique(df$site)){
  # i <- 9
  
  df %>% filter(site == i) -> temp
  
  temp <- full_join(temp, md) %>% 
    arrange(datetime) %>% as.data.table()
  
  firstd <- temp %>% filter(!is.na(at)) %>% pull(datetime) %>% max() - days(28)
  lastd_data <- temp %>% filter(!is.na(at)) %>% pull(datetime) %>% max()
  lastd <- temp %>% pull(datetime) %>% max() + days(2)
  
  temp %>% filter(datetime >= firstd) -> move_data
  
  max_move <- length(seq(from = lastd_data, to = lastd, by = "2 hours"))-1
  
  moves <- 0:max_move
  
  cors <- rep(NA, length(moves))
  names(cors) <- moves
  
  for(mi in moves){
    #mi <- 1
    move_data %>% filter(!is.na(at)) %>% 
      select(datetime, at, arh) %>% 
      mutate(datetime = datetime + hours(2*mi)) %>% 
      full_join(move_data %>% select(datetime, md, md_arh)) %>% 
      arrange(datetime) %>% 
      filter(complete.cases(.)) -> temp2
    
    cors[[paste0(mi)]] <- cor(temp2$at, temp2$md)+cor(temp2$arh, temp2$md_arh)
  }
  
  move_n <- as.numeric(names(which.max(cors)))
  
  if(move_n  != 0){
    
    print(i)
    
    df %>% filter(site == i) -> temp
    
    temp <- full_join(temp, md) %>% 
      arrange(datetime) %>% as.data.table()
    
    temp$corr24 <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"md"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr48 <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"md"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr168 <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"at"]),
                                                        as.numeric(x[,"md"])),
                              by.column=FALSE, fill = NA)
    
    temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
                                                           as.numeric(x[,"md_arh"])),
                                 by.column=FALSE, fill = NA)
    
    temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
                                                           as.numeric(x[,"md_arh"])),
                                 by.column=FALSE, fill = NA)
    
    temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
                                                            as.numeric(x[,"md_arh"])),
                                  by.column=FALSE, fill = NA)
    
    temp %>% 
      mutate(lead_probl = rollapply(haxo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
    
    temp %>% mutate(tdiff = abs(md-at)) %>% 
      mutate(corr24 = ifelse(corr24<0 | haxo_probl != 0 | lead_probl != 0,0,corr24),
             corr48 = ifelse(corr48<0 | haxo_probl != 0 | lead_probl != 0,0,corr48),
             corr168 = ifelse(corr168<0 | haxo_probl != 0 | lead_probl != 0,0,corr168),
             corr24_arh = ifelse(corr24_arh<0 | haxo_probl != 0 | lead_probl != 0,0,corr24_arh),
             corr48_arh = ifelse(corr48_arh<0 | haxo_probl != 0 | lead_probl != 0,0,corr48_arh),
             corr168_arh = ifelse(corr168_arh<0 | haxo_probl != 0 | lead_probl != 0,0,corr168_arh)) -> temp
    
    temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
      mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
      mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) %>% 
      mutate(comb = ifelse(haxo_probl != 0, 0, comb)) %>% 
      mutate(comb = ifelse(lead_probl != 0, 0, comb)) -> temp
    
    firstd <- temp$datetime[min(which(temp$comb >= 10))]
    if(is.na(firstd)){
      d %>% filter(site == i) -> temp
      
      temp <- full_join(temp, md) %>% as.data.table()
      
      temp$corr24 <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"at"]),
                                                         as.numeric(x[,"md"])),
                               by.column=FALSE, fill = NA)
      
      temp$corr48 <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"at"]),
                                                         as.numeric(x[,"md"])),
                               by.column=FALSE, fill = NA)
      
      temp$corr168 <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"at"]),
                                                          as.numeric(x[,"md"])),
                                by.column=FALSE, fill = NA)
      
      temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
                                                             as.numeric(x[,"md_arh"])),
                                   by.column=FALSE, fill = NA)
      
      temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
                                                             as.numeric(x[,"md_arh"])),
                                   by.column=FALSE, fill = NA)
      
      temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
                                                              as.numeric(x[,"md_arh"])),
                                    by.column=FALSE, fill = NA)
      
      temp %>% 
        mutate(lead_probl = rollapply(haxo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
      
      temp %>% mutate(tdiff = abs(md-at)) %>% 
        mutate(corr24 = ifelse(corr24<0,0,corr24),
               corr48 = ifelse(corr48<0,0,corr48),
               corr168 = ifelse(corr168<0,0,corr168),
               corr24_arh = ifelse(corr24_arh<0,0,corr24_arh),
               corr48_arh = ifelse(corr48_arh<0,0,corr48_arh),
               corr168_arh = ifelse(corr168_arh<0,0,corr168_arh)) -> temp
      
      temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
        mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
        mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) -> temp
      
      firstd <- temp$datetime[min(which(temp$comb >= 10))]
    }
    
    if(!is.na(firstd)){
      lastd_data <- temp %>% filter(!is.na(at)) %>% pull(datetime) %>% max()
      lastd <- temp %>% pull(datetime) %>% max() + days(2)
      
      temp %>% filter(datetime >= firstd) -> move_data
      
      max_move <- length(seq(from = lastd_data, to = lastd, by = "2 hours"))-1
      
      moves <- 0:max_move
      
      cors <- rep(NA, length(moves))
      names(cors) <- moves
      
      for(mi in moves){
        #mi <- 1
        move_data %>% filter(!is.na(at)) %>% 
          select(datetime, at, arh) %>% 
          mutate(datetime = datetime + hours(2*mi)) %>% 
          full_join(move_data %>% select(datetime, md, md_arh)) %>% 
          arrange(datetime) %>% 
          filter(complete.cases(.)) -> temp2
        
        cors[[paste0(mi)]] <- cor(temp2$at, temp2$md)+cor(temp2$arh, temp2$md_arh)
      }
      
      move_n <- as.numeric(names(which.max(cors)))
      print(move_n)
      
      move_data %>% mutate(datetime = datetime + hours(2*move_n)) %>% 
        select(datetime, at, arh, haxo_probl) %>% 
        bind_rows(temp %>% filter(datetime < firstd) %>% select(datetime, at, arh, haxo_probl)) %>% 
        full_join(md) %>% arrange(datetime) %>% 
        filter(!is.na(md)) %>% 
        mutate(site = i) -> temp
    }
    
  }
  
  df2 <- bind_rows(df2, temp %>% select(site, datetime, at, arh, haxo_probl))
  
}

df2 %>% filter(!is.na(site)) -> df2

# ROUND 3

df3 <- data.frame()
for(i in unique(df2$site)){
  # i <- 77
  
  df2 %>% filter(site == i) -> temp
  
  temp <- full_join(temp, md) %>% as.data.table()
  
  firstd <- temp %>% filter(!is.na(at)) %>% pull(datetime) %>% max() - days(28)
  lastd_data <- temp %>% filter(!is.na(at)) %>% pull(datetime) %>% max()
  lastd <- temp %>% pull(datetime) %>% max() + days(2)
  
  temp %>% filter(datetime >= firstd) -> move_data
  
  max_move <- length(seq(from = lastd_data, to = lastd, by = "2 hours"))-1
  
  moves <- 0:max_move
  
  cors <- rep(NA, length(moves))
  names(cors) <- moves
  
  for(mi in moves){
    #mi <- 1
    move_data %>% filter(!is.na(at)) %>% 
      select(datetime, at, arh) %>% 
      mutate(datetime = datetime + hours(2*mi)) %>% 
      full_join(move_data %>% select(datetime, md, md_arh)) %>% 
      arrange(datetime) %>% 
      filter(complete.cases(.)) -> temp2
    
    cors[[paste0(mi)]] <- cor(temp2$at, temp2$md)+cor(temp2$arh, temp2$md_arh)
  }
  
  move_n <- as.numeric(names(which.max(cors)))
  
  if(move_n  != 0){
    
    print(i)
    
    df2 %>% filter(site == i) -> temp
    
    temp <- full_join(temp, md) %>% as.data.table()
    
    temp$corr24 <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"md"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr48 <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"at"]),
                                                       as.numeric(x[,"md"])),
                             by.column=FALSE, fill = NA)
    
    temp$corr168 <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"at"]),
                                                        as.numeric(x[,"md"])),
                              by.column=FALSE, fill = NA)
    
    temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
                                                           as.numeric(x[,"md_arh"])),
                                 by.column=FALSE, fill = NA)
    
    temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
                                                           as.numeric(x[,"md_arh"])),
                                 by.column=FALSE, fill = NA)
    
    temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
                                                            as.numeric(x[,"md_arh"])),
                                  by.column=FALSE, fill = NA)
    
    temp %>% 
      mutate(lead_probl = rollapply(haxo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
    
    temp %>% mutate(tdiff = abs(md-at)) %>% 
      mutate(corr24 = ifelse(corr24<0 | haxo_probl != 0 | lead_probl != 0,0,corr24),
             corr48 = ifelse(corr48<0 | haxo_probl != 0 | lead_probl != 0,0,corr48),
             corr168 = ifelse(corr168<0 | haxo_probl != 0 | lead_probl != 0,0,corr168),
             corr24_arh = ifelse(corr24_arh<0 | haxo_probl != 0 | lead_probl != 0,0,corr24_arh),
             corr48_arh = ifelse(corr48_arh<0 | haxo_probl != 0 | lead_probl != 0,0,corr48_arh),
             corr168_arh = ifelse(corr168_arh<0 | haxo_probl != 0 | lead_probl != 0,0,corr168_arh)) -> temp
    
    temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
      mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
      mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) %>% 
      mutate(comb = ifelse(haxo_probl != 0, 0, comb)) %>% 
      mutate(comb = ifelse(lead_probl != 0, 0, comb)) -> temp
    
    firstd <- temp$datetime[min(which(temp$comb >= 10))]
    if(is.na(firstd)){
      d %>% filter(site == i) -> temp
      
      temp <- full_join(temp, md) %>% as.data.table()
      
      temp$corr24 <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"at"]),
                                                         as.numeric(x[,"md"])),
                               by.column=FALSE, fill = NA)
      
      temp$corr48 <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"at"]),
                                                         as.numeric(x[,"md"])),
                               by.column=FALSE, fill = NA)
      
      temp$corr168 <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"at"]),
                                                          as.numeric(x[,"md"])),
                                by.column=FALSE, fill = NA)
      
      temp$corr24_arh <- rollapply(temp, 12, function(x) cor(as.numeric(x[,"arh"]),
                                                             as.numeric(x[,"md_arh"])),
                                   by.column=FALSE, fill = NA)
      
      temp$corr48_arh <- rollapply(temp, 24, function(x) cor(as.numeric(x[,"arh"]),
                                                             as.numeric(x[,"md_arh"])),
                                   by.column=FALSE, fill = NA)
      
      temp$corr168_arh <- rollapply(temp, 84, function(x) cor(as.numeric(x[,"arh"]),
                                                              as.numeric(x[,"md_arh"])),
                                    by.column=FALSE, fill = NA)
      
      temp %>% 
        mutate(lead_probl = rollapply(haxo_probl, width=12*5, FUN=max, fill = NA, partial = T, align = "right")) -> temp
      
      temp %>% mutate(tdiff = abs(md-at)) %>% 
        mutate(corr24 = ifelse(corr24<0,0,corr24),
               corr48 = ifelse(corr48<0,0,corr48),
               corr168 = ifelse(corr168<0,0,corr168),
               corr24_arh = ifelse(corr24_arh<0,0,corr24_arh),
               corr48_arh = ifelse(corr48_arh<0,0,corr48_arh),
               corr168_arh = ifelse(corr168_arh<0,0,corr168_arh)) -> temp
      
      temp %>% mutate(tchange = round(abs(at-lag(at)),2)) %>%
        mutate(dchange = round(abs(tdiff-lag(tdiff)),2)) %>%
        mutate(comb = round(((1-corr24)*(1-corr48)*(1-corr168)*(1-corr168_arh)*tdiff*tchange*dchange),1)) -> temp
      
      firstd <- temp$datetime[min(which(temp$comb >= 10))]
    }
    
    if(!is.na(firstd)){
      lastd_data <- temp %>% filter(!is.na(at)) %>% pull(datetime) %>% max()
      lastd <- temp %>% pull(datetime) %>% max() + days(2)
      
      temp %>% filter(datetime >= firstd) -> move_data
      
      max_move <- length(seq(from = lastd_data, to = lastd, by = "2 hours"))-1
      
      moves <- 0:max_move
      
      cors <- rep(NA, length(moves))
      names(cors) <- moves
      
      for(mi in moves){
        #mi <- 1
        move_data %>% filter(!is.na(at)) %>% 
          select(datetime, at, arh) %>% 
          mutate(datetime = datetime + hours(2*mi)) %>% 
          full_join(move_data %>% select(datetime, md, md_arh)) %>% 
          arrange(datetime) %>% 
          filter(complete.cases(.)) -> temp2
        
        cors[[paste0(mi)]] <- cor(temp2$at, temp2$md)+cor(temp2$arh, temp2$md_arh)
      }
      
      move_n <- as.numeric(names(which.max(cors)))
      print(move_n)
      
      move_data %>% mutate(datetime = datetime + hours(2*move_n)) %>% 
        select(datetime, at, arh, haxo_probl) %>% 
        bind_rows(temp %>% filter(datetime < firstd) %>% select(datetime, at, arh, haxo_probl)) %>% 
        full_join(md) %>% arrange(datetime) %>% 
        filter(!is.na(md)) %>% 
        mutate(site = i) -> temp
    }
  }
  
  df3 <- bind_rows(df3, temp %>% select(site, datetime, at, arh, haxo_probl))
  
}

df3 %>% filter(!is.na(site)) -> df3

# TRIM THE GAP MARGINS
# BACKWARD

df4 <- data.frame()
for(i in unique(df3$site)){
  # i <- 77
  
  print(i)
  print(Sys.time())
  
  df3 %>% filter(site == i) %>% 
    #select(-md, -md_arh) %>% 
    filter(complete.cases(.)) %>% 
    mutate(timediff = datetime - lag(datetime)) %>% 
    full_join(., md) %>% 
    arrange(datetime) %>% as.data.table() -> temp
  
  temp[1,"timediff"] <- 2
  
  splits <- which(temp$timediff > 2)
  
  if(length(splits) > 0){
    for(ii in splits){
      
      temp %>% slice(1:(ii-1)) %>% pull(at) %>% rev() -> ats
      
      gap_length <- which.min(is.na(ats))-1
      
      test_df <- expand.grid(data_l = 11:167,
                             move_l = 1:gap_length,
                             cor = NA,
                             abse = NA)
      
      test_df <- bind_rows(data.frame(data_l = 167,
                                      move_l = 0,
                                      cor = NA,
                                      abse = NA),
                           test_df)
      
      temp %>% slice(ii:(ii+167)) %>% pull(at) -> att
      # temp %>% slice(ii:(ii+167)) %>% pull(md) -> mdt
      # 
      # test_df[1,"cor"] <- cor(mdt, att, use = "pairwise.complete.obs")
      # test_df[1,"abse"] <- mean(abs(mdt-att), na.rm = T)
      
      maxl <- max(test_df$data_l)
      
      for(iii in 1:nrow(test_df)){
        
        if(test_df[iii,"data_l"] == maxl){
          temp %>% slice((ii-test_df[iii,"move_l"]):(ii-test_df[iii,"move_l"]+test_df[iii,"data_l"])) %>%
            pull(md) -> mdt
        } else {
          temp %>% slice(c((ii-test_df[iii,"move_l"]):(ii-test_df[iii,"move_l"]+test_df[iii,"data_l"]),
                           (ii+test_df[iii,"data_l"]+1):(ii+167))) %>%
            pull(md) -> mdt
        }
        
        test_df[iii,"cor"] <- cor(mdt, att, use = "pairwise.complete.obs")
        test_df[iii,"abse"] <- mean(abs(mdt-att), na.rm = T)
        
      }
      
      test_df %>% mutate(fac = (1-cor)*abse) %>% pull(fac) %>% which.min() -> result
      
      temp %>% pull(at) -> ats
      move <- ats[ii:(ii+test_df[result,"data_l"])]
      ats[ii:(ii+test_df[result,"data_l"])] <- NA
      ats[(ii-test_df[result,"move_l"]):(ii-test_df[result,"move_l"]+test_df[result,"data_l"])] <- move
      temp$at <- ats
      
      temp %>% pull(arh) -> ats
      move <- ats[ii:(ii+test_df[result,"data_l"])]
      ats[ii:(ii+test_df[result,"data_l"])] <- NA
      ats[(ii-test_df[result,"move_l"]):(ii-test_df[result,"move_l"]+test_df[result,"data_l"])] <- move
      temp$arh <- ats
      
    }
  }
  
  temp$site <- i
  
  df4 <- bind_rows(df4, temp %>% select(site, datetime, at, arh, haxo_probl))
  
}

df4 %>% filter(!is.na(site)) -> df4

# fwrite(df4, "test.csv")
# df4 <- fread("test.csv") %>% 
#   mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2"))
# FORWARD

df5 <- data.frame()
for(i in unique(df4$site)){
  # i <- 10
  
  print(i)
  print(Sys.time())
  
  df4 %>% filter(site == i) %>% 
    #select(-md, -md_arh) %>% 
    filter(complete.cases(.)) %>% 
    mutate(timediff = lead(datetime) - datetime) %>% 
    full_join(., md) %>% 
    arrange(datetime) %>% as.data.table() -> temp
  
  splits <- which(temp$timediff > 2)
  
  if(length(splits) > 0){
    for(ii in splits){
      # ii <- 232
      temp %>% slice((ii+1):nrow(.)) %>% pull(at) -> ats
      
      gap_length <- which.min(is.na(ats))-1
      
      if(gap_length > 3){
        maxdata_l <- ifelse(gap_length < 83, gap_length, 83)
        maxdata_l <- ifelse(ii-1 < maxdata_l, ii-1, maxdata_l)
        
        test_df <- expand.grid(data_l = 1:maxdata_l,
                               move_l = 1:gap_length,
                               cor = NA,
                               abse = NA)
        
        test_df <- bind_rows(data.frame(data_l = maxdata_l,
                                        move_l = 0,
                                        cor = NA,
                                        abse = NA),
                             test_df)
        
        temp %>% slice((ii-maxdata_l):ii) %>% pull(at) -> att
        # temp %>% slice(ii:(ii+167)) %>% pull(md) -> mdt
        # 
        # test_df[1,"cor"] <- cor(mdt, att, use = "pairwise.complete.obs")
        # test_df[1,"abse"] <- mean(abs(mdt-att), na.rm = T)
        
        maxl <- max(test_df$data_l)
        
        for(iii in 1:nrow(test_df)){
          
          if(test_df[iii,"data_l"] == maxl){
            temp %>% slice((ii-maxl+test_df[iii,"move_l"]):(ii+test_df[iii,"move_l"])) %>%
              pull(md) -> mdt
          } else {
            temp %>% slice(c((ii-maxl):(ii-test_df[iii,"data_l"]),
                             (ii-test_df[iii,"data_l"]+1+test_df[iii,"move_l"]):(ii+test_df[iii,"move_l"]))) %>%
              pull(md) -> mdt
          }
          
          test_df[iii,"cor"] <- cor(mdt, att, use = "pairwise.complete.obs")
          test_df[iii,"abse"] <- mean(abs(mdt-att), na.rm = T)
          
        }
        
        test_df %>% mutate(fac = (1-cor)*abse) %>% pull(fac) %>% which.min() -> result
        
        temp %>% pull(at) -> ats
        move <- ats[(ii-test_df[result,"data_l"]+1):ii]
        ats[(ii-test_df[result,"data_l"]+1):ii] <- NA
        ats[(ii-test_df[result,"data_l"]+1+test_df[result,"move_l"]):(ii+test_df[result,"move_l"])] <- move
        temp$at <- ats
        
        temp %>% pull(arh) -> ats
        move <- ats[(ii-test_df[result,"data_l"]+1):ii]
        ats[(ii-test_df[result,"data_l"]+1):ii] <- NA
        ats[(ii-test_df[result,"data_l"]+1+test_df[result,"move_l"]):(ii+test_df[result,"move_l"])] <- move
        temp$arh <- ats
        
      }
    }
  }
  
  temp$site <- i
  
  df5 <- bind_rows(df5, temp %>% select(site, datetime, at, arh, haxo_probl, md))
  
}

df5 %>% filter(!is.na(site)) -> df5

# TRIM WITH THE LAST DAY ON FIELD

info <- read_csv("data/reading_times_2021.csv") %>% 
  mutate(maxdt = with_tz(maxdt, tzone = "Etc/GMT-2")) %>% 
  rename(last_date = maxdt)

unique(df5$site)[!unique(df5$site) %in% info$site]
# info <- bind_rows(info,
#                   tibble(site = 42,
#                          last_date = as_datetime("2021-07-09 14:00:00", tz = "Etc/GMT-2")))
info %>% filter(!complete.cases(.))

data.frame(site = unique(df5$site)) %>% left_join(., info) -> info

for(i in unique(df5$site)){
  
  df5 %>% filter(!c(site == i & datetime > info$last_date[info$site == i])) %>% 
    filter(!c(site == i & datetime < d %>% filter(site == i,
                                                    complete.cases(.)) %>% pull(datetime) %>% min())) -> df5
  
  # df5 %>% filter(site == i) %>% as.data.table()
}

##################################################################
#

df6 <- data.frame()
for(i in unique(df5$site)){
  # i <- 23
  
  print(i)
  print(Sys.time())
  
  df5 %>% filter(site == i) %>% 
    #select(-md, -md_arh) %>% 
    filter(complete.cases(.)) %>% 
    mutate(timediff = lead(datetime) - datetime) %>% 
    full_join(., md) %>% 
    arrange(datetime) %>% as.data.table() -> temp
  
  temp %>% mutate(grp = rleid(ifelse(is.na(at), 0, 1))) %>% 
    group_by(grp) %>% 
    summarise(n = n(),
              val = mean(ifelse(is.na(at), 0, 1))) -> grps
  
  if((grps$val[1] == 0 & nrow(grps) > 3) | (grps$val[1] == 1 & nrow(grps) > 2)){
    mind <- temp$datetime[which.min(is.na(temp$at))]
    strt <- which(temp$datetime == mind)
    
    temp %>% filter(datetime >= mind) %>% pull(at) -> ats
    
    maxdata_l <- which.min(!is.na(ats))-1
    
    gap_length <- which.min(is.na(ats[which.min(!is.na(ats)):length(ats)]))-1
    
    if(gap_length > 3 & maxdata_l > 11){
      
      test_df <- expand.grid(data_l = maxdata_l,
                             move_l = 0:gap_length,
                             cor = NA,
                             abse = NA)
      
      temp %>% slice(strt:(strt + maxdata_l - 1)) %>% pull(at) -> att
      
      for(iii in 1:nrow(test_df)){
        # print(iii)
        
        temp %>% slice((strt+test_df[iii,"move_l"]):(strt + maxdata_l - 1 +test_df[iii,"move_l"])) %>%
          pull(md) -> mdt
        
        test_df[iii,"cor"] <- cor(mdt, att, use = "pairwise.complete.obs")
        test_df[iii,"abse"] <- mean(abs(mdt-att), na.rm = T)
        
      }
      
      test_df %>% mutate(fac = (1-cor)*abse) %>% pull(fac) %>% which.min() -> result
      
      temp %>% pull(at) -> ats
      move <- ats[strt:(strt + maxdata_l - 1)]
      ats[strt:(strt + maxdata_l - 1)] <- NA
      ats[(strt+test_df[result,"move_l"]):((strt + maxdata_l - 1)+test_df[result,"move_l"])] <- move
      temp$at <- ats
      
      temp %>% pull(arh) -> ats
      move <- ats[strt:(strt + maxdata_l - 1)]
      ats[strt:(strt + maxdata_l - 1)] <- NA
      ats[(strt+test_df[result,"move_l"]):((strt + maxdata_l - 1)+test_df[result,"move_l"])] <- move
      temp$arh <- ats
      
    }
  }
  
  temp$site <- i
  
  df6 <- bind_rows(df6, temp %>% select(site, datetime, at, arh, haxo_probl))
  
}

df6 %>% filter(!is.na(site)) -> df6
# fwrite(df6, "temp.csv")
# Combine with previous year data

old <- fread("data/haxo_data_corrected_2020.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2"))

df7 <- bind_rows(old, df6) %>% 
  arrange(site, datetime)

df7 %>% filter(duplicated(df7 %>% select(site, datetime)))
df7 %>% filter(!duplicated(df7 %>% select(site, datetime))) -> df7

# Get rid of leading and trailing NAs
df8 <- data.table()
for(i in unique(df7$site)){
  
  df7 %>% filter(site == i) -> temp
  
  temp %>% mutate(grp = rleid(ifelse(is.na(at), 0, 1))) %>% 
    group_by(grp) %>% 
    summarise(n = n(),
              val = mean(ifelse(is.na(at), 0, 1))) %>% 
    mutate(cumn = cumsum(n)) -> grps
  
  if(tail(grps$val,1) == 0){
    temp %>% slice(1:rev(grps$cumn)[2]) -> temp
  }
  
  if(grps$val[1] == 0){
    temp %>% slice(grps$cumn[1]+1:nrow(temp)) -> temp
  }
  
  df8 <- bind_rows(df8, temp)
}


# PLOT

df8 %>% filter(haxo_probl == 0) %>% 
  group_by(datetime) %>% 
  summarise(md = median(at, na.rm = T),
            md_arh = median(arh, na.rm = T)) %>% 
  ungroup() -> md

pdf("visuals/Haxo_corrected_combined.pdf", 12, 8)
for(i in unique(df8$site)){
  # i <- 53
  print(i)
  
  df8 %>% filter(site == i) %>% 
    left_join(., md) -> temp
  
  temp %>% ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "md"), col = "gray60", size = 1) +
    geom_line(aes_string(y = "at"), col = "blue4") +
    theme_minimal() +
    ylab("Temperature") + xlab("Date")+
    scale_y_continuous(limits = c(-25, 35))+
    ggtitle(i) -> GG1
  
  temp %>% ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "md_arh"), col = "gray60", size = 1) +
    geom_line(aes_string(y = "arh"), col = "blue4") +
    theme_minimal() +
    ylab("Relative humidity") + xlab("Date")+
    scale_y_continuous(limits = c(20, 100))+
    ggtitle(i) -> GG2
  
  print(plot_grid(plotlist = list(GG1,GG2), nrow = 2))
  
}
dev.off()


fwrite(df8, "output/haxo_data_corrected.csv")

