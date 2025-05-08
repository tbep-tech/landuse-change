library(tidyverse)

data(acres)

calc <- function(acres, yrsel, intertid){
  
  intertid <- c('Mangrove Forests', 'Salt Barrens', 'Salt Marshes')
  
  yrfilt <- acres %>% 
    ungroup() %>% 
    filter(name %in% c(yrsel[1], yrsel[2])) %>% 
    filter(HMPU_TARGETS %in% intertid)

  totint <- summarise(yrfilt, Acres = sum(Acres), .by = 'name')
  totintchg <- totint$Acres[2] - totint$Acres[1]
  totintchgper <- 100 * (totint$Acres[2] - totint$Acres[1]) / totint$Acres[1]

  tothab <- yrfilt %>%
    pivot_wider(names_from = name, values_from = Acres) %>% 
    rename(
      yr1 = as.character(!!yrsel[1]), 
      yr2 = as.character(!!yrsel[2])
    ) %>% 
    mutate(
      chg = yr2 - yr1 ,
      chgper = 100 * chg / yr1
    ) %>% 
    select(
      est = HMPU_TARGETS, 
      chg, 
      chgper
    )
  
  out <- tibble(
      est = 'total',
      chg = totintchg, 
      chgper = totintchgper
    ) %>% 
    bind_rows(tothab)
  
  return(out)
  
}

calc(acres, yrsel = c(1990, 2023))

