library(flexdashboard)
library(ggplot2)
library(data.table)
library(formattable)
library(dplyr)
library(plotly)

asset.dt <- output.dt %>% 
  select(ds, amount, details) %>% 
  data.table(. , tmp = paste0(.$ds ,.$details)) %>% 
  group_by(tmp) %>% 
  summarise(total = sum(amount))

asset.dt <-  asset.dt %>% 
  data.table(ds = as.Date(substr(.$tmp, 1, 10)), amount = .$total, details = substr(.$tmp, 11, 20)) %>% 
  dcast(ds ~ details, value.var = "amount") %>% 
  data.frame()
asset.dt[is.na(asset.dt)] <- 0


for(i in 1:nrow(asset.dt)){
  for(j in 2:ncol(asset.dt))
    if(i != 1){
      asset.dt[i,j] <- asset.dt[i-1,j] + asset.dt[i,j]
    }
}
for(i in 1:nrow(asset.dt)){
  for(j in 3:ncol(asset.dt)){
    asset.dt[i,j] <- asset.dt[i,j-1] + asset.dt[i,j]
  }
}

asset.dt <- asset.dt %>% rbind(., .[nrow(.),])
asset.dt$ds[nrow(asset.dt)] <- Sys.Date() %>% as.Date()