library(tidyverse)

rm(list=ls())

# loading data from stations

dadose=read.table("data/estacoes_coordenadas.csv",sep=";",header=T)

ro=order(dadose[,2])
dadose=dadose[ro,]
coordi=dadose[,c(5,4,2)]

moved_row <- coordi %>% dplyr::slice(4)
coordi_modified <- coordi %>% slice(-4)

coordi_new <- coordi_modified %>% 
  slice(0:(2- 1)) %>%  
  bind_rows(moved_row) %>%         
  bind_rows(coordi_modified %>% slice(2:n())) #

coordi_new <- coordi_new %>% 
  mutate(location_id = 1:27)

# loading data from streams

lista=as.matrix(read.table("data/listaresb.txt",sep="\t"))

anosf=1965:2023
indexf=c()
for (k in 1:length(anosf)){
  indexf=c(indexf,paste(rep(anosf[k],12),"-",1:12,sep=""))
}

timef=seq(1,length(indexf))

Tamanho=0

lindex=list()
tindex=list()
ldados=list()
lanos=list()

Tini=1
Tfim=1
for (j in 1:dim(lista)[1]){
  #if(j==21) next
  namef=paste("data/",lista[j],sep="")
  data1=read.table(namef,na.strings = "NULL")
  anos=data1[,1]
  dados=data1[,2:13]
  dadosm1=matrix(as.matrix(dados),dim(dados)[1]*dim(dados)[2],1)
  dadosm1[dadosm1<=0]=NA
  ldados[[j]]=dadosm1
  Tamanho=Tamanho+dim(dados)[1]*dim(dados)[2]
  Tfim=Tamanho
  tindex[[j]]=Tini:Tfim
  Tini=Tamanho+1
  index=c()
  for (k in 1:length(anos)){
    index=c(index,paste(rep(anos[k],12),"-",1:12,sep=""))
  }
  lindex[[j]]=index
}

library(RANN) 

data_df <- purrr::map_dfr(
  1:nrow(coordi_new),
  ~ data.frame(
    gauge_id = coordi_new$location_id[.x],
    year = unlist(lindex[[.x]]),
    value = as.numeric(unlist(ldados[[.x]]))
  )
)

data_df <- data_df %>% 
  group_by(gauge_id) %>% 
  mutate(series_time = row_number()) %>%
  ungroup()

data_df <- data_df %>%
  separate(year, into = c("data1", "data2"), sep = "-", remove = FALSE) %>%
  mutate(
    data2 = sprintf("%02d", as.numeric(data2)),  # Add leading zero
    year = paste(data1, data2, sep = "-")
  ) %>%
  dplyr::select(-data1, -data2)

start_date <- "1965-01"
end_date <- "2023-12"

start_date <- as.Date(paste0(start_date, "-01"))
end_date <- as.Date(paste0(end_date, "-01"))

# Fill missing values with NA
data_filled <- data_df %>% 
  group_by(year) %>% 
  mutate(
    time_index = cur_group_id()
  ) %>% 
  ungroup()

data_filled$value[is.na(data_filled$value)] <- NA  

data_filled$value <- as.numeric(data_filled$value)

data_filled <- data_filled %>% arrange(time_index)

data_filled <-  data_filled %>%
  mutate(index = row_number(),
         #gauge_id = as.factor(gauge_id)
  )

for(i in unique(data_filled$gauge_id)) {
  varname <- paste0("time", i,"_index")
  data_filled[[varname]] <- ifelse(data_filled$gauge_id == i, data_filled$time_index,NA)
}

series_ids <- unique(data_filled$gauge_id)  # should be 27 unique values
n <- nrow(data_filled)

y_matrix <- matrix(NA, nrow = n, ncol = length(series_ids))

# Fill each column only where that series_id appears
for (i in seq_along(series_ids)) {
  y_matrix[, i] <- ifelse(data_filled$gauge_id == series_ids[i],
                          data_filled$value,
                          NA)
}

save(data_filled,y,file ='multavariate_input.RData')
