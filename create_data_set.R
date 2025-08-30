library(spdep)
library(sf) 
library(raster)  
library(rasterVis) 
library(rgeos)
library(dismo)
library(INLA)
library(lattice)
library(gridExtra)
library(splancs)
library(fields)
library(tidyverse)
library(sp)
library(lubridate) 
library(purrr)
library(inlabru)
library(MetricGraph)

rm(list=ls())

# Creating the mesh 

rios=st_read("D:/Dropbox/vitor_fluviometria/espacial/GEOFT_BHO_REF_RIO/GEOFT_BHO_REF_RIO.shp")
dadose=read.table("D:/Dropbox/vitor_fluviometria/espacial/estacoes_coordenadas.csv",sep=";",header=T)


id=c(37203,39167,38444,44469,32842,39188, 1944,39982,40949,42555,40962,
     41771,41185,43913,6732,38643,38442,40107)

#id=c(37203,39167)
#plot(rios[id,])

udi=unique(c(id,40107,38643,37203,39167,38444,38442,44469, 6732,32842,39188,1944,39982,40949,42555,40962,41771,41185,43913))
#plot(rios[udi,])
#udi=unique(c(id,37203,39167))

# ordering the coordinates of the stations 
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

my_sp_points <- SpatialPoints(coords = coordi_new[, c("Longitude", "Latitude")], proj4string = CRS("+init=epsg:4326"))
my_sp_points <- spTransform(my_sp_points, CRS("+init=epsg:32723 +proj=utm +zone=23 +ellps=intl +units=km +no_defs")) 

my_sp_points <- SpatialPointsDataFrame(coords = coordi_new[, c("Longitude", "Latitude")], proj4string = CRS("+init=epsg:4326"), data = coordi_new[,3:4])
my_sp_points <- spTransform(my_sp_points, CRS("+init=epsg:32723 +proj=utm +zone=23 +ellps=intl +units=km +no_defs")) 

rios2=rios[udi,]

rios2_new <- rios2%>%
  select(-CORIO) 

rivers_sf <- st_as_sf(rios2_new, wkt = "geometry", crs = 32723)

rivers_sf <- rivers_sf %>%
  mutate(length_m = st_length(geometry))

rivers_sp <- as(rivers_sf, "Spatial")

osm.utm <- spTransform(rivers_sp, CRS("+init=epsg:32723 +proj=utm +zone=23 +ellps=intl +units=km +no_defs"))

line_buff <- gBuffer(osm.utm, width = 1, 
                     capStyle="SQUARE",
                     joinStyle="BEVEL")

osm_buff <- line_buff

mesh2 <- inla.mesh.2d(boundary = osm_buff,
                      cutoff = 1,
                      offset = c(0.1,1)
)

mesh2$n 

mesh.nw2 <- mesh2
plot(mesh.nw2)
points(my_sp_points, col = "red", pch = 19, cex = 1, )


#df <- coordi
#coordinates(df) <- ~ Longitude + Latitude  # Assign coordinates from columns "long" and "lat"
#proj4string(df) <- CRS("+init=epsg:32723 +proj=utm +zone=23 +ellps=intl +units=km +no_defs")  # Set CRS

#plot(df, pch = 20, col = "red", axes = TRUE, main = "SpatialPointsDataFrame")
#text(df$Longitude, df$Latitude, labels = df$Nome, pos = 4, cex = 0.8, col = "blue")

#plot(mesh.nw2, main = "Highlighted Triangle (ID = 10)", col = "gray")
#triangle_id <- 410
#vertices <- mesh.nw2$graph$tv[triangle_id, ]  # Get vertex indices
#coords <- mesh.nw2$loc[vertices, 1:2]         # Get coordinates of vertices

# Plot the specific triangle (polygon)
#polygon(coords, col = "red", border = "black", lwd = 10)

# loading data from streams

lista=as.matrix(read.table("D:/Dropbox/vitor_fluviometria/Base de dados - Cribari/Dados - Corregidos - Cribari/listaresb.txt",sep="\t"))

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
  namef=paste("D:/Dropbox/vitor_fluviometria/Base de dados - Cribari/Dados - Corregidos - Cribari/",lista[j],sep="")
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

#Creating the stack

library(RANN) 

points <- as_tibble(my_sp_points@coords) %>% 
  mutate(z = 0)
nearest <- nn2(mesh.nw2$loc, points, k = 1)

nearest_indices <- nearest$nn.idx[, 1]

positions <- mesh.nw2$loc[nearest_indices, ]

positions <- as_tibble(positions)

positions <- positions %>% 
  mutate(mesh_index = nearest_indices) %>% 
  rename( long = V1,
          lat = V2) %>% 
  dplyr::select(-V3) %>% 
  mutate( codigo = coordi_new$Nome,
          nome = coordi_new$Nome,
          gauge_id = coordi_new$location_id)

positions_2 <- positions %>% 
  dplyr::select(1,2,3)

#my_sp_points_test <- SpatialPoints(coords = positions[, c("long", "lat")], proj4string = CRS("+init=epsg:4326"))
#plot(mesh.nw2)
#points(my_sp_points_test, col = "red", pch = 19, cex = 1)
#points(my_sp_points, col = "green", pch = 19, cex = 1)

data_df <- purrr::map_dfr(
  1:nrow(positions),
  ~ data.frame(
    mesh_index = positions$mesh_index[.x],
    gauge_id = positions$gauge_id[.x],
    year = unlist(lindex[[.x]]),
    value = as.numeric(unlist(ldados[[.x]]))
  )
)

data_df <- data_df %>% 
  group_by(mesh_index) %>% 
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

# Create a full space-time grid for observed points
#full_grid <- expand.grid(mesh_index = nearest_indices,
#                         year = format(seq(as.Date(start_date), as.Date(end_date), by = "months"), "%Y-%m"))



# Merge original data with full grid
#data_filled <- full_grid %>%
#  left_join(data_df, by = c("mesh_index", "year"))
#  arrange(mesh_index, year)  

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

data_filled <- data_filled %>% 
  left_join(positions_2, by = 'mesh_index')

data_filled <-  data_filled %>%
  mutate(index = row_number(),
         #gauge_id = as.factor(gauge_id)
  )

data_filled$spacetime_index <- ceiling(data_filled$time_index/ 36)

for(i in unique(data_filled$mesh_index)) {
  varname <- paste0("time", i,"_index")
  data_filled[[varname]] <- ifelse(data_filled$mesh_index == i, data_filled$time_index,NA)
}

coords_matrix <- as.matrix(data_filled[, c("long", "lat")])

series_ids <- unique(data_filled$gauge_id)  # should be 27 unique values
n <- nrow(data_filled)

y_matrix <- matrix(NA, nrow = n, ncol = length(series_ids))

# Fill each column only where that series_id appears
for (i in seq_along(series_ids)) {
  y_matrix[, i] <- ifelse(data_filled$gauge_id == series_ids[i],
                          data_filled$value,
                          NA)
}

# Define the SPDE model on the mesh
A_spatial <- inla.spde.make.A(mesh.nw2, loc = coords_matrix)

spde <- inla.spde2.matern(mesh = mesh.nw2)

spatial_index <- inla.spde.make.index("space", n.spde = mesh.nw2$n)
temporal_index <- inla.spde.make.index("time", n.spde = max(data_filled$time_index))


save(data_filled,spde,A_spatial,spatial_index,file ='multavariate_input.RData')
