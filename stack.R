rm(list=ls())

library(fmesher)
library(inlabru)
library(INLA)
library(rSPDE)
library(MetricGraph)
library(sf)
library(sp)
library(tidyverse) 

num_cores <- parallel::detectCores() - 2

load("/home/vitorsr/data_fluviometria/net_metricgraph_input.RData")

graph$add_observations(
  data = df_data_2,
  normalized = TRUE,
  clear_obs = TRUE, 
  group = 'space_time_group',
  group_sep = '.'
)


t <- seq(from = 1, to = 236, length.out = 236)

mesh_time <- fm_mesh_1d(t)

st_graph <- rspde.spacetime(mesh_space = graph, 
                            mesh_time = mesh_time, 
                            alpha = 2,
                            beta = 1,
                            parameterization = "matern")


st_data_inla <- graph_data_rspde(st_graph, 
                                 name = "field", 
                                 time = "space_index",
                                 bru = FALSE)
                                 
A <- st_data_inla$basis 
 
stk_dat <- inla.stack(data = list( value = y_matrix), 
                      A = list(A,
                               1,
                               1) ,
                      effects = list(c(st_data_inla[["index"]], Intercept = 1),
                                     global = list(rwtimebasin =df_data_2$time_index,
                                                   seastimetbasin =df_data_2$time_index,
                                                   fgntimebasin =df_data_2$time_index),
                                     local_scaling =  list(mu1      = df_data_2$time1_index,
                                                           mu2      = df_data_2$time2_index,
                                                           mu3      = df_data_2$time3_index,
                                                           mu4      = df_data_2$time4_index,
                                                           mu5      = df_data_2$time5_index,
                                                           mu6      = df_data_2$time6_index,
                                                           mu7      = df_data_2$time7_index,
                                                           mu8      = df_data_2$time8_index,
                                                           mu9      = df_data_2$time9_index,
                                                           mu10     = df_data_2$time10_index,
                                                           mu11     = df_data_2$time11_index,
                                                           mu12     = df_data_2$time12_index,
                                                           mu13     = df_data_2$time13_index,
                                                           mu14     = df_data_2$time14_index,
                                                           mu15     = df_data_2$time15_index,
                                                           mu16     = df_data_2$time16_index,
                                                           mu17     = df_data_2$time17_index,
                                                           mu18     = df_data_2$time18_index,
                                                           mu19     = df_data_2$time19_index,
                                                           mu20     = df_data_2$time20_index,
                                                           mu21     = df_data_2$time21_index,
                                                           mu22     = df_data_2$time22_index,
                                                           mu23     = df_data_2$time23_index,
                                                           mu24     = df_data_2$time24_index,
                                                           mu25     = df_data_2$time25_index,
                                                           mu26     = df_data_2$time26_index,
                                                           mu27     = df_data_2$time27_index))
                                     )
                                                           
                                                           
save.image("/home/vitorsr/data_fluviometria/net_metricgraph_input_2.Rdata")
