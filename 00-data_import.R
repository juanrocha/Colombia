library(tidyverse)
library(stars)
library(fs)
library(tictoc)
## FluxCom 0.083deg res
## bbox Colombia: (-78.9909352282, -4.29818694419, -66.8763258531, 12.4373031682)
## source: https://gist.github.com/graydon/11198540

bb <- st_bbox(c(xmin = -78.9909352282, ymin = -4.29818694419, xmax = -66.8763258531, ymax= 12.4373031682))

fls <- dir_ls("/Users/juanrocha/Documents/Projects/DATA/Fluxcom/FluxCom_CarbonFluxes2017_RS_ensemble_4320-2160_8daily/")
fls

dat <- map(fls, read_ncdf, var = "GPP", proxy = TRUE)
dat

col <- map(dat, function(x) x[bb])

# col <- c(col[[1]], col[[2]], col[[3]], col[[4]], col[[5]],
#           col[[6]], col[[7]], col[[8]], col[[9]], col[[10]],
#           col[[11]], col[[12]], col[[13]], col[[14]], col[[15]],
#           along = 3)

dim(col[[1]]) # 146 lons, 202 lats, 46 time steps (8day-weeks)

# write_mdim(
#     x = col [[1]],
#     filename = "data/GPP_Colombia_2001.nc"
# )

##Doesn't work:
# write_mdim(
#     x = col,
#     filename = "data/GPP_Colombia.nc"
# )

## The data is less than 700MB in RAM, I can work with that in R
tic()
df_dat <- map(col, as.data.frame) |> 
    bind_rows() |> 
    as_tibble()
toc() # 18s
# First get rid of pixels for which all are NAs. Then impute NAs for pixels with
# some missing values
tic()
all_nas <- df_dat |> 
    group_by(longitude, latitude) |> 
    mutate(na = is.na(GPP)) |> 
    summarize(nas = all(na)) |> 
    filter(nas == TRUE)
toc() # 3s

# this reduces the dataset from 20M obs to 16.6M
tic()
df_dat <- df_dat |> 
    anti_join(all_nas)
toc() # 3s

df_dat |> filter(is.na(GPP)) # there is no missing values left to impute!

## save the data and continue in another script:
save(df_dat, file = "data/Colombia_GPP_8d.Rda")



