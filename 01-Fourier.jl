# set up packages:
using Pkg
Pkg.activate(".")
Pkg.add("RCall")
#Pkg.add(PackageSpec(url="https://github.com/esa-esdl/ESDL.jl"))
#Pkg.add(PackageSpec(url="https://github.com/esa-esdl/ESDLPlots.jl"))
Pkg.add("ColorSchemes");
Pkg.add("Plots");
Pkg.add("Statistics")
Pkg.add("NetCDF")
Pkg.add("FFTW")
Pkg.add("DataFrames")
Pkg.add("Polynomials")

using RCall, Plots, ColorSchemes;
using Statistics;
using NetCDF;
using FFTW;
using DataFrames
using ESDL;
using Polynomials: fit

## Read files
path = raw"Documents/Projects/DATA/Fluxcom/FluxCom_CarbonFluxes2017_RS_ensemble_4320-2160_8daily"

Pkg.installed()["RCall"]

pwd()
cd("/Users/juanrocha/Documents/Projects/DATA/Fluxcom/FluxCom_CarbonFluxes2017_RS_ensemble_4320-2160_8daily")

R"""
library(tidyverse)
fls <- fs::dir_ls($path) |>
    str_subset( pattern = "nc$")
"""

@rget fls


## This is a lazy AbstractArray that does not load data on memory
c = NetCDF.open(fls[1])
ncinfo(fls[1])
## This is the data:
t = @timed dat = ncread(fls[1], "GPP"); # 0.14s
t.time
## Extract lon and lat
lon = ncread(fls[1], "longitude");
lat = ncread(fls[1], "latitude");
size(dat)

## Colombian coordinates are (following R):
## 1213:1358 longitude (these are the cell numberes)
## 931:1132 latitude (cell index)

m = map(!isnan, dat);

# it will be useful to set missing values for later
function set_missing(x)
    ## this one is for boolean, but I need one for nans.
    if x == 0
        x = missing
    else
        x
    end
    return(x)
end

function set_missing_nan(x)
    ## this one is for boolean, but I need one for nans.
    if isnan(x)
        x = missing
    else
        x
    end
    return(x)
end

m1 = map(set_missing, m)
m[250,250] == 1