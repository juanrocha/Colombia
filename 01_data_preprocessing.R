# Compute the Fourier Transform for all pixels
library(tidyverse)
library(tictoc)
library(fpp3)
library(furrr)

load("data/Colombia_GPP_8d.Rda")

# df_dat |> 
#     mutate(fft = stats::fft(GPP))

tic()
df_dat <- df_dat |>
    mutate(longitude = as.character(longitude),
           latitude = as.character(latitude)) |> 
    unite(col = "id", latitude, longitude, sep = "_")
toc() # 36s

# tests with one pxl
out <- df_dat |> 
    filter(id == "12.3749995905_-71.6250004335") |> 
    arrange(time) |> 
    rownames_to_column() |> 
    mutate(rowname = as.numeric(rowname)) %>% 
    # (. works only with magrittr pipe, \(d) function(..., data = d)) with base pipe
    #lm(GPP ~ time, data=.)
    mutate(gpp = log10(GPP)) |> 
    mutate(gpp_dt= pracma::detrend(gpp))   
    
dcomp <- forecast::mstl(out$linear, s.window = 46/4 + 46)
timevec <- make_date("2001") + days(seq(0,5473,by=8))
dfts <- tsibble(time = timevec, x = rnorm(685), index = time)
dfts |> model(classical_decomposition(x ~ season(11.5))) |> 
    components()

## alternative time series decompositions
## I cannot use real time indexes because the raw data is not regular or periodically
## sample across years. Every year starts in 1-jan, meaning there is no 8 dates between
## the last and first observation after newyears. Use a fake index instead that preserves ordering.
out |> 
    #mutate(lag1 = slider::slide(time, diff, 0, 1))
    #mutate(time2= make_date("2001") + days(seq(0,5473, by =8)) ) |> 
    #build_tsibble(index = time2, interval = new_interval(year = 1, month = 1, day = 8)) 
    as_tsibble(key = "id", index = rowname) |> 
    mutate(gpp_dt = as.vector(gpp_dt)) |> 
    ## classical: years of 46 8day weeks, 4 seasons per year
    #model(classical_decomposition(GPP ~ season(11.5) + season(46))) |> 
    # Multiple seasonality with STL
    model(STL(gpp_dt~ season(period = 11.5) + season(46) )) |> 
    # Fourier
    #model(ARIMA(GPP ~ trend + fourier(period = 46, K = 10) + fourier(11.5, K = 5))) |> 
    components()

## make it into a function
decompose <- function(x){
    comps <- x |> 
        rownames_to_column() |> 
        mutate(rowname = as.numeric(rowname),
               gpp = log1p(GPP)) |> 
        # detrend
        mutate(gpp_dt = pracma::detrend(gpp)) |> 
        mutate(gpp_dt = as.vector(gpp_dt)) |>
        as_tsibble(key = "id", index = rowname) |> 
        model(STL(gpp_dt~ season(period = 11.5) + season(46) )) |> 
        components()
    
    return(comps |> select(-.model))
}

safe_decomp <- safely(decompose)


tic()
x <- safe_decomp(out)
toc() # 0.5s

timevec <- df_dat |> pull(time) |> unique()

tic()
df_dat <- split(df_dat , ~id)
toc() # 6.5s

plan(multisession, workers = 10)

out <- list()
out <- future_map(df_dat, safe_decomp, .progress = TRUE)


out <- transpose(out)

is_ok <- map_lgl(out$error, is.null)
all(is_ok) # TRUE: all are okay, no errors

tic()
out <- out$result |> 
    bind_rows()
toc()


save(out, file = "data/detrended_gpp.Rda")


#mutate(fft = stats::fft(GPP)) |> 
# df_dat |> 
#     filter(id == "12.3749995905_-71.6250004335") |> 
#     ggplot(aes(time, GPP)) +
#     geom_line() +
#     geom_abline(intercept = 1.104e+00, slope = -1.828e-10, col = "red")
