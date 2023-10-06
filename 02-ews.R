## Compute basic early warning signals

library(tidyverse)
library(tictoc)
library(fpp3)
library(furrr)
library(fractaldim)
library(slider)

load("data/detrended_gpp.Rda")

## split again by pixel
tic()
out <- split(out, ~id) #1.1Gb as df, 1.2Gb as list
toc()

out[[1]]

# set up the rolloing window
window <- out[[1]] |> nrow() / 2
## test later if you need uniroot test and first-diff
early_warnings <- function(x){ # x is the dataset, y is the ts variable to calculate ews
    x <- x |> 
        mutate(
            ews_std = slider::slide_dbl(
                .x = remainder, .f = sd, na.rm = TRUE, .before = window, .after = 0, .complete = TRUE),
            ews_ac1 = slider::slide_dbl(
                .x = remainder, # turned back to pearson, kendall does not work better
                .f = function(x) cor(x,lag(x,1), use = "pairwise.complete.obs", "pearson"),
                .before = window, .after = 0, .complete = TRUE),
            ews_kur = slider::slide_dbl(
                .x = remainder, .f = moments::kurtosis, na.rm = TRUE, 
                .before = window, .after = 0, .complete = TRUE),
            ews_skw = slider::slide_dbl(
                .x = remainder, .f = function(x) abs(moments::skewness(x, na.rm = TRUE)), 
                .before = window, .after = 0, .complete = TRUE) ,
            # calculating fractal dimension here doubles the time but it's done.
            ews_fd = slider::slide_dbl(
                .x = remainder,
                .f = function(x) {
                    z <- fd.estimate(x, window.size = window, method = "madogram")$fd
                    return(as.vector(z))} , .before = window, .after = 0, .complete = TRUE)
        )
    return(x)
}

safe_ews <- safely(early_warnings)

## test on one pxl:
tic()
early_warnings(out[[1]])
toc() # 0.285 s

## apply to all in parallel
plan(multisession, workers = 10)

ews <- list()

tic()
ews <- future_map(out, safe_ews)
toc() # 617.558s, around 10mins!

ews <- transpose(ews)
is_ok <- map_lgl(ews$error, is.null)
all(is_ok) # all good no errors!

ews$result[[1]]

ews <- ews$result # remove error reports

# make a df again
tic()
ews <- ews |> bind_rows()
toc() # 683.689s | I think this takes long time because each df is grouped...mistake


tic()
ews <- split(ews, ~id)
toc() #119.561


save(ews, file = "data/ews_colombia.Rda")

