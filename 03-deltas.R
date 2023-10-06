## calulate deltas
library(tidyverse)
library(tictoc)
library(fpp3)
library(furrr)
library(fractaldim)
library(slider)

load("data/ews_colombia.Rda")

extract_delta <- function(x) {
    d <- bind_rows (
        # maximum
        x |> 
            select(id, rowname, starts_with("ews")) |> 
            pivot_longer(starts_with("ews"), names_to = "ews", values_to = "value") |> 
            as_tibble() |> 
            group_by(ews) |> 
            mutate(is_max = value == max(value, na.rm = TRUE)) |> 
            filter(is_max == TRUE) |> select(-is_max) |> 
            mutate(type = "max") |> 
            #add_count() |> ## detecting cases where max returns more than one hit
            ## when that happens, filter selects the first instance in time. It's mainly FD
            filter(rowname == min(rowname)) , 
        # minimum
        x |> 
            select(id, rowname, starts_with("ews")) |> 
            pivot_longer(starts_with("ews"), names_to = "ews", values_to = "value") |> 
            as_tibble() |> 
            group_by(ews) |> 
            mutate(is_min = value == min(value, na.rm = TRUE)) |> 
            filter(is_min == TRUE) |> select(-is_min) |> 
            mutate(type = "min") |> 
            # selects first instance when diff == 0 many consecutive time steps
            filter(rowname == min(rowname))
    ) |> arrange(ews, rowname) |> 
        group_by(ews, id) |> 
        summarize(delta = diff(value), .groups = "drop") |> 
        pivot_wider(names_from = ews, values_from = delta)
    return(d)
}

# test
ews[[14]] |> extract_delta()

tic()
deltas <- map(ews, extract_delta)
toc() # 3379.525 s, 56mins

tic()
deltas <- bind_rows(deltas)
toc() # 0.3s


deltas |> 
    separate(id, sep = "_", into = c("lon", "lat")) |> 
    mutate(lon = as.numeric(lon), lat = as.numeric(lat)) |> 
    ggplot(aes(lat, lon)) +
    geom_tile(aes(fill = ews_std)) +
    scale_fill_viridis_c()

deltas |> 
    pivot_longer(cols = starts_with("ews"), names_to = "ews", values_to = "value") |> 
    ggplot(aes(value)) +
    geom_density() + 
    facet_wrap(~ews, scales = "free")


#save(deltas, file = "data/deltas.Rda")
