# New task: match everything

# 1. Start with the CAR-region data (for each parcel, you need a region)
# 2. label each of the three VNP folder datasets: [NB, Lavoura, VNP]
# 3. Get each of these three datasets into wide shape (one region per row)
#    and get NB into year-level columns.
# 4. Left-join all three to the CAR-region data
#       - Some CAR parcels might have no matches
#       - Some might have many matches (will have > 1 populated columns)

# Example column in NB: price_terra_pastagem_2008_nb

# Example: parcel A is in region macapá, and macapá is in NB and lavoura, but not VNP
# in the result set, you will have the following row

# parcel | region | price_2002_nb|... |price_2002_lavoura|...|price_2002_vnp|...
# A      |macapá  |  400         |... |300               |...|NA            |...

# 1. How many CAR parcels are still unmatched (absolute value and as a pct.)?
# 2. Compute mean difference in prices across sources (for this exercise only, average out land types for every year/source)
# 3. join up the VTN datasets across years (so one vtn dataset) and turn into wide format (each year one column)

df |> mutate(diff_2002 = abs(price_2002_nb - price_2002_vnp)) |> pull(diff_2002) |> mean(na.rm=TRUE)

 car_region_df <- left_join(nb) |> left_join(lavoura) |> left_join(vnp)