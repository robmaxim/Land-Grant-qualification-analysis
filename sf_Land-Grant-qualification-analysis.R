library(tidyverse)
library(geodist)
library(sf)


# Load longitude and latitude data for all RPUs in the US
RPUs_long_lat <- read_csv("data/All-RPUs_long-lat.csv", col_types = 
           cols("Longitude location of institution (HD2018)" = col_character(),
                "Latitude location of institution (HD2018)" = col_character()))

RPUs_long_lat

# Convert RPU column names from what they were on the IPEDS download
RPUs_long_lat <- RPUs_long_lat %>%
  rename(longitude = "Longitude location of institution (HD2018)") %>%
  rename(latitude = "Latitude location of institution (HD2018)") %>%
  rename(state = "State abbreviation (HD2018)")

RPUs_long_lat

# Add column indicating they're NOT flagships or R1s
RPUs_long_lat <- RPUs_long_lat %>%
  add_column(RPU = 1, .before = "longitude")


# Load longitude and latitude data for all flagships and R1 campuses in the US
R1s_long_lat <- read_csv("data/All-flagship-R1s_long-lat.csv", col_types =
                          cols("Longitude location of institution (HD2018)"
                               = col_character(),
                               "Latitude location of institution (HD2018)"
                               = col_character()))

R1s_long_lat

# Convert flagship and R1 column names from what they were on the IPEDS download
R1s_long_lat <- R1s_long_lat %>%
  rename(longitude = "Longitude location of institution (HD2018)") %>%
  rename(latitude = "Latitude location of institution (HD2018)") %>%
  rename(state = "State abbreviation (HD2018)")

# Add column indicating they're flagships or R1s
R1s_long_lat <- R1s_long_lat %>%
  add_column(RPU = 0, .before = "longitude")


# Combine all of the public universities into one dataframe
Unis_long_lat <- bind_rows(RPUs_long_lat, R1s_long_lat)


# Convert the longitude/latitude points into geometry 
Unis_long_lat_sf <- st_as_sf(
  Unis_long_lat,
  coords = c("longitude", "latitude"),
  crs = 4326)

Unis_long_lat_sf

st_is_longlat(Unis_long_lat_sf)


# Measure university distances from one another
dist.mat <- st_distance(Unis_long_lat_sf)

num.60mile <- apply(dist.mat, 1, function(x) {sum(x < 96560.6) - 1})


# Calculate nearest neighbor distance and create index of nearest neighbors
nn.dist <- apply(dist.mat, 1, function(x) {return(sort(x, partial = 2) [2])})

nn.index <- apply(dist.mat, 1, function(x) { order(x, decreasing=F) [2] })

n.data <- Unis_long_lat
colnames(n.data)[1] <- "neighborID"
colnames(n.data)[2:ncol(n.data)] <- paste0("n.", colnames(n.data)[2:ncol(n.data)])


# Create new data frame showing number of public universities within 60 miles and the
# closest neighboring public university
Unis_qualification <- data.frame(Unis_long_lat,
                                 radius60miles = num.60mile,
                                 n.distance = nn.dist,
                                 n.data[nn.index, ])
rownames(Unis_qualification) <- seq(nrow(Unis_qualification))

Unis_qualification

# Convert nearest neighbor distance from meters
Unis_qualification <- mutate(.data = Unis_qualification, 
                             n.distance = n.distance * 0.000621371)


# Filter to remove flagships and R1s from data
RPUs_qualification <- Unis_qualification %>%
  filter(RPU == 1)

RPUs_qualification

# ------------------------------------------------------------------------------------

# Counting just the number of schools that are more than 60 miles from the next
# closest 4-year public university

# Count number of RPUs with no public 4-year universities within 60 miles
count(RPUs_qualification, radius60miles)

# Filter to just show the 68 RPUs with no public 4-year neighbors within 60 miles
RPUs_n.60 <- RPUs_qualification %>%
  filter(radius60miles == 0)

# ------------------------------------------------------------------------------------

# Calculating admit rates of RPUs

# Load admissions data
RPUs_admissions <- read_csv("data/All-RPUs_admissions.csv")

# Rename columns
RPUs_admissions <- RPUs_admissions %>%
  rename(applications = "Applicants total (ADM2018)") %>%
  rename(admissions = "Admissions total (ADM2018)")

# Create new column for admit rate
RPUs_admissions <- mutate(.data = RPUs_admissions, 
                          admit_rate = admissions/applications)

# Check number of schools with admit rate of >= 80%
RPUs_admissions %>%
  filter(admit_rate >= .8)

# -----------------------------------------------------------------------------------

# Calculating which schools qualify based on having an admit rate of at least 80% or
# being at least 60 miles away from the next closest public university and having an
# admit rate of no less than 40%

# Join admissions data to RPU distance data
RPUs_dist_admit <- left_join(x = RPUs_qualification, y = RPUs_admissions, 
                             by = "UnitID")

# Filter data frame to keep just essential information
RPUs_dist_admit <- select(RPUs_dist_admit, -c(longitude, latitude, neighborID, 
                            "n.Institution.Name", "n.state", "n.RPU", "n.longitude", 
                            "n.latitude", "Institution Name", applications, 
                            admissions))

# Filter data to keep only schools that have an admit rate of >= 80% or that are more
# than 60 miles from the next closest public university but don't have an admit rate
# of less than 40%
RPUs_dist_admit_filtered <- filter(.data = RPUs_dist_admit, radius60miles == 0 | 
                                     admit_rate >= 0.8, !admit_rate < 0.4)

write_csv(RPUs_dist_admit_filtered, "qualified-RPUs_admit-distance.csv")




       