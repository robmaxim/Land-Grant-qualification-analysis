library(tidyverse)
library(geodist)
library(sf)

# Load longitude and latitude data for all RPUs in the US
RPUs_long_lat <- read_csv("data/All-RPUs_long-lat-CBSA.csv", col_types = 
           cols("Longitude location of institution (HD2018)" = col_character(),
                "Latitude location of institution (HD2018)" = col_character()))

RPUs_long_lat

# Convert RPU column names from what they were on the IPEDS download
RPUs_long_lat <- RPUs_long_lat %>%
  rename(longitude = "Longitude location of institution (HD2018)") %>%
  rename(latitude = "Latitude location of institution (HD2018)") %>%
  rename(state = "State abbreviation (HD2018)") %>%
  rename(CBSA = "Core Based Statistical Area (CBSA) (HD2018)")

RPUs_long_lat

# Add column indicating they're NOT flagships or R1s
RPUs_long_lat <- RPUs_long_lat %>%
  add_column(RPU = 1, .before = "longitude")

RPUs_long_lat

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

R1s_long_lat

# Add column indicating they're flagships or R1s
R1s_long_lat <- R1s_long_lat %>%
  add_column(RPU = 0, .before = "longitude")

R1s_long_lat

# Combine all of the public universities into one dataframe
Unis_long_lat <- bind_rows(RPUs_long_lat, R1s_long_lat)

Unis_long_lat

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


# Calculating which schools qualify based on having an admit rate of at least 80%;
# being at least 60 miles away from the next closest public university and having an
# admit rate of no less than 40%; or being in a distressed CBSA

# Join admissions data to RPU distance data
RPUs_dist_admit <- left_join(x = RPUs_qualification, y = RPUs_admissions, 
                             by = "UnitID")

# Filter data frame to keep just essential information
RPUs_dist_admit <- select(RPUs_dist_admit, -c(longitude, latitude, neighborID, 
                            "n.Institution.Name", "n.state", "n.RPU", "n.longitude", 
                            "n.latitude", "n.CBSA", "Institution Name", applications, 
                            admissions))

# Load Bartik distressed community data
distressed_comms <- read_csv("Bartik/Public use file on local labor market epop & fed distress grant.csv")

distressed_comms

# Rename CBSA column so it can be joined to distance and admission data and rename
# Distress indicator column
distressed_comms <- distressed_comms %>%
  rename(CBSA = "cbsacode") %>%
  rename(distressed = "Distress indicator 2014-18")

distressed_comms

# Join distressed communities data to distance and admissions data
RPUs_dist_admit_stress <- left_join(x = RPUs_dist_admit, y = distressed_comms, 
                                    by = "CBSA")

# Remove excess columns
RPUs_dist_admit_stress <- select(RPUs_dist_admit_stress, -c(psucz, geotype, "2000 Civilian prime-age employment",
                                  "2000 Civilian prime-age population", 
                                  "2000 Total Civilian Employment",
                                  "2000 Total Population (All ages)", 
                                  "2000 Civilian employment to population ratio for prime-age",
                                  "2000 natinoal average", "2000 differential",
                                  "2014-18 Civilian prime-age employment",
                                  "2014-18 Civilian prime-age population", 
                                  "2014-18 Total Civilian Employment",
                                  "2014-18 Total Population (All ages)",
                                  "2014-18 Civilian employment to population ratio for prime-age",
                                  "2014-18 national average", "2014-18 differential",
                                  "Cumulative population", "Cumulative pop percent",
                                  "Needed jobs for prime-age if fill half gap",
                                  "Needed increase in total employment at .4 effect",
                                  "Cumulative increase in employment",
                                  "Ratio to baseline employment",
                                  "Maximizing at 10% of baseline",
                                  "Cumul job with max at 10% of baseline",
                                  "Cost at $80K per job", "Cost per year per cap",
                                  "Cumulative cost", "Cumulative cost per year per cap",
                                  "Annual fed grant if 2/3rd fed share",
                                  "Cumulative federal grant", 
                                  "Federal grant per capita"))

# -----------------------------------------------------------------------------------

# Filter data to keep only schools that have an admit rate of >= 70%; that are more
# than 60 miles from the next closest public university but don't have an admit rate
# of less than 40%; or that are in a distressed community
RPUs_dist_admit_stress_filt <- filter(.data = RPUs_dist_admit_stress, 
                                      radius60miles == 0 | admit_rate >= 0.7 |
                                        distressed == 1)


# Filter data to exclude schools that have an admit rate below 50% unless they are in
# a distressed community or are more than 60 miles away from the next closest public
# college
RPUs_dist_admit_stress_filt <- filter(.data = RPUs_dist_admit_stress, 
                                      admit_rate >= 0.5 | distressed == 1 |
                                        radius60miles == 0)

write_csv(RPUs_dist_admit_filt, "qualified-RPUs_admit-dist-stress.csv")




       