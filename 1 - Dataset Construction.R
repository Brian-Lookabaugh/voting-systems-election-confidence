########################################################################
############################ Load Packages #############################
########################################################################

pacman::p_load(
  "dplyr", # Data Management
  "tidyr", # Reshaping Wide Data
  "haven", # Importing SPSS/Stata Files
  "writexl", # Export R Data Frame to Spreadsheet
  "stringr", # Dealing With Strings
  "tidycensus", # Directly Access Census Bureau Data
  "priceR", # Adjusting for Inflation
  install = FALSE
)

# Turn Off Scientific Notation for FIPS Codes
options(scipen = 999)

########################################################################
######################### Load and Clean SPAE ##########################
########################################################################

spae.2022 <- read_sav("SPAE_2022.sav")
spae.2020 <- read_sav("SPAE_2020.sav")
spae.2016 <- read_dta("SPAE_2016.dta")
spae.2014 <- read_dta("SPAE_2014.dta")
spae.2012 <- read_dta("SPAE_2012.dta")

spae.2022 <- spae.2022 %>%
  # Create Year ID
  mutate(year = 2022) %>%
  # Convert FIPS Codes to Numeric
  mutate(countyfips = as.numeric(countyfips),
  # Convert Individual Race to White/Non-White
         inv.race = ifelse(race == 1, 1, 0),
  # Convert Individual Education to BA/No BA
          inv.educ = ifelse(educ >= 5, 1, 0)) %>%
  # Filter Only In-Person Voters
  filter(Q1 == 6 & Q4 < 3) %>%
  # Order Variables
  dplyr::select(countyfips, countyname, year, Q39, Q40, Q41, Q42, birthyr, inv.race, gender, marstat, inv.educ, ideo5) %>%
  # Rename Columns
  rename(FIPSCode = countyfips,
         conf.personal = Q39,
         conf.county = Q40,
         conf.state = Q41,
         conf.nation = Q42)

spae.2020 <- spae.2020 %>%
  # Create Year ID
  mutate(year = 2020) %>%
  # Convert FIPS Codes to Numeric
  mutate(countyfips = as.numeric(countyfips),
         # Convert Individual Race to White/Non-White
         inv.race = ifelse(race == 1, 1, 0),
         # Convert Individual Education to BA/No BA
         inv.educ = ifelse(educ >= 5, 1, 0)) %>%
  # Filter Only In-Person Voters
  filter(Q1 == 6 & Q4 < 3) %>%
  # Order Variables
  dplyr::select(countyfips, countyname, year, Q44, Q45, Q46, Q47, birthyr, inv.race, gender, marstat, inv.educ, ideo5) %>%
  # Rename Columns
  rename(FIPSCode = countyfips,
         conf.personal = Q44,
         conf.county = Q45,
         conf.state = Q46,
         conf.nation = Q47)

spae.2016 <- spae.2016 %>%
  # Create Year ID
  mutate(year = 2016) %>%
  # Convert FIPS Codes to Numeric
  mutate(countyfips = as.numeric(countyfips),
         # Convert Individual Race to White/Non-White
         inv.race = ifelse(race == 1, 1, 0),
         # Convert Individual Education to BA/No BA
         inv.educ = ifelse(educ >= 5, 1, 0)) %>%
  # Filter Only In-Person Voters
  filter(Q1 == 6 & Q4 < 3) %>%
  # Order Variables
  dplyr::select(countyfips, countyname, year, Q33, Q34, Q35, Q36, birthyr, inv.race, gender, marstat, inv.educ, ideo5) %>%
  # Rename Columns
  rename(FIPSCode = countyfips,
         conf.personal = Q33,
         conf.county = Q34,
         conf.state = Q35,
         conf.nation = Q36)

spae.2014 <- spae.2014 %>%
  # Create Year ID
  mutate(year = 2014) %>%
  # Convert FIPS Codes to Numeric
  mutate(countyfips = as.numeric(countyfips),
         # Convert Individual Race to White/Non-White
         inv.race = ifelse(race == 1, 1, 0),
         # Convert Individual Education to BA/No BA
         inv.educ = ifelse(educ >= 5, 1, 0)) %>%
  # Filter Only In-Person Voters
  filter(Q1 == 6 & Q4 < 3) %>%
  # Order Variables
  dplyr::select(countyfips, countyname, year, Q33, Q34, Q35, Q36, birthyr, inv.race, gender, marstat, inv.educ, ideo5) %>%
  # Rename Columns
  rename(FIPSCode = countyfips,
         conf.personal = Q33,
         conf.county = Q34,
         conf.state = Q35,
         conf.nation = Q36)

spae.2012 <- spae.2012 %>%
  # Create Year ID
  mutate(year = 2012) %>%
  # Convert FIPS Codes to Numeric
  mutate(countyfips = as.numeric(countyfips),
         # Convert Individual Race to White/Non-White
         inv.race = ifelse(race == 1, 1, 0),
         # Convert Individual Education to BA/No BA
         inv.educ = ifelse(educ >= 5, 1, 0)) %>%
  # Filter Only Voters
  filter(q1 == 6 & q4 < 3) %>%
  # Order Variables
  dplyr::select(countyfips, countyname, year, q25, q26, q27, q28, birthyr, inv.race, gender, marstat, inv.educ, ideo5) %>%
  # Rename Columns
  rename(FIPSCode = countyfips,
         conf.personal = q25,
         conf.county = q26,
         conf.state = q27,
         conf.nation = q28)

# Merge the SPAE Data Sets Into One SPAE Panel Data Set
spae <- bind_rows(spae.2022, spae.2020, spae.2016, spae.2014, spae.2012)

# Second, Modify a Couple of Things in the SPAE As Well for Merging
spae <- spae %>%
  # Filter Missing County Cases
  filter(!is.na(FIPSCode)) %>%
  # Re-Code Alaska Cases At the State, Rather Than Borough-Level To Match Verified Voting
  mutate(FIPSCode = if_else(str_detect(countyname, "AK"),
                              2000,
                              FIPSCode))

# Remove Individual SPAE Data Sets
rm(spae.2022, spae.2020, spae.2016, spae.2014, spae.2012)

########################################################################
###################### Load, Clean, and Merge VV #######################
########################################################################

vv.2022 <- read.csv("vv.off.2022.csv", header = TRUE)
vv.2020 <- read.csv("vv.off.2020.csv", header = TRUE)
vv.2016 <- read.csv("vv.off.2016.csv", header = TRUE)
vv.2014 <- read.csv("vv.off.2014.csv", header = TRUE)
vv.2012 <- read.csv("vv.off.2012.csv", header = TRUE)

# Create Year ID Variables
vv.2012 <- vv.2012 %>%
  mutate(year = 2012)

vv.2014 <- vv.2014 %>%
  mutate(year = 2014)

vv.2016 <- vv.2016 %>%
  mutate(year = 2016)

vv.2020 <- vv.2020 %>%
  mutate(year = 2020)

vv.2022 <- vv.2022 %>%
  mutate(year = 2022)

# Merge the Data Sets Together
vv <- bind_rows(vv.2022, vv.2020, vv.2016, vv.2014, vv.2012)

# Remove Individual VV Data Sets
rm(vv.2022, vv.2020, vv.2016, vv.2014, vv.2012)

vv <- vv %>%
  # Re-Code Strings That Differ from the VV Dashboard
  mutate(Marking.Method = case_when(
    Marking.Method == "Vote Center Jurisdiction, Ballot Marking Devices for all voters" ~ "Ballot Marking Devices for all voters",
    Marking.Method == "All Mail Ballot Jurisdiction, Election Day Vote Centers: Ballot Marking Devices for all voters" ~ "Ballot Marking Devices for all voters",
    Marking.Method == "Hand marked paper ballots, BMDs and DREs with VVPAT" ~ "Hand marked paper ballots and BMDs",
    Marking.Method == "Hand marked paper ballots, No accessible equipment" ~ "Hand marked paper ballots and BMDs",
    Marking.Method == "Vote Center Jurisdiction, Hand marked paper ballots and BMDs" ~ "Hand marked paper ballots and BMDs",
    Marking.Method == "All Mail Ballot Jurisdiction, Election Day Vote Centers: Hand marked paper ballots and BMDs" ~ "Hand marked paper ballots and BMDs",
    Marking.Method == "Vote Center Jurisdiction, DREs with VVPAT for all voters" ~ "Hand marked paper ballots and DREs with VVPAT",
    Marking.Method == "All Mail Ballot Jurisdiction, Election Day Vote Centers: DREs with VVPAT" ~ "Hand marked paper ballots and DREs with VVPAT",
    Marking.Method == "All Mail Ballot Jurisdiction, Election Day Vote Centers: Hand marked paper ballots and DREs with VVPAT" ~ "Hand marked paper ballots and DREs with VVPAT",
    Marking.Method == "Vote Center Jurisdiction, Hand marked paper ballots and DREs with VVPAT" ~ "Hand marked paper ballots and DREs with VVPAT",
    Marking.Method == "Vote Center Jurisdiction, Hand marked paper ballots and DREs without VVPAT" ~ "Hand marked paper ballots and DREs without VVPAT",
    Marking.Method == "Hand marked paper ballots; Direct recording assistive interface without VVPAT for accessibility" ~ "Hand marked paper ballots and DREs without VVPAT",
    Marking.Method == "All Mail Ballot Jurisdiction, Election Day Vote Centers: Hand marked paper ballots; Direct recording assistive interface without VVPAT for accessibility" ~ "Hand marked paper ballots and DREs without VVPAT",
    Marking.Method == "All Mail Ballot Jurisdiction, Election Day Vote Centers: Hand marked paper ballots and DREs without VVPAT" ~ "Hand marked paper ballots and DREs without VVPAT",
    Marking.Method == "Vote Center Jurisdiction, DREs without VVPAT for all voters" ~ "DREs without VVPAT for all voters",
    Marking.Method == "All Mail Ballot Jurisdiction, Election Day Vote Centers: DREs without VVPAT" ~ "DREs without VVPAT for all voters",
    Marking.Method == "not available" | Marking.Method == "" ~ NA,
    TRUE ~ Marking.Method
  )) %>%
  # Create Voting Systems Tier Variable
  mutate(vs.tier.dc = case_when(
           Marking.Method == "Hand marked paper ballots and BMDs" ~ 1,
           Marking.Method == "Ballot Marking Devices for all voters" ~ 2,
           Marking.Method == "Hand marked punch card ballots and BMDs" ~ 3,
           (Marking.Method == "DREs with VVPAT for all voters" | Marking.Method == "Hand marked paper ballots and DREs with VVPAT") ~ 4,
           (Marking.Method == "DREs without VVPAT for all voters" | Marking.Method == "Hand marked paper ballots and DREs without VVPAT") ~ 5
         ),
         vs.tier.kg = case_when(
           Marking.Method == "Hand marked paper ballots and BMDs" ~ 1,
           Marking.Method == "Ballot Marking Devices for all voters" ~ 2,
           (Marking.Method == "DREs with VVPAT for all voters" | Marking.Method == "Hand marked paper ballots and DREs with VVPAT") ~ 3,
           Marking.Method == "Hand marked punch card ballots and BMDs" ~ 4,
           (Marking.Method == "DREs without VVPAT for all voters" | Marking.Method == "Hand marked paper ballots and DREs without VVPAT") ~ 5
         ),
         vs.tier.dva = case_when(
           Marking.Method == "Ballot Marking Devices for all voters" ~ 1,
           Marking.Method == "Hand marked paper ballots and BMDs" ~ 2,
           (Marking.Method == "Hand marked paper ballots and DREs with VVPAT" | Marking.Method == "DREs with VVPAT for all voters") ~ 3,
           Marking.Method == "Hand marked punch card ballots and BMDs" ~ 4,
           Marking.Method == "Hand marked paper ballots and DREs without VVPAT" ~ 5,
           Marking.Method == "DREs without VVPAT for all voters" ~ 6
         )) %>%
  # Create Voting System Upgrade Variables
  arrange(FIPS.code, year) %>%
  group_by(FIPS.code) %>%
  mutate(vsu.dc = ifelse(vs.tier.dc < lag(vs.tier.dc, default = first(vs.tier.dc)), 1, 0),
         vsu.kg = ifelse(vs.tier.kg < lag(vs.tier.kg, default = first(vs.tier.kg)), 1, 0),
         vsu.dva = ifelse(vs.tier.dva < lag(vs.tier.dva, default = first(vs.tier.dva)), 1, 0),
         any.change = ifelse(Marking.Method != lag(Marking.Method, default = first(Marking.Method)), 1, 0)) %>%
  ungroup() %>%
  # Modify FIPS Codes to Align with SPAE Values
  mutate(FIPS.code = as.numeric(substr(as.character(FIPS.code), 1, nchar(FIPS.code) - 5))) %>%
  # Modify DC FIPS Code 
  mutate(FIPS.code = ifelse(FIPS.code == 11000, 11001, FIPS.code)) %>%
  # Keep Select Columns
  dplyr::select(FIPS.code, year, vsu.dc, vsu.kg, vsu.dva, any.change) %>%
  # Collapse to the County-Level
  group_by(FIPS.code, year) %>%
  summarise(
    FIPS.code = max(FIPS.code),
    year = max(year),
    vsu.dc = max(vsu.dc),
    vsu.kg = max(vsu.kg),
    vsu.dva = max(vsu.dva),
    any.change = max(any.change)
  )

# Merge with the SPAE-VV Data
spae.vv <- left_join(spae, vv, by = c("FIPSCode" = "FIPS.code", "year"))

########################################################################
##################### Load, Clean, and Merge ACS #######################
########################################################################

# Note That 2018 ACS Data Is Not Included Because There Is No 2018 SPAE Data
# A Census API Key is Needed to Access the Data - This Is My Personal Key
census_api_key("24e38cd6da4443e1edec90415cb7d5ef87335e6a", overwrite = TRUE)

acs.2022 <- get_acs(geography = "county",
                    variables = c("DP02_0065PE", # Percent with Bachelors 25+
                                  "DP05_0018E", # Median Age
                                  "DP05_0033E", # Total Population
                                  "DP05_0037E", # Total - One Race - White
                                  "S1901_C01_012E" # Household Median Income
                                  ),
                    year = 2022,
                    output = "wide")

acs.2020 <- get_acs(geography = "county",
                    variables = c("DP02_0068E", # Number with Bachelors 25+
                                  "DP05_0018E", # Median Age
                                  "DP05_0033E", # Total Population
                                  "DP05_0037E", # Total - One Race - White
                                  "S1901_C01_012E" # Household Median Income
                    ),
                    year = 2020,
                    output = "wide")

acs.2016 <- get_acs(geography = "county",
                    variables = c("B15003_022E", # Number with Bachelors 25+
                                  "B15003_023E", # Number with Masters 25+
                                  "B15003_024E", # Number with Professional School Degree 25+
                                  "B15003_025E", # Number with Doctorate 25+
                                  "B23013_001E", # Median Age
                                  "C02003_001E", # Total Population
                                  "C02003_003E", # Total - One Race - White
                                  "B19013_001E" # Median Household Income
                    ),
                    year = 2016,
                    output = "wide")

acs.2014 <- get_acs(geography = "county",
                    variables = c("B15003_022E", # Number with Bachelors 25+
                                  "B15003_023E", # Number with Masters 25+
                                  "B15003_024E", # Number with Professional School Degree 25+
                                  "B15003_025E", # Number with Doctorate 25+
                                  "B01002_001E", # Median Age
                                  "C02003_001E", # Total Population
                                  "C02003_003E", # Total - One Race - White
                                  "B19013_001E" # Median Household Income
                    ),
                    year = 2014,
                    output = "wide")

acs.2012 <- get_acs(geography = "county",
                    variables = c("B15003_022E", # Number with Bachelors 25+
                                  "B15003_023E", # Number with Masters 25+
                                  "B15003_024E", # Number with Professional School Degree 25+
                                  "B15003_025E", # Number with Doctorate 25+
                                  "B01002_001E", # Median Age
                                  "C02003_001E", # Total Population
                                  "C02003_003E", # Total - One Race - White
                                  "B19013_001E" # Median Household Income
                    ),
                    year = 2012,
                    output = "wide")

# Clean The ACS Data
acs.2022 <- acs.2022 %>%
  # Create Year Variable
  mutate(year = 2022) %>%
  # Rename Variables
  rename(percent.bachelor = DP02_0065PE,
         med.age = DP05_0018E,
         total.pop = DP05_0033E,
         white.pop = DP05_0037E,
         med.income = S1901_C01_012E
  ) %>%
  # Create Percent White Variable
  mutate(percent.white = white.pop / total.pop) %>%
  # Order Variables
  dplyr::select(GEOID, NAME, year, percent.bachelor, med.age, total.pop, percent.white, med.income)

acs.2020 <- acs.2020 %>%
  # Create Year Variable
  mutate(year = 2020) %>%
  # Rename Variables
  rename(
    num.bachelor = DP02_0068E,
    med.age = DP05_0018E,
    total.pop = DP05_0033E,
    white.pop = DP05_0037E,
    med.income = S1901_C01_012E
  ) %>%
  # Create Percent White Variable
  mutate(percent.white = white.pop / total.pop,
         percent.bachelor = num.bachelor / total.pop) %>%
  # Order Variables
  dplyr::select(GEOID, NAME, year, percent.bachelor, med.age, total.pop, percent.white, med.income)

acs.2016 <- acs.2016 %>%
  # Create Year Variable
  mutate(year = 2016) %>%
  # Rename Variables
  rename(
    num.bachelor = B15003_022E,
    num.master = B15003_023E,
    num.professional = B15003_024E,
    num.doctor = B15003_025E,
    med.age = B23013_001E,
    total.pop = C02003_001E,
    white.pop = C02003_003E,
    med.income = B19013_001E
  ) %>%
  # Create Percentage Variables
  mutate(
    percent.bachelor = (num.bachelor + num.master + num.professional + num.doctor) / total.pop,
    percent.white = white.pop / total.pop
  ) %>%
  # Order Variables
  dplyr::select(GEOID, NAME, year, percent.bachelor, med.age, total.pop, percent.white, med.income)

acs.2014 <- acs.2014 %>%
  # Create Year Variable
  mutate(year = 2014) %>%
  # Rename Variables
  rename(
    num.bachelor = B15003_022E,
    num.master = B15003_023E,
    num.professional = B15003_024E,
    num.doctor = B15003_025E,
    med.age = B01002_001E,
    total.pop = C02003_001E,
    white.pop = C02003_003E,
    med.income = B19013_001E
  ) %>%
  # Create Percentage Variables
  mutate(
    percent.bachelor = (num.bachelor + num.master + num.professional + num.doctor) / total.pop,
    percent.white = white.pop / total.pop
  ) %>%
  # Order Variables
  dplyr::select(GEOID, NAME, year, percent.bachelor, med.age, total.pop, percent.white, med.income)

acs.2012 <- acs.2012 %>%
  # Create Year Variable
  mutate(year = 2012) %>%
  # Rename Variables
  rename(
    num.bachelor = B15003_022E,
    num.master = B15003_023E,
    num.professional = B15003_024E,
    num.doctor = B15003_025E,
    med.age = B01002_001E,
    total.pop = C02003_001E,
    white.pop = C02003_003E,
    med.income = B19013_001E
  ) %>%
  # Create Percentage Variables
  mutate(
    percent.bachelor = (num.bachelor + num.master + num.professional + num.doctor) / total.pop,
    percent.white = white.pop / total.pop
  ) %>%
  # Order Variables
  dplyr::select(GEOID, NAME, year, percent.bachelor, med.age, total.pop, percent.white, med.income)

# Merge These Into a Single Panel Data Set
acs <- bind_rows(acs.2022, acs.2020, acs.2016, acs.2014, acs.2012) %>%
  # Clean the Data Prior to Merging
  # Convert 2022 Percent Bachelor Variable to Percentage (xx.xx to .xx)
  mutate(percent.bachelor = ifelse(year == 2022, percent.bachelor / 100, percent.bachelor)) %>%
  # Re-Code Alaska FIPS Codes to 2000 to Merge with Existing Data
  mutate(GEOID = ifelse(str_detect(NAME, ", Alaska"), 2000, GEOID)) %>%
  # Reformat FIPS Codes
  mutate(GEOID = ifelse(str_starts(GEOID, "0"), str_replace(GEOID, "^0", ""), GEOID)) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  # Collapse Alaska Demographics to the State-Level
  group_by(GEOID, year) %>%
  summarise(
    percent.bachelor = mean(percent.bachelor),
    med.age = mean(med.age),
    total.pop = mean(total.pop),
    percent.white = mean(percent.white),
    med.income = mean(med.income)
  ) %>% 
    ungroup() %>%
  # Adjust Median Household Income to 2022 Dollars
  mutate(med.income = ifelse(year == 2012, adjust_for_inflation(acs.2012$med.income, 2012, "US", to_date = 2022), med.income),
         med.income = ifelse(year == 2014, adjust_for_inflation(acs.2014$med.income, 2014, "US", to_date = 2022), med.income),
         med.income = ifelse(year == 2016, adjust_for_inflation(acs.2016$med.income, 2016, "US", to_date = 2022), med.income),
         med.income = ifelse(year == 2020, adjust_for_inflation(acs.2020$med.income, 2020, "US", to_date = 2022), med.income)) %>%
  # Create Log-Transformed Median Income
  mutate(l.med.income = log(med.income))

# Remove Individual ACS Data Sets
rm(acs.2022, acs.2020, acs.2016, acs.2014, acs.2012)

# Merge Into SPAE-VV Data
spae.vv.acs <- left_join(spae.vv, acs, by = c("FIPSCode" = "GEOID", "year"))

########################################################################
################ Load, Clean, and Merge Ideology Data ##################
########################################################################

ideo.16 <- read.csv("2016_US_County_Level_Presidential_Results.csv", header = TRUE)
ideo.20 <- read.csv("2020_US_County_Level_Presidential_Results.csv", header = TRUE)

ideo.16 <- ideo.16 %>%
  dplyr::select(combined_fips, county_name, per_gop, county_name) %>%
  mutate(year = 2016) %>%
  # Re-Code Alaska
  mutate(combined_fips = ifelse(county_name == "Alaska", 2000, combined_fips))

ideo.20 <- ideo.20 %>%
  dplyr::select(county_fips, per_gop, state_name) %>%
  mutate(year = 2020) %>%
  # Re-Code Alaska
  mutate(county_fips = ifelse(state_name == "Alaska", 2000, county_fips))

# Combine This Data
ideo <- full_join(ideo.16, ideo.20, by = c("combined_fips" = "county_fips", "year", "per_gop"))

# Remove Individual Ideology Data Sets
rm(ideo.16, ideo.20)

ideo <- ideo %>%
  dplyr::select(-state_name, -county_name) %>%
  # Collapse Alaska
  group_by(combined_fips, year) %>%
  summarise(
    combined_fips = max(combined_fips),
    year = max(year),
    per.gop = max(per_gop)
  )

# Merge Into Master Data Frame
final <- full_join(spae.vv.acs, ideo, by = c("FIPSCode" = "combined_fips", "year"))

# Replace Midterm Years with Imputed Percent GOP Values
final <- final %>%
  arrange(FIPSCode, year) %>%
  group_by(FIPSCode) %>%
  mutate(per.gop = ifelse(year == 2022, per.gop[year == 2020],
                          ifelse(
                            year == 2014, per.gop[year == 2016],
                            ifelse(year == 2012, per.gop[year == 2016], per.gop)
                          ))) %>%
  ungroup() %>%
  # Filter Out Non-Merged Cases for SPAE
  filter(!is.na(conf.personal))

# Remove Prior Datasets
rm(acs, ideo, spae, spae.vv, spae.vv.acs, vv)
