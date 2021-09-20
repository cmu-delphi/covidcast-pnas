# This code calculates vaccine acceptance by race/ethnicity, using data files
# that are not yet available to users with a data use agreement. Information
# about obtaining access is available on our website, and will be updated when
# race/ethnicity data becomes available:
# https://cmu-delphi.github.io/delphi-epidata/symptom-survey/data-access.html

# Code by Marissa Reitsma.

library(dplyr)
library(data.table)
library(ggplot2)
library(tidycensus)
library(scales)

df <- fread("2021-03-race-ethnicity-microdata.csv.gz") %>%
  select(StartDatetime, fips, V1, V3, D6, D7, D2, D1, weight) %>%
  filter(!is.na(fips)) %>%
  mutate(fips = sprintf("%05d", fips),
         accepting = case_when(
           V1 == 1 ~ TRUE,
           V3 %in% 1:2 ~ TRUE,
           V3 %in% 3:4 ~ FALSE))

regions <- fread("data/regions.csv")

fips <- as.data.table(tidycensus::fips_codes)
fips <- fips[, fips := paste0(state_code, county_code)]

############################
## Prepare the indicators ##
############################

df <- merge(df, fips, by = "fips")
df <- merge(df, regions[, .(state_name, Region)], by = "state_name") # This excludes Puerto Rico

## Race/ethnicity
df <- df[D6 == 1, race_grp:="Hispanic"]
df <- df[D7 %like% "," & is.na(race_grp), race_grp := "Multiracial"]
df <- df[D7 %like% "1" & is.na(race_grp), race_grp := "American Indian or Alaska Native"]
df <- df[D7 %like% "2" & is.na(race_grp), race_grp := "Asian"]
df <- df[D7 %like% "3" & is.na(race_grp), race_grp := "Black"]
df <- df[D7 %like% "4" & is.na(race_grp), race_grp := "Native Hawaiian or other Pacific Islander"]
df <- df[D7 %like% "5" & is.na(race_grp), race_grp := "White"]
df <- df[D7 %like% "6" & is.na(race_grp), race_grp := "Other"]

## Age
df <- df[D2 == 1, age_cat_fb := "18-24"]
df <- df[D2 == 2, age_cat_fb := "25-34"]
df <- df[D2 == 3, age_cat_fb := "35-44"]
df <- df[D2 == 4, age_cat_fb := "45-54"]
df <- df[D2 == 5, age_cat_fb := "55-64"]
df <- df[D2 %in% 6:7, age_cat_fb := "65+"]

## Gender
df <- df[D1 == 1, gender := "Male"]
df <- df[D1 == 2, gender := "Female"]
df <- df[D1 %in% 3:4, gender := "Non-binary/self-describe"]

#############################
## Collapse into Summaries ##
#############################

# Analyze complete cases only
df <- df[!is.na(accepting) & !is.na(age_cat_fb) & !is.na(race_grp) & !is.na(gender)]

# Compute percent vaccinated or accepting
df <- df[, pct_accepting := weighted.mean(accepting, weight), by = c("Region", "race_grp", "gender", "age_cat_fb")]
nat <- copy(df)
nat <- nat[, pct_accepting := weighted.mean(accepting, weight), by = c("race_grp", "gender", "age_cat_fb")]
nat <- nat[, Region := "National"]

# Compute sample size by group
df <- df[, sample_size := .N, by = c("Region", "race_grp", "gender", "age_cat_fb")]
nat <- nat[, sample_size := .N, by = c("race_grp", "gender", "age_cat_fb")]

df <- unique(df[, .(pct_accepting, Region, race_grp, gender, age_cat_fb, sample_size)])
nat <- unique(nat[, .(pct_accepting, Region, race_grp, gender, age_cat_fb, sample_size)])

df <- rbind(df, nat)
df <- df[, age_cat_fb := factor(age_cat_fb, levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))]
df <- df[, Region := factor(Region,
                            levels = c("National", "West", "South", "Midwest", "Northeast"))]

# Respondents who indicate their gender as non-binary or self-described are not
# included, as these groups are typically too small in each region; when
# applying the sample size threshold of 50, many individual points are censored
# and the trends are difficult to interpret.
ggplot(df[race_grp %in% c("Asian", "Black", "Hispanic", "White") &
            gender %in% c("Female", "Male") &
            sample_size >= 50],
       aes(x = age_cat_fb, y = pct_accepting, color = race_grp, group = race_grp)) +
  geom_point(size = 2) +
  geom_line(size = 1.2) +
  facet_grid(cols = vars(Region), rows = vars(gender)) +
  theme_bw() +
  labs(x = "Age Group", y = "Vaccinated or accepting",
       color = "") +
  scale_y_continuous(labels = label_percent(accuracy = 1.0)) +
  scale_color_manual(breaks = c("Hispanic", "White", "Black", "Asian"),
                     values = c("Hispanic" = "#c42e31", "White" = "#832543",
                                "Asian" = "#6399AC", "Black" = "#e5a825")) +
  theme(text = element_text(size = 12), legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../paper/fig/age-race-vaccine-acceptance.pdf", width = 8, height = 5)
