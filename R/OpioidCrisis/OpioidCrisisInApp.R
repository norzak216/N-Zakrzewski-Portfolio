library(tidyverse)
library(janitor)
library(lubridate)

#Pt. 1- Is the Appalachian region disproportionately affected by Opioid Epidemic? (within App. states)
app_overdose <- read.csv("/Users/nora/Desktop/DSS R Programming/Overdose_App.csv")
head(app_overdose)

#remove rows with NA (West Virginia)
overdose_clean <- app_overdose[!is.na(app_overdose$Overdose_NonApp), ]

str(overdose_clean) #we see Overdose_NonApp is a chr right now
overdose_clean$Overdose_NonApp <- as.numeric(as.character(overdose_clean$Overdose_NonApp))

#paired t-test to test for significance
t.test(overdose_clean$Overdose_App, overdose_clean$Overdose_NonApp, paired = TRUE)



#Pt. 2- Comparing the Epidemic to Rest of the Nation. Which States Have Managed the Crisis Best?
cdc <- read.csv("/Users/nora/Desktop/DSS R Programming/VSRR_Provisional_Drug_Overdose_Death_Counts.csv") %>%
  clean_names()
head(cdc)

#keep only necessary columns & values (indicator)
cdc_reduced <- cdc %>%
  select(state,
         year,
         month,
         indicator,
         data_value)
head(cdc_reduced)

cdc_filtered <- cdc_reduced %>%
  filter(indicator %in% c(
    "Number of Deaths",
    "Number of Drug Overdose Deaths"
  ))
head(cdc_filtered)

#turn indicator values into columns for future aggregation
cdc_wide <- cdc_filtered %>%
  pivot_wider(
    names_from = indicator,
    values_from = data_value
  )
head(cdc_wide)

#convert into numeric
cdc_wide <- cdc_wide %>%
  mutate(
    `Number of Deaths` = as.numeric(gsub(",", "", `Number of Deaths`)),
    `Number of Drug Overdose Deaths` =
      as.numeric(gsub(",", "", `Number of Drug Overdose Deaths`))
  )


#Aggregate by state and year (month to year)
cdc_yearly <- cdc_wide %>%
  group_by(state, year) %>%
  summarise(
    total_deaths = sum(`Number of Deaths`, na.rm = TRUE),
    overdose_deaths = sum(`Number of Drug Overdose Deaths`, na.rm = TRUE),
    .groups = "drop"
  )
head(cdc_yearly)

cdc_final <- cdc_yearly %>%
  mutate(
    overdose_rate = (overdose_deaths / total_deaths) * 100
  )
head(cdc_final)


#MAPPING
library(sf)
library(maps)

states_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

#adding state names to abbrv.
state_lookup <- tibble(
  state_abbr = state.abb,
  state_name = state.name
)

#map data
map_data <- states_map %>%
  left_join(
    cdc_final,
    by = c("ID" = "state_lower")
  )

ggplot(map_data) +
  geom_sf(aes(fill = overdose_rate), color = "white", size = 0.2) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(
    name = "Overdose Death Rate (%)",
    option = "plasma"
  ) +
  labs(
    title = "Drug Overdose Death Rate by State",
    subtitle = "United States, by Year"
  ) +
  theme_minimal()


#Which states had the largest decrease in overdose death rate?
#calculate change between first and last year
library(broom)

state_trends <- cdc_final %>%
  group_by(state, state_name) %>%
  do(
    tidy(lm(overdose_rate ~ year, data = .))
  ) %>%
  filter(term == "year") %>%
  select(state, state_name, slope = estimate, p_value = p.value)

managing_states <- state_trends %>%
  filter(
    slope < 0.05,
    p_value < 0.05
  ) %>%
  arrange(slope)
managing_states

#compare to appalachia
appalachia_states <- c(
  "AL", "GA", "KY", "MD", "MS", "NC", "NY",
  "OH", "PA", "SC", "TN", "VA", "WV"
)


managing_list <- managing_states$state


cdc_compare_groups <- cdc_final %>%
  filter(
    state %in% c(appalachia_states, managing_list)
  ) %>%
  mutate(
    group = case_when(
      state %in% managing_list ~ "Managing States",
      state %in% appalachia_states ~ "Appalachia"
    )
  )

#visualize

ggplot(cdc_compare_groups, aes(year, overdose_rate, color = group)) +
  stat_summary(
    fun = mean,
    geom = "line",
    linewidth = 1.3
  ) +
  labs(
    title = "Overdose Death Rate Trends",
    subtitle = "Appalachia vs Managing States",
    x = "Year",
    y = "Overdose Death Rate",
    color = "Group"
  ) +
  theme_minimal()


#Pt. 3- Which States Should Appalachia Model After? (Most Socio-economically similar)
#Already established that there is difference between part of state in vs. out of Appalachia
#So looking at WV, only state entirely in Appalachia

##Cluster comparison of states
library(tidyverse)
library(cluster)
library(factoextra)
socio <- read.csv("/Users/nora/Desktop/DSS R Programming/Socioeconomic_Estimate.csv")

#only use states of interest (only WV as that is the only state entirely in Appalachia)
filtered_socio <- socio %>%
  filter(STUSAB %in% c(managing_list, "WV"))

# Remove rows with missing values (optional but recommended for clustering)
clean_socio <- na.omit(filtered_socio)
# Separate numeric columns (for clustering math)
socio_numeric <- clean_socio[, sapply(clean_socio, is.numeric)]
# Scale numeric data
scaled <- socio_data %>%
  select(STUSAB, where(is.numeric)) %>%
  column_to_rownames("STUSAB") %>%
  scale()


# Run clustering 
set.seed(123)
k3 <- kmeans(scaled, centers = 3, nstart = 25)

#visualize clusters
fviz_cluster(
  k3, 
  data = scaled, 
  ggtheme = theme_minimal(),
  main = "State Clusters Based on Socio-Economic Indicators"
)


#most similar to South Dakota, Montana, and Idaho. Ideally, model opioid relief initiatives after
#these states



