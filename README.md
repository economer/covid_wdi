

# COVID_WDI

R codes for combining COVIDdata with World Development Indicators (WDI)

Use the codes below in case you are interested in combining COVID data with WDI

library(tidyverse)

## download the death and number of confirmed cases data (COVID)
death <-  read_csv("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_deaths_global_iso3_regions.csv?dest=data_edit&filter01=merge&merge-url01=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D1326629740%26single%3Dtrue%26output%3Dcsv&merge-keys01=%23country%2Bname&merge-tags01=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bmain%2Bname%2C%23region%2Bsub%2Bcode%2C%23region%2Bsub%2Bname%2C%23region%2Bintermediate%2Bcode%2C%23region%2Bintermediate%2Bname&filter02=merge&merge-url02=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D398158223%26single%3Dtrue%26output%3Dcsv&merge-keys02=%23adm1%2Bname&merge-tags02=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bmain%2Bname%2C%23region%2Bsub%2Bcode%2C%23region%2Bsub%2Bname%2C%23region%2Bintermediate%2Bcode%2C%23region%2Bintermediate%2Bname&merge-replace02=on&merge-overwrite02=on&tagger-match-all=on&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv")

case <- read_csv("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_iso3_regions.csv?dest=data_edit&filter01=merge&merge-url01=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D1326629740%26single%3Dtrue%26output%3Dcsv&merge-keys01=%23country%2Bname&merge-tags01=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bmain%2Bname%2C%23region%2Bsub%2Bcode%2C%23region%2Bsub%2Bname%2C%23region%2Bintermediate%2Bcode%2C%23region%2Bintermediate%2Bname&filter02=merge&merge-url02=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D398158223%26single%3Dtrue%26output%3Dcsv&merge-keys02=%23adm1%2Bname&merge-tags02=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bmain%2Bname%2C%23region%2Bsub%2Bcode%2C%23region%2Bsub%2Bname%2C%23region%2Bintermediate%2Bcode%2C%23region%2Bintermediate%2Bname&merge-replace02=on&merge-overwrite02=on&tagger-match-all=on&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv")


## clean COVID data and join death and case data 
death <- death %>%
  select( country= `Country/Region`, ends_with("/20"),sub_region = `Sub-region Name`) %>%
  filter(country != "#country+name")

### data is wide turn it into long
death <- death %>%
  pivot_longer(cols = ends_with("/20"),names_to = "date") 

### clean the date variable into readable format
death <- death %>%
  mutate(date = lubridate::mdy(date)) 

### Number of confirmed cases (clean the data)
case <- case %>%
  select(country = `Country/Region`, ends_with("/20"),sub_region = `Sub-region Name`) %>%
  filter(country != "#country+name")

### data is wide turn it into long
case <- case %>%
  pivot_longer(cols = ends_with("/20"),names_to = "date")

### clean the date variable into readable format
case <- case %>%
  mutate(date = lubridate::mdy(date)) 


### calculate the sum of death and cases
death <- death %>% 
  group_by(country,date) %>%
  summarise(death = sum(value))

### make an id variable for joining the datasets
death <- death %>% 
  unite(col = "id", c(country,date), remove = F, sep = "_")
 
### calculate the sum of death and cases

case <- case %>% 
  group_by(country,date) %>%
  summarise(case = sum(value)) 


### make an id variable for joining the datasets

case <- case %>%  
  unite(col = "id", c(country,date), remove = F,sep = "_")

### join the datasets
all <- dplyr::left_join(death, case, by="id")

final_covid <- all %>%
  select(country = country.x,date = date.x,death,case)
## Downlad WDI

install.packages("downloader")

library(downloader)

### WDI data location
url <- "http://databank.worldbank.org/data/download/WDI_csv.zip"

### download WDI data
download(url, dest="WDI_csv.zip", mode="wb") 

### unzip it in a path. My path is exdir = “/Users/shh/Downloads”, choose yours 
unzip("WDI_csv.zip", exdir = "/Users/shh/Downloads")

### import WDI data
wdi <- read_csv("/Users/shh/Downloads/WDIData.csv") # use your own path here 

### clean WDI data. 
wdi <- wdi %>% 
  select(-`Country Code`,-`Indicator Code`, country=`Country Name`, index= `Indicator Name`) 


### data is wide make it long and make it wide again as follows:
wdi_wide <- wdi %>%
  pivot_longer(names_to="year", values_to = "val", cols = c(-country, -index)) %>%
  pivot_wider(names_from = index, values_from = val)
 

### Keep only a subset of countries you are interested in (here I have the 23 countries with the highest number of death reported. 

wdi_5 <- wdi_wide %>%
  filter(country %in% c("China", "United States", "Iran, Islamic Rep.", "Russia", "Italy", "France", "Spain", "United Kingdom", "Belgium", "Germany", "Netherlands", "Brazil" , "Turkey", "Sweden", "Canada", "Switzerland", "Mexico", "Ireland", "Portugal","Russian Federation", "Ecuador","Romania","Austria","Poland")) %>%
  janitor::clean_names()

### The name of countries in WDI and COVID data are not the same in few cases.  Make few changes in the names of countries for joining with COVID data. Also here I only selected a subset of variables from WDI. 

wdi_6 <- wdi_5 %>%
  mutate(country = fct_recode(country, Iran = "Iran, Islamic Rep.", US = "United States", Russia = "Russian Federation")) %>%
  select(country = country,year, populatio_65 = population_ages_65_and_above_percent_of_total_population, population = population_total, pop_growth = population_growth_annual_percent) %>% 
  filter(year == 2018) %>%
  mutate(population_2019 = population*(1 + pop_growth/100))
  
### make a dataset for COVID data with the subset of countries. 
wdi_COVID<- final_COVID%>%
  filter(country %in% c("China", "US", "Iran", "Italy", "France", "Spain", "United Kingdom", "Belgium", "Germany", "Netherlands", "Brazil" , "Turkey", "Sweden", "Canada", "Switzerland", "Mexico", "Ireland", "Portugal","Russia", "Ecuador","Romania","Austria","Poland"), date == "2020-04-24") 

### join wdi_covid and wdi_6 by country
wdi_join1 <- left_join(wdi_covid,wdi_6, by="country")
 

### in some cases WDI data does not exist for 2019 or 2018, so you can use the average values since a date where the data is available (here I chose 2010). 

### mutate some new variables 
wdi_join1 <- wdi_join1 %>%
  mutate(death_1000 = 1000*death/population_2019, case_1000 = 1000*case/population_2019) %>%
  mutate(country_la = fct_recode(country, UK= "United Kingdom"))

### mutate more varaibles
wdi_t <- wdi_5 %>%
  select(country,year,doctor_1000 = physicians_per_1_000_people,death_crude = death_rate_crude_per_1_000_people,emp_service =employment_in_services_percent_of_total_employment_modeled_ilo_estimate, 
         emp_ind = employment_in_industry_percent_of_total_employment_modeled_ilo_estimate,smoking_percent = smoking_prevalence_total_ages_15, tourism_arrival = international_tourism_number_of_arrivals, tourism_dep = international_tourism_number_of_departures, diabetes_prev = diabetes_prevalence_percent_of_population_ages_20_to_79,cause_death_commun = cause_of_death_by_communicable_diseases_and_maternal_prenatal_and_nutrition_conditions_percent_of_total) %>%
  mutate(net_tourism = tourism_arrival-tourism_dep) %>%
  filter(year >2010)

### calculate the mean of added variables
mean_var <- wdi_t %>%
  mutate(country = fct_recode(country, Iran = "Iran, Islamic Rep.", US = "United States", Russia = "Russian Federation")) %>%
  group_by(country) %>%
  summarise(mean_doctor_1000 = mean(doctor_1000,na.rm=T),mean_death_c = mean(death_crude,na.rm=T),
            mean_emp_service = mean(emp_service,na.rm=T),mean_emp_ind = mean(emp_ind, na.rm=T), mean_smoking = mean(smoking_percent, na.rm=T), mean_tourism_arr =mean( tourism_arrival,na.rm=T), mean_tourism_dep = mean(tourism_dep,na.rm=T ), mean_net_tourism = mean(net_tourism,na.rm=T), 
            mean_cause_death = mean(cause_death_commun, na.rm=T), mean_diabetes = mean(diabetes_prev, na.rm=T)
            
            )


## join to get the final dataset. 
wdi_join2 <- left_join(wdi_join1,mean_var, by="country")

