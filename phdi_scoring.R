library(readxl)
library(dplyr)
library(tidyr)

### Data read in and cleaning ###
# Filter to 2018 for both sex
fbs_kcal <- read.csv("/Users/xgu/Desktop/Ongoing Project/PAF/From Marco/03072024/cons_FBS_011824_2.csv") %>% 
  filter(sex == "BTH" & stats == "mean" & year == 2018 & unit == "kcal/d_w" & age == "20+") %>%
  filter(food_group %in% c("butter", "cream", "fat_ani", "oil_palm", "oil_veg", "sugar", "all-fg"))

fbs_gram <- read.csv("/Users/xgu/Desktop/Ongoing Project/PAF/From Marco/03072024/cons_FBS_011824_2.csv") %>% 
  filter(sex == "BTH" & stats == "mean" & year == 2018 & unit == "g/d_w" & age == "20+" & !(food_group %in% c("butter", "cream", "fat_ani", "oil_palm", "oil_veg", "sugar", "all-fg")))

# Combine foods in grams and foods in kcal
fbs <- rbind(fbs_gram, fbs_kcal) %>%
  select(c("food_group", "region","value"))

# Transpose data
fbs_wd <- fbs %>% 
  group_by(region) %>%
  pivot_wider(names_from = food_group, values_from = value) %>%
  rename("all_fg" = "all-fg") %>%
  filter(!is.na(all_fg))

# Missing values should be assigned 0 according to Marco Springmann
fbs_wd[is.na(fbs_wd)] <- 0

# Merge country names by iso3 code
country <- read.csv("/Users/xgu/Desktop/Ongoing Project/PAF/gdd_countrycode.csv") %>% rename("region" = "iso3")

fbs_wd_lb <- fbs_wd %>%
  left_join(country, by = "region") %>%
  rename("milk_acl" = "milk (actual)")


### PHDI scoring ###

# Food grouping (confirmed with Marco)
all_fg <- fbs_wd_lb %>%
  mutate(phd_whgrn = whole_grains,
         phd_stveg = roots,
         phd_nsveg = vegetables,
         phd_whfru = fruits_trop+fruits_temp+fruits_starch,
         #phd_dairy = milk_acl+yoghurt+cheese,
         phd_dairy2 = milk, # Milk equivalent
         phd_rmeat = prc_meat+red_meat,
         phd_pultr = poultry,
         phd_eggws = eggs,
         phd_fishs = shellfish+fish_freshw+fish_pelag+fish_demrs+fish_other,
         phd_nutss = nuts+seeds,
         phd_legum = legumes,
         phd_soyfd = soybeans,
         phd_unsat = 100*(oil_veg/all_fg), # Pct of energy
         phd_satft = 100*((butter+cream+fat_ani+oil_palm)/all_fg),
         phd_sugjc = 100*(sugar/all_fg))

# Scoring
all_fg_pre <- all_fg %>%
  mutate(phd_whgrnIp = ifelse(phd_whgrn == 0, 0, phd_whgrn/8),
         phd_stvegIp = ifelse(phd_stveg <= 50, 10, (200-phd_stveg)/((200-50)/10)),
         phd_nsvegIp = ifelse(phd_nsveg == 0, 0, phd_nsveg/30),
         phd_whfruIp = ifelse(phd_whfru == 0, 0, phd_whfru/20),
         phd_dairyIp = ifelse(phd_dairy2 <= 250, 10, (1000-phd_dairy2)/((1000-250)/10)),
         phd_rmeatIp = ifelse(phd_rmeat <= 14, 10, (100-phd_rmeat)/((100-14)/10)),
         phd_pultrIp = ifelse(phd_pultr <= 29, 10, (100-phd_pultr)/((100-29)/10)),
         phd_eggwsIp = ifelse(phd_eggws <= 13, 10, (120-phd_eggws)/((120-13)/10)),
         phd_fishsIp = ifelse(phd_fishs == 0, 0, phd_fishs/2.8),
         phd_nutssIp = ifelse(phd_nutss == 0, 0, phd_nutss/5),
         phd_legumIp = ifelse(phd_legum == 0, 0, phd_legum/20),
         phd_soyfdIp = ifelse(phd_soyfd == 0, 0, phd_soyfd/10),
         phd_unsatIp = ifelse(phd_unsat <= 3.5, 0, (phd_unsat-3.5)/((21-3.5)/10)),
         phd_satftIp = ifelse(phd_satft == 0, 10, (10-phd_satft)/1),
         phd_sugjcIp = ifelse(phd_sugjc <= 5, 10, (25-phd_sugjc)/((25-5)/10))) %>%
  mutate(phd_whgrnI = ifelse(phd_whgrnIp <= 0, 0, ifelse(phd_whgrnIp >= 10, 10, phd_whgrnIp)),
         phd_stvegI = ifelse(phd_stvegIp <= 0, 0, ifelse(phd_stvegIp >= 10, 10, phd_stvegIp)),
         phd_nsvegI = ifelse(phd_nsvegIp <= 0, 0, ifelse(phd_nsvegIp >= 10, 10, phd_nsvegIp)),
         phd_whfruI = ifelse(phd_whfruIp <= 0, 0, ifelse(phd_whfruIp >= 10, 10, phd_whfruIp)),
         phd_dairyI = ifelse(phd_dairyIp <= 0, 0, ifelse(phd_dairyIp >= 10, 10, phd_dairyIp)),
         phd_rmeatI = ifelse(phd_rmeatIp <= 0, 0, ifelse(phd_rmeatIp >= 10, 10, phd_rmeatIp)),
         phd_pultrI = ifelse(phd_pultrIp <= 0, 0, ifelse(phd_pultrIp >= 10, 10, phd_pultrIp)),
         phd_eggwsI = ifelse(phd_eggwsIp <= 0, 0, ifelse(phd_eggwsIp >= 10, 10, phd_eggwsIp)),
         phd_fishsI = ifelse(phd_fishsIp <= 0, 0, ifelse(phd_fishsIp >= 10, 10, phd_fishsIp)),
         phd_nutssI = ifelse(phd_nutssIp <= 0, 0, ifelse(phd_nutssIp >= 10, 10, phd_nutssIp)),
         phd_legumI = ifelse(phd_legumIp <= 0, 0, ifelse(phd_legumIp >= 5, 5, phd_legumIp)),
         phd_soyfdI = ifelse(phd_soyfdIp <= 0, 0, ifelse(phd_soyfdIp >= 5, 5, phd_soyfdIp)),
         phd_unsatI = ifelse(phd_unsatIp <= 0, 0, ifelse(phd_unsatIp >= 10, 10, phd_unsatIp)),
         phd_satftI = ifelse(phd_satftIp <= 0, 0, ifelse(phd_satftIp >= 10, 10, phd_satftIp)),
         phd_sugjcI = ifelse(phd_sugjcIp <= 0, 0, ifelse(phd_sugjcIp >= 10, 10, phd_sugjcIp))) %>%
  mutate(phdi = phd_whgrnI + phd_stvegI + phd_nsvegI + phd_whfruI + phd_dairyI + phd_rmeatI + phd_pultrI + phd_eggwsI + phd_fishsI + phd_nutssI + phd_legumI + phd_soyfdI + phd_unsatI + phd_satftI + phd_sugjcI)


### Standard deviation imputation ###

# Get the ratio of mean and sd from GBD (GBD data cleaning see 'cleaning_gbd.R')
# Using 2018 data in a previous version, should update to 2018 later for consistency
load("/Users/xgu/Desktop/Ongoing Project/PAF/PAF/resultsv2/gbd2018.Rdata")
load("/Users/xgu/Desktop/Ongoing Project/PAF/PAF/resultsv2/gbd2018_sd.Rdata")

# Merge mean and sd data and calculate the ratios, rename countries to be consistent with Marco's data
gbd2018_ratio <- gbd2018 %>%
  ungroup() %>%
  left_join(gbd2018_sd, by = c("location_id", "location_name", "sex_id", "sex_name", "year_id")) %>%
  mutate(calcium_rt = calcium/calcium_sd,
         fiber_rt = fiber/fiber_sd,
         fruit_rt = fruit/fruit_sd,
         legumes_rt = legumes/legumes_sd,
         milk_rt = milk/milk_sd,
         nuts_rt = nuts/nuts_sd,
         omega3_rt = omega3/omega3_sd,
         omega6_rt = omega6/omega6_sd,
         procmeat_rt = procmeat/procmeat_sd,
         redmeat_rt = redmeat/redmeat_sd,
         sodium_rt = sodium/sodium_sd,
         ssbs_rt = ssbs/ssbs_sd,
         transfat_rt = transfat/transfat_sd,
         veg_rt = veg/veg_sd,
         wholegrains_rt = wholegrains/wholegrains_sd) %>%
  mutate(location_name2 = case_when(
    location_name == "Democratic People's Republic of Korea" ~ "North Korea",
    location_name == "Taiwan (Province of China)" ~ "Taiwan",
    location_name == "Lao People's Democratic Republic" ~ "Laos",
    location_name == "Viet Nam" ~ "Vietnam",
    location_name == "Micronesia (Federated States of)" ~ "Federated States of Micronesia",
    location_name == "Czechia" ~ "Czech Republic",
    location_name == "North Macedonia" ~ "Macedonia",
    location_name == "Republic of Moldova" ~ "Moldova",
    location_name == "Russian Federation" ~ "Russia",
    location_name == "Brunei Darussalam" ~ "Brunei",
    location_name == "Republic of Korea" ~ "South Korea",
    location_name == "United States of America" ~ "United States",
    location_name == "Bahamas" ~ "The Bahamas",
    location_name == "Bolivia (Plurinational State of)" ~ "Bolivia",
    location_name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    location_name == "Iran (Islamic Republic of)" ~ "Iran",
    location_name == "Syrian Arab Republic" ~ "Syria",
    location_name == "Türkiye" ~ "Turkey",
    location_name == "United Republic of Tanzania" ~ "Tanzania",
    location_name == "Cabo Verde" ~ "Cape Verde",
    location_name == "Côte d'Ivoire" ~ "Cote d'Ivoire",
    location_name == "Gambia" ~ "The Gambia",
    location_name == "Eswatini" ~ "Swaziland",
    TRUE ~ location_name
  )) %>%
  select(-c("location_name")) %>%
  rename(location_name = location_name2, year = year_id) %>%
  select(50, 4, 35:49)

# Merge GBD ratios with Marco's FBS data
all_fg2 <- all_fg %>%
  ungroup() %>%
  select(1, 45:60)

fbs_gbdrt <- all_fg2 %>%
  left_join(gbd2018_ratio, by = "location_name")

# Calculate region-specific ratios based on country-specific ratios, weighted by country populations within regions
# Country to region mapping data from Marco
mapping <- read_excel("/Users/xgu/Desktop/Ongoing Project/PAF/rgs_mapping_WHO.xlsx", sheet = "mapping") 

# Population data in 2018 from UN
totpop <- read_excel("/Users/xgu/Desktop/Ongoing Project/PAF/unpop.xlsx", col_types = c("text", "text", "numeric", "numeric")) %>%
  filter(year == 2018 & !is.na(region)) %>%
  select(region, totpop)

fbs_gbdrt2 <- fbs_gbdrt %>%
  left_join(totpop, by = "region")

# Loop to calculate region-specific ratios as weighted mean of country-specific ratios within regions
fgpname <- colnames(fbs_gbdrt2[,19:33])
subreg <- fbs_gbdrt2$region[1:12]
wtrt <- matrix(NA, length(subreg), length(fgpname))
colnames(wtrt) <- fgpname
for (i in 1:length(fgpname)) {
  for (j in 1:length(subreg)) {
    country <- pull(mapping[mapping$Region==subreg[j],][1])
    tempdat <- fbs_gbdrt2[fbs_gbdrt2$region %in% country, c("region", fgpname[i], "totpop")] %>% 
      mutate(totpop_wt = totpop/sum(totpop))
    wtrt[j, i] <- sum(tempdat[,2]*tempdat[,4])
  }
}
wtrt2 <- cbind.data.frame(subreg, wtrt) %>% rename(region = subreg)

# Fill in region ratios to the data
fbs_gbdrt3 <- fbs_gbdrt2[1:12,-c(19:33)] %>%
  left_join(wtrt2, by = "region") %>%
  bind_rows(fbs_gbdrt2[-c(1:12),]) %>%
  select(-year)

# Impute SD
# For food groups do not exist in GBD data, use the ratio of the closest food groups
# For food groups that do not have similar any close food groups, use the average ratio of all food groups
# Should test with different assumed ratios as a sensitivity analysis
fbs_sdimp <- fbs_gbdrt3 %>%
  mutate(phd_whgrn_sd = phd_whgrn/wholegrains_rt,
         phd_stveg_sd = phd_stveg/veg_rt,
         phd_nsveg_sd = phd_nsveg/veg_rt,
         phd_whfru_sd = phd_whfru/fruit_rt,
         phd_dairy_sd = phd_dairy2/milk_rt,
         phd_rmeat_sd = phd_rmeat/((procmeat_rt+redmeat_rt)/2),
         phd_pultr_sd = phd_pultr/((calcium_rt + fiber_rt + fruit_rt + legumes_rt + milk_rt + nuts_rt + omega3_rt + omega6_rt + procmeat_rt + redmeat_rt + sodium_rt + ssbs_rt + transfat_rt + veg_rt + wholegrains_rt)/15),
         phd_eggws_sd = phd_eggws/((calcium_rt + fiber_rt + fruit_rt + legumes_rt + milk_rt + nuts_rt + omega3_rt + omega6_rt + procmeat_rt + redmeat_rt + sodium_rt + ssbs_rt + transfat_rt + veg_rt + wholegrains_rt)/15),
         phd_fishs_sd = phd_fishs/omega3_rt,
         phd_nutss_sd = phd_nutss/nuts_rt,
         phd_legum_sd = phd_legum/legumes_rt,
         phd_soyfd_sd = phd_soyfd/((calcium_rt + fiber_rt + fruit_rt + legumes_rt + milk_rt + nuts_rt + omega3_rt + omega6_rt + procmeat_rt + redmeat_rt + sodium_rt + ssbs_rt + transfat_rt + veg_rt + wholegrains_rt)/15),
         phd_unsat_sd = phd_unsat/omega6_rt,
         phd_satft_sd = phd_satft/((calcium_rt + fiber_rt + fruit_rt + legumes_rt + milk_rt + nuts_rt + omega3_rt + omega6_rt + procmeat_rt + redmeat_rt + sodium_rt + ssbs_rt + transfat_rt + veg_rt + wholegrains_rt)/15),
         phd_sugjc_sd = phd_sugjc/ssbs_rt)

# Simulate populations of 10000 for each country to calculate the SD for PHDI for each country 
library(faux)
set.seed(3872)
phdi_sd <- matrix(NA, length(fbs_sdimp$location_name), 3)
colnames(phdi_sd) <- c("location_name", "imp_phdi", "imp_phdi_sd")
for(i in 1:length(fbs_sdimp$location_name)){
  para <- fbs_sdimp[i,]
  temp <- rnorm_multi(
    n = 10000,
    mu = c(whgrn = para$phd_whgrn, stveg = para$phd_stveg, nsveg = para$phd_nsveg, whfru = para$phd_whfru, dairy = para$phd_dairy2, rmeat = para$phd_rmeat, pultr = para$phd_pultr, 
           eggws = para$phd_eggws, fishs = para$phd_fishs, nutss = para$phd_nutss, legum = para$phd_legum, soyfd = para$phd_soyfd, unsat = para$phd_unsat, satft = para$phd_satft, 
           sugjc = para$phd_sugjc),
    sd = c(whgrn = para$phd_whgrn_sd, stveg = para$phd_stveg_sd, nsveg = para$phd_nsveg_sd, whfru = para$phd_whfru_sd, dairy = para$phd_dairy_sd, rmeat = para$phd_rmeat_sd, pultr = para$phd_pultr_sd, 
           eggws = para$phd_eggws_sd, fishs = para$phd_fishs_sd, nutss = para$phd_nutss_sd, legum = para$phd_legum_sd, soyfd = para$phd_soyfd_sd, unsat = para$phd_unsat_sd, satft = para$phd_satft_sd, 
           sugjc = para$phd_sugjc_sd),
    r = 0.4) # Assume Multivariate Normal Distribution, with a correlation of 0.4 between food groups
  temp2 <- temp %>%
    mutate(whgrnIp = ifelse(whgrn == 0, 0, whgrn/8),
           stvegIp = ifelse(stveg <= 50, 10, (200-stveg)/((200-50)/10)),
           nsvegIp = ifelse(nsveg == 0, 0, nsveg/30),
           whfruIp = ifelse(whfru == 0, 0, whfru/20),
           dairyIp = ifelse(dairy <= 250, 10, (1000-dairy)/((1000-250)/10)),
           rmeatIp = ifelse(rmeat <= 14, 10, (100-rmeat)/((100-14)/10)),
           pultrIp = ifelse(pultr <= 29, 10, (100-pultr)/((100-29)/10)),
           eggwsIp = ifelse(eggws <= 13, 10, (120-eggws)/((120-13)/10)),
           fishsIp = ifelse(fishs == 0, 0, fishs/2.8),
           nutssIp = ifelse(nutss == 0, 0, nutss/5),
           legumIp = ifelse(legum == 0, 0, legum/20),
           soyfdIp = ifelse(soyfd == 0, 0, soyfd/10),
           unsatIp = ifelse(unsat <= 3.5, 0, (unsat-3.5)/((21-3.5)/10)),
           satftIp = ifelse(satft == 0, 10, (10-satft)/1),
           sugjcIp = ifelse(sugjc <= 5, 10, (25-sugjc)/((25-5)/10))) %>%
    mutate(whgrnI = ifelse(whgrnIp <= 0, 0, ifelse(whgrnIp >= 10, 10, whgrnIp)),
           stvegI = ifelse(stvegIp <= 0, 0, ifelse(stvegIp >= 10, 10, stvegIp)),
           nsvegI = ifelse(nsvegIp <= 0, 0, ifelse(nsvegIp >= 10, 10, nsvegIp)),
           whfruI = ifelse(whfruIp <= 0, 0, ifelse(whfruIp >= 10, 10, whfruIp)),
           dairyI = ifelse(dairyIp <= 0, 0, ifelse(dairyIp >= 10, 10, dairyIp)),
           rmeatI = ifelse(rmeatIp <= 0, 0, ifelse(rmeatIp >= 10, 10, rmeatIp)),
           pultrI = ifelse(pultrIp <= 0, 0, ifelse(pultrIp >= 10, 10, pultrIp)),
           eggwsI = ifelse(eggwsIp <= 0, 0, ifelse(eggwsIp >= 10, 10, eggwsIp)),
           fishsI = ifelse(fishsIp <= 0, 0, ifelse(fishsIp >= 10, 10, fishsIp)),
           nutssI = ifelse(nutssIp <= 0, 0, ifelse(nutssIp >= 10, 10, nutssIp)),
           legumI = ifelse(legumIp <= 0, 0, ifelse(legumIp >= 5, 5, legumIp)),
           soyfdI = ifelse(soyfdIp <= 0, 0, ifelse(soyfdIp >= 5, 5, soyfdIp)),
           unsatI = ifelse(unsatIp <= 0, 0, ifelse(unsatIp >= 10, 10, unsatIp)),
           satftI = ifelse(satftIp <= 0, 0, ifelse(satftIp >= 10, 10, satftIp)),
           sugjcI = ifelse(sugjcIp <= 0, 0, ifelse(sugjcIp >= 10, 10, sugjcIp))) %>%
    mutate(phdi = whgrnI+stvegI+nsvegI+whfruI+dairyI+rmeatI+pultrI+eggwsI+fishsI+nutssI+legumI+soyfdI+unsatI+satftI+sugjcI)
  phdi_sd[i, 1] <- para$location_name
  phdi_sd[i, 2] <- mean(temp2$phdi)
  phdi_sd[i, 3] <- sd(temp2$phdi) # SD of PHDI among the simulated populations
}


### Output data for analyses ###

# Merge SD of PHDI back to the main data
out_phdi2018 <- all_fg_pre %>%
  inner_join(as.data.frame(phdi_sd), by = "location_name") %>%
  mutate(type = case_when(
    region %in% subreg ~ "subregion",
    TRUE ~ "country"
  ))
  
save(out_phdi2018, file = "/Users/xgu/Desktop/Ongoing Project/PAF/PAF/03072024/out_phdi2018.Rdata")  
  
  
  
  
  
  