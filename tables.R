### STable 1 & 2 ###
# Total and Component score and intake by country/region
load("/Users/xgu/Desktop/Ongoing Project/PAF/PAF/03072024/out_phdi2018.Rdata")

phdi_out <- out_phdi2018 %>%
  ungroup() %>%
  select(location_name, region, type, phdi, 76:90, 46:60)
write.csv(phdi_out, "/Users/xgu/Desktop/Ongoing Project/PAF/PAF/03072024/results/phdi_all_01282024.csv", row.names=FALSE)

### Table 1 ###
# Total and Component score and intake averaged across countries
totpop <- read_excel("/Users/xgu/Desktop/Ongoing Project/PAF/unpop.xlsx", col_types = c("text", "text", "numeric", "numeric")) %>%
  filter(year == 2018 & !is.na(region)) %>%
  select(region, totpop)

phdi_out2 <- phdi_out %>%
  filter(type == "country") %>%
  left_join(totpop, by = "region") %>%
  mutate(totpop_wt = totpop/sum(totpop)) 

components <- colnames(phdi_out2[,4:34])
avgscore <- matrix(NA, length(components), 1)
for(i in 1:length(components)){
  avgscore[i] <- sum(phdi_out2[,components[i]]*phdi_out2[,"totpop_wt"])
}
tb1 <- cbind.data.frame(components, avgscore)

write.csv(tb1, "/Users/xgu/Desktop/Ongoing Project/PAF/PAF/03072024/results/phdi_avg_01282024.csv", row.names=FALSE)


### STable 3, 4, 5 ###
# Number of death from GHE2019
death <- read_excel("/Users/xgu/Desktop/Ongoing Project/PAF/death.xlsx")
death2 <- cbind(death[,1], death[,2:8]*1000)
ndeath <- out_phdi2018 %>%
  ungroup() %>%
  left_join(death2, by = c("region")) 

# Fill in number of death for regions
mapping <- read_excel("/Users/xgu/Desktop/Ongoing Project/PAF/rgs_mapping_WHO.xlsx", sheet = "mapping")

deathname <- colnames(ndeath[,95:101])
subreg <- ndeath$region[1:12]
sumdeathreg <- matrix(NA, length(subreg), length(deathname))
colnames(sumdeathreg) <- deathname
for (i in 1:length(deathname)) {
  for (j in 1:length(subreg)) {
    country <- pull(mapping[mapping$Region==subreg[j],][1])
    tempdat <- ndeath[ndeath$region %in% country, c("region", deathname[i])]
    sumdeathreg[j, i] <- sum(tempdat[,2], na.rm = TRUE)
  }
}
sumdeathreg2 <- cbind.data.frame(subreg, sumdeathreg) %>% rename(region = subreg)

ndeath2 <- ndeath[1:12,-c(95:101)] %>%
  left_join(sumdeathreg2, by = "region") %>%
  bind_rows(ndeath[-c(1:12),]) %>%
  select(45, 95:101)
colnames(ndeath2) <- c("location_name", paste0("death_", colnames(ndeath2[,-1]), sep = ""))

# Load PAF
output <- read.csv("/Users/xgu/Desktop/Ongoing Project/PAF/PAF/03072024/results/paf_all_01282024.csv")

prev_death <- output %>%
  left_join(ndeath2, by = c("location_name")) %>%
  mutate(d_all = all*death_all,
         d_ntm = ntm*death_ntm,
         d_cvd = cvd*death_cvd,
         d_can = can*death_can,
         d_res = res*death_res,
         d_neu = neu*death_neu,
         d_inf = inf*death_inf,
         
         d_all_lci = all_lci*death_all,
         d_ntm_lci = ntm_lci*death_ntm,
         d_cvd_lci = cvd_lci*death_cvd,
         d_can_lci = can_lci*death_can,
         d_res_lci = res_lci*death_res,
         d_neu_lci = neu_lci*death_neu,
         d_inf_lci = inf_lci*death_inf,
         
         d_all_hci = all_hci*death_all,
         d_ntm_hci = ntm_hci*death_ntm,
         d_cvd_hci = cvd_hci*death_cvd,
         d_can_hci = can_hci*death_can,
         d_res_hci = res_hci*death_res,
         d_neu_hci = neu_hci*death_neu,
         d_inf_hci = inf_hci*death_inf)

write.csv(prev_death, "/Users/xgu/Desktop/Ongoing Project/PAF/PAF/03072024/results/death_all_01282024.csv", row.names=FALSE)


### Table 2 ###
prev_death2 <- prev_death %>%
  filter(type == "country")

cause <- colnames(prev_death2[,3:9])
tb2 <- matrix(NA, length(cause)*2, 3)
colnames(tb2) <- c("muPAF", "muPAFlci", "muPAFhci")
for(i in 1:length(cause)){
  tb2[i+7, 1] <- sum(prev_death2[,paste0("d_", cause[i], sep="")], na.rm = TRUE)
  tb2[i+7, 2] <- sum(prev_death2[,paste0("d_", cause[i], "_lci", sep="")], na.rm = TRUE)
  tb2[i+7, 3] <- sum(prev_death2[,paste0("d_", cause[i], "_hci", sep="")], na.rm = TRUE)
  tb2[i, 1] <- tb2[i+7, 1]/sum(prev_death2[,paste0("death_", cause[i], sep="")], na.rm = TRUE)
  tb2[i, 2] <- tb2[i+7, 2]/sum(prev_death2[,paste0("death_", cause[i], sep="")], na.rm = TRUE)
  tb2[i, 3] <- tb2[i+7, 3]/sum(prev_death2[,paste0("death_", cause[i], sep="")], na.rm = TRUE)
}
tb2 <- cbind.data.frame(c(cause, paste0(cause, "_prevd", sep="")), tb2)

write.csv(tb2, "/Users/xgu/Desktop/Ongoing Project/PAF/PAF/03072024/results/paf_prevd_01282024.csv", row.names=FALSE)
















