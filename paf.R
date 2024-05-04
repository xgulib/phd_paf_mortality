### PAF calculation ###

# Read in phdi data from 'phdi_scoring.R'
load("/Users/xgu/Desktop/Ongoing Project/PAF/PAF/03072024/out_phdi2018.Rdata")

# HR per unit increment from Bui et al
rr_all<- read_excel("/Users/xgu/Desktop/Ongoing Project/PAF/PAF/results_03072024/hr_mort.xlsx") %>% 
  filter(cohort == c("all"))

# Input data for PAF calculation
phdi_in <- out_phdi2018 %>%
  ungroup() %>%
  mutate(imp_phdi = as.numeric(imp_phdi),
         imp_phdi_sd = as.numeric(imp_phdi_sd)) %>%
  select(c("location_name", "type", "phdi", "imp_phdi_sd"))

nsim<-1000 # number of simulations
ncountry<-length(phdi_in$location_name) 
endpoint <- c("all", "ntm", "cvd", "can", "res", "neu", "inf")

allout <- matrix(NA, ncountry, length(endpoint))
allout_sd <- matrix(NA, ncountry, length(endpoint))
allout_lci <- matrix(NA, ncountry, length(endpoint))
allout_hci <- matrix(NA, ncountry, length(endpoint))

set.seed(8943)
for (k in 1:length(endpoint)){
  out <- matrix(NA,ncountry, nsim)
  for (i in 1:ncountry){
    for(j in 1:nsim){
      beta <- rnorm(n=1, mean=as.numeric(rr_all[,endpoint[k]]),sd=as.numeric(rr_all[,paste0(endpoint[k], "se", sep="")]))
      
      mu_tmrd <- runif(1, min=118, max=122) # Reference PHDI of 120
      sd_tmrd <- runif(1, min=4, max=5) # Assumed sd between 4 and 5
      
      sdd<- as.numeric(phdi_in[i, "imp_phdi_sd"]) # SD of current PHDI
      mm <- as.numeric(phdi_in[i, "phdi"]) # Current PHDI
      q<-seq(-4,4,by=.01) # Generate the "slices" of standard normal distribution between -4 and 4 by 0.01 increments
      mtc<-rep(mm, times=801)  # Vector with 801 repetitions of mean
      tmmtc<-rep(mu_tmrd, times=801) # Vector with 801 repetitions of the theoretical low-risk PHDI (reference)
      qsx<-sdd*q    # Vector multiplying sd by each "slice"
      x=(mtc+(qsx))   # Vector summing 801 repetitions of mean with the "sliced" sd
      qsy<-sd_tmrd*q  # Vector multiplying theoretical low-risk sd by slices
      y<-(tmmtc+(qsy))   # Vector summing 801 repetitions of theoretical low-risk mean with the "sliced" low-risk sd
      delat<-(y-x)   # Delta is the difference between the actual and theoretical minimum distributions  ######### divide this by RR units if it is not 1.     ## change to x-y for harmful risks; y-x is for protective risks
      
      p.pp<-dnorm(x, mean=mm, sd =sdd, log = FALSE)*(.01*sdd)   # Determine the prevalence for each slice
      
      rr.all <- exp(delat*beta*(-1))  # rr.all is the calculated attributable risk: exp(difference between actual exposure and counterfactual exposure times logRR)
      rr.all [rr.all<1]<- 1                   # set attributable risk to 1 if actual exposure is lower than theoretical minimum
      p.rr.all<-p.pp*(rr.all-1)              # numerator of the paf calc for each "slice"
      p.all<-sum(p.rr.all, na.rm=TRUE)       # sum the numerator
      out[i,j]<- p.all/(p.all+1)         # actual paf calculation, generates a matrix of i rows and j columns
    }
  }
  allout[,k] <- rowMeans(out)
  allout_sd[,k] <- apply(out, 1, sd, na.rm = TRUE)
  allout_lci[,k] <- allout[,k]-qnorm(0.975)*allout_sd[,k]
  allout_hci[,k] <- allout[,k]+qnorm(0.975)*allout_sd[,k]
}

# Output PAF
colnames(allout) <- endpoint
colnames(allout_sd) <- paste0(endpoint, "_sd", sep = "")
colnames(allout_lci) <- paste0(endpoint, "_lci", sep = "")
colnames(allout_hci) <- paste0(endpoint, "_hci", sep = "")
output <- cbind(phdi_in[,c("location_name", "type")], allout, allout_lci, allout_hci)

write.csv(output, "/Users/xgu/Desktop/Ongoing Project/PAF/PAF/03072024/results/paf_all_01282024.csv", row.names=FALSE)


