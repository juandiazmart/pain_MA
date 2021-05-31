wd <- "/home/antonio/pain_MA"
setwd(wd)
source("functions_MA.R")


df=read_excel("Formatted TOTPAR.SPID.Global (continuous).xlsx",sheet = "Reformatted 2 TOTPAR 6")
df %<>% mutate(studlab=paste0(firstauthor,"_",refid)) %>% rename(mean=`converted_central tendency`,sd=converted_variability)

pairwise=pairwise(intervention_1, mean = mean, n = sampleSize,sd=sd,studlab = studlab, data = df, sm = "MD")

pairwise %<>% as_tibble() %>% rename(t1=treat1,t2=treat2,study=studlab,tid1=intervention_21,tid2=intervention_22) %>% 
  select(study,t1,tid1,t2,tid2,n1,mean1,sd1,n2,mean2,sd2)

# pairwise.mutate = pairwise %>%
#   mutate(n2aux = if_else((as.numeric(pairwise.mutate$tid1) > as.numeric(pairwise.mutate$tid2)), n2, n1),
#          n1 = if_else((as.numeric(pairwise.mutate$tid1) > as.numeric(pairwise.mutate$tid2)), n1, n2),
#          mean2aux = if_else((as.numeric(pairwise.mutate$tid1) > as.numeric(pairwise.mutate$tid2)), mean2, mean1),
#          mean1 = if_else((as.numeric(pairwise.mutate$tid1) > as.numeric(pairwise.mutate$tid2)), mean1, mean2),
#          sd2aux = if_else((as.numeric(pairwise.mutate$tid1) > as.numeric(pairwise.mutate$tid2)), sd2, sd1),
#          sd1 = if_else((as.numeric(pairwise.mutate$tid1) > as.numeric(pairwise.mutate$tid2)), sd1, sd2),
#          t2aux = if_else((as.numeric(pairwise.mutate$tid1) > as.numeric(pairwise.mutatet$id2)), t2, t1),
#          t1 = if_else((as.numeric(pairwise.mutate$tid1) > as.numeric(pairwise.mutate$tid2)), t1, t2)
#          ) %>%
#   select(study, t1, t2 = t2aux, n1, mean1, sd1, n2 = n2aux, mean2 = mean2aux, sd2 = sd2aux)
# 
# pairwise.placebo = pairwise.mutate %>%
#   mutate(n2aux = if_else(t1 == "Placebo", n2, n1),
#          n1 = if_else(t1 == "Placebo", n1, n2),
#          mean2aux = if_else(t1 == "Placebo", mean2, mean1),
#          mean1 = if_else(t1 == "Placebo", mean1, mean2),
#          sd2aux = if_else(t1 == "Placebo", sd2, sd1),
#          sd1 = if_else(t1 == "Placebo", sd1, sd2),
#          t2aux = if_else(t1 == "Placebo", t2, t1),
#          t1 = if_else(t1 == "Placebo", t1, t2)
#          ) %>%
#   select(study, t1, t2 = t2aux, n1, mean1, sd1, n2 = n2aux, mean2 = mean2aux, sd2 = sd2aux)

for (i in 1:nrow(pairwise)){
  if ((pairwise[i,"t1"] == "Placebo") | (as.numeric(pairwise[i,"tid1"]) > as.numeric(pairwise[i, "tid2"]))){
    auxt = pairwise[i,"t2"]
    pairwise[i,"t2"] = pairwise[i,"t1"]
    pairwise[i,"t1"] = auxt
    
    auxtid = pairwise[i,"tid2"]
    pairwise[i,"tid2"] = pairwise[i,"tid1"]
    pairwise[i,"tid1"] = auxtid
    
    auxn = pairwise[i,"n2"]
    pairwise[i,"n2"] = pairwise[i,"n1"]
    pairwise[i,"n1"] = auxn
    
    auxmean = pairwise[i,"mean2"]
    pairwise[i,"mean2"] = pairwise[i,"mean1"]
    pairwise[i,"mean1"] = auxmean
    
    auxsd = pairwise[i,"sd2"]
    pairwise[i,"sd2"] = pairwise[i,"sd1"]
    pairwise[i,"sd1"] = auxsd
    
  }
}

pairwise = pairwise %>% select(study, t1, t2, n1, mean1, sd1, n2, mean2, sd2)

####

TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=pairwise %>% filter(t1=="Placebo" | t2=="Placebo") %>% 
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "TOTPAR.csv"
folder <- "TOTPAR"


list.estimates <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates, folder,name,"Frequentist.random.DL")

#############
#############

