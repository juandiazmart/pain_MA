wd <- "/home/antonio/pain_MA"
setwd(wd)
source("functions_MA.R")

#### TOTPAR ####

df=read_excel("Formatted TOTPAR.SPID.Global (continuous).xlsx",sheet = "Reformatted 2 TOTPAR 6")
df %<>% mutate(studlab=paste0(firstauthor,"_",refid)) %>% rename(mean=`converted_central tendency`,sd=converted_variability)

pairwise=pairwise(intervention_1, mean = mean, n = sampleSize,sd=sd,studlab = studlab, data = df, sm = "MD")

pairwise %<>% as_tibble() %>% rename(t1=treat1,t2=treat2,study=studlab,tid1=intervention_21,tid2=intervention_22) %>% 
  select(study,t1,tid1,t2,tid2,n1,mean1,sd1,n2,mean2,sd2)


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
name.grade <- "grade_TOTPAR.csv"
folder <- "TOTPAR"
type.filter <- "Frequentist.random.DL"

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

grade <- write.estimates.csv(list.estimates.turnerless, folder,name,filter.type = "Frequentist.random.DL")

get.network.pdf(pairwise, measure, folder, "TOTPAR_network")

out <- get.grade.csv(pairwise, measure, folder, name.grade, grade)

#### SPID ####

df=read_excel("Formatted TOTPAR.SPID.Global (continuous).xlsx",sheet = "Reformatted 2 SPID 6”")
df %<>% mutate(studlab=paste0(firstauthor,"_",refid)) %>% rename(mean=`converted_central tendency`,sd=converted_variability)

pairwise=pairwise(intervention_1, mean = mean, n = sampleSize,sd=sd,studlab = studlab, data = df, sm = "MD")

pairwise %<>% as_tibble() %>% rename(t1=treat1,t2=treat2,study=studlab,tid1=intervention_21,tid2=intervention_22) %>% 
  select(study,t1,tid1,t2,tid2,n1,mean1,sd1,n2,mean2,sd2)

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
name <- "SPID.csv"
name.grade <- "grade_SPID.csv"
folder <- "SPID"
type.filter <- "Frequentist.random.DL"

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)
#list.estimates <- getestimates(pairwise, TP, TP1, baseline, measure, name,folder)

grade <- write.estimates.csv(list.estimates.turnerless, folder,name,"Frequentist.random.DL")

get.network.pdf(pairwise, measure, folder, name)

out <- get.grade.csv(pairwise, measure, folder, name.grade, grade)

