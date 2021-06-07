# wd <- "/home/antonio/pain_MA"
# setwd(wd)
source("functions_MA.R")

#### TOTPAR ####

df=read_excel("ADP outcomes revised.xlsx",sheet = "TOTPAR 6h")
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

df=read_excel("ADP outcomes revised.xlsx",sheet = "SPID 6h")
df %<>% mutate(studlab=paste0(firstauthor,"_",refid)) %>% rename(mean=`central tendency`,sd=converted_variability)

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

get.network.pdf(pairwise, measure, folder, "SPID_network")

out <- get.grade.csv(pairwise, measure, folder, name.grade, grade)

#### Pain relief ####

df=read_excel("ADP outcomes revised.xlsx",sheet = "Pain relief 6h",na=c("","NR"))
df %<>% mutate(studlab=paste0(firstauthor,"_",refid),sampleSize=if_else(is.na(sampleSize),50,sampleSize)) %>% 
  rename(mean=`central tendency`,sd=variability)

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
name <- "Pain_relief.csv"
name.grade <- "grade_Pain_relief.csv"
folder <- "Pain_relief"
type.filter <- "Frequentist.random.DL"

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder,c(-10,10),c(-10,10))
#list.estimates <- getestimates(pairwise, TP, TP1, baseline, measure, name,folder)

grade <- write.estimates.csv(list.estimates.turnerless, folder,name,"Frequentist.random.DL")

get.network.pdf(pairwise, measure, folder, "Pain_relief_network")

out <- get.grade.csv(pairwise, measure, folder, name.grade, grade)

#### Global ####

df=read_excel("ADP outcomes revised.xlsx",sheet = "Global 6h")
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
name <- "Global.csv"
name.grade <- "grade_Global.csv"
folder <- "Global"
type.filter <- "Frequentist.random.DL"

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder,c(-10,10),c(-10,10))

grade <- write.estimates.csv(list.estimates.turnerless, folder,name,filter.type = "Frequentist.random.DL")

get.network.pdf(pairwise, measure, folder, "Global_network")

out <- get.grade.csv(pairwise, measure, folder, name.grade, grade)


#### Rescue ####

df=read_excel("ADP outcomes revised.xlsx",sheet = "Rescue analgesia 6h",na=c("","NR"))
df %<>% filter(!is.na(responder)) %>% mutate(studlab=paste0(firstauthor,"_",refid)) %>% 
  group_by(studlab,intervention_1,intervention_2) %>% summarise(responder=sum(responder),sample=sum(sample)) %>% 
  ungroup() %>% mutate(responder=round(responder,0))

pairwise=pairwise(intervention_1, event = responder, n = sample,studlab = studlab, data = df, sm = "OR")

pairwise %<>% as_tibble() %>% rename(t1=treat1,t2=treat2,study=studlab,tid1=intervention_21,tid2=intervention_22,
                                     e.events=event1,c.events=event2,e.total=n1,c.total=n2) %>% 
  select(study,t1,tid1,t2,tid2,e.total,e.events,c.total,c.events)

for (i in 1:nrow(pairwise)){
  if ((pairwise[i,"t1"] == "Placebo") | (as.numeric(pairwise[i,"tid1"]) > as.numeric(pairwise[i, "tid2"]))){
    auxt = pairwise[i,"t2"]
    pairwise[i,"t2"] = pairwise[i,"t1"]
    pairwise[i,"t1"] = auxt
    
    auxtid = pairwise[i,"tid2"]
    pairwise[i,"tid2"] = pairwise[i,"tid1"]
    pairwise[i,"tid1"] = auxtid
    
    auxn = pairwise[i,"c.total"]
    pairwise[i,"c.total"] = pairwise[i,"e.total"]
    pairwise[i,"e.total"] = auxn
    
    auxmean = pairwise[i,"c.events"]
    pairwise[i,"c.events"] = pairwise[i,"e.events"]
    pairwise[i,"e.events"] = auxmean
  }
}

pairwise = pairwise %>% select(study,t1,t2,e.total,e.events,c.total,c.events)

####

TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=pairwise %>% filter(t1=="Placebo" | t2=="Placebo") %>% 
  mutate(rate=c.events/c.total) %>%
  summarise(median=median(rate,na.rm = T)) %>% as.numeric()


measure <- "OR"
name <- "Rescue.csv"
name.grade <- "grade_Rescue.csv"
folder <- "Rescue"
type.filter <- "Frequentist.random.DL"

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)
#list.estimates <- getestimates(pairwise, TP, TP1, baseline, measure, name,folder)

grade <- write.estimates.csv(list.estimates.turnerless, folder,name,"Frequentist.random.DL")

get.network.pdf(pairwise, measure, folder, "Rescue_network")

out <- get.grade.csv(pairwise, measure, folder, name.grade, grade)


