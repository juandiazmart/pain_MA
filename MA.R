# wd <- "/home/antonio/pain_MA"
# setwd(wd)
source("functions_MA2.R")
#### TOTPAR ####

df=read_excel("ADP outcomes revised 2.xlsx",sheet = "TOTPAR 6h")
df %<>% mutate(studlab=paste0(firstauthor,"_",refid)) %>% rename(mean=`converted_central tendency`,sd=converted_variability)

pairwise=pairwise(intervention_1, mean = mean, n = sampleSize,sd=sd,studlab = studlab, data = df, sm = "MD")

pairwise %<>% as_tibble() %>% rename(t1=treat1,t2=treat2,study=studlab,tid1=intervention_21,tid2=intervention_22) %>% 
  select(study,t1,tid1,t2,tid2,n1,mean1,sd1,n2,mean2,sd2)

order.df <- function(pairwise){
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
  return(pairwise)
}
pairwise = order.df(pairwise)

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
type.filter <- "Frequentist.fixed"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = F)


list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

grade <- write.estimates.csv(list.estimates.turnerless, folder,name,filter.type = type.filter)

get.network.pdf(pairwise, measure, folder, "TOTPAR_network")

out <- get.grade.csv(pairwise, measure, folder, name.grade, grade, filter=type.filter)

#### SPID ####

df=read_excel("ADP outcomes revised 2.xlsx",sheet = "SPID 6h UPDATED")
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

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)
#list.estimates <- getestimates(pairwise, TP, TP1, baseline, measure, name,folder)

grade <- write.estimates.csv(list.estimates.turnerless, folder,name,type.filter)

get.network.pdf(pairwise, measure, folder, "SPID_network")

out <- get.grade.csv(pairwise, measure, folder, name.grade, grade, filter=type.filter)

#### Pain relief ####

df=read_excel("ADP outcomes revised 2.xlsx",sheet = "Pain relief 6h",na=c("","NR"))
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

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder,c(-10,10),c(-10,10))
#list.estimates <- getestimates(pairwise, TP, TP1, baseline, measure, name,folder)

grade <- write.estimates.csv(list.estimates.turnerless, folder,name,type.filter)

get.network.pdf(pairwise, measure, folder, "Pain_relief_network")

out <- get.grade.csv(pairwise, measure, folder, name.grade, grade,filter=type.filter)

#### Global ####

df=read_excel("ADP outcomes revised 2.xlsx",sheet = "Global 6h")
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

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder,c(-10,10),c(-10,10))

grade <- write.estimates.csv(list.estimates.turnerless, folder,name,type.filter)

get.network.pdf(pairwise, measure, folder, "Global_network")

out <- get.grade.csv(pairwise, measure, folder, name.grade, grade, filter=type.filter)


#### Rescue ####

df=read_excel("ADP outcomes revised 2.xlsx",sheet = "Rescue analgesia 6h",na=c("","NR"))
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

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)
#list.estimates <- getestimates(pairwise, TP, TP1, baseline, measure, name,folder)

grade <- write.estimates.csv(list.estimates.turnerless, folder,name,type.filter)

get.network.pdf(pairwise, measure, folder, "Rescue_network")

out <- get.grade.csv(pairwise, measure, folder, name.grade, grade, baseline,filter=type.filter)

#### AE (Drowsiness) ####

df=read_excel("ADP adverse effects.xlsx",na=c("","NR"))
AE=unique(df$adverse_effect)
df %<>% filter(!is.na(responder),adverse_effect==AE[1]) %>% mutate(studlab=paste0(firstauthor,"_",refid))
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
name <- "AE_drowsiness.csv"
folder <- "AE"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates.turnerless, folder,name,type.filter) %>% 
  write_csv(file = paste0(folder,"/output/", name))


#### AE (Nausea) ####

df=read_excel("ADP adverse effects.xlsx",na=c("","NR"))
df %<>% filter(!is.na(responder),adverse_effect==AE[2]) %>% mutate(studlab=paste0(firstauthor,"_",refid))
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
name <- "AE_nausea_vom.csv"
folder <- "AE"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates.turnerless, folder,name,type.filter) %>% 
  write_csv(file = paste0(folder,"/output/", name))

#### AE (Headache) ####

df=read_excel("ADP adverse effects.xlsx",na=c("","NR"))
df %<>% filter(!is.na(responder),adverse_effect==AE[3]) %>% mutate(studlab=paste0(firstauthor,"_",refid))
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
name <- "AE_headache.csv"
folder <- "AE"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates.turnerless, folder,name,type.filter) %>% 
  write_csv(file = paste0(folder,"/output/", name))

#### AE (Dizziness) ####

df=read_excel("ADP adverse effects.xlsx",na=c("","NR"))
df %<>% filter(!is.na(responder),adverse_effect==AE[4]) %>% mutate(studlab=paste0(firstauthor,"_",refid))
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
name <- "AE_dizziness.csv"
folder <- "AE"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates.turnerless, folder,name,type.filter) %>% 
  write_csv(file = paste0(folder,"/output/", name))

#### AE (Vision) ####

df=read_excel("ADP adverse effects.xlsx",na=c("","NR"))
df %<>% filter(!is.na(responder),adverse_effect==AE[5]) %>% mutate(studlab=paste0(firstauthor,"_",refid))
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
name <- "AE_vision.csv"
folder <- "AE"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates.turnerless, folder,name,type.filter) %>% 
  write_csv(file = paste0(folder,"/output/", name))

#### AE (Dyspepsia) ####

df=read_excel("ADP adverse effects.xlsx",na=c("","NR"))
df %<>% filter(!is.na(responder),adverse_effect==AE[6]) %>% mutate(studlab=paste0(firstauthor,"_",refid))
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
name <- "AE_dyspepsia.csv"
folder <- "AE"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates.turnerless, folder,name,type.filter) %>% 
  write_csv(file = paste0(folder,"/output/", name))

#### AE (Dysphagia) ####

df=read_excel("ADP adverse effects.xlsx",na=c("","NR"))
df %<>% filter(!is.na(responder),adverse_effect==AE[7]) %>% mutate(studlab=paste0(firstauthor,"_",refid))
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
name <- "AE_dysphagia.csv"
folder <- "AE"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates.turnerless, folder,name,type.filter) %>% 
  write_csv(file = paste0(folder,"/output/", name))

#### AE (Diarrhea) ####

df=read_excel("ADP adverse effects.xlsx",na=c("","NR"))
df %<>% filter(!is.na(responder),adverse_effect==AE[8]) %>% mutate(studlab=paste0(firstauthor,"_",refid))
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
name <- "AE_diarrhea.csv"
folder <- "AE"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates.turnerless, folder,name,type.filter) %>% 
  write_csv(file = paste0(folder,"/output/", name))

#### AE (abdominal) ####

df=read_excel("ADP adverse effects.xlsx",na=c("","NR"))
df %<>% filter(!is.na(responder),adverse_effect==AE[9]) %>% mutate(studlab=paste0(firstauthor,"_",refid))
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
name <- "AE_abdominal_pain.csv"
folder <- "AE"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates.turnerless, folder,name,type.filter) %>% 
  write_csv(file = paste0(folder,"/output/", name))

#### AE (Mood) ####

df=read_excel("ADP adverse effects.xlsx",na=c("","NR"))
df %<>% filter(!is.na(responder),adverse_effect==AE[10]) %>% mutate(studlab=paste0(firstauthor,"_",refid))
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
name <- "AE_mood.csv"
folder <- "AE"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates.turnerless, folder,name,type.filter) %>% 
  write_csv(file = paste0(folder,"/output/", name))

#### AE (Constipation) ####

df=read_excel("ADP adverse effects.xlsx",na=c("","NR"))
df %<>% filter(!is.na(responder),adverse_effect==AE[11]) %>% mutate(studlab=paste0(firstauthor,"_",refid))
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
name <- "AE_constipation.csv"
folder <- "AE"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates.turnerless, folder,name,type.filter) %>% 
  write_csv(file = paste0(folder,"/output/", name))

#### AE (Synco) ####

df=read_excel("ADP adverse effects.xlsx",na=c("","NR"))
df %<>% filter(!is.na(responder),adverse_effect==AE[12]) %>% mutate(studlab=paste0(firstauthor,"_",refid))
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
name <- "AE_syncope.csv"
folder <- "AE"

write(paste(gsub(".{4}$", "", name),baseline),file="baseline.txt",append = T)

list.estimates.turnerless <- getestimates.turnerless(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates.turnerless, folder,name,type.filter) %>% 
  write_csv(file = paste0(folder,"/output/", name))
