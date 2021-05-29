source("functions_MA.R")


df=read_excel("Formatted TOTPAR.SPID.Global (continuous).xlsx",sheet = "Reformatted 2 TOTPAR 6")
df %<>% mutate(studlab=paste0(firstauthor,"_",refid)) %>% rename(mean=`converted_central tendency`,sd=converted_variability)

pairwise=pairwise(intervention_1, mean = mean, n = sampleSize,sd=sd,studlab = studlab, data = df, sm = "MD")

pairwise %<>% as_tibble() %>% rename(t1=treat1,t2=treat2,study=studlab) %>% select(study,t1,t2,n1,mean1,sd1,n2,mean2,sd2)


####

TP <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "placebo / control")

TP1 <- TurnerEtAlPrior("signs / symptoms reflecting continuation / end of condition", "pharma", "pharma")

baseline=pairwise %>% filter(t1=="Placebo" | t2=="Placebo") %>% 
  summarise(median=median(mean2)) %>% as.numeric()

measure <- "MD"
name <- "TOTPAR.csv"
folder <- "TOTPAR"


list.estimates <- getestimates(pairwise, TP, TP1, baseline, measure, name,folder)

write.estimates.csv(list.estimates, folder,name)

#############
#############

