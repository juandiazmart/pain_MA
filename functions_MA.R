library(tidyverse)
library(metafor)
library(magrittr)
library(netmeta)

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

order.df2 <- function(pairwise){
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
  return(pairwise)
}

getestimates.turnerless <- function(data, baseline, measure, name.pdf,folder,xlim=c(-20,20),alim=c(-20,20),Placebo="Placebo"){
  
  data=as.data.frame(data) # tibble doesnt work for subsetting
  #get 1 subdataframes with the treatment columns
  treatments <- select(data, t1, t2)
  
  #all the categories that could be found, even swapped columns
  categories <- distinct(treatments)
  
  #lists of the objects obtained in the loop 
  list.effsize = list()
  list.estimates = list()
  
  pathname <- paste0(folder,"/output/", gsub(".{4}$", "", name.pdf),".pdf")
  
  pdf(pathname, width = 8, height = 5, pointsize = 6)
  
  for (i in 1:nrow(categories)) {
    p1 <- categories[i,1]
    p2 <- categories[i,2]
    
    if (measure == "OR") {
      
      effsize <- escalc(measure = measure,
                        ai = e.events,  n1i = e.total,
                        ci = c.events, n2i = c.total,
                        slab = study,
                        subset = ((data[,"t1"]==p1&data[,"t2"]==p2)|(data[,"t1"]==p2&data[,"t2"]==p1)),
                        data = data)
      
      yrange <- c(-7 - nrow(effsize), 1)
      forest.default(effsize$yi, vi = effsize$vi, refline = 0,
                     rows = seq(-2, -length(effsize$yi) - 1, by = -1),width=0,
                     alim = c(-2.5,2.5),
                     xlim = c(-10,10),
                     ylim = yrange, top=2, steps=5, level=95,
                     xlab="Odds ratio", slab = effsize[,"study"],efac=1, pch=15,cex=1.5,cex.lab=1.5,
                     atransf=exp, digits=2)
      
    } else if (measure == "ROM") {
      
      effsize <- escalc(measure = measure,
                        m1i = mean1,  sd1i = sd1, n1i = n1,
                        m2i = mean2,  sd2i = sd2, n2i = n2,
                        slab = study,
                        subset = ((data[,"t1"]==p1&data[,"t2"]==p2)|(data[,"t1"]==p2&data[,"t2"]==p1)),
                        data = data)
      
      yrange <- c(-7 - nrow(effsize), 1)
      forest.default(effsize$yi, vi = effsize$vi, refline = 0,
                     rows = seq(-2, -length(effsize$yi) - 1, by = -1),width=0,
                     alim = c(-2.5, 2.5),
                     xlim = c(-10,10),
                     ylim = yrange, top=2, steps=5, level=95,
                     xlab="Ratio of means", slab = effsize[,"study"],efac=1, pch=15,cex=1.5,cex.lab=1.5,
                     atransf=exp, digits=2)
      
    } else if (measure == "MD") {
      
      effsize <- escalc(measure = measure,
                        m1i = mean1,  sd1i = sd1, n1i = n1,
                        m2i = mean2,  sd2i = sd2, n2i = n2,
                        slab = study,
                        subset = ((data[,"t1"]==p1&data[,"t2"]==p2)|(data[,"t1"]==p2&data[,"t2"]==p1)),
                        data = data)
      
      yrange <- c(-7 - nrow(effsize), 1)
      forest.default(effsize$yi, vi = effsize$vi, refline = 0,
                     rows = seq(-2, -length(effsize$yi) - 1, by = -1),width=0,
                     alim = alim,
                     xlim = xlim,
                     ylim = yrange, top=2, steps=5, level=95,
                     xlab="Mean difference", slab = effsize[,"study"],efac=1, pch=15,cex=1.5,cex.lab=1.5,
                     digits=2)
    }
    
    to.append <- effsize
    
    #if the new dataframe isn't already in the compiling list, append it
    #this filters out swapped treatments in columns t1 and t2
    if (is.element(list(to.append), list.effsize) == FALSE) {
      
      titlename <- paste(gsub('.{4}$', '', name.pdf), 
                         ": ", 
                         p1,
                         " vs ",
                         p2,
                         sep = "")
      
      title(titlename, line=0)
      colvec <- c("green","blue","red","yellow")
      par(mar=c(4,0.5,2,0.5))
      
      list.effsize[[i]] <- to.append
      
      rma.fixed <- rma.uni(effsize, method="FE")
      rma.random.DL <- rma.uni(effsize, method="DL")
      
      #concat of the summarys, called estimates, same scheize, calculate and append
      estimates <- rbind("Frequentist.fixed" = c(sqrt(rma.fixed$tau2.fix), rma.fixed$b, rma.fixed$ci.lb, rma.fixed$ci.ub),
                         "Frequentist.random.DL" = c(sqrt(rma.random.DL$tau2), rma.random.DL$b, rma.random.DL$ci.lb, rma.random.DL$ci.ub))

      if (measure == "OR") {

        for (j in 1:nrow(estimates)){
          addpoly(estimates[j,2], ci.lb=estimates[j,3], ci.ub=estimates[j,4], atransf=exp,
                  mlab=row.names(estimates)[j], rows=yrange[1]+5-j, col=colvec[j],cex=1.5,width =0)}

        if (p1 == Placebo | p2 == Placebo){

          estimates = estimates %>%
            as_tibble(rownames="type") %>% rename("tau"="V1", "mu"="V2", "95% lower"="V3", "95% upper" = "V4") %>%
            mutate(t1=p1,t2=p2,OR=exp(mu),OR_l=exp(`95% lower`),OR_u=exp(`95% upper`),
                   risk=-1*1000*(baseline-((OR*baseline)/(1-baseline+OR*baseline))),
                   risk_l=-1*1000*(baseline-((OR_l*baseline)/(1-baseline+OR_l*baseline))),
                   risk_u=-1*1000*(baseline-((OR_u*baseline)/(1-baseline+OR_u*baseline)))) %>%
            rename(mu_l=`95% lower`,mu_u=`95% upper`)
        } else {

          estimates = estimates %>% as_tibble(rownames="type") %>% rename("tau"="V1", "mu"="V2", "95% lower"="V3", "95% upper" = "V4") %>%
            mutate(t1=p1,t2=p2,OR=exp(mu),OR_l=exp(`95% lower`),OR_u=exp(`95% upper`)) %>%
            rename(mu_l=`95% lower`,mu_u=`95% upper`)

        }
      } else if (measure == "ROM") {

        for (j in 1:nrow(estimates)){
          addpoly(estimates[j,2], ci.lb=estimates[j,3], ci.ub=estimates[j,4], atransf=exp,
                  mlab=row.names(estimates)[j], rows=yrange[1]+5-j, col=colvec[j],cex=1.5,width =0)}

        if (p1 == Placebo | p2 == Placebo){

          estimates = estimates %>%
            as_tibble(rownames="type") %>% rename("tau"="V1", "mu"="V2", "95% lower"="V3", "95% upper" = "V4") %>%
            mutate(t1=p1,t2=p2,ROM=exp(mu),ROM_l=exp(`95% lower`),ROM_u=exp(`95% upper`),
                   risk=mu_exp*baseline-baseline,
                   risk_l=mu_l*baseline-baseline,
                   risk_u=mu_u*baseline-baseline) %>%
            rename(mu_l=`95% lower`,mu_u=`95% upper`)
        } else {

          estimates = estimates %>% as_tibble(rownames="type") %>% rename("tau"="V1", "mu"="V2", "95% lower"="V3", "95% upper" = "V4")%>%
            mutate(t1=p1,t2=p2,ROM=exp(mu),ROM_l=exp(`95% lower`),ROM_u=exp(`95% upper`)) %>%
            rename(mu_l=`95% lower`,mu_u=`95% upper`)

        }

      } else if (measure == "MD") {

        for (j in 1:nrow(estimates)){
          addpoly(estimates[j,2], ci.lb=estimates[j,3], ci.ub=estimates[j,4],
                  mlab=row.names(estimates)[j], rows=yrange[1]+5-j, col=colvec[j],cex=1.5,width =0)}

        estimates = estimates %>% as_tibble(rownames="type") %>% rename("tau"="V1", "mu"="V2", "95% lower"="V3", "95% upper" = "V4") %>% mutate(t1=p1,t2=p2) %>%
          rename(mu_l=`95% lower`,mu_u=`95% upper`)

      }
      estimates.to.append <- estimates
      
      list.estimates[[i]] <- estimates.to.append

    }
  }
  dev.off()
  return(list.estimates)
}

swap.lower.upper <- function(df, t1, t2, mean, lower, upper){
  dfout = df %>% rename("t1"="t2", "t2"="t1") %>%
    mutate(mean = (-1)*mean,
           lower_aux = (-1)*upper, 
           upper = (-1)*lower) %>%
    select(t1, t2, mean, lower = lower_aux, upper)
  return(dfout)
}

get.RD2 <-function(t1,t2, column1, column2, baseline){
  dfout = data.frame(matrix(ncol=5,nrow=length(t1), dimnames=list(NULL, c("t1", "t2", "cr1", "cr2", "RD"))))
  dfout$t1 <- t1
  dfout$t2 <- t2
  dfout$cr1 <- exp(column1 + log(baseline) - log(1 + baseline*(exp(column1) - 1)))
  dfout$cr2 <- exp(column2 + log(baseline) - log(1 + baseline*(exp(column2) - 1)))
  dfout$RD <- 1000*(dfout$cr1 - dfout$cr2)
  dfout = dfout %>% select(t1,t2,RD)
  return(dfout)
}

get.grade.csv <- function(pairwise, measure, folder, name, grade,filter, baseline = 0, Placebo = "Placebo"){
  
  if (measure=="MD") {
    contrast_df=pairwise(list(t1,t2), mean = list(mean1,mean2), n = list(n1,n2),sd=list(sd1,sd2),studlab = study, data = pairwise, sm = measure)
  } else {
    contrast_df=pairwise(list(t1,t2), event = list(e.events,c.events), n = list(e.total,c.total),studlab = study, data = pairwise, sm = measure) 
  }

  network=netmeta(contrast_df,reference.group = Placebo,details.chkmultiarm = T)
  split=netsplit(network)
  
  pathname <- paste0(folder,"/output/", gsub(".{4}$", "", name),"_netsplit",".pdf")
  pdf(pathname, width = 9, height = 18)
  
  forest(split,pooled = ifelse(filter=="Frequentist.random.DL","random","fixed"))
  dev.off()
  
  if (measure=="MD") {
    grade=grade %>% rename(relative_direct = mu, relative_direct_lower = mu_l, relative_direct_upper = mu_u) %>%
      select(t1,t2,relative_direct,relative_direct_lower,relative_direct_upper)
  } else {
    grade=grade %>% rename(relative_direct = mu, relative_direct_lower = mu_l, relative_direct_upper = mu_u,
                           absolute_direct=risk,absolute_direct_lower=risk_l,absolute_direct_upper=risk_u) %>%
      select(t1,t2,relative_direct,relative_direct_lower,relative_direct_upper,
             absolute_direct,absolute_direct_lower,absolute_direct_upper)
  }
  
  
  if (filter=="Frequentist.random.DL") {
    random=split$random[,c(1,2,4,5)]
  } else {
    random=split$fixed[,c(1,2,4,5)]
  }
  
  random$t1 <- str_split_fixed(random$comparison, ":", 2)[,1]
  random$t2 <- str_split_fixed(random$comparison, ":", 2)[,2]
  random= random %>% rename(relative_nma = TE, relative_nma_lower = lower, relative_nma_upper = upper) %>%
    select(t1,t2,relative_nma,relative_nma_lower,relative_nma_upper)
  random.inv = swap.lower.upper(random, random$t1, random$t2, random$relative_nma, 
                                random$relative_nma_lower, random$relative_nma_upper)
  
  if (measure=="OR") {
    random.RD = random %>% filter(t2 == Placebo) %>% select(t1,relative_nma,relative_nma_lower,relative_nma_upper)
    random.RD[nrow(random.RD) + 1,] = list(Placebo,0,0,0)

    cross.RD = full_join(random.RD, random.RD, by = character())
    
    cross.RD.abs = get.RD2(cross.RD$t1.x,cross.RD$t1.y, cross.RD$relative_nma.x, cross.RD$relative_nma.y, baseline) %>% 
      rename(absolute_nma = RD)
    cross.RD.low = get.RD2(cross.RD$t1.x,cross.RD$t1.y, cross.RD$relative_nma_lower.x, cross.RD$relative_nma_lower.y, baseline) %>% 
      rename(absolute_nma_lower = RD)
    cross.RD.upp = get.RD2(cross.RD$t1.x,cross.RD$t1.y, cross.RD$relative_nma_upper.x, cross.RD$relative_nma_upper.y, baseline) %>% 
      rename(absolute_nma_upper = RD)
    
    cross.RDout = left_join(cross.RD, cross.RD.abs, by = c("t1.x" = "t1", "t1.y" = "t2"))
    cross.RDout = left_join(cross.RDout, cross.RD.low, by = c("t1.x" = "t1", "t1.y" = "t2"))
    cross.RDout = left_join(cross.RDout, cross.RD.upp, by = c("t1.x" = "t1", "t1.y" = "t2")) %>%
      rename(t1 = t1.x, t2 = t1.y) %>%
      select(t1,t2,absolute_nma,absolute_nma_lower,absolute_nma_upper)
  
  }
  
  if (filter=="Frequentist.random.DL") {
    indirect=split$indirect.random[,c(1,2,4,5)]
  } else {
    indirect=split$indirect.fixed[,c(1,2,4,5)]
  }
  
  
  indirect$t1 <- str_split_fixed(indirect$comparison, ":", 2)[,1]
  indirect$t2 <- str_split_fixed(indirect$comparison, ":", 2)[,2]
  indirect = indirect %>% rename(relative_indirect = TE, relative_indirect_lower = lower, relative_indirect_upper = upper) %>%
    select(t1,t2,relative_indirect,relative_indirect_lower,relative_indirect_upper)
  indirect.inv = swap.lower.upper(indirect, indirect$t1, indirect$t2, indirect$relative_indirect, 
                                  indirect$relative_indirect_lower, indirect$relative_indirect_upper)
  
  randindaux = inner_join(random, indirect, by=c("t1"="t1","t2"="t2"))
  randindaux.inv = inner_join(random.inv, indirect.inv, by=c("t1"="t1","t2"="t2"))
  
  outaux = right_join(grade, randindaux.inv, by=c("t1"="t1","t2"="t2"))
  outbase = right_join(grade, randindaux, by=c("t1"="t1","t2"="t2"))
  
  if (nrow(outaux) !=0){
    for (i in 1:nrow(outbase)) {
      for (j in 1:nrow(outaux)){
        if (((outbase[i,1]==outaux[j,2]) & (outbase[i,2]==outaux[j,1])) & is.na(outbase[i,3])){
          for (k in 1:ncol(outbase)){
            outbase[i,k] = outaux[j,k]
          }
        }
      }
    }
  }
  
  if (measure=="MD") {
    outbase %>% 
      write_csv(paste0(folder,"/output/", name))
  } else if (measure=="OR") {
    outbase = left_join(outbase, cross.RDout, by= c("t1" = "t1", "t2" = "t2"))
    
    outbase %>% mutate_at(vars(-t1, -t2,-absolute_direct,-absolute_direct_lower,-absolute_direct_upper,
                               -absolute_nma,-absolute_nma_lower,-absolute_nma_upper), exp) %>% 
      relocate(starts_with("absolute_nma"),.after = relative_nma_upper) %>% 
      mutate(
        absolute_indirect = case_when(
          relative_nma==relative_indirect ~ absolute_nma,
          t2==Placebo     ~ -1*1000*(baseline-((relative_indirect*baseline)/(1-baseline+relative_indirect*baseline))),
          TRUE                      ~ NA_real_
        ),
        absolute_indirect_lower = case_when(
          relative_nma==relative_indirect ~ absolute_nma_lower,
          t2==Placebo     ~ -1*1000*(baseline-((relative_indirect_lower*baseline)/(1-baseline+relative_indirect_lower*baseline))),
          TRUE                      ~ NA_real_
        ),
        absolute_indirect_upper = case_when(
          relative_nma==relative_indirect ~ absolute_nma_upper,
          t2==Placebo     ~ -1*1000*(baseline-((relative_indirect_upper*baseline)/(1-baseline+relative_indirect_upper*baseline))),
          TRUE                      ~ NA_real_
        )
      ) %>% 
      write_csv(paste0(folder,"/output/", name))
  } else {
    outbase %>% mutate_at(vars(-t1, -t2,-absolute_direct,-absolute_direct_lower,-absolute_direct_upper), exp) %>% 
      write_csv(paste0(folder,"/output/", name))
  }
  
  return(outbase)
}

get.network.pdf <- function(pairwise, measure, folder, name, Placebo = "Placebo"){
  
  pathname <- paste0(folder,"/output/", gsub(".{4}$", "", name),".pdf")
  pdf(pathname, width = 8, height = 5, pointsize = 6)
  
  if (measure=="MD") {
    contrast_df=pairwise(list(t1,t2), mean = list(mean1,mean2), n = list(n1,n2),sd=list(sd1,sd2),studlab = study, data = pairwise, sm = measure)
  } else {
    contrast_df=pairwise(list(t1,t2), event = list(e.events,c.events), n = list(e.total,c.total),studlab = study, data = pairwise, sm = measure) 
  }
  
  network=netmeta(contrast_df,reference.group = Placebo,details.chkmultiarm = T)
  netgraph(network,multiarm = F)
  dev.off()
}

write.estimates.csv <- function(list.estimates ,folder,name, filter.type="Turner Prior") {
  
  rows.estimates <- tibble()
  for (i in 1:length(list.estimates)) {
    est <- as.data.frame(list.estimates[i][1])
    rows.estimates <- bind_rows(rows.estimates, est)
  }
  rows.estimates %<>% filter(type==filter.type)
  return(rows.estimates)
}

get.pscore <- function(pairwise, measure, folder, name,type.filter="Frequentist.fixed",small.values="bad", Placebo = "Placebo"){
  
  if (measure=="MD") {
    contrast_df=pairwise(list(t1,t2), mean = list(mean1,mean2), n = list(n1,n2),sd=list(sd1,sd2),studlab = study, data = pairwise, sm = measure)
  } else {
    contrast_df=pairwise(list(t1,t2), event = list(e.events,c.events), n = list(e.total,c.total),studlab = study, data = pairwise, sm = measure) 
  }
  
  network=netmeta(contrast_df,reference.group = Placebo,details.chkmultiarm = T)
  score=netrank(network, small.values = small.values)
  
  if (type.filter=="Frequentist.fixed") {
    score$Pscore.fixed %>% as_tibble(rownames = "intervention") %>% rename(pscore=value) %>% 
      arrange(desc(pscore)) %>% write_csv(paste0(folder,"/output/pscore_", name))
  } else { 
    score$Pscore.random %>% as_tibble(rownames = "intervention") %>% rename(pscore=value) %>% 
      arrange(desc(pscore)) %>% write_csv(paste0(folder,"/output/pscore_", name))
  }
  
}
