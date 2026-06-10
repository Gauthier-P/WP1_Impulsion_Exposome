comp <- function(vars,comp.var,labels,label.comp,data)
{
  data.comp <- data[,c(vars)]
  res <-c(1:7)
  l0 <- c("","","","","","","")
  lvide <- c(NA,NA,NA,NA,NA,NA,NA)
  test <-"test"
  mod <- levels(comp.var)
  for(i in 1:length(vars)){
    if (is.numeric(data.comp[,i]))
    {
      if (length(data.comp[,i][!is.na(data.comp[,i])])<5000){norm.test <- shapiro.test(data.comp[,i])$p.value}else{norm.test=0.02}
      if(norm.test > 0.05)
      {
        l1 <-c(paste(labels[i],sep=""),NA)
        l2 <-c(NA,"Mean (SD)")
        for(k in 1:length(mod))
        {
          data.k <- data.comp[comp.var==mod[k] & !is.na(comp.var),]
          n <- sum(!is.na(data.k[,i]))
          moy <- round(mean(data.k[,i],na.rm=TRUE),1)
          sd <- round(sd(data.k[,i],na.rm=TRUE),1)
          med <- round(median(data.k[,i],na.rm=TRUE),1)
          p25 <- round(quantile(data.k[,i],c(0.25),na.rm=TRUE),1)
          p75 <- round(quantile(data.k[,i],c(0.75),na.rm=TRUE),1)
          
          l0[k+2] <- paste("n=",n)
          l1 <- c(l1,NA)
          l2 <- c(l2,paste(moy," (",sd,")",sep=""))
        }
        if(length(mod)==2)
        {
          p.val <- t.test(data.comp[,i]~comp.var,var.equal=FALSE)$p.value
          test <- "T-test"}
        if(length(mod)>2)
        {
          p.val <- 1-pf(summary(lm(data.comp[,i]~comp.var))$fstatistic[1],summary(lm(data.comp[,i]~comp.var))$fstatistic[2],summary(lm(data.comp[,i]~comp.var))$fstatistic[3])
          test <- "One-way Anova test"}
        
        l1 <- c(l1,p.val, test)
        l2 <- c(l2,NA, NA)
        l12 <- rbind(l1,l2)
      }
      
      if(norm.test <= 0.05)
      {   
        l1 <-c(paste(labels[i],sep=""),NA)
        l2 <-c(NA,"Median (IQR)")
        for(k in 1:length(mod))
        {
          data.k <- data.comp[comp.var==mod[k] & !is.na(comp.var),]
          n <- sum(!is.na(data.k[,i]))
          moy <- round(mean(data.k[,i],na.rm=TRUE),1)
          sd <- round(sd(data.k[,i],na.rm=TRUE),1)
          med <- round(median(data.k[,i],na.rm=TRUE),1)
          p25 <- round(quantile(data.k[,i],c(0.25),na.rm=TRUE),1)
          p75 <- round(quantile(data.k[,i],c(0.75),na.rm=TRUE),1)
          
          l1 <- c(l1,NA)
          l2 <- c(l2,paste(med," (",p25,"-",p75,")",sep=""))
        }
        if(length(mod)==2)
        {
          p.val <- wilcox.test(data.comp[,i]~comp.var)$p.value
          test <- "Wilcoxon rank test"}
        if(length(mod)>2)
        {
          p.val <- kruskal.test(data.comp[,i]~comp.var)$p.value
          test <- "Kruskal-Wallis test"}
        
        l1 <- c(l1,p.val,test)
        l2 <- c(l2,NA,NA)
        l12 <- rbind(l1,l2)
      }
      table.comp <- rbind(res,l12,lvide)
      res <-table.comp
    }
    
    if (is.factor(data.comp[,i]))
    {
      mod2 <- levels(data.comp[,i])
      
      chi2_result <- tryCatch(
        {
          list(p.val = chisq.test(data.comp[,i], comp.var)$p.value,
               test  = "Chi-squared test")
        },
        error   = function(e) list(p.val = NA, test = "Chi-squared test (not done)"),
        warning = function(w) list(p.val = NA, test = "Chi-squared test (not done)")
      )
      p.val <- chi2_result$p.val
      test  <- chi2_result$test
      
      for(j in 1:length(mod2))
      {
        l1 <- c(paste(labels[i],sep=""),NA)
        l2 <- c(NA,paste(mod2[j],", n(%)",sep=""))
        for(k in 1:length(mod))
        {
          data.k <- data.comp[comp.var==mod[k] & !is.na(comp.var),]
          n <- sum(!is.na(data.k[,i]))
          n.tab <- as.vector(table(data.k[,i]))
          n.pct <- round((n.tab/n)*100,2)
          l0[k+2] <- paste("n=",n)
          if (j==1 & k!=length(mod)) 
          {
            l1 <- c(l1,NA)
            l2 <- c(l2,paste(n.tab[j]," (",n.pct[j],")",sep=""))
          }
          if (j==1 & k==length(mod))
          {
            l1 <- c(l1,NA,p.val, test)
            l2 <- c(l2,paste(n.tab[j]," (",n.pct[j],")",sep=""),NA,NA)
          }
          if (j>1 & k!=length(mod))
          {
            l1 <- l
            l2 <- c(l2,paste(n.tab[j]," (",n.pct[j],")",sep=""))
          }
          if (j>1 & k==length(mod))
          {
            l1 <- l
            l2 <- c(l2,paste(n.tab[j]," (",n.pct[j],")",sep=""),NA,NA)
          }            
        }
        l <- rbind(l1,l2)
      }
      
      table.comp <- rbind(res,l,lvide)
      res <-table.comp
    }
  }
  
  nb.col <- dim(table.comp)[2]
  table.comp[,nb.col-1] <- ifelse(as.numeric(table.comp[,nb.col-1])<0.05 & table.comp[,nb.col-1]!=0,"<.001",table.comp[,nb.col-1])
  #table.comp[,nb.col-1] <- ifelse(table.comp[,nb.col-1]<0.05 & table.comp[,nb.col-1]!=0,paste(table.comp[,nb.col-1],sep=""),table.comp[,nb.col-1])
  table.comp[,nb.col-1] <- ifelse(table.comp[,nb.col-1]==0,"<0.001",table.comp[,nb.col-1])
  
  table.comp <- rbind(l0,table.comp[-1,])
  colnames(table.comp)=c("Variable","", paste(label.comp[1:length(mod)],sep=""),"p", "Test")
  return(table.comp)
}

