# ###Fonction description 
desc <- function(vars,labels,data)
{
  data.desc <- data[,vars]
  res <-c(1:3)
  lvide <- c("","","")
  
  for(i in 1:length(vars))
  {
    if (is.numeric(data.desc[,i]))
    {
      n <- sum(!is.na(data.desc[,i]))
      na <- sum(is.na(data.desc[,i]))
      pct.na <- round(na/nrow(data.desc)*100,1)
      moy <- round(mean(data.desc[,i],na.rm=TRUE),1)
      sd <- round(sd(data.desc[,i],na.rm=TRUE),1)
      med <- round(median(data.desc[,i],na.rm=TRUE),1)
      p25 <- round(quantile(data.desc[,i],c(0.25),na.rm=TRUE),2)
      p75 <- round(quantile(data.desc[,i],c(0.75),na.rm=TRUE),2)
      t33 <- round(quantile(data.desc[,i],c(0.33),na.rm=TRUE),2)
      t66 <- round(quantile(data.desc[,i],c(0.66),na.rm=TRUE),2)
      min <- round(min(data.desc[,i],na.rm=TRUE),1)
      max <- round(max(data.desc[,i],na.rm=TRUE),1)
      
      l1 <- c(labels[i], "", "")
      l2 <- c("","Mean (SD)",paste(moy," (",sd,")",sep=""))
      l3 <- c("","Median (IQR)",paste(med," (",p25,"-",p75,")",sep=""))
      l3b <- c("","Tercile (T1-T3)",paste(t33,"-",t66,sep=""))
      l4 <- c("","Min, Max",paste(min,", ",max,sep=""))
      l5<- c("","N",paste(n," (",100-pct.na,"%)", sep=""))
      l6 <- c("","Missing",paste(na," (",pct.na,"%)", sep=""))
      l123456 <- rbind(l1,l2,l3,l3b,l4,lvide, l5,l6)
      # l1234 <- rbind(l1,l2)
      
      table.desc <- rbind(res,l123456)
      res <-table.desc
    }
    
    if (is.factor(data.desc[,i]))
    {
      n <- sum(!is.na(data.desc[,i]))
      na <- sum(is.na(data.desc[,i]))
      pct.na <- round(na/nrow(data.desc)*100,1)
      n.tab <- as.vector(table(data.desc[,i]))
      n.pct <- round((n.tab/n)*100,2)
      mod <- levels(data.desc[,i])
      
      l1 <- c(paste0(labels[i], ", n(%)"), "", "")
      for(k in 1:length(mod)){
        l2 <- c("",mod[k],paste(n.tab[k]," (",n.pct[k],")",sep=""))
        l <- rbind(l1,l2)
        l1<- l
      }
      l5<- c("","N",paste(n," (",100-pct.na,"%)", sep=""))
      l6 <- c("","Missing", paste(na," (",pct.na,"%)", sep=""))
      l123 <- l
      table.desc <- rbind(res,l123,lvide,l5,l6)
      res <-table.desc
      
    }
  }
  table.desc <- table.desc[-1,]
  colnames(table.desc)=c("Variables","","Description")
  return(table.desc)
}
