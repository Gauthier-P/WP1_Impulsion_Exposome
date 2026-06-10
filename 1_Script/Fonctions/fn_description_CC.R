###Fonction description 
desc.cc <- function(vars,labels,data)
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
      p25 <- round(quantile(data.desc[,i],c(0.25),na.rm=TRUE),1)
      p75 <- round(quantile(data.desc[,i],c(0.75),na.rm=TRUE),1)
      min <- round(min(data.desc[,i],na.rm=TRUE),1)
      max <- round(max(data.desc[,i],na.rm=TRUE),1)
      
      l1 <- c(labels[i], "", "")
      l2 <- c(NA,"Mean (SD)",paste(moy," (",sd,")",sep=""))
      l3 <- c(NA,"Median (IQR)",paste(med," (",p25,"-",p75,")",sep=""))
      l4 <- c(NA,"Min, Max",paste(min,", ",max,sep=""))
      l1234 <- rbind(l1,l2,l3,l4,lvide,lvide,lvide)
      # l1234 <- rbind(l1,l2)
      
      table.desc <- rbind(res,l1234)
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
      l123 <- l
      table.desc <- rbind(res,l123,lvide,lvide,lvide)
      res <-table.desc
      
    }
  }
  table.desc[1, ] <- c("","", paste("n=",n))
  colnames(table.desc)=c("Variables","","Description")
  return(table.desc)
}
