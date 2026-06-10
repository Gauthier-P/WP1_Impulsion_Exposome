###############################
####------Forest plot------####
###############################
library(cowplot)
library(patchwork)
library(ggplot2)
library(grid)


rm(list = ls())

All <- readRDS("2_Results/Regression/All_air_pollution.rds")
All <- All %>% 
  mutate(term = case_when(
    term == "(Intercept)" ~ "Intercept",
    .default = "Beta"
  ) %>%  factor(levels = c("Beta", "Intercept")))

All <- All %>% 
  mutate(Urbanicity = case_when(
    Urbanicity == "A" ~ "Ensemble",
    Urbanicity == "U" ~ "Urbain",
    Urbanicity == "UI" ~ "Urbain intermédiaire",
    Urbanicity == "R" ~ "Rural"
  ) )


All <- All %>% 
  mutate(Urbanicity = factor(Urbanicity, levels = c("Ensemble", "Rural", "Urbain intermédiaire", "Urbain")))

# All <- All[grepl("_norm", All$Sociale), ]
All<- subset(All , All$Sociale!= "RD")

Polluants <-c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
sociales <-c("High_edu_prop","Q2")

Polluants.N <- paste0(Polluants, "_norm")
sociales.N <- paste0(sociales, "_norm") 

Label.P <- c("MOY.NO2" = "NO2", "MOY.O3" = "O3", "MOY.PM25" ="PM2.5" ,"MOY.PM10"= "PM10",
             "MOY.NO2_norm" = "NO2", "MOY.O3_norm" = "O3", "MOY.PM25_norm" ="PM2.5" ,"MOY.PM10_norm"= "PM10")
Label.S <- c("Low_edu_prop" = "Low education", "Medium_edu_prop" = "Medium education", "High_edu_prop" = "High education", 
             "Prop_Immigre" = "Immigration",
             "D121" = "1er Décile","Q2" = "Revenu médian","D921" = "9e Décile","RD" = "Rapport interdécile D9/D1")

Annee <-c("2011","2015","2021")

All$term_model <- paste(All$term, All$model)
graph_data <-  subset(All, All$model == "M0" & All$Annee == "2021")
expo <- "MOY.PM25_norm"
S <- "High_edu_prop_norm"
legend.p <- "none"
title.para <- element_text(size = 8, face="bold")
Term.p <- "Intercept"

fn_graph <- function(graph_data, S,expo, legend.p, Term){
  
  vertical_line_data <- data.frame(xintercept = 0, xmin = -Inf, xmax = Inf)
  graph_data_I <- filter(graph_data, Sociale == S, Exposure == expo, term == "Intercept")%>%
    mutate(term = droplevels(term))
  
  graph_data_B <- filter(graph_data, Sociale == S, Exposure == expo, term == "Beta")%>%
    mutate(term = droplevels(term))
  
  plot_I <- ggplot(data = graph_data_I ,
                 aes(
                   y = term,
                   x = estimate,
                   xmin = conf.low,
                   xmax = conf.high, 
                   col = Urbanicity,
                   linetype = model
                 ))+
    geom_pointrange(
      aes(colour = Urbanicity),
      linewidth = 0.25, 
      size = 0.25) +
    ggtitle(Label.S[ gsub("_norm$", "", S)])+
    ylab(Label.P[ gsub("_norm$", "", expo)])+
    
    scale_color_manual(values = c("Ensemble" = "#F72585",
                                  "Rural"= "#4CC9F0",
                                  "Urbain intermédiaire"= "#4361EE",
                                  "Urbain"= "#3A0CA3"))+

    theme_classic() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          
          strip.background = element_blank(),
          strip.text = element_blank(),
          
          axis.text.y = element_text(size = 7),
          axis.text.x = element_text(size = 7),
          
          axis.title.y = element_text(size = 8),
          axis.title.x = element_blank(),
          
          title = element_text(size = 8, face="bold"),
          
          panel.spacing = unit(0.2, "lines"),
          legend.position = legend.p,
          legend.title = element_text(size=13, face="bold"),
          legend.direction = "vertical") +
    geom_vline(data = vertical_line_data, aes(xintercept = xintercept), lty = 2, color="black") +
    labs(color = "Education")+
    guides(shape = guide_legend(order = 1, title = "Model", override.aes = list(size = 1))) 
  
  plot_B <- ggplot(data = graph_data_B ,
                   aes(
                     y = term,
                     x = estimate,
                     xmin = conf.low,
                     xmax = conf.high, 
                     col = Urbanicity,
                     linetype = model
                   ))+
    geom_pointrange(
      aes(colour = Urbanicity),
      linewidth = 0.25, 
      size = 0.25) +
    # ggtitle(Label.S[ gsub("_norm$", "", S)])+
    # ylab(Label.P[ gsub("_norm$", "", expo)])+
    scale_color_manual(values = c("Ensemble" = "#F72585",
                                  "Rural"= "#4CC9F0",
                                  "Urbain intermédiaire"= "#4361EE",
                                  "Urbain"= "#3A0CA3"))+
                       
    theme_classic() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          
          strip.background = element_blank(),
          strip.text = element_blank(),
          
          axis.text.y = element_text(size = 7),
          axis.text.x = element_text(size = 7),
          
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          
          title = element_text(size = 8, face="bold"),
          
          panel.spacing = unit(0.2, "lines"),
          legend.position = legend.p,
          legend.title = element_text(size=13, face="bold"),
          legend.direction = "vertical") +
    geom_vline(data = vertical_line_data, aes(xintercept = xintercept), lty = 2, color="black") +
    labs(color = "Education")+
    guides(shape = guide_legend(order = 1, title = "Model", override.aes = list(size = 1))) 
  
  plot <- wrap_plots(c(plot_I,plot_B), ncol =1)
  
  return(plot)
}

p.NO2 <- fn_graph(All, S = "Medium_edu_prop_norm",expo = "MOY.NO2_norm", legend.p = "none" )

################################################################################

##non normalisé

#2011
plot_list <- list()
i<-1
for (p in Polluants){
  for (s in sociales){
    print(c(s,p))
    df <- subset(All, All$model == "M1" & All$Annee == "2011")
    plot <- fn_graph(df,S = s, expo = p, legend.p = "none")
    plot_list[[i]] <- plot 
    i <- i+1
    
    
  }
}

combined_plot <- wrap_plots(plot_list, ncol =2) + plot_layout(guides = "collect") &   
  theme(legend.position = "bottom") 

combined_plot  

ggsave("2_results/Regression/Forest_plot/FP_airpol_edu_rev_w_All_2011.jpeg", plot = combined_plot, width = 12, height = 7, dpi = 300)

#2015
plot_list <- list()
i<-1
for (p in Polluants){
  for (s in sociales){
    print(c(s,p))
    df <- subset(All, All$model == "M1" & All$Annee == "2015")
    plot <- fn_graph(df,S = s, expo = p, legend.p = "none")
    plot_list[[i]] <- plot 
    i <- i+1
    
    
  }
}

combined_plot <- wrap_plots(plot_list, ncol =2) + plot_layout(guides = "collect") &   
  theme(legend.position = "bottom") 

combined_plot  

ggsave("2_results/Regression/Forest_plot/FP_airpol_edu_rev_w_All_2015.jpeg", plot = combined_plot, width = 12, height = 7, dpi = 300)

#2021
plot_list <- list()
i<-1
for (p in Polluants){
  for (s in sociales){
    print(c(s,p))
    df <- subset(All, All$model == "M1" & All$Annee == "2021")
    plot <- fn_graph(df,S = s, expo = p, legend.p = "none")
    plot_list[[i]] <- plot 
    i <- i+1
    
    
  }
}

combined_plot <- wrap_plots(plot_list, ncol =2) + plot_layout(guides = "collect") &   
  theme(legend.position = "bottom") 

combined_plot  

ggsave("2_results/Regression/Forest_plot/FP_airpol_edu_rev_w_All_2021.jpeg", plot = combined_plot, width = 12, height = 7, dpi = 300)


##normalisé

#2011
plot_list <- list()
i<-1
for (p in Polluants.N){
  for (s in sociales.N){
    print(c(s,p))
    df <- subset(All, All$model == "M1" & All$Annee == "2011")
    plot <- fn_graph(df,S = s, expo = p, legend.p = "none")
    plot_list[[i]] <- plot 
    i <- i+1
    
    
  }
}

combined_plot <- wrap_plots(plot_list, ncol =2) + plot_layout(guides = "collect") &   
  theme(legend.position = "bottom") 

combined_plot  

ggsave("2_results/Regression/Forest_plot/Normalized/FP_airpol_edu_rev_w_norm_All_2011.jpeg", plot = combined_plot, width = 12, height = 7, dpi = 300)

#2015
plot_list <- list()
i<-1
for (p in Polluants.N){
  for (s in sociales.N){
    print(c(s,p))
    df <- subset(All, All$model == "M1" & All$Annee == "2015")
    plot <- fn_graph(df,S = s, expo = p, legend.p = "none")
    plot_list[[i]] <- plot 
    i <- i+1
    
    
  }
}

combined_plot <- wrap_plots(plot_list, ncol =2) + plot_layout(guides = "collect") &   
  theme(legend.position = "bottom") 

combined_plot  

ggsave("2_results/Regression/Forest_plot/Normalized/FP_airpol_edu_rev_w_norm_All_2015.jpeg", plot = combined_plot, width = 12, height = 7, dpi = 300)

#2021
plot_list <- list()
i<-1
for (p in Polluants.N){
  for (s in sociales.N){
    print(c(s,p))
    df <- subset(All, All$model == "M1" & All$Annee == "2021")
    plot <- fn_graph(df,S = s, expo = p, legend.p = "none")
    plot_list[[i]] <- plot 
    i <- i+1
    
    
  }
}

combined_plot <- wrap_plots(plot_list, ncol =2) + plot_layout(guides = "collect") &   
  theme(legend.position = "bottom") 

combined_plot  

ggsave("2_results/Regression/Forest_plot/Normalized/FP_airpol_edu_rev_w_norm_All_2021.jpeg", plot = combined_plot, width = 12, height = 7, dpi = 300)


