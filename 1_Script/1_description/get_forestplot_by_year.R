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
All<- subset(All , All$Sociale == "High_edu_prop" | All$Sociale =="High_edu_prop_norm")

Polluants <-c("MOY.NO2", "MOY.O3", "MOY.PM25", "MOY.PM10")
sociales <-c("High_edu_prop")

Polluants.N <- paste0(Polluants, "_norm")
sociales.N <- paste0(sociales, "_norm") 

Label.P <- c("MOY.NO2" = "NO2", "MOY.O3" = "O3", "MOY.PM25" ="PM2.5" ,"MOY.PM10"= "PM10",
             "MOY.NO2_norm" = "NO2", "MOY.O3_norm" = "O3", "MOY.PM25_norm" ="PM2.5" ,"MOY.PM10_norm"= "PM10")
Label.S <- c("Low_edu_prop" = "Low education", "Medium_edu_prop" = "Medium education", "High_edu_prop" = "High education", 
             "Prop_Immigre" = "Immigration",
             "D121" = "1er Décile","Q2" = "Revenu médian","D921" = "9e Décile","RD" = "Rapport interdécile D9/D1")

Annee <-c("2011","2015","2021")

All$term_model <- paste(All$term, All$model)
graph_data <-  subset(All, All$model == "M0"  & All$Annee == "2021")
expo <- "MOY.PM25_norm"
S <- "High_edu_prop_norm"
A <- "2021"
legend.p <- "none"
title.para <- element_text(size = 8, face="bold")
Term.p <- "Intercept"

fn_graph <- function(graph_data, A,S,expo, legend.p, Term){
  
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
                     col = Urbanicity
                   ))+
    geom_pointrange(
      aes(colour = Urbanicity),
      linewidth = 0.25, 
      size = 0.25) +
    ggtitle(A, subtitle = Label.P[ gsub("_norm$", "", expo)])+
    # xlab(Label.P[ gsub("_norm$", "", expo)], pos)+
    # facet_wrap(.~Annee)+
    
    scale_color_manual(values = c("Ensemble" = "#264653",
                                  "Rural"= "#BCC184",
                                  "Urbain intermédiaire"= "#F4A261",
                                  "Urbain"= "#C53D1B"))+
    
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          
          strip.background = element_blank(),
          # strip.text = element_blank(),
          
          axis.text.y = element_text(size = 7),
          axis.text.x = element_text(size = 7),
          
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
         
          
          title = element_text(size = 15, face="bold", hjust = 0.5),
          
          panel.spacing = unit(0.2, "lines"),
          legend.position = legend.p,
          legend.title = element_text(size=13, face="bold"),
          legend.direction = "vertical") +
    geom_vline(data = vertical_line_data, aes(xintercept = xintercept), lty = 2, color="black") +
    labs(color = "Niveau d'urbanisation")
    # guides(shape = guide_legend(order = 1, title = "Model", override.aes = list(size = 1))) 
  
  plot_B <- ggplot(data = graph_data_B ,
                   aes(
                     y = term,
                     x = estimate,
                     xmin = conf.low,
                     xmax = conf.high, 
                     col = Urbanicity
                   ))+
    geom_pointrange(
      aes(colour = Urbanicity),
      linewidth = 0.25, 
      size = 0.25) +
    # ggtitle(Label.S[ gsub("_norm$", "", S)])+
    # ylab(Label.P[ gsub("_norm$", "", expo)])+
    scale_color_manual(values = c("Ensemble" = "#264653",
                                  "Rural"= "#BCC184",
                                  "Urbain intermédiaire"= "#F4A261",
                                  "Urbain"= "#C53D1B"))+
    
    theme_bw() +
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
    labs(color = "Niveau d'urbanisation")
    # guides(shape = guide_legend(order = 1, title = "Model", override.aes = list(size = 1))) 
  
  plot <- wrap_plots(c(plot_I,plot_B), ncol =1)
  
  return(plot)
}

################################################################################

##non normalisé

plot_list <- list()

i<-1
for (p in  Polluants){
  
  for (a in Annee){
      print(c("High_edu_prop",p, a))
      df <- subset(All, All$model == "M1" & All$Annee == a & All$Sociale == "High_edu_prop")
      titre <- ifelse(i <= length(Annee), a, "")
      
      plot <- fn_graph(df,A =titre,S = "High_edu_prop", expo = p, legend.p = "none")
      plot_list[[i]] <- plot 
      i <- i+1
  }
}


combined_plot <- wrap_plots(plot_list, ncol =3) + plot_layout(guides = "collect") &   
  theme(legend.position = "right") 

combined_plot  

ggsave("2_results/Regression/Forest_plot/FP_edu_Weighted.png", combined_plot, width = 10, height =10, dpi = 300)


plot_list_norm <- list()
i<-1
for (p in  Polluants.N){
  
  for (a in Annee){
    print(c("High_edu_prop_norm",p, a))
    df <- subset(All, All$model == "M1" & All$Annee == a & All$Sociale == "High_edu_prop_norm")
    titre <- ifelse(i <= length(Annee), a, "")
    
    plot <- fn_graph(df,A =titre,S = "High_edu_prop_norm", expo = p, legend.p = "none")
    plot_list_norm[[i]] <- plot 
    i <- i+1
  }
}

combined_plot_norm <- wrap_plots(plot_list_norm, ncol =3) + plot_layout(guides = "collect") &   
  theme(legend.position = "right") 

combined_plot_norm  

ggsave("2_results/Regression/Forest_plot/FP_edu_Weighted_norm.png", combined_plot_norm, width = 10, height =10, dpi = 300)

###############################################################################

##2021


combined_2021 <- wrap_plots(c(plot_list[[3]],plot_list[[6]],plot_list[[9]],plot_list[[12]]), ncol =2) + plot_layout(guides = "collect") &   
  theme(legend.position = "right") 
ggsave("2_results/Regression/Forest_plot/FP_edu_Weighted_2021.png", combined_2021, width = 10, height =10, dpi = 300)
combined_norm_2021 <- wrap_plots(c(plot_list_norm[[3]],plot_list_norm[[6]],plot_list_norm[[9]],plot_list_norm[[12]]), ncol =2) + plot_layout(guides = "collect") &   
  theme(legend.position = "right") 
ggsave("2_results/Regression/Forest_plot/FP_edu_Weighted_norm_2021.png", combined_norm_2021, width = 10, height =10, dpi = 300)

