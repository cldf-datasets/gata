#' Title: Plots GATA
#' Author: D E Blasi (12.12.2021)
#' Updated by: Frederic Blum (087.06.2022)

#' Load libraries
require(tidyverse)
require(ggrepel)

#' Load data
gata_cldf <- read_csv("../gata_raw.csv")

# st <- c(1,2)
# states <- gata_cldf %>% group_by(Language_ID, Year) %>% count()
# states_new <- tibble(states, state = rep(st, 52)) %>% select(-n)
# rm(st, states)
# gata_cldf <- gata_cldf %>% left_join(states_new)

langs <- read_csv("../../etc/languages.csv")
parameters<-read_csv("../../etc/parameters.csv")

gata <- left_join(gata_cldf, parameters, by=c("Parameter_ID" = "Shortname"))

#' Map

#' Shift coordinates to accommodate a Pacific-centered view
langs$Longitude<-sapply(langs$Longitude,function(x) ifelse(x<(-25),x + 360,x))

#' Produce maps
world <- map_data('world', interior=F, wrap=c(-25,335), ylim=c(-54,79))

ggplot()+
  theme_bw()+ 
  scale_y_continuous(expand = c(0,0),limits = c(-57,78)) + 
  scale_x_continuous(expand = c(0,0)) + 
  theme(panel.border = element_rect(size=1,color="black"),
        panel.background = element_rect(),
        plot.title = element_text(size=26,hjust=0.7),
        plot.subtitle = element_text(size=16),
        axis.line = element_blank(),
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks = element_blank(),  
        axis.title.x = element_blank(),  
        axis.title.y = element_blank(),
        legend.position = c(0.62,0.3),
        legend.title = element_text(face="bold",size=20),
        legend.text = element_text(size=16),
        plot.margin=margin(0.5,0.5,0.5,0.5,unit="cm"),
        legend.key.height = unit(6,"mm"),
        legend.background = element_rect(fill="white",color="black"))+
  geom_polygon(data=world,
               aes(x=long,y=lat,group=group),
               colour="#F2DDC1",size=0.2, fill="#F2DDC1")+
  geom_point(data=langs, 
                  aes(x=Longitude,y=Latitude),
                  color="black",
                  size=1,
                  alpha=0.8)+
  geom_text_repel(data=langs, 
             aes(x=Longitude, y=Latitude,
                 label=Name),
             color="black",
             size=4,
             alpha=0.8)

ggsave("plot_gata_map.png",width = 9, height=4)

#' Time interval density
plyr::ddply(gata,"Language_ID",function(x) abs(diff(unique(x$Year)))) %>%
  ggplot(aes(x=V1,fill=..density..))+
  geom_histogram(bins=20)+
  labs(x="Years between grammars",y="")+
  theme_minimal()+
  theme(axis.title =element_text(size=15),
        axis.text.x = element_text(size=15),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(),
        panel.background = element_rect(),
        legend.background = element_rect(fill="white",color="black"),
        plot.margin=margin(0.5,0.5,0.5,0.5,unit="cm"))+
  scale_fill_gradient(low="#4C3F91",high="#FF5677")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand=c(0,0))

ggsave("plot_gata_intervals.png",width = 6,height=2.5)

#' Data coverage
gata %>% plyr::ddply("Category",function(x) data.frame(Value=c(1-sum(is.na(x$Value)/nrow(x)), sum(is.na(x$Value))/nrow(x)),
                                                       Type=c("Attested","Not attested"))) %>%
  ggplot(aes(x="",y=Value,fill=Type))+
    geom_col()+
    coord_polar(theta="y")+
    facet_wrap(~Category)+
  theme_minimal()+
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        panel.grid = element_blank(),
        strip.text = element_text(size=12),
        plot.background = element_rect(color="white"))+
  scale_fill_manual(values=c("#09015F","#AF0069"))+
  labs(x="",y="")

ggsave("plot_gata_coverage.png",width = 6)


#' Show change
gata_change<-gata %>% 
  plyr::ddply(c("Language_ID","Parameter_ID","Category"),
              function(x) data.frame(Interval=abs(diff(unique(x$Year))),
                                     Difference=ifelse(anyNA(x$Value),NA,
                                                       ifelse(length(unique(x$Value))==1,0,1))))

gata_change %>%
  ggplot(aes(y=Difference,x=Interval))+
  geom_point(size=2,alpha=0.7,color="#00A19D")+
  geom_smooth(se=F,size=3,color="#99DDCC")+
  theme_minimal()+
  facet_wrap(~Category)+
  theme(legend.position = "none",
        panel.background = element_rect(color="black"),
        strip.text = element_text(size=15),
        axis.text = element_text(size=12),
        axis.title = element_text(size=15),
        plot.background = element_rect(color="white"))+
  scale_y_continuous(breaks=c(0,0.5,1),
                     expand=c(0.05,0.05))+
  labs(x="Years between grammars",
       y="Change")

ggsave("plot_gata_change.png",width = 9,height=6)
