#load packages
library(plyr) #wrangling
library(ragg) #download plots as png files
library(tidyverse) #wrangling
library(ggplot2) #plotting
library(ggthemes) #plot themes
library(reshape2) #melt() function
library(showtext) #font
library(extrafont) #font

#load data
b_path <- paste0(getwd(), "/Resources/cancer_b.csv")
cancer_b <- read.csv(b_path)
m_path <- paste0(getwd(), "/Resources/cancer_m.csv")
cancer_m <- read.csv(m_path)
#merge data
tumors <- rbind(cancer_b, cancer_m)
#subset data 
tumors <- tumors[,1:12]
#rename columns
names(tumors) <- c("id", "Diagnosis", "Radius", "Texture", "Perimeter", "Area", "Smoothness", "Compactness", "Concavity",
                   "Concave_Points", "Symmetry", "Fractal_Dimension")
#change diagnosis to full word
for (i in 1:nrow(tumors)){
  if (tumors[i,2]=="B"){
    tumors[i,2] <- "Benign"
  }
  if (tumors[i,2]=="M"){
    tumors[i,2] <- "Malignant"
  }
}
#save plot
ggsave("Assets/images/texture.png",
       plot = texture,
       device = agg_png(width = 5, height = 3, units = "in", res = 300))


font_add(family = "regular", "Resources/Roboto-Regular.ttf")
showtext_auto()

#plotting theme
my_theme <- theme(
  
  # panel and plot background
  panel.grid.major = element_line(color="#454545"),
  panel.grid.minor = element_line(color="#454545"),
  panel.border = element_blank(),
  panel.background = element_rect(fill = "#2D2D2D"),
  plot.background = element_rect(fill = "#2D2D2D"),
  
  # axis
  axis.title.x = element_text(size=50, color="white", family="regular"),
  axis.text = element_text(family="regular", size=30, color="white"),
  axis.ticks = element_line(color="white"),
  axis.line = element_line(color="white", size=0.6),
  
  # legend
  legend.position = c(0.8, 0.6),
  legend.background = element_rect(fill="black"),
  legend.key = element_rect(fill="#2D2D2D"),
  legend.text = element_text(size=26, color="white", family="regular"),
  legend.title = element_blank()
)


#TEXTURE
mu_texture <- ddply(tumors, "Diagnosis", summarise, grp.mean=mean(Texture))
texture <- ggplot(tumors, aes(x=Texture)) + geom_density(aes(fill=Diagnosis, color=Diagnosis), size=0.8, alpha=0.4) + 
  scale_color_manual(values=c("#409dcd","#cb5673"))+
  scale_fill_manual(values=c("#2B92C7","#C54363"))+
  geom_vline(data=mu_texture, aes(xintercept=grp.mean, color=Diagnosis),linetype="dashed", size=0.8)+
  ylab("")+my_theme
texture


#COMPACTNESS
mu_compactness <- ddply(tumors, "Diagnosis", summarise, grp.mean=mean(Compactness))
compactness <- ggplot(tumors, aes(x=Compactness)) + geom_density(aes(fill=Diagnosis, color=Diagnosis), size=0.8, alpha=0.4) + 
  scale_color_manual(values=c("#409dcd","#cb5673"))+
  scale_fill_manual(values=c("#2B92C7","#C54363"))+
  geom_vline(data=mu_compactness, aes(xintercept=grp.mean, color=Diagnosis),linetype="dashed", size=0.8)+
  ylab("")+my_theme
compactness


#RADIUS
mu_radius <- ddply(tumors, "Diagnosis", summarise, grp.mean=mean(Radius))
radius <- ggplot(tumors, aes(x=Radius)) + geom_density(aes(fill=Diagnosis, color=Diagnosis), size=0.8, alpha=0.4) + 
  scale_color_manual(values=c("#409dcd","#cb5673"))+
  scale_fill_manual(values=c("#2B92C7","#C54363"))+
  geom_vline(data=mu_radius, aes(xintercept=grp.mean, color=Diagnosis),linetype="dashed", size=0.8)+
  ylab("")+my_theme
radius


#AREA
mu_area <- ddply(tumors, "Diagnosis", summarise, grp.mean=mean(Area))
area <- ggplot(tumors, aes(x=Area)) + geom_density(aes(fill=Diagnosis, color=Diagnosis), size=0.8, alpha=0.4) + 
  scale_color_manual(values=c("#409dcd","#cb5673"))+
  scale_fill_manual(values=c("#2B92C7","#C54363"))+
  geom_vline(data=mu_area, aes(xintercept=grp.mean, color=Diagnosis),linetype="dashed", size=0.8)+
  ylab("")+my_theme


#SMOOTHNESS
mu_smoothness <- ddply(tumors, "Diagnosis", summarise, grp.mean=mean(Smoothness))
smoothness <- ggplot(tumors, aes(x=Smoothness)) + geom_density(aes(fill=Diagnosis, color=Diagnosis), size=0.8, alpha=0.4) + 
  scale_color_manual(values=c("#409dcd","#cb5673"))+
  scale_fill_manual(values=c("#2B92C7","#C54363"))+
  geom_vline(data=mu_smoothness, aes(xintercept=grp.mean, color=Diagnosis),linetype="dashed", size=0.8)+
  ylab("")+my_theme


#CONCAVITY
mu_concavity <- ddply(tumors, "Diagnosis", summarise, grp.mean=mean(Concavity))
concavity <- ggplot(tumors, aes(x=Concavity)) + geom_density(aes(fill=Diagnosis, color=Diagnosis), size=0.8, alpha=0.4) + 
  scale_color_manual(values=c("#409dcd","#cb5673"))+
  scale_fill_manual(values=c("#2B92C7","#C54363"))+
  geom_vline(data=mu_concavity, aes(xintercept=grp.mean, color=Diagnosis),linetype="dashed", size=0.8)+
  ylab("")+my_theme

#SYMMETRY
mu_symmetry <- ddply(tumors, "Diagnosis", summarise, grp.mean=mean(Symmetry))
symmetry <- ggplot(tumors, aes(x=Symmetry)) + geom_density(aes(fill=Diagnosis, color=Diagnosis), size=0.8, alpha=0.4) + 
  scale_color_manual(values=c("#409dcd","#cb5673"))+
  scale_fill_manual(values=c("#2B92C7","#C54363"))+
  geom_vline(data=mu_symmetry, aes(xintercept=grp.mean, color=Diagnosis),linetype="dashed", size=0.8)+
  ylab("")+my_theme


#PERIMETER
mu_perimeter <- ddply(tumors, "Diagnosis", summarise, grp.mean=mean(Perimeter))
perimeter <- ggplot(tumors, aes(x=Perimeter)) + geom_density(aes(fill=Diagnosis, color=Diagnosis), size=0.8, alpha=0.4) + 
  scale_color_manual(values=c("#409dcd","#cb5673"))+
  scale_fill_manual(values=c("#2B92C7","#C54363"))+
  geom_vline(data=mu_perimeter, aes(xintercept=grp.mean, color=Diagnosis),linetype="dashed", size=0.8)+
  ylab("")+my_theme


##SAVE PLOTS

#TEXTURE
ggsave("Assets/images/texture.png",
       plot = texture,
       device = agg_png(width = 5, height = 3, units = "in", res = 300))

#COMPACTNESS
ggsave("Assets/images/compactness.png",
       plot = compactness,
       device = agg_png(width = 5, height = 3, units = "in", res = 300))

#RADIUS
ggsave("Assets/images/radius.png",
       plot = radius,
       device = agg_png(width = 5, height = 3, units = "in", res = 300))


#AREA
ggsave("Assets/images/area.png",
       plot = area,
       device = agg_png(width = 5, height = 3, units = "in", res = 300))

#SMOOTHNESS
ggsave("Assets/images/smoothness.png",
       plot = smoothness,
       device = agg_png(width = 5, height = 3, units = "in", res = 300))

#CONCAVITY
ggsave("Assets/images/concavity.png",
       plot = concavity,
       device = agg_png(width = 5, height = 3, units = "in", res = 300))

#SYMMETRY
ggsave("Assets/images/symmetry.png",
       plot = symmetry,
       device = agg_png(width = 5, height = 3, units = "in", res = 300))

#PERIMETER
ggsave("Assets/images/perimeter.png",
       plot = perimeter,
       device = agg_png(width = 5, height = 3, units = "in", res = 300))


