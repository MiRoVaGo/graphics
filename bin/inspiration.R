################## MARGINAL PLOT ###################

library(ggplot2)
library(ggExtra)

data(mpg, package="ggplot2")

# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
mpg_select <- mpg[mpg$hwy >= 35 & mpg$cty > 27, ]
g <- ggplot(mpg, aes(cty, hwy, col = factor(cyl))) + 
  geom_count() + 
  #geom_smooth(method="lm", se=F) +
  theme(legend.position="none") +
  theme_generic + 
  scale_color_manual(values = palettes_bright$colset_cheer_brights) 

ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")
ggMarginal(g, type="density")

library(ggpubr)
# Grouped Scatter plot with marginal density plots
ggscatterhist(
  iris, x = "Sepal.Length", y = "Sepal.Width",
  color = "Species", size = 3, alpha = 0.6,
  palette = palettes_bright$colset_cheer_brights,
  margin.params = list(fill = "Species", color = "black", size = 0.2)
)

ggscatterhist(
  iris, x = "Sepal.Length", y = "Sepal.Width",
  color = "Species", size = 3, alpha = 0.6,
  palette = palettes_bright$colset_urban_oasis[2:4],
  margin.params = list(fill = "Species", color = "black", size = 0.2),
  margin.plot = "boxplot"
)


################# CORPLOT #####################
library(GGally)
ggpairs(iris[,-5]) + theme_bw()
ggpairs(iris[,-5]) + 
  theme_generic 

################ RADAR CHARTS #######################

library(fmsb)

# Create data: note in High school for Jonathan:
data=as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data)=c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(20,10) , rep(0,10) , data)

# The default radar chart proposed by the library:
radarchart(data)

# Custom the radarChart !
radarchart( data  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)


################# PATCH WORK ########################

library(ggplot2)
library(patchwork)

devtools::install_github("thomasp85/patchwork")

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p4 <- ggplot(mtcars) + geom_bar(aes(carb))

p1 + p2 + p3 + p4
p1 + p2 - p3 + plot_layout(ncol = 1)
(p1 | p2 | p3) / p4
(p1 + (p2 + p3) + p4 + plot_layout(ncol = 1)) * theme_bw()
p1 + (p2 + p3) + p4 + plot_layout(ncol = 1) & theme_bw()
