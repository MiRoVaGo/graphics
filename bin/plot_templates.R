source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

# basic scatterplot
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, col = Species)) + 
  geom_point() +
  scale_color_manual(values = palettes_bright$colset_cheer_brights) +
  theme_generic

# violin
ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_violin(aes(fill = factor(cyl))) +
  scale_fill_manual(values = palettes_bright$colset_cheer_brights) +
  theme_generic

# density
ggplot(data = diamonds, aes(x = price, group = cut, fill=cut)) + 
  geom_density(adjust = 1.5, alpha = 0.5)+
  scale_fill_manual(values = colset_mid) +
  theme_generic

# histogram
data=data.frame(value=rnorm(10000))
ggplot(data, aes(x=value)) + 
  geom_histogram(binwidth = 0.2, color="white", fill = palettes_bright$colset_cheer_brights[1]) +
  theme_generic

# ridges
library(ggridges)
ggplot(diamonds, aes(x = price, y = cut, fill = cut)) +
  geom_density_ridges() +
  scale_fill_manual(values = colset_mid) +
  theme_generic

# boxplot
names=c(rep("A", 20) , rep("B", 8) , rep("C", 30), rep("D", 80))
value=c( sample(2:5, 20 , replace=T) , sample(4:10, 8 , replace=T), sample(1:7, 30 , replace=T), sample(3:8, 80 , replace=T) )
data=data.frame(names,value)

ggplot(data, aes(x=names, y=value, fill=names)) +
  geom_boxplot(alpha=0.4) +
  stat_summary(aes(color=names),fun.y=mean, geom="point", shape=20, size=10) +
  theme(legend.position="none") +
  scale_fill_manual(values = palettes_bright$colset_cheer_brights) +
  scale_color_manual(values = palettes_bright$colset_cheer_brights) +
  theme_generic

# scatter
ggplot(diamonds[sample(x = c(1:50000),300),], aes(x=carat, y=price, size=depth, color=cut(x = carat, breaks = c(0,2,4)))) +
  geom_point(alpha=0.4) +
  scale_size_continuous( trans="exp", range=c(1, 25)) +
  scale_color_manual(values = palettes_bright$colset_cheer_brights) +
  theme_generic

# radar chart
library(fmsb)
set.seed(99)
data=as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data)=c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data)=paste("mister" , letters[1:3] , sep="-")
data=rbind(rep(20,5) , rep(0,5) , data)

colors_border=c(palettes_bright$colset_cheer_brights[1:3])
colors_in=c(palettes_bright$colset_cheer_brights[1:3])
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=alpha(colors_in, 0.4) , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)
legend(x=1, y=1.2, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


# polar
mtcars$car = row.names(mtcars)
p = ggplot(mtcars, aes(x=car, y=mpg, fill=cut(mpg, breaks = c(10,20,30,40)))) +
  geom_histogram(stat='identity') +theme_light() +
  scale_fill_gradient(low='red', high='white', limits=c(5,40)) +
  theme(axis.title.y=element_text(angle=0))+
  scale_fill_manual(values = palettes_bright$colset_cheer_brights) +
  theme_generic

p + theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))
p + coord_polar()


