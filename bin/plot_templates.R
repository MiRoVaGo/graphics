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



