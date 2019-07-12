# Basic libraries needed - don't modify this for a library just used in one/two scripts

if(!require(pacman))install.packages("pacman")

pacman::p_load('dplyr', 'tidyr', 'DescTools',
               'ggplot2', 'ggalt', 'ggExtra',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')
