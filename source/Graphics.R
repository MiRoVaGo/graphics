library(latticeExtra)
library(raster)
library(rasterVis)
library(gridExtra)
library(maptools)
library(rgdal)
library(ggplot2)
library(gstat)
library(sp)
library(corrplot)

theme_opts <- list(theme(axis.ticks.length=unit(-0.1, "cm"),  
                         axis.text.x = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm")), 
                         axis.text.y = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm"))))


world.shp = maptools::readShapeLines("C:/Users/markonis/Documents/R/Projects/owda/ne_50m_admin_0_countries_lakes.shp", proj4string=CRS('+proj=longlat +ellps=WGS84'))

#Current Palettes and color sets

colset_bright <- c("#6a3d9a", "#375E97", "#008DCB", "#31A9B8", 
                   "#486B00", "#258039", "#A2C523", "#FFCE38", 
                   "#F0810F", "#FA6775", "#D61800", "#9B4F0F")
colset_bright_qual <- colset_bright[c(11, 2, 6, 8, 3, 7, 9, 1, 5, 12, 4, 10)]
palette_bright <- colorRampPalette(colset_bright)
palette_bright_qual <- colorRampPalette(colset_bright_qual)

colset_mid <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                 "#F4CC70", "#EBB582",  "#BF9A77",
                 "#E38B75", "#CE5A57",  "#D24136", "#785A46" )
colset_mid_qual <- colset_mid[c(11, 2, 4, 6,  1, 8, 10, 5, 7, 3, 9, 12)]
palette_mid <- colorRampPalette(colset_mid)
palette_mid_qual <- colorRampPalette(colset_mid_qual)

colset_light_qual <- c("#8dd3c7",   "#fdb462", "#bebada", "#fb8072", 
                       "#80b1d3",  "#b3de69", "#ffed6f","#bc80bd",      
                       "#d9d9d9",  "#fccde5","#ccebc5", "#a1d6e2")
palette_light_qual <- colorRampPalette(colset_light_qual)

palette_light_gr_bu <- colorRampPalette(c( "#ACBD78", "grey80","#97B8C2"))

# https://www.canva.com/learn/100-color-combinations/

palettes_bright <- list(
  colset_cheer_brights = c("#C73721", "#31A9B8", "#F5BE41", "#258039"),
  colset_pool_party = c("#344D90", "#5CC5EF", "#FFB745", "#E7552C"),
  colset_beyond_bw = c("#31A2AC", "#AF1C1C", "#F0EFF0", "#2F2F28"),
  colset_sleek_modern = c("#2F2E33", "#D5D6D2", "#FFFFFF", "#3A5199"),
  colset_bold_culture = c("#4C3F54", "#D13525", "#F2C057", "#486824"),
  colset_modern_urban = c("#217CA3", "#E29930", "#32384D", "#211F30"), 
  colset_sun_sky = c("#F9BA32", "#426E86", "#F8F1E5", "#2F3131"),
  colset_subdued_prof = c("#90AFC5", "#336B87", "#2A3132", "#763626"),
  colset_urban_oasis = c("#506D2F", "#2A2922", "#F3EBDD", "#7D5642")
)

palettes_mid <- list(
  colset_golden_afternoon = c("#882426", "#CDBEA7", "#323030", "#C29545"),
  colset_orange_accent = c("#756867", "#D5D6D2", "#353C3F", "#FF8D3F"),
  colset_misty_greens = c("#04202C", "#304040", "#5B7065", "#8E9B97","#C9D1C8"),
  colset_spicy_neutrals = c("#AF4425", "#662E1C", "#EBDCB2", "#C9A66B"),
  colset_warm_naturals = c("#2E2300", "#6E6702", "#C05805", "#DB9501"),
  colset_autumn_vermont = c("#8D230F", "#1E434C", "#9B4F0F", "#C99E10"),
  colset_greens_blues = c("#324851", "#86AC41", "#34675C", "#7DA3A1"),
  colset_surf_turf = c("#F4CC70", "#DE7A22", "#20948B", "#6AB187")
)

palettes_light <- list(
  colset_sunkissed_village = c("#D24136", "#EB8A3E", "#EBB582", "#785A46"),
  colset_warm_cool = c("#444C5C", "#CE5A57", "#78A5A3", "#E1B164"),
  colset_muted_antique = c("#A4CABC", "#EAB364", "#B2473E", "#ACBD78"),
  colset_retro_relaxing = c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
)

test_palette(colorRampPalette(palettes_mid$colset_surf_turf), 4)
#Testing palettes
test_palette <- function(palette_name, n = 4){
  
  test <- data.table(var = c(rep("A", 250), rep("B", 250), rep("C", 250), rep("D", 250)), x = rnorm(1000), y = rnorm(1000))
  
  plot(1:n, rep(1, n), col = palette_name(n), cex = 4, pch = 16)
  
  print(ggplot(test, aes(x, y, col = var)) +
    geom_point(size = 2) + 
    scale_color_manual(values = palette_name(n)) + 
    theme_bw())
  
  ggplot(test, aes(x, y, fill = var)) +
    geom_boxplot() + 
    scale_fill_manual(values =  palette_name(n)) + 
    theme_bw()
}

dec_pts <- function(x) sprintf("%.3f", x)

sci_10 <- function(x) {
  out <- parse(text = gsub("e", " %*% 10^", scales::scientific_format()(x)))
  return(out)
}

#Older Palettes
rgb.palette.RdBu = colorRampPalette(rev(c('#d73027','#f46d43','#fdae61','#fee090','#fef0d9','#e0f3f8','#abd9e9','#74add1','#4575b4')), space = "rgb")
gradient_RdBu = rgb.palette.RdBu(100)
my.drought.col = colorRampPalette(c('#8c510a','#d8b365','#f6e8c3','#f5f5f5','skyblue1','skyblue3','skyblue4'), interpolate = "spline", space = "rgb")
my.purples.uneven = colorRampPalette(c(rep('#edf8fb',6),'#bfd3e6','#9ebcda','#8c96c6','#8c6bb1'), space = "rgb")
my.reds = colorRampPalette(c('#fef0d9','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#990000'), space = "rgb")
rgb.palette.Qualitative.1 = colorRampPalette(c("#4575b4", "#78c679", "#f46d43", "#74add1", "#807dba", "#fee090", "#d9f0a3", "#d73027",     "#abd9e9", "#fdae61", "#fa9fb5", "#ffed6f"), space = "rgb")
rgb.palette.Qualitative.2 = colorRampPalette(c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5",     "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f"))
rgb.palette.Qualitative.3 = colorRampPalette(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",     "#cab2d6", "#6a3d9a", "#ffed6f", "#b15928"))

compare.two.hist = function(x1, x2, name1 = "hist1", name2 = "hist2", title = "", 
                            xlab = "what is this?", lim = c(0, 0.2), ...) {
  cukes <- data.frame(x = x1)
  carrots <- data.frame(x = x2)
  
  cukes$Legend <- name1
  carrots$Legend <- name2
  vegLengths <- rbind(cukes, carrots)
  
  ggplot(vegLengths, aes(x, fill = Legend)) + 
    ggtitle(title) + 
    geom_density(alpha = 0.4) + 
    xlab(xlab) + 
    scale_fill_manual(values = c("orange", "tan4")) + 
    scale_x_continuous(limits = lim) + 
    theme(plot.title = element_text(size = 20, face = "bold"), 
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"))
}

compare.three.hist = function(x1, x2, x3, name1 = "hist1", name2 = "hist2", name3 = "hist3", title = "", ...) {
  cukes <- data.frame(x = x1)
  carrots <- data.frame(x = x2)
  panzars <- data.frame(x = x3)
  cukes$Legend <- name1
  carrots$Legend <- name2
  panzars$Legend <- name3
  vegLengths <- rbind(cukes, carrots, panzars)
  
  ggplot(vegLengths, aes(x, fill = Legend)) + 
    geom_density(alpha = 0.4) + 
    scale_fill_manual(values = c("orange", "tan4", "darkolivegreen4")) + 
    ggtitle(title) + 
    theme_bw()+
    theme(plot.title = element_text(size = 20, face = "bold"), 
          axis.title.x = element_text(size = 16, face = "bold"), 
          axis.title.y = element_text(size = 16, face = "bold"))
}

map_path = "C:/Users/markonis/Documents/R/Projects/geodata/"

theme_map_opts <- list(theme(panel.grid.minor = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.background = element_blank(),
                             plot.background = element_rect(fill="#e6e8ed"),
                             panel.border = element_blank(),
                             axis.line = element_blank(),
                             axis.text.x = element_blank(),
                             axis.text.y = element_blank(),
                             axis.ticks = element_blank(),
                             axis.title.x = element_blank(),
                             axis.title.y = element_blank(),
                             plot.title = element_text(size=22)))

wmap <- readOGR(dsn=paste0(map_path, "ne_110m_land"), layer="ne_110m_land")
wmap_df <- fortify(wmap)

countries <- readOGR(paste0(map_path,"ne_110m_admin_0_countries"), layer="ne_110m_admin_0_countries") 
countries_df <- fortify(countries_robin)

grat <- readOGR(paste0(map_path,"ne_110m_graticules_all"), layer="ne_110m_graticules_15") 
grat_df <- fortify(grat)

bbox <- readOGR(paste0(map_path,"ne_110m_graticules_all"), layer="ne_110m_wgs84_bounding_box") 
bbox_df <- fortify(bbox)


ggplot(bbox_wintri, aes(long, lat, group=group)) +
  geom_path(data=countries_wintri, aes(long,lat, group=group, fill=hole), color="grey10", size=1.0) +
  geom_path(data=grat_wintri, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  coord_equal(ratio=1) + 
  theme_map_opts 

countries_wintri <- spTransform(countries, CRS("+proj=wintri"))
bbox_wintri <- spTransform(bbox, CRS("+proj=wintri"))
wmap_wintri <- spTransform(wmap, CRS("+proj=wintri"))
grat_wintri <- spTransform(grat, CRS("+proj=wintri"))


fig_size <- ggplot(ts_1990_meta, aes(sample_size)) + scale_fill_manual(values = proxy.cols[c(2, 4)]) 
g1 <- fig_size + geom_histogram(aes(fill = archive), 
                                col = "black", 
                                size = .1) +  
  xlab(label = "Sample Size") + 
  ylab(label = "Count") +
  theme_bw() +
  theme(plot.margin = unit(c(1.5, 1, 1.3, 1), "cm")) 

g2 <- ggplot(bbox_wintri, aes(long, lat, group = group)) + 
  geom_polygon(fill = palettes_mid$colset_misty_greens[4], alpha = 0.2) +
  geom_polygon(data = countries_wintri, aes(long,lat, group = group, fill = hole)) + 
  geom_path(data = grat_wintri, aes(long, lat, group = group, fill = NULL), 
            size = 0.1, color = "slategray4", alpha = 0.2) +
  geom_point(data = ts_1990_meta, 
             aes(long_wintri, lat_wintri, group = NULL, fill = NULL, 
                 col = archive, size = sample_size), alpha = I(7 / 10)) +
  scale_size_continuous(range = c(0, 5), name = "Sample size") + 
  scale_color_manual(values = proxy.cols[c(2, 4)], name = "Archive") +
  coord_equal(ratio = 1) + 
  map_opts +
  scale_fill_manual(values = c("ivory3", "white"), guide = "none")

gg_all <- ggarrange(g2, g1,
                    labels = c("a", "b"),
                    nrow = 1, ncol = 2,
                    widths = c(1.5, 1), 
                    legend = "right",
                    common.legend = T)
ggsave(paste0(results_path, "figures/1990_size_b.png"), plot = gg_all, device = "png", width = 20, height = 7, units = "cm")

g2 <- ggplot(bbox_wintri, aes(long, lat, group = group)) + 
  geom_polygon(fill = palettes_light$colset_retro_relaxing[3], alpha = 0.2) +
  geom_polygon(data = countries_wintri, aes(long,lat, group = group, fill = hole)) + 
  geom_path(data = grat_wintri, aes(long, lat, group = group, fill = NULL), 
            size = 0.1, color = "slategray4", alpha = 0.2) +
  geom_point(data = ts_1990_meta, 
             aes(long_wintri, lat_wintri, group = NULL, fill = NULL, 
                 col = archive, size = sample_size), alpha = I(7 / 10)) +
  scale_size_continuous(range = c(0, 5), name = "Sample size") + 
  scale_color_manual(values = proxy.cols[c(2, 4)], name = "Archive") +
  coord_equal(ratio = 1) + 
  map_opts +
  scale_fill_manual(values = c(palettes_light$colset_retro_relaxing[3], "white"), guide = "none")

plot.owda.year = function(yr, variable){
  bb = owda.1000[Time == yr,.(y=lon, x=lat, z= eval(parse(text = variable)))]
  dd = rasterFromXYZ(bb, crs = CRS('+proj=longlat +datum=WGS84'))
  oo = max(abs(bb$z), na.rm = T)
  col.seq = seq(-oo,oo,0.1)
  levelplot(dd, xlab.top = yr, xlab="", ylab="", margin=F, scales = list(y = list(tck=c(-1, -1)), x = list(tck=c(-1, -1))), main = as.character(yr),
            col.regions=my.drought.col, cuts=600, at = col.seq)  + latticeExtra::layer(sp.lines(world.shp, col = "grey30",lwd=0.5))   
}

plot.owda.many.years = function(yr, variable, col.seq = seq(-6,6,0.1)){
  bb = owda.1000[Time == yr,.(y=lon, x=lat, z= eval(parse(text = variable)))]
  dd = rasterFromXYZ(bb, crs = CRS('+proj=longlat +datum=WGS84'))
  levelplot(dd, xlab.top = yr, xlab="", ylab="", margin=F, scales = list(y = list(tck=c(-1, -1)), x = list(tck=c(-1, -1))), main = as.character(yr),
            col.regions=my.drought.col, cuts=600, at = col.seq)  + latticeExtra::layer(sp.lines(world.shp, col = "grey30",lwd=0.5))   
}
