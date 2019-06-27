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