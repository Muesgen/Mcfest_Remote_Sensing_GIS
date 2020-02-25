root_folder <- envimaR::alternativeEnvi(root_folder = "~/edu/mpg-envinsys-plygrnd", alt_env_id = "COMPUTERNAME",
                                        alt_env_value = "PCRZP", alt_env_root_folder = "F:\\edu\\mpg-envinsys-plygrnd")

source(paste0(root_folder, "/mpg-envinfosys-teams-2018-rs_18_mcfest/src/000_setup.R"))

plotframe <- readRDS(paste0(envrmt$path_data_plots, "plotframe_sum_range.rds"))
plotframe <- reshape2::melt(plotframe, id="mul_x")

seg_met <- ggplot(plotframe)+ 
  geom_line(aes(x=mul_x, y=value, colour=variable), size = 1) +
  geom_point(aes(x=mul_x, y=value, colour=variable), size = 2) +
  scale_colour_manual(values=c("black","blue","red", "green", "orange"), name = "Legend", labels = c("Tree has single polygon", "Accuracy", "Error", "Deviation", "Polygon has one or more trees")) +
  ggtitle("Segmentation metrics") + ylab("Metrics") + xlab("Multiplicator") +
  theme(plot.title = element_text(hjust=0.5, face = "bold"), axis.text = element_text(size = 22),axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 20, b = 0, l = 0)), title = element_text(size = 24), 
        legend.position = "bottom", legend.background = element_rect(fill = "transparent"), legend.title = element_text(size = 22, face = "bold"), legend.text = element_text(size = 22))
  
seg_met
png(paste0(envrmt$path_data_plots, "seg_met.png"), res=300, width=20, height = 13, units = "in")
print(seg_met)
dev.off()


cseg55 <- cseg[cseg@data$crownArea>5.5,] #filter out polygons (trees) lower than 5.5 square meters 
writeOGR(cseg55, dsn = paste0(envrmt$path_data_mof, "cseg55.shp"), driver = "ESRI Shapefile", layer ="cseg55", overwrite_layer = TRUE)
cseg55 <- readOGR(paste0(envrmt$path_data_mof, "cseg55.shp"), layer = "cseg55")

boxplot(cseg55@data$height)
heightplot <- rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "#D9D9D9")
heightplot <-c(heightplot, boxplot(cseg55@data$height, main = "Distribution of the crown height", col = "#659EC7", borders = "black", 
        add=TRUE, pars = list(staplewex = 0.5)))

heightplot <-c(heightplot,legend("bottomleft", inset=.02, title="Range of the crown height",
       c(""), fill= "#659EC7", horiz=TRUE, cex=0.8))


boxplot(cseg55@data$crownArea)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "#D9D9D9")
boxplot(cseg55@data$crownArea, main = "Distribution of the crown area", col = "#659EC7", borders = "black", 
        add=TRUE)

legend("bottomleft", inset=.02, title="Range of the crown area",
       c(""), fill= "#659EC7", horiz=TRUE, cex=0.8)



head(cseg55@data)
mean(cseg55@data$height)
mean(cseg55@data$crownArea)
length(cseg55@polygons)

