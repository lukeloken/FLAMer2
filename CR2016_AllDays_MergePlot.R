rm(list=ls(all=TRUE))

setwd("E:/Dropbox/FLAME_ColumbiaRiver/Data")

library(maptools)
library(RgoogleMaps)
library(rgdal)
library(gtools)


#This function creates a color scale for use with the image()
#function. Input parameters should be consistent with those
#used in the corresponding image plot. The "axis.pos" argument
#defines the side of the axis. The "add.axis" argument defines
#whether the axis is added (default: TRUE)or not (FALSE).
image.scale <- function(z, zlim, col = heat.colors(12),
                        breaks, axis.pos=1, add.axis=TRUE, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  if(axis.pos %in% c(1,3)){ylim<-c(0,1); xlim<-range(breaks)}
  if(axis.pos %in% c(2,4)){ylim<-range(breaks); xlim<-c(0,1)}
  plot(1,1,t="n",ylim=ylim, xlim=xlim, axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(axis.pos %in% c(1,3)){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(axis.pos %in% c(2,4)){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
  box()
  if(add.axis) {axis(axis.pos)}
}


# End plotting function




# =======================================
# Merge Columbia River shapefiles
# Read the cleaned shapefile
# Can do this with either shapefiles or csvs. Comment out code
# =======================================

# files<-list.files()
# files<-files[which(files!="ColumbiaRiver2016_AllDays")]
# 
# i=1
# for (i in 1:length(files)){
#   
#   # Set directory
#   dir<-files[i]
#   
#   # Read the cleaned shapefile
#   shapenames<-list.files(path=paste(dir, '/shapefiles', sep=""))
#   file_number<-grep(pattern='cleaned.shp', shapenames)
#   exact_name<-shapenames[file_number]
#   short_name<-sub('.shp', '', exact_name )
#   shape<-readOGR(paste(dir, '/shapefiles', sep=""), short_name)
#   
#     # Merge all shapefiles
#   if (i == 1){
#     shape_all<-shape
#   }
#   if (i>1){
#     shape_all<-spRbind(shape_all, shape)
#   }
# }
# 
# # End shapefile merge

# ============================================
# Merge with csvs and convert to shapefile
# Read the cleaned csv
# Works if csv's contain different columns (e.g., one sensor didn't work on one day)
# ===========================================

files<-list.files()
files<-files[which(files!="ColumbiaRiver2016_AllDays")]

i=1
for (i in 1:length(files)){
  
  # Set directory
  dir<-files[i]
  csvnames<-list.files(path=dir)
  file_number<-grep(pattern='cleaned.csv', csvnames)
  exact_name<-csvnames[file_number]
  short_name<-sub('.shp', '', exact_name )
  csv<-read.csv(paste(dir, exact_name, sep="/"), header=T, stringsAsFactors = F)
  
  # Merge all csvs
  if (i == 1){
    csv_all<-csv
  }
  if (i>1){
    csv_all<-smartbind(csv_all, csv)
  }
}

write.table(csv_all, file = 'ColumbiaRiver2016_AllDays/ColumbiaRiver2016_AllDays_cleaned.csv', col.names=TRUE,row.names=F, sep=",")

# Call spatial_dataset, assign projection (Decimal Degrees), write shapefile
shape_all<-csv_all[which(!is.na(csv_all$Latitude) & !is.na(csv_all$Longitude)),  ]
coordinates(shape_all) = ~Longitude+Latitude

# End csv merge

# ==================
# Set projection
# plot spatial data
# save shapefile
# ==================

proj4string(shape_all)=CRS("+init=epsg:4326")

plot(shape_all, pch=16)

writeOGR(shape_all, dsn="ColumbiaRiver2016_AllDays/shapefiles", layer="ColumbiaRiver_AllDays_cleaned", driver="ESRI Shapefile",  verbose=F, overwrite=T)

# End shapefile merge

# =======================================
# Plot Columbia all days with GMAP background for all variables
# setwd("E:/Dropbox/FLAME_ColumbiaRiver/Data")
# =======================================

# Manually load shape_all function if you don't want to loop through all files
# shape_all<-readOGR("ColumbiaRiver2016_AllDays/shapefiles", "ColumbiaRiver_AllDays")

map<-GetMap(center=c(mean(range(shape_all@coords[,2])), mean(range(shape_all@coords[,1]))), size=c(640,266), zoom=min(MaxZoom(range(shape_all@coords[,2]), range(shape_all@coords[,1]))), maptype=c("satellite"), GRAYSCALE=TRUE)

#1st image
j=2
B=100
colors<-bpy.colors(n=B, cutoff.tails=0.1, alpha=1)

#Plot all maps with Bathy background and color ramps
for (j in 2:length(shape_all@data)){
  if (is.numeric(shape_all@data[,j])==TRUE){
    name<-names(shape_all@data)[j]
    a<-shape_all[is.na(shape_all@data[,j])==FALSE,]
    a$Col <- as.numeric(cut(a@data[,j],breaks = B))
    a$Color<-colors[a$Col]
    
    png(paste("ColumbiaRiver2016_AllDays/maps2/2016_ColumbiaRiver_AllDays_", name,  ".png", sep=""), res=600, width=12, height=6.5, units="in")    
    
    #par( oma = c( 0,2,1,0 ) )
    layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(12), heights=c(5,1.5))
    #layout.show(3)
    
    breaks <- seq(min((a@data)[j], na.rm = TRUE), max((a@data)[j], na.rm = TRUE),length.out=100)
    par(mar=c(1,1,1,1))
    
    PlotOnStaticMap(map, lat=a@coords[,2], lon=a@coords[,1], col=a$Color, pch=16, FUN=points)
    
    #Add scale
    par(mar=c(4,1,1,1))
    image.scale((a@data), col=colors[1:(B-1)], breaks=breaks-1e-8,axis.pos=1)
    mtext((paste(name)), 1, 2.5)
    #abline(v=levs)
    box()
    
    
    dev.off()
  }
}

# End plotting loop

