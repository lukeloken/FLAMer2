rm(list=ls(all=TRUE))

setwd("E:/Dropbox/FLAME_ColumbiaRiver/Data")

library(maptools)
library(RgoogleMaps)
library(rgdal)



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

# Same function as above, but use the 1% and 99% quantile as your min and max

image.scale99 <- function(z, zlim, col = heat.colors(12),
                        breaks, axis.pos=1, add.axis=TRUE, ...){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- quantile(z, c(.005, .995), na.rm=TRUE)
    zrange<- range(z, na.rm=TRUE)
    zrange[2] <- zrange[2]+c(zrange[2]-zrange[1])*(1E-3)#adds a bit to the range in both directions
    zrange[1] <- zrange[1]-c(zrange[2]-zrange[1])*(1E-3)
    breaks <- c(zlim[1], seq(zrange[1], zrange[2], length.out=(length(col)+1)), zrange[2])
  }
  
  
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  span<-poly[[3]][1]-poly[[2]][1]
  poly[[1]]<-c(poly[[2]][1]-span , poly[[2]][1], poly[[2]][1], poly[[2]][1]-span ) 
  poly[[length(poly)]]<-c(poly[[length(poly)-1]][2], poly[[length(poly)-1]][2]+span, poly[[length(poly)-1]][2]+span, poly[[length(poly)-1]][2]) 
  
  Xrange<-extendrange(poly, f=0.06)
  
  if(axis.pos %in% c(1,3)){ylim<-c(0,1); xlim<-Xrange}
  if(axis.pos %in% c(2,4)){ylim<-Xrange; xlim<-c(0,1)}
  plot(1,1,t="n",ylim=ylim, xlim=xlim, axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i", ...)  
  for(i in seq(poly)){
    if(axis.pos %in% c(1,3)){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
      polygon(c(Xrange[1], poly[[1]][2],  poly[[1]][2], Xrange[1]), c(0,0,1,1), col=col[1], border=NA)
      polygon(c(poly[[length(poly)]][1], Xrange[2], Xrange[2], poly[[length(poly)]][1]), c(0,0,1,1), col=col[length(col)], border=NA)
      
    }
    if(axis.pos %in% c(2,4)){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
      
      polygon(c(0,0,1,1), c(Xrange[1], poly[[1]][2],  poly[[1]][2], Xrange[1]), col=col[1], border=NA)
      polygon(c(0,0,1,1), c(poly[[length(poly)]][1], Xrange[2], Xrange[2], poly[[length(poly)]][1]), col=col[length(col)], border=NA)
      
      
    }
  }
  box()
  if(add.axis) {
    ticks<-axTicks(1)
    digits<-(-1)*log10(axTicks(1)[2]-axTicks(1)[1])+1
    axis(axis.pos, at=ticks[ticks > poly[[1]][2] & ticks < poly[[length(poly)]][1]])
    axis(axis.pos, at=c(mean(c(Xrange[1], poly[[1]][2])), mean(c(poly[[length(poly)]][1],Xrange[2]))), labels=c(
      paste(round(breaks[1], digits=digits), " -", sep="")
      , paste(round(breaks[length(breaks)-1], digits=digits), " -", sep="")
      )
      , padj=-.8)
    
    axis(axis.pos, at=c(mean(c(Xrange[1], poly[[1]][2])), mean(c(poly[[length(poly)]][1],Xrange[2]))), labels=c(
      paste(round(breaks[2], digits=digits), sep="")
      , paste(round(breaks[length(breaks)], digits=digits), sep="")
      )
      , padj=0.8)
    
  }
}

# End plotting function

# =======================================
# Plot Columbia all days with GMAP background for all variables
# Top and bottom 1% of data is plotted as a single color then, a linear color ramp from 1%->99%
# =======================================

# setwd("E:/Dropbox/FLAME_ColumbiaRiver/Data")
# 
# # Manually load shape_all function if you don't want to loop through all files
# shape_all<-readOGR("ColumbiaRiver2016_AllDays/shapefiles", "ColumbiaRiver_AllDays_cleaned")

map<-GetMap(center=c(mean(range(shape_all@coords[,2])), mean(range(shape_all@coords[,1]))), size=c(640,266), zoom=min(MaxZoom(range(shape_all@coords[,2]), range(shape_all@coords[,1]))), maptype=c("satellite"), GRAYSCALE=TRUE)

#1st image
j=2
B=100
colors_end<-bpy.colors(n=2, cutoff.tails=0.01, alpha=1)
colors_main<-bpy.colors(n=B-2, cutoff.tails=0.1, alpha=1)
colors<-c(colors_end[1], colors_main, colors_end[2])

#Plot all maps with Bathy background and color ramps
for (j in 2:length(shape_all@data)){
  if (is.numeric(shape_all@data[,j])==TRUE){
    name<-names(shape_all@data)[j]
    a<-shape_all[is.na(shape_all@data[,j])==FALSE,]
    
    zlim <- quantile(a@data[,j], c(.01, .99), na.rm=TRUE)
    zrange<- range(a@data[,j], na.rm=TRUE)
    zrange[2] <- zrange[2]+c(zrange[2]-zrange[1])*(1E-3)#adds a bit to the range in both directions
    zrange[1] <- zrange[1]-c(zrange[2]-zrange[1])*(1E-3)
    breaks <- c(zrange[1], seq(zlim[1], zlim[2], length.out=(length(colors)-1)), zrange[2])
    
    a$Col <- as.numeric(cut(a@data[,j],breaks = breaks))
    a$Color<-colors[a$Col]
    

    png(paste("ColumbiaRiver2016_AllDays/maps3/2016ColumbiaRiver_AllDays_", name,  ".png", sep=""), res=600, width=12, height=6.5, units="in")    
    
    #par( oma = c( 0,2,1,0 ) )
    layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(12), heights=c(5,1.5))
    #layout.show(3)
    

    par(mar=c(1,1,1,1))

    PlotOnStaticMap(map, lat=a@coords[,2], lon=a@coords[,1], col=a$Color, pch=16, FUN=points)
    
    #Add scale
    par(mar=c(4,1,1,1))
    image.scale99((a@data), col=colors, breaks=breaks,axis.pos=1)
    mtext((paste(name)), 1, 2.5)
    #abline(v=levs)
    box()
    
    
    dev.off()
  }
}

# End plotting loop

