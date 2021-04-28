#########
## The objective of this script is to average simulations from different pilots
## and across years
## This script assume a specific file structure and file names
################

# extract simulation results from ouranos
extract_ouranos<-function(dir,scenario=c("rcp45","rcp85"),var,year_sel,stations,lat_long){
  
  output_long=data.frame(station=NA,parameter=NA,value=NA,scenario=NA,year=NA)
  
  
  #we need to extract data drom each simulation by navigating properly in the file system
  files=list.files(dir)
  
  
  #print(files_sel_dir)
  
  tmp.slice_list=list()
  
  
  xy_spdf <- SpatialPointsDataFrame(coords = lat_long, data = nla_coords,
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  tmp_list=list()
  
  for(var in var){
    
    for(scenario in scenario){
      
      #identify folders with the right scenario
      g=grep(pattern = scenario, x = files)
      
      #files from the selected scenario + print
      files_sel_dir=files[g]
      
      for(j in year_sel){
        
        
        for(i in files_sel_dir){
          
          #dir_temp=paste0(dir,i)
          #print(dir_temp)
          
          #identify simulation for the right variable 
          #files=list.files( dir_temp)
          g=grep(pattern = var, x = files_sel_dir)
          
          files_sel=files_sel_dir[g]
          #for now should only have ne file, if more need to implement another loop
          #print(files_sel)
          print(j)
          
          #open netCDF file (.nc)
          # good tutorial: https://www.youtube.com/watch?v=roMf6xzB9NI&ab_channel=EmpowerDat
          sim=nc_open(paste0(dir,"/",files_sel))
          
          
          #print what is in the object
          #print(sim)
          
          # extract the temperature data (tas)
          temp_array=ncvar_get(sim,var)
          
          
          # extract lat and lon
          lon_array=ncvar_get(sim,"lon")
          lat_array=ncvar_get(sim,"lat")
          
          # extract the time (year)
          time_array=ncvar_get(sim,"time")
          
          
          
          #time units are in days since 1955-12-31T12:00:00 so 
          years_array=seq(1955,1955+length(time_array)-1,1)
          
          #close the connection when the data is extracted
          nc_close(sim)
          
          tmp.slice=temp_array[,,years_array==j]
          
          # attribute a longitude to each row
          rownames(tmp.slice)=lon_array
          # attribute a latitude to each column
          colnames(tmp.slice)=lat_array
          
          
          tmp.slice_list[[i]]=tmp.slice
        }
        
        #average and sd for the different pilots
        tmp.slice_avg=apply(simplify2array(tmp.slice_list), 1:2, mean)
        tmp.slice_sd=apply(simplify2array(tmp.slice_list), 1:2, sd)
        
        
        #tmp.slice to rater
        tmp.slice_avg_df = as.data.frame(as.table(tmp.slice_avg))
        tmp.slice_sd_df = as.data.frame(as.table(tmp.slice_sd))
        
        
        newColNames = c("X", "Y", "Z")
        colnames(tmp.slice_avg_df) = newColNames
        colnames(tmp.slice_sd_df) = newColNames
        
        tmp.slice_avg_raster = rasterFromXYZ(tmp.slice_avg_df)
        tmp.slice_sd_raster = rasterFromXYZ(tmp.slice_sd_df)
        
        crs(tmp.slice_avg_raster) = CRS("+proj=longlat +datum=WGS84")
        crs(tmp.slice_sd_raster) = CRS("+proj=longlat +datum=WGS84")
        
        #conserve the first base plot, just want to line to move
        if(j==year_sel[1])data_base=tmp.slice_avg_raster
        
        #temp_polygons=cut(tmp.slice_avg_raster, breaks= c(0,17.6,100))
        #temp_polygons <- rasterToPolygons(temp_polygons, dissolve=T)
        
        #hydrolake_base=point.in.poly(hydrolake_raw,temp_polygons)
        #hydrolake_base=hydrolake_base[!is.na(hydrolake_base$layer),]
        
        #nrow(hydrolake_base[hydrolake_base$layer==2,])/nrow(hydrolake_base)
        #prop=nrow(hydrolake_base[hydrolake_base$layer==2&hydrolake_base$Depth_avg<2,])/nrow(hydrolake_base[hydrolake_base$Depth_avg<2,])
        #prop=nrow(hydrolake_base[hydrolake_base$layer==2,])/nrow(hydrolake_base[,])
        
        #dat=c(nrow(hydrolake_base[hydrolake_base$layer==2,]),nrow(hydrolake_base[hydrolake_base$layer==1,]))
        #barplot(dat)
        
        #png(units = "in" ,width = 8 ,height = 5.5,res=300,filename = paste0("figures/gif_pie/pie_",scenario,"_",j,".png"))
        #pie(c(prop,1-prop),labels="",col=c("#fc4103","#38cf42"))
        #barplot(dat,ylim=c(0,35000),col=c(rgb(235,90,61,alpha=155,maxColorValue =255),col=c(rgb(21,194,81,alpha=155,maxColorValue =255))),border=NA)
        #dev.off()
        
        
        
        
        #png(units = "in" ,width = 8 ,height = 5.5,res=300,filename = paste0("figures/gif_map/map_",scenario,"_",j,".png"))
        #plot(data_base,legend=F)
        # plot(tmp.slice_avg_raster,legend=T,breaks=seq(0,30,0.1),col=terrain.colors(length(seq(30,0,0.1))))
        #temp_contour_base<- rasterToContour(tmp.slice_avg_raster,levels=17.6)
        # lines(temp_contour_base, col="black",lwd=2,lty=1)
        # text(j, x=-70,y=50)
        #dev.off()
        #}
        
        
        #http://www.sthda.com/english/articles/2-r/6-create-an-animated-gif-image-with-r-and-imagemagick/
        #base_dir=getwd()
        #setwd("figures/gif_map")
        
        #system(paste0("magick convert -delay 80 *.png ../",scenario,"_map.gif"))
        #file.remove(list.files(pattern=".png"))
        
        #setwd(base_dir)
        
        #setwd("figures/gif_pie")
        
        #system(paste0("magick convert -delay 80 *.png ../",scenario,"_pie.gif"))
        #file.remove(list.files(pattern=".png"))
        
        #setwd(base_dir)
        
        #system("convert myPlot.png myPlot.gif")
        #system("magick convert -delay 80 figures/gif*.png example_1.gif")
        
        
        
        #}, movie.name = "test2.gif", interval = 1, ani.width = 720, ani.height = 480)
        
        
        #raster_list=list()
        #raster_list[[as.character(j)]]=tmp.slice_avg_raster
        
        
        #plot(tmp.slice_avg_raster)
        #plot(tmp.slice_sd_raster)
        
        #https://rspatial.org/raster/spatial/5-files.html#raster-files
        #writeRaster(tmp.slice_avg_raster, filename=paste0("data/interim/USA_",var,"_","avg_",scenario,"_",j,".tif")
                   # , overwrite=TRUE)
        #writeRaster(tmp.slice_sd_raster, filename=paste0("data/interim/USA_",var,"_","sd_",scenario,"_",j,".tif")
                   # , overwrite=TRUE)
        
        tmp_list[[as.character(j)]]=extract(tmp.slice_avg_raster,xy_spdf)
        
        output_temp=data.frame(station=stations,
                               parameter=var
                               ,value=extract(tmp.slice_avg_raster,xy_spdf)
                               ,scenario=scenario,year=j)
        
        output_long=rbind(output_long,output_temp)
        
        #plot(tmp.slice_avg_raster,legend=F,breaks=seq(0,30,0.1),col=terrain.colors(length(seq(30,0,0.1))))
        #temp_contour_base<- rasterToContour(tmp.slice_avg_raster,levels=17.6)
        #lines(temp_contour_base, col="black",lwd=2,lty=1)
        #text(j, x=-70,y=50)
        
        #hydrolake_raw=readOGR( 
        #   dsn= "data/raw/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10_us.shp"
        #)
        #plot(hydrolake_raw[hydrolake_raw$Depth_avg<2,],add=T)
        
        
        #library(animation)
        #
        #png(units = "in" ,width = 8 ,height = 5.5,res=300,filename = paste0("figures/gif_map/map_",scenario,"_",j,".png"))
        #plot(data_base,legend=F)
        #plot(tmp.slice_avg_raster,legend=T)
        #temp_contour_base<- rasterToContour(tmp.slice_avg_raster,levels=17.6)
        #lines(temp_contour_base, col="black",lwd=2,lty=1)
        #text(j, x=-70,y=50)
        #dev.off()
      }
    }}
  
  #enlever la ligne de NA a la creation du data.frame
  output_long=output_long[-1,]
  
  return(output_long)
}