latlong = "+init=epsg:4326"
ukgrid = "+init=epsg:27700"
google = "+init=epsg:3857"


dotref_points <- as(dotref_points, 'Spatial')

dotref_roads <- as(dotref_roads, 'Spatial')

dotref_points <- spTransform(dotref_points, ukgrid)

dotref_roads <- spTransform(dotref_roads, ukgrid)

sfInit(parallel=TRUE, cpus=parallel:::detectCores()-1)

groups_to_do <- as.integer(seq(1,nrow(dotref_points),length.out = 100))

for (i in 1:length(groups_to_do))
  
{
 start_time <- Sys.time()
 
  start <- groups_to_do[[i]]
  if (i < length(groups_to_do)) {
    end   <- groups_to_do[[i+1]]-1 } else { 
      end   <- groups_to_do[[i]] 
      }
  print(paste(start, end))
  
  temp_result <- snapPointsToLines(dotref_points[start:end,], dotref_roads, withAttrs = T, idField = 2)
  
  if(i == 1) {result <- temp_result} else {result <- rbind(result, temp_result)}
  
  print(i)
  
  end_time <- Sys.time()
  
  print(end_time - start_time)
  
}

sfStop()

names(result)[names(result)=="nearest_line_id"] <- "street_type"
result <- as.data.frame(result)
write.csv(result, 'Oscar_DOT_Toid_10m_V2.csv')
