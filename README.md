# track_rain
Radar Nowcasting
1. Boot up R studio and download relevant packages: 'spatstat', 'NMOF', 'Matrix'

2. Execute: rain_data <- readRDS('raw_data1.rds') which contains 5 consecutive radar data matrices 15 minutes apart.

3. Source: 'matrixtoimage.R' which contains relevant functions used.

4. Run 'advection_v3.R'

Parameter Estimation 
1. Run 'para_search_v3.R' Output = the speed of the clouds in a subsection of the 500x500 in both the x direction and y direction

2. Run 'error_map.R' Output = A heatmap of the most likely speed of the clouds between the 2 images/matrices selected
