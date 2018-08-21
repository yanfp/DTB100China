
##Resample_management (in_raster, out_raster, {cell_size}, {resampling_type})

import arcpy
import os

filePath="H:/data/landuse/all"
arcpy.env.workspace = filePath

fileNameList=os.listdir(filePath)
newFileName=""
for i in fileNameList:
    if len(i)==20:
        newFileName=i[0:6]+".tif"
        arcpy.Resample_management(i, newFileName, "90", "#")
