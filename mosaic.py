##==================================
##Mosaic To New Raster
##Usage: MosaicToNewRaster_management inputs;inputs... output_location raster_dataset_name_with_extension 
##                                    {coordinate_system_for_the_raster} 8_BIT_UNSIGNED | 1_BIT | 2_BIT | 4_BIT 
##                                    | 8_BIT_SIGNED | 16_BIT_UNSIGNED | 16_BIT_SIGNED | 32_BIT_FLOAT | 32_BIT_UNSIGNED 
##                                    | 32_BIT_SIGNED | | 64_BIT {cellsize} number_of_bands {LAST | FIRST | BLEND  | MEAN 
##                                    | MINIMUM | MAXIMUM} {FIRST | REJECT | LAST | MATCH}
import arcpy
import os
#"C:/Users/yanfap/Desktop/1"
filePath="G:/qrf_1h"
arcpy.env.workspace = filePath
fileNameList=os.listdir(filePath)
fileStrToMosaic=""
for i in fileNameList:
    if(i!="Mosaic"):
        fileStrToMosaic=fileStrToMosaic+i+";"
try:
    if(not os.path.exists(filePath+"/Mosaic")):
        os.makedirs(filePath+"/Mosaic")
    ##Mosaic several TIFF images to a new TIFF image
    arcpy.MosaicToNewRaster_management(fileStrToMosaic,"Mosaic", "UNC_MAP_CHINA.tif", "#","32_BIT_FLOAT", "#", "1", "LAST","FIRST")
    print "Success"
except:
    print "Mosaic To New Raster example failed."
    print arcpy.GetMessages()

