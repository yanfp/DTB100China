#clip area of interest into every 1°×1° block

import arcpy
import os
import gc

gc.collect()
def block(inPath,col,row,outPath):
    xInt=(136-73)/col
    yInt=(54-18)/row
    for i in range(0,col):
        for j in range(0,row):  
            #extend="{0} {1} {2} {3}".format(73+i*xInt-0.2,54-(j+1)*yInt-0.2,73+(i+1)*xInt,54-j*yInt+0.2)
            extend="{0} {1} {2} {3}".format(73+i*xInt,54-(j+1)*yInt,73+(i+1)*xInt+0.01,54-j*yInt+0.01)
            print(i,j)
            print(extend)
            arcpy.Clip_management(inPath,extend,outPath+"\\{0}_{1}.tif".format(73+i*xInt,54-(j+1)*yInt),"#","#","NONE")
x=input("number of block in longitude direction：")
y=input("number of block in latitute direction:")
block("F:\\China\\Mask\\China_1k.tif",x,y,"F:\\China\\Mask\\1k")
