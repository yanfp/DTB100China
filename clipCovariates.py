# -*- coding: cp936 -*-
# coding:utf-8

# clip the covariates into every 1°×1° 
import arcpy
import os
import gc
import shutil

gc.collect()
def block(inPath,col,row,outPath,covaName):
    xInt=(136-73)/col
    yInt=(54-18)/row
    for i in range(0,col):
        for j in range(0,row):  
            #extend="{0} {1} {2} {3}".format(73+i*xInt-0.2,54-(j+1)*yInt-0.2,73+(i+1)*xInt,54-j*yInt+0.2)
            lb_lon=73+i*xInt-0.01
            lb_lat=54-(j+1)*yInt-0.01
            rt_lon=73+(i+1)*xInt+0.01
            rt_lat=54-j*yInt+0.01
            extend="{0} {1} {2} {3}".format(lb_lon,lb_lat,rt_lon,rt_lat)
            #print(i,j)
            #print(extend)
            arcpy.Clip_management(inPath,extend,outPath+"\\"+"{0}_{1}.tif".format(73+i*xInt,54-(j+1)*yInt),"#","#","NONE")
x=input("Enter the block number of longitudinal direction:")
y=input("Enter the block number of latitudinal direction:")

for i,j,k in os.walk("F:\\China\\Temp"):
    for filename in k:
        s=filename.split(".")
        filetype=s[-1]
        if filetype!="tif":
            continue
        path=os.path.join("F:/China/Mask/",s[0])
        if(not os.path.exists(path)):
            os.makedirs(path)
        block(os.path.join(i,filename),x,y,path,s[0])

""""
### Move File 
path1="F:/China/Covariates/ImpCovariates3"
path2="F:/China/Covariates/BlockCovariatesAll"
fileList1=os.listdir(path1)
for i in fileList1:
    filePath1=path1+"/"+i
    fileList2=os.listdir(filePath1)
    for j in fileList2:
        strList1=j.split(".")
        if(strList1[-1]!="tif"):
            continue
        filePath3=filePath1+"/"+j
        filePaht4=path2+"/"+strList1[0]
        if(not os.path.exists(filePaht4)):
            os.makedirs(filePaht4)
        dstPath=filePaht4+"/"+i+".tif"
        shutil.copyfile(filePath3,dstPath)
print i
"""
