#This script is designed to take a disctruct .indivq file and sort individuals
#by altitude.
#Created by David E. Hufnagel
import sys

inp = open(sys.argv[1])         #unsorted disctruct input file (*.indivq)
out = open(sys.argv[2], "w")    #sorted disctruct inputfile
info = open(sys.argv[3])        #ZeaAllInfo.pmz
altOut = open(sys.argv[4], "w") #file with names and altitudes in order

#remove all empty elements from a list
def RemoveEmpty(listx):
    newList = []
    for item in listx:
        if not item == "":
            newList.append(item)
    return newList

#Go through inp and make a dict of key: name val: wholeLine
lineDict = {}
for line in inp:
    lineLst = RemoveEmpty(line.strip().split(" "))
    lineDict[lineLst[1]] = line
    
#Go through info and make a list of tuples with (altitude, name)
altLst = []
for line in info:
    if not line.startswith("#"):
        lineLst = line.strip().split("\t")
        name = lineLst[0]
        if name in lineDict.keys():
            if lineLst[10] != "NA":
                alt = int(lineLst[10])
            else:
                alt = lineLst[10]
            newTup = (alt,name)
            altLst.append(newTup)
        
#sort the list by altitude
altLst.sort()

#Output title line for altOut
title = "name\taccession\taltitude\n"
altOut.write(title)

#Go through the list and output whole lines
for pair in altLst:
    name = pair[1]
    out.write(lineDict[name])

    #output to altOut
    group = RemoveEmpty(lineDict[name].split(" "))[3]
    alt = pair[0]
    newLine = "%s\t%s\t%s\n" % (name, group, alt)
    altOut.write(newLine)


inp.close()
out.close()
info.close()
altOut.close()
