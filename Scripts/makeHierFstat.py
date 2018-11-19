"""
This script is designed to take a big info input and make a hierfstat output 
for the purpose of calculating global Fst for big groups (like CB hybrids or 
Jalisco Parv). Only Parv, Mex and Mexican Maize are represented here.
Created by David E. Hufnagel
"""

import sys
inp = open(sys.argv[1])      #ZeaAllInfo.pmz
out = open(sys.argv[2], "w") #hierFstat output


#Make title line in output
inp.readline()
markers = inp.readline().strip().split("\t")[-1].split(":")[1].split(",")
newLine = "pop\tindiv\t%s\n" % ("\t".join(markers))
out.write(newLine)

#Make other lines in output
indNum = 0
for line in inp:
    lineLst = line.strip().split("\t")
    name = lineLst[0];tax = lineLst[1];hyb = lineLst[4]
    pop = lineLst[5];cntry = lineLst[8]; grp = lineLst[2]

    #Deal with only Parv, Mex and Mexican Maize
    if (tax == "Zea_mays_parviglumis" and (hyb == "hybrid" or hyb == "parvHC"))\
    or (tax == "Zea_mays_mexicana" and (hyb == "hybrid" or hyb == "mexHC")) or\
    (tax == "Zea_mays_mays" and cntry == "Mexico"):
        #Determine popNum
        popNum = -9
        if tax != "Zea_mays_mays":
            if hyb == "hybrid":
                if pop == "South_Guerrero":
                    popNum = 5
                elif pop == "Central_Balsas":
                    popNum = 6
                elif pop == "Central_Plateau":
                    popNum = 7
            else:
                if grp == "ZMPBA" and hyb == "parvHC":
                    popNum = 1
                elif grp == "ZMPJA" and hyb == "parvHC":
                    popNum = 2
                elif grp == "ZMXCH" and hyb == "mexHC":
                    popNum = 3
                elif grp == "ZMXCP" and hyb == "mexHC":
                    popNum = 4

        #Determine genos
        genos = lineLst[-1].split(",")
        newGenos = []
        for pair in genos:
            #replace ATCG with 1234
            newPair = (pair[0] + pair[2]).replace("A","1").replace("T","2").replace("C","3").replace("G","4").replace("NN","NA")         
            newGenos.append(newPair)
        newGenos = "\t".join(newGenos)
        
        #Determine indNum        
        indNum += 1
        
        #Output line
        if popNum != -9:
            newLine = "%s\t%s\t%s\n" % (popNum, indNum, newGenos)
            out.write(newLine)


inp.close()
out.close()