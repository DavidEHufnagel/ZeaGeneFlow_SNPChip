"""
This script is designed to take a big info file and determine the genotype
frequency of the major chromosome4 inversion SNP in all the teosintes as 
well as all other relevant groups. The diagnostic SNPs is PZD00030.1.
Created by David E. Hufnagel
"""
import sys

data = open(sys.argv[1])     #ZeaAllInfo.all
out = open(sys.argv[2], "w") #inv4mSNPtable.txt

def SaveIntoDict(key, val, dictX):
    if key not in dictX:
        dictX[key] = [val]
    else:
        dictX[key].append(val) 
    
def SaveIntoDictCnt(key, dictX):
    if key not in dictX:
        print key
        print "error"
        sys.exit()
    else:
        dictX[key] += 1
        
        
#Go through title and determine the index of diagnostic SNP
data.readline()
markers = data.readline().strip().split(":")[-1].split(",")
ind = markers.index("PZD00030.1")

#Go through data and make a dict of key: tax val: inv4mSNPstate
groupDict = {}
for line in data:
    lineLst = line.strip().split("\t")
    tax = lineLst[1]
    hyb = lineLst[4]
    pop = lineLst[5]
    geno = lineLst[-1].split(",")
    SNP = geno[ind]

    #determine group
    if tax in ["Zea_diploperennis","Zea_perennis","Zea_luxurians","Zea_mays_huehuetenangensis"]:
        group = tax
    elif hyb == "parvHC":
        group = hyb
    elif hyb == "mexHC":
        group = hyb
    elif tax == "Zea_mays_parviglumis" and hyb == "other":
        group = "parvAmb"
    elif tax == "Zea_mays_mexicana" and hyb == "other":
        group = "mexAmb"
    elif hyb == "hybrid":
        if pop == "Central_Plateau":
            group = "hybCP"
        elif pop == "Central_Balsas":
            group = "hybCB"
        elif pop == "South_Guerrero":
            group = "hybSG"
        elif pop == "NA":
            group = "hybOther"
        else:
            print "ERROR1!"
            sys.exit()
    else:
        if not tax == "Zea_mays_mays":
            print "ERROR2!"
            sys.exit()
        else:
            group = "maize"

    #Calculate genotype state  
    hetType1 = "C_T" 
    hetType2 = "T_C"
    hetTypes = [hetType1,hetType2]                   
    
    #missing data
    if SNP == "N_N":
        genoType = "N"

    #parv and mex types
    elif SNP == "C_C":
        genoType = "P"
    elif SNP == "T_T":
        genoType = "M"
    
    #heterozygotes
    elif SNP in hetTypes:
        genoType = "H"   
        
    if group != "maize":
        SaveIntoDict(group, genoType, groupDict)
    
#Go through the dictionary and calculate genotype frequencies per group
diplo = {"P":0, "M":0, "H":0, "N":0}  #dictionary of key: invType val: count
peren = {"P":0, "M":0, "H":0, "N":0}
lux = {"P":0, "M":0, "H":0, "N":0}
huehue = {"P":0, "M":0, "H":0, "N":0}
parvHC = {"P":0, "M":0, "H":0, "N":0}
parvAmb = {"P":0, "M":0, "H":0, "N":0}
mexHC = {"P":0, "M":0, "H":0, "N":0}
mexAmb = {"P":0, "M":0, "H":0, "N":0}
hybAll = {"P":0, "M":0, "H":0, "N":0}
hybCP = {"P":0, "M":0, "H":0, "N":0}
hybCB = {"P":0, "M":0, "H":0, "N":0}
hybSG = {"P":0, "M":0, "H":0, "N":0}
for group,genos in groupDict.items():
    if group == "Zea_diploperennis":
        for geno in genos:
            SaveIntoDictCnt(geno, diplo)
    elif group == "Zea_perennis":
        for geno in genos:
            SaveIntoDictCnt(geno, peren)
    elif group == "Zea_luxurians":
        for geno in genos:
            SaveIntoDictCnt(geno, lux)
    elif group == "Zea_mays_huehuetenangensis":
        for geno in genos:
            SaveIntoDictCnt(geno, huehue)
    elif group == "parvHC":
        for geno in genos:
            SaveIntoDictCnt(geno, parvHC)
    elif group == "parvAmb":
        for geno in genos:
            SaveIntoDictCnt(geno, parvAmb)
    elif group == "mexHC":
        for geno in genos:
            SaveIntoDictCnt(geno, mexHC)
    elif group == "mexAmb":
        for geno in genos:
            SaveIntoDictCnt(geno, mexAmb)
    elif group == "hybOther":
        for geno in genos:
            SaveIntoDictCnt(geno, hybAll)
    elif group == "hybCP":
        for geno in genos:
            SaveIntoDictCnt(geno, hybCP)
            SaveIntoDictCnt(geno, hybAll)
    elif group == "hybCB":
        for geno in genos:
            SaveIntoDictCnt(geno, hybCB)
            SaveIntoDictCnt(geno, hybAll)
    elif group == "hybSG":
        for geno in genos:
            SaveIntoDictCnt(geno, hybSG)
            SaveIntoDictCnt(geno, hybAll)
    else:
        print "ERROR3!"
        sys.exit()



#Calculate total inds in each group
diploTotal = float(sum([diplo["P"], diplo["M"], diplo["H"], diplo["N"]]))
perenTotal = float(sum([peren["P"], peren["M"], peren["H"], peren["N"]]))
luxTotal = float(sum([lux["P"], lux["M"], lux["H"], lux["N"]]))
huehueTotal = float(sum([huehue["P"], huehue["M"], huehue["H"], huehue["N"]]))
parvHCTotal = float(sum([parvHC["P"], parvHC["M"], parvHC["H"], parvHC["N"]]))
parvAmbTotal = float(sum([parvAmb["P"], parvAmb["M"], parvAmb["H"], parvAmb["N"]]))
mexHCTotal = float(sum([mexHC["P"], mexHC["M"], mexHC["H"], mexHC["N"]]))
mexAmbTotal = float(sum([mexAmb["P"], mexAmb["M"], mexAmb["H"], mexAmb["N"]]))
hybAllTotal = float(sum([hybAll["P"], hybAll["M"], hybAll["H"], hybAll["N"]]))
hybCPTotal = float(sum([hybCP["P"], hybCP["M"], hybCP["H"], hybCP["N"]]))
hybCBTotal = float(sum([hybCB["P"], hybCB["M"], hybCB["H"], hybCB["N"]]))
hybSGTotal = float(sum([hybSG["P"], hybSG["M"], hybSG["H"], hybSG["N"]]))

#Output genotype frequencies
newLine = "-- Distribution of Chromosome 4 PZD00030.1 SNP genotypes: --\n"
out.write(newLine)
newLine = "--         P=parviglumis type, M=mexicana type,           --\n"
out.write(newLine)
newLine = "--           H=heterozygous, N=missing data               --\n"
out.write(newLine)
newLine = "------------------------------------------------------------\n"
out.write(newLine)
newLine = "        group   | P(%)  | M(%)  | H(%)  | N(%)\n"
out.write(newLine)
newLine = "        ----------------------------------\n"
out.write(newLine)
newLine = "        diplo   | %.1f   | %.1f | %.1f | %.1f\n" % (diplo["P"]/diploTotal*100.0, diplo["M"]/diploTotal*100.0, diplo["H"]/diploTotal*100.0, diplo["N"]/diploTotal*100.0)
out.write(newLine)
newLine = "        ----------------------------------\n"
out.write(newLine)
newLine = "        peren   | %.1f   | %.1f | %.1f | %.1f\n" % (peren["P"]/perenTotal*100.0, peren["M"]/perenTotal*100.0, peren["H"]/perenTotal*100.0, peren["N"]/perenTotal*100.0)
out.write(newLine)
newLine = "        ----------------------------------\n"
out.write(newLine)
newLine = "        lux     | %.1f   | %.1f | %.1f | %.1f\n" % (lux["P"]/luxTotal*100.0, lux["M"]/luxTotal*100.0, lux["H"]/luxTotal*100.0, lux["N"]/luxTotal*100.0)
out.write(newLine)
newLine = "        ----------------------------------\n"
out.write(newLine)
newLine = "        huehue  | %.1f | %.1f   | %.1f | %.1f\n" % (huehue["P"]/huehueTotal*100.0, huehue["M"]/huehueTotal*100.0, huehue["H"]/huehueTotal*100.0, huehue["N"]/huehueTotal*100.0)
out.write(newLine)
newLine = "        ----------------------------------\n"
out.write(newLine)

newLine = "        parvHC  | %.1f  | %.1f   | %.1f | %.1f\n" % (parvHC["P"]/parvHCTotal*100.0, parvHC["M"]/parvHCTotal*100.0, parvHC["H"]/parvHCTotal*100.0, parvHC["N"]/parvHCTotal*100.0)
out.write(newLine)
newLine = "        ----------------------------------\n"
out.write(newLine)
newLine = "        parvAmb | %.1f  | %.1f   | %.1f | %.1f\n" % (parvAmb["P"]/parvAmbTotal*100.0, parvAmb["M"]/parvAmbTotal*100.0, parvAmb["H"]/parvAmbTotal*100.0, parvAmb["N"]/parvAmbTotal*100.0)
out.write(newLine)
newLine = "        ----------------------------------\n"
out.write(newLine)
newLine = "        mexHC   | %.1f   | %.1f  | %.1f | %.1f\n" % (mexHC["P"]/mexHCTotal*100.0, mexHC["M"]/mexHCTotal*100.0, mexHC["H"]/mexHCTotal*100.0, mexHC["N"]/mexHCTotal*100.0)
out.write(newLine) 
newLine = "        ----------------------------------\n"
out.write(newLine)
newLine = "        mexAmb  | %.1f   | %.1f  | %.1f | %.1f\n" % (mexAmb["P"]/mexAmbTotal*100.0, mexAmb["M"]/mexAmbTotal*100.0, mexAmb["H"]/mexAmbTotal*100.0, mexAmb["N"]/mexAmbTotal*100.0)
out.write(newLine)
newLine = "        ----------------------------------\n"
out.write(newLine)

newLine = "        hybAll  | %.1f  | %.1f   | %.1f | %.1f\n" % (hybAll["P"]/hybAllTotal*100.0, hybAll["M"]/hybAllTotal*100.0, hybAll["H"]/hybAllTotal*100.0, hybAll["N"]/hybAllTotal*100.0)
out.write(newLine)
newLine = "        ----------------------------------\n"
out.write(newLine)
newLine = "        hybCP   | %.1f  | %.1f  | %.1f | %.1f\n" % (hybCP["P"]/hybCPTotal*100.0, hybCP["M"]/hybCPTotal*100.0, hybCP["H"]/hybCPTotal*100.0, hybCP["N"]/hybCPTotal*100.0)
out.write(newLine)
newLine = "        ----------------------------------\n"
out.write(newLine)
newLine = "        hybCB   | %.1f  | %.1f   | %.1f | %.1f\n" % (hybCB["P"]/hybCBTotal*100.0, hybCB["M"]/hybCBTotal*100.0, hybCB["H"]/hybCBTotal*100.0, hybCB["N"]/hybCBTotal*100.0)
out.write(newLine)
newLine = "        ----------------------------------\n"
out.write(newLine)
newLine = "        hybSG   | %.1f  | %.1f   | %.1f | %.1f\n" % (hybSG["P"]/hybSGTotal*100.0, hybSG["M"]/hybSGTotal*100.0, hybSG["H"]/hybSGTotal*100.0, hybSG["N"]/hybSGTotal*100.0)
out.write(newLine)
newLine = "        ----------------------------------\n"
out.write(newLine)


data.close()
out.close()
