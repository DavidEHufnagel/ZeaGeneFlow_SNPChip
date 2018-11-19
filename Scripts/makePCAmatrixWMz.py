#This script is designed to take my big info file, grab parviglumis,
#mexicana, maize and hybrid individuals and make a file displaying their SNP data in
#the format:
"""   SNPa   SNPb ... color (Parv=blue, Mex=red, Hybrid=purple)
indA   X      X
indB   X      X
...
where X=O if first allele homozygote, X=2 if second allele homozygote, X=3 if heterozygote
"""
#Created by David E. Hufnagel

import sys
inp = open(sys.argv[1])      #ZeaAllInfo.pmz
out = open(sys.argv[2], "w") #ZeaAllInfo.pmz.PCAmatrix.wMz

#Output header info
out.write("#python %s\n" % (" ".join(sys.argv)))

#use the title to determine the number of markers
inp.readline()
markers = inp.readline().strip().split(":")[-1].split(",")

#create hash table representing what the "first" allele at each loci is where the index defines what marker the data is for
firstAlleles = []
for ind in range(len(markers)):
    firstAlleles.append("X")

#Go through inp and if individual is of the right taxonomy convert SNP to 0, 1, or 2 and output info in proper format
for line in inp:
    lineLst = line.strip().split("\t")   
    if lineLst[1] == "Zea_mays_parviglumis" or lineLst[1] == "Zea_mays_mexicana" or (lineLst[1] == "Zea_mays_mays" and lineLst[8] == "Mexico"): #no maize
        name = lineLst[0]
        out.write("%s\t" % (name))  ### comment out this line to remove names ###
        SNPs = lineLst[-1].split(",")
        cnt  = 0 #index for connecting to firstAlleles
        for pair in SNPs:
            one = pair.split("_")[0]
            two = pair.split("_")[1]
            
            ##Determine SNPval where SNPval is the value to go in to SNP matrix
            if one == "N": #two will also be "N"
                one = "?"
                two = "?"

            ##Output SNPvals
            out.write(one)
            out.write("/")
            out.write(two)
            out.write("\t")
            cnt += 1                

        out.write("\n")

inp.close()
out.close()
