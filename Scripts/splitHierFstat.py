"""
This script is designed to split a hierFstat file into all pairwise comparisons
Created by David E. Hufnagel
"""
import sys

inp = open(sys.argv[1])         #ZeaAllInfo.pmz.hierFstat
out1_2 = open("ZeaAllInfo.pmz.hierFstat.PB_PJ", "w")
out1_3 = open("ZeaAllInfo.pmz.hierFstat.PB_Mch", "w") 
out1_4 = open("ZeaAllInfo.pmz.hierFstat.PB_Mcp", "w") 
out1_5 = open("ZeaAllInfo.pmz.hierFstat.PB_Hsg", "w") 
out1_6 = open("ZeaAllInfo.pmz.hierFstat.PB_Hcb", "w") 
out1_7 = open("ZeaAllInfo.pmz.hierFstat.PB_Hcp", "w") 

out2_3 = open("ZeaAllInfo.pmz.hierFstat.PJ_Mch", "w") 
out2_4 = open("ZeaAllInfo.pmz.hierFstat.PJ_Mcp", "w") 
out2_5 = open("ZeaAllInfo.pmz.hierFstat.PJ_Hsg", "w") 
out2_6 = open("ZeaAllInfo.pmz.hierFstat.PJ_Hcb", "w") 
out2_7 = open("ZeaAllInfo.pmz.hierFstat.PJ_Hcp", "w") 

out3_4 = open("ZeaAllInfo.pmz.hierFstat.Mch_Mcp", "w") 
out3_5 = open("ZeaAllInfo.pmz.hierFstat.Mch_Hsg", "w") 
out3_6 = open("ZeaAllInfo.pmz.hierFstat.Mch_Hcb", "w") 
out3_7 = open("ZeaAllInfo.pmz.hierFstat.Mch_Hcp", "w") 

out4_5 = open("ZeaAllInfo.pmz.hierFstat.Mcp_Hsg", "w") 
out4_6 = open("ZeaAllInfo.pmz.hierFstat.Mcp_Hcb", "w") 
out4_7 = open("ZeaAllInfo.pmz.hierFstat.Mcp_Hcp", "w") 

out5_6 = open("ZeaAllInfo.pmz.hierFstat.Hsg_Hcb", "w") 
out5_7 = open("ZeaAllInfo.pmz.hierFstat.Hsg_Hcp", "w") 

out6_7 = open("ZeaAllInfo.pmz.hierFstat.Hcb_Hcp", "w") 


#output title lines
title = inp.readline()
out1_2.write(title); out1_3.write(title); out1_4.write(title)
out1_5.write(title); out1_6.write(title); out1_7.write(title)  
out2_3.write(title); out2_4.write(title); out2_5.write(title) 
out2_6.write(title); out2_7.write(title); out3_4.write(title) 
out3_5.write(title); out3_6.write(title); out3_7.write(title) 
out4_5.write(title); out4_6.write(title); out4_7.write(title) 
out5_6.write(title); out5_7.write(title); out6_7.write(title) 

#Go through inp, determine pop, and output according to pop
for line in inp:
    lineLst = line.strip().split("\t")
    pop = lineLst[0]
    if pop == "1":
        out1_2.write(line); out1_3.write(line); out1_4.write(line)
        out1_5.write(line); out1_6.write(line); out1_7.write(line)  
    if pop == "2":
        out1_2.write(line); out2_3.write(line); out2_4.write(line)
        out2_5.write(line); out2_6.write(line); out2_7.write(line)
    if pop == "3":
        out1_3.write(line); out2_3.write(line); out3_4.write(line) 
        out3_5.write(line); out3_6.write(line); out3_7.write(line) 
    if pop == "4":
        out1_4.write(line); out2_4.write(line); out3_4.write(line)
        out4_5.write(line); out4_6.write(line); out4_7.write(line)
    if pop == "5":
        out1_5.write(line); out2_5.write(line); out3_5.write(line)
        out4_5.write(line); out5_6.write(line); out5_7.write(line)
    if pop == "6":
        out1_6.write(line); out2_6.write(line); out3_6.write(line)
        out4_6.write(line); out5_6.write(line); out6_7.write(line)
    if pop == "7":
        out1_7.write(line); out2_7.write(line); out3_7.write(line)
        out4_7.write(line); out5_7.write(line); out6_7.write(line)



inp.close(); out1_2.close(); out1_3.close(); out1_4.close(); out1_5.close()
out1_6.close(); out1_7.close(); out2_3.close(); out2_4.close(); out2_5.close() 
out2_6.close(); out2_7.close(); out3_4.close(); out3_5.close()
out3_6.close(); out3_7.close(); out4_5.close(); out4_6.close(); out4_7.close()
out5_6.close(); out5_7.close(); out6_7.close()