library(compositions)
library(seqinr)


##### Question 1 ######

numbers = c(4,0,6,5,7,1,3,2)
s11=binary(numbers) # [1] "100" "000" "110" "101" "111" "001" "011" "010"

numbers2 = c(5,3,0,7,6,2,1,4)
s12=binary(numbers2) # [1] "001" "100" "110" "010" "000" "111" "101" "011"

s11_function = function(binary_val){
  b=unlist(strsplit(as.character(binary_val), ""))
  tempVal=b
  tempVal=c(tempVal[2:4])
  tempVal=paste(tempVal[1:3])
  tempVal=toString(c(tempVal[1], tempVal[2], tempVal[3]))
  tempVal=gsub(pattern = ", ", replacement = "", x = tempVal, fixed = TRUE)
  tempVal=strtoi(tempVal, base=2)
 #print("tempVal is below")
 #print(tempVal)
  
  if (b[1]==0) {
    tempRet= s11[tempVal+1]
  } else {
    tempRet= s12[tempVal+1]
  }
  
  return(tempRet)
}

s12_function = function(binary_val){
  b=unlist(strsplit(as.character(binary_val), ""))
  tempVal=b
  tempVal=c(tempVal[2:4])
  tempVal=paste(tempVal[1:3])
  tempVal=toString(c(tempVal[1], tempVal[2], tempVal[3]))
  tempVal=gsub(pattern = ", ", replacement = "", x = tempVal, fixed = TRUE)
  tempVal=strtoi(tempVal, base=2)
 #print("tempVal is below")
 #print(tempVal)
  
  if (b[1]==0) {
    tempRet= s11[tempVal+1]
  } else {
    tempRet= s12[tempVal+1]
  }
  
  return(tempRet)
}
s12_function("1000")

##### Question 2

expand = function(binaryVec){
  
  swap(binaryVec[3], binaryVec[4])
  ev = vector(mode="character", length = 8)
  ev = append(binaryVec[1:6], binaryVec[3:4], after = 2)
  return(ev)
}

round1 = function(binaryNum, key){
  binaryNum=unlist(strsplit(binaryNum, ""))
  keyNum=unlist(strsplit(key, ""))
  
 #print("split below")
  l0=binaryNum[1:6]
  r0=binaryNum[7:12]
 #print(l0)
 #print(r0)
  
 #print("keys below")
  k1=keyNum[1:8]
  k2=keyNum[2:9]
 #print(k1)
 #print(k2)
  
 #print("extended below")
  el0=expand(l0)
  er0=expand(r0)
 #print(el0)
 #print(er0)
  
 #print("xor below")
  xord=bitwXor(as.integer(er0),as.integer(k1))
 #print(xord)
  
 #print("blocks below")
  block1=xord[1:4]
  block2=xord[5:8]
 #print(block1)
 #print(block2)
  
 #print("e and f below")
  e=s11_function(block1)
  f=s12_function(block2)
 #print(e)
 #print(f)
  
  fr0=paste(e,f, sep = "")
 #print("fr0 below")
 #print(fr0)
  
 #print("new l0")
  l0=toString(l0)
  l0=gsub(",", "", toString(l0))
  l0=gsub(" ", "", toString(l0))
 #print(l0)
 #print("new r0")
  r0=toString(r0)
  r0=gsub(",", "", toString(r0))
  r0=gsub(" ", "", toString(r0))
 #print(r0)
  
  #print("binary l0 below")
  #print(binary(l0, mb=5))

  #print("l0 binary below")
  l0=unlist(strsplit(l0,""))
 #print(l0)
  fr0=unlist(strsplit(fr0,""))
 #print(fr0)
  
  r1=bitwXor(as.integer(l0),as.integer(fr0))
 #print("r1 below")
  r1=gsub(" ", "", toString(r1))
  r1=gsub(",", "", toString(r1))
 #print(r1)
  l1=unlist(strsplit(as.character(r0), ""))

  
  l1=gsub(",", "", toString(l1))
  l1=gsub(" ", "", toString(l1))
 #print("r1 and l1 below")
 #print(r1)
 #print(l1)
  
  
  return(paste(l1, r1, sep = ""))
}

test = round1("000001010011", "111000111") #010011000101


