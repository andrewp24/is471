library(seqinr)
library(compositions)

##### Question 1 #####

s = "110011"

##### Question 2 #####

v = unlist(strsplit(s, "")) # "1" "1" "0" "0" "1" "1"

##### Question 3 #####

swap(v[3], v[4]) # 
v = unlist(strsplit(s, "")) # "1" "1" "0" "0" "1" "1"

##### Question 4 #####

ev = vector(mode="character", length = 8)
ev = append(v[1:6], v[3:4], after = 2) # # "1" "1" "0" "0" "0" "0" "1" "1"

##### Question 5 #####

expand = function(binaryVec){
  
  v = unlist(strsplit(binaryVec, ""))
  swap(v[3], v[4])
  ev = vector(mode="character", length = 8)
  ev = append(v[1:6], v[3:4], after = 2)
  return(ev)
}
expand("123456") # "1" "2" "4" "3" "4" "3" "5" "6"

##### Question 6 #####
numbers = c(5,2,1,6,3,4,7,0)
s11=binary(numbers) # [1] "101" "010" "001" "110" "011" "100" "111" "000"

numbers2 = c(1,4,6,2,0,7,5,3)
s12=binary(numbers2) # [1] "001" "100" "110" "010" "000" "111" "101" "011"

##### Question 7 #####
b=unlist(strsplit("1101", ""))
tempVal=b
tempVal=c(tempVal[2:4])
tempVal=paste(tempVal[1:3])
tempVal=toString(c(tempVal[1], tempVal[2], tempVal[3]))
tempVal=gsub(pattern = ", ", replacement = "", x = tempVal, fixed = TRUE)
tempVal=strtoi(test, base=2)

if (b[1]==0) {
  tempRet= s11[tempVal]
} else {
  tempRet= s12[tempVal]
}

##### Question 8 #####

s11_function = function(binary_val){
  b=unlist(strsplit(binary_val, ""))
  tempVal=b
  tempVal=c(tempVal[2:4])
  tempVal=paste(tempVal[1:3])
  tempVal=toString(c(tempVal[1], tempVal[2], tempVal[3]))
  tempVal=gsub(pattern = ", ", replacement = "", x = tempVal, fixed = TRUE)
  tempVal=strtoi(tempVal, base=2)
  
  if (b[1]==0) {
    tempRet= s11[tempVal+1]
  } else {
    tempRet= s12[tempVal+1]
  }
  
  return(tempRet)
}
s11_function("1000")
