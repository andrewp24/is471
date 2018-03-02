library(seqinr)

##### Question 1 #####

s = "110011"

##### Question 2 #####

v = unlist(strsplit(s, "")) # "1" "1" "0" "0" "1" "1"

##### Question 3 #####

swap(v[3], v[4]) # 
v = unlist(strsplit(s, "")) # "1" "1" "0" "0" "1" "1"

##### Question 4 #####

#Question 4
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
