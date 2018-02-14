log10(1000) + log10(100)
#part 1
utf8ToInt('A')  #65
utf8ToInt('Z')  #90
utf8ToInt('a')  #97
intToUtf8(65)   #"A"
intToUtf8(48)   #"0"
intToUtf8(100)  #"d"
intToUtf8(10)   #"\n"
bitwXor(2,3)    #1
bitwXor(45,89)  #116
bitwXor(8,8)    #0
bitwXor(0,1)    #1
#part2
p<-utf8ToInt(p_text)
k<-99
c<-bitwXor(p,k)
c_text<-intToUtf8(c)
d<-bitwXor(c,k)
d_text<-intToUtf8(d)
length(p)
#part 3
?utf8ToInt
?bitwXor
?length
