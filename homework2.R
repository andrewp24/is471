##### Question 1 #####

ptext = "Members of the Senate and of the House of Representatives of the United States, I feel greatly honored that you should have thus invited me to enter the United States Senate Chamber and address the representatives of both branches of Congress. The fact that my American forebears have for so many generations played their part in the life of the United States, and that here I am, an Englishman, welcomed in your midst, makes this experience one of the most moving and thrilling in my life, which is already long and has not been entirely uneventful. I wish indeed that my mother, whose memory I cherish, across the vale of years, could have been here to see. By the way, I cannot help reflecting that if my father had been American and my mother British instead of the other way around, I might have got here on my own. In that case this would not have been the first time you would have heard my voice. In that case I should not have needed any invitation. But if I had it is hardly likely that it would have been unanimous. So perhaps things are better as they are."

char_vec = gsub(pattern = " ", replacement = "", x = ptext, fixed = TRUE)

char_vec = gsub(pattern = ",", replacement = "", x = char_vec, fixed = TRUE)

char_vec = gsub(pattern = ".", replacement = "", x = char_vec, fixed = TRUE)

char_vec = toupper(char_vec)

words = unlist(strsplit(char_vec, ""))

table(words)

key = "ZGYHXIWJVKULTMSARBQCPDOENF"

key_char = unlist(strsplit(key, ""))

alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

alpha_char = unlist(strsplit(alpha, ""))

num_char= nchar(char_vec)

ctext = words
for (i in 1:num_char){
  alpha_match = match(words[i], alpha_char)
  ctext[i] = key_char[alpha_match]
}
print(ctext)