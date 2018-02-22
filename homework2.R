##### Question 1 #####

#1a
ptext = "Members of the Senate and of the House of Representatives of the United States, I feel greatly honored that you should have thus invited me to enter the United States Senate Chamber and address the representatives of both branches of Congress. The fact that my American forebears have for so many generations played their part in the life of the United States, and that here I am, an Englishman, welcomed in your midst, makes this experience one of the most moving and thrilling in my life, which is already long and has not been entirely uneventful. I wish indeed that my mother, whose memory I cherish, across the vale of years, could have been here to see. By the way, I cannot help reflecting that if my father had been American and my mother British instead of the other way around, I might have got here on my own. In that case this would not have been the first time you would have heard my voice. In that case I should not have needed any invitation. But if I had it is hardly likely that it would have been unanimous. So perhaps things are better as they are."

#1e, takes out spaces
char_vec = gsub(pattern = " ", replacement = "", x = ptext, fixed = TRUE)

#1e, takes out commas
char_vec = gsub(pattern = ",", replacement = "", x = char_vec, fixed = TRUE)

#1e, takes out periods
char_vec = gsub(pattern = ".", replacement = "", x = char_vec, fixed = TRUE)

#1e, makes all characters uppercase
char_vec = toupper(char_vec)

#1b/c, unlists and string splits
words = unlist(strsplit(char_vec, ""))

#1f
table(words)
#output = words
# A   B   C   D   E   F   G   H   I   K   L   M   N   O   P   R   S   T   U   V   W   X   Y 
# 72  14  17  33 126  22  11  66  59   2  25  28  62  53   8  46  51  86  20  16  10   1  24 

#1g
key = "ZGYHXIWJVKULTMSARBQCPDOENF"

key_char = unlist(strsplit(key, ""))

#1h
alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

alpha_char = unlist(strsplit(alpha, ""))

#1i, answer is 852
num_char= nchar(char_vec)

ctext = words

#1j
for (i in 1:num_char){
  alpha_match = match(words[i], alpha_char)
  ctext[i] = key_char[alpha_match]
}

##### Question 2 #####

#Output = TXTGXBQSICJXQXMZCX
substitution_encrypt = function(plainText, key_letters) {
  plainText_subbed = gsub(pattern = " ", replacement = "", x = plainText, fixed = TRUE)
  plainText_subbed = gsub(pattern = ",", replacement = "", x = plainText_subbed, fixed = TRUE)
  plainText_subbed = gsub(pattern = ".", replacement = "", x = plainText_subbed, fixed = TRUE)
  plainText_upper = toupper(plainText_subbed)
  plainText_unl_str = unlist(strsplit(plainText_upper, ""))
  encrypt_key = unlist(strsplit(key_letters, ""))
  alpha_chars = LETTERS
  plainText_num = nchar(plainText_upper)
  encrypted_text = plainText_unl_str
  
  for (i in 1:plainText_num){
    alpha_match = match(plainText_unl_str[i], alpha_chars)
    encrypted_text[i] = encrypt_key[alpha_match]
  }
  print(encrypted_text)
}

encrypted = substitution_encrypt("Members of the senate,.", "ZGYHXIWJVKULTMSARBQCPDOENF")

##### Question 3 #####

#Output = MEMBERSOFTHESENATE
substitution_decrypt = function(encryptedText, key){
  encrypted_letters = unlist(strsplit(encryptedText, ""))
  encrypt_key = unlist(strsplit(key, ""))
  alpha_chars = LETTERS
  encryptedText_num = nchar(encryptedText)
  decrypted_text = encrypted_letters
  
  for (i in 1:encryptedText_num){
    key_match = match(encrypted_letters[i], encrypt_key)
    decrypted_text[i] = alpha_chars[key_match]
  }
  print(decrypted_text)
  
}

plainText = substitution_decrypt("TXTGXBQSICJXQXMZCX", "ZGYHXIWJVKULTMSARBQCPDOENF")
print(plainText)