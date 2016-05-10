a <- c("that guy is cool",
  "what a wierd mane",
  "man, that's a cool",
  "that's a cool guyt",
  "interesting that this guy's guitar is orange",
  "congressman jones",
  "chairman jones")
a.df <- as.data.frame(a)
colnames(a.df) <- c("all")

a.df$men <- ifelse(grepl("\\b(guy'?s?'?|spokesm[ae]n'?|chairm[ae]n'?|congressm[ae]ns?|m[ae]n'?
|him|he|his|boys?|boyfriends?|brothers?|dads?|dudes?|fathers?|gentlem[ae]n|gods?
                                 |grandfathers?|grandpas?|grandsons?|grooms?|himself|hisself|husbands?|kings?|males?
                                 |mr|nephews?|priests?|princes?|sons?|uncles?|widowers?)\\b", 
                                 a.df$all, ignore.case = TRUE), 1, 0)
