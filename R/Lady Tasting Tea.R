#############################################################

# A lady declares that by tasting a cup of tea made with milk she can 
#discriminate whether the milk or the tea infusion was first added to the cup.

# The null hypothesis is that the subject has no ability to distinguish the teas.

# In Fisher's approach, there was no alternative hypothesis, 
#unlike in the Neyman–Pearson approach.

# The test statistic is a simple count of the number of successes in 
# selecting the 4 cups (the number of cups of the given type successfully selected).

# The distribution of possible numbers of successes, assuming the null hypothesis 
# is true, can be computed using the number of combinations.

#############################################################

# provides a subject with 8 randomly ordered cups of tea : 
# 4 prepared by first pouring the tea, then adding milk, 
# 4 prepared by first pouring the milk, then adding the tea.

# The subject has to select 4 cups prepared by one method.

#############################################################

# Initialize cups
cups = c(0, 1, 2, 3, 4, 5, 6, 7)

# Selecting exactly 4 out of 8: (4 of 8) = 8! / (4! * (8-4)!)
pnum = (8*7*6*5)/(4*3*2*1)

# The probability to select exactly the correct 4 out of 8 cups
prob = (1/70)*100

print(cat("The probability to select exactly the correct 4 out of 8 cups = ", prob, '%'))


#############################################################

#                  All Combinations

#############################################################

poss = t(combn(8,4))
print(cat("Length = ", nrow(poss)))

df = data.frame(poss[1:70, 1:4])

poss2 = paste(df$X1, df$X2, df$X3, df$X4, sep=" , ")
df$new = poss2

#############################################################

#                  Random Selection

#############################################################

listy = c()
rc_list = c()
success_list = c()
for (i in 1:length(df$X1)) {
    
    # Random Choice from combinations
    rc <- sample(df$new, 1)
    rc_list <- c(rc_list, rc)
    
    # Iterate over sample from combinations
    ii = df$new[i]
    
    rc2 = strsplit(sample(df$new, 1), ' , ')[[1]]
    ii2 = strsplit(df$new[i], ' , ')[[1]]
    
    print(cat("Random Choice = ", rc, " | Length = ", length(rc2)))
    print(cat("Sample = ", ii, " | Length = ", length(ii2)))
    
    listy <- c(listy, list(str_detect(rc, ii2)))    
    print(cat("Success = ", table(list(str_detect(rc, ii2)))["TRUE"]))
    success_list <- c(success_list, table(list(str_detect(rc, ii2)))["TRUE"])
    print("")
    print("===========================================================")
    print("")
  }

df$random_choice <- rc_list
df$counter <- listy
df$success_list <- success_list


#############################################################

#                    Distribution

#############################################################

count_table = as.data.frame(table(df$success_list))

ggplot(count_table) + geom_bar(aes(x=Var1, y=Freq), stat="identity") + 
  ggtitle("Distribution")

