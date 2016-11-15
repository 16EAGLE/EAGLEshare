#1
#cut() converts a numeric variable by a defined intervall (breaks) into intervalls
#d is a numeric array with values from 0 to 50 and 60 cells 
d <- sample(0:50, 60, replace=TRUE)

#d_cut is the cutted array d within the intervalls 0 to 30 and 30 to 60
#the resulting array consists of a cut using two breaking levels
d_cut <- cut(d, breaks = c(0,30,60))


#2 #comment
#with sort() the values within an vector array can be get into a decreasing or increasing order
d_sort_inc <- sort(d)
d_sort_dec <- sort(d, decreasing = TRUE)

#in comparisin to sort(), the order() function returns an array of the subscripts of the input
#array sorted by the values connected to the subscript value
d_subs_inc <- order(d)
d_subs_dec <- order(d, decreasing = TRUE)

#for prove, the subscripts stored ind d_subs_inc can be used to access the d values
#the result is the same as sort(d)
d_sort_order <- d[d_subs_inc]


#3
#quantile() produces the distribution measure of quantiles of an input array.
#By default, it outputs breaking values for each quantile.
#Those describe, between which intervalls the first, second, third and fourth quantile
#(25% of the values with increasing order) are situated.
d_quant <- quantile(d)

