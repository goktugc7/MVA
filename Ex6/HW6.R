###############################
# Henry Qiu  ,  Goktug Cengiz #
#    Mehmet Fatih Cagil       #
###############################
#Practice of Decision Trees

#libraries
library(FactoMineR)
library(dplyr)
library(arules)


# 1) Read the file: tic_tt. Check the ??class?? of every variable of tic_tt.
tic = read.csv("/Users/goktugcengiz/Desktop/Datasets/tic_tt.csv",
              header = TRUE, sep = ";", quote = "\"", dec = ",", row.names = 1, fill = TRUE)
summary(tic)

# 2) Find the profile of the people who do payments by Internet
tic_fac=tic %>% mutate_if(is.character, as.factor)
summary(tic_fac)
catdes(tic_fac,num.var=28)
colnames(tic_fac)

#!!!TOBEREMOVED(results Ha comprat per Internet high because it is obvious, if you buy by internet you pay by internet)

# 3) Convert the tic_tt file to a transactions file.
trns <-as(tic,"transactions")

# 4) Define the parameters: Min_support, min_confidence and maximum size of itemsets, and run the apriori function.
min_sup = 0.01
min_conf = 0.4
max_size = 5
rules = apriori(trns, parameter = list(support = min_sup, confidence = min_conf, maxlen = max_size))
rules

# 5) List the 10 most frequent itemsets.
# generatingItemsets returns a collection of the itemsets which generated the rules, one itemset for each rule
# the collection can be a multiset, so we need to use unique method to unify and contain duplicated elements
fsets = unique(generatingItemsets(rules))
fsets <- as(fsets, "data.frame")
ord_freq = fsets[order(-fsets$support),]
ord_freq[1:10,]

# 6) List the first 10 rules sorted by the lift.
ord_lift = sort(rules, by="lift")
inspect(ord_lift[1:10])

# 7) List the 10 rules according the lift, where the Consequent is "Pagament.a.trav??s.d.Internet.".
# Otbain the rules which has "Pagaments.a.traves.d.Internet." in the right hand side
# %oin% means that we only select itemsets matching only the given item Pagament.a.trav??s.d.Internet. as the statement requires
rhs_patdi <-subset(rules, subset = rhs %oin% "Pagament.a.traves.d.Internet.")
ord_lift_patdi <-sort(rhs_patdi, by="lift")
inspect(ord_lift_patdi[1:100])
