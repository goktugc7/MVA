###############################
# Henry Qiu  ,  Goktug Cengiz #
#    Mehmet Fatih Cagil       #
###############################

#libraries
#we will use how to compute and interpret correspondence analysis using two R packages: 
#i) FactoMineR for the analysis and ii) factoextra for data visualization. 
library(FactoMineR)
library(factoextra)
library(MASS)           
library(ggplot2)
library(gplots)
library(fpc)
library(gridExtra)

#1)Read the PCA_quetaltecaen data.
qData = read.delim("/Users/goktugcengiz/Desktop/Datasets/PCA_quetaltecaen.txt", 
                        header = TRUE, sep="\t", dec =".")

#Data Understanding
str(qData)
summary(qData)
apply(qData, 2, function(x) sum(is.na(x))) #Missing Value Detection
#We assign the variables of CCAA column to rownames
rownames(qData) = qData[,1]
qData = qData[,-1]
head(qData)
#visually inspect and interpret row and column profiles
#1.convert the data as a table
dt = as.table(as.matrix(qData))
#2.Graph
balloonplot(t(dt), main ="Que tal te caen?", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
#Chi-square test to evaluate whether 
#there is a significant dependence between row and column categories:
chisq = chisq.test(qData)
chisq
#2) Perform a CA of this data. 
#How many dimensions are significant?.Interpret the first factorial plan.

res.ca = CA(qData, graph = TRUE)
eigenvalues = get_eigenvalue(res.ca)  #Extract the eigenvalues/variances retained by each dimension (axis)
#Visualization and interpretation
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50)) #Scree Plot
fviz_ca_biplot(res.ca, repel = TRUE) #Make a biplot of rows and columns. The distance between any row points or column points gives a measure of their similarity (or dissimilarity). Row points with similar profile are closed on the factor map. The same holds true for column points.
fviz_ca_row(res.ca, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE) 
fviz_cos2(res.ca, choice = "row", axes = 1:2)
fviz_ca_col(res.ca, col.col = "cos2", 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)  #Visualize the results for rows and columns, respectively.
fviz_cos2(res.ca, choice = "col", axes = 1:2)


#3)For the PCA_quetaltecaendata, 
#compute the contribution of each cell to the total inertia, 
#that is: (fij-fi.xf.j)2/(fi.xf.j).
#Compute the percentage of inertia due to the diagonal cells.

#total number of counts
n = sum(qData)
#frequency matrix
fij = qData/n
#row weights 
(fi = rowSums(fij))  
#column weights 
(fj = colSums(fij)) 
sum(fi) #check = 1
sum(fj) #check = 1

#total intertia contrib of each cell in contigency table
tot_int_cont = matrix(0,nrow=nrow(fij),ncol=ncol(fij),byrow=T)
colnames(tot_int_cont)=colnames(fij); rownames(tot_int_cont)=rownames(fij)

for (ii in 1:nrow(fij)) {
  for (jj in 1:ncol(fij)) {
    tot_int_cont[ii,jj] = (fij[ii,jj]-fi[ii]*fj[jj])^2/(fi[ii]*fj[jj])
  }
}

#Computation for Total Inertia
totInt = sum(tot_int_cont)  
cat("\nTotal inertia:",totInt)

#fij[11,jj] = fi[ii]*fj[jj]
#Conclusions:
cat ("\nDiagonal's contribution to inertia:",
     round(100*sum(diag(tot_int_cont))/totInt,2),
     "%","\nConclusion: Overloaded diagonal and Gutmann effect.\n")
row = get_ca_row(res.ca)
row$contrib
#Conclusions: 
#Andalusia contributes almost 40% to inertia in the 5th direction and 14.6% in the first
#Catalonia contributes almost 38% and 30% to inertia in the 1st and 2nd direction
row$cos2
#Quality of representation of row profiles in the given directions (correlations with PCs)

#4)Clearly, the overloaded diagonal of the data set 
#influences the results obtained
#(the overall inertia is mainly due to this overload diagonal). 
#Try to nullify this influence by imputingthe diagonal values 
#by the independence hypothesis values of the product of marginal probabilities 
#(=n x fi.xf.j). 
#Take into account that each imputation modifies the marginal, 
#hence you need an iterative algorithm.

eps = 1e-4         
qData_new = qData    
n_new = n         
tot_int_cont_new = tot_int_cont     
totInt_old = 1e4
err = 1e4
cnt = 0

while(err > eps) {
  cnt = cnt + 1   
  fij_new = qData_new/n_new
  fi_new = rowSums(fij_new)
  fj_new = colSums(fij_new)
  diag(qData_new) = n_new * fi_new * fj_new
  n_new = sum(qData_new)
  #total inertia
  for (ii in 1:nrow(fij_new)) {
    for (jj in 1:ncol(fij_new)) {
      tot_int_cont_new[ii,jj] <- (fij_new[ii,jj] - fi_new[ii] * fj_new[jj] )^2 / (fi_new[ii]*fj_new[jj])
    }
  }
  totInt_new = sum(tot_int_cont_new)
  err = abs(totInt_new - totInt_old)
  totInt_old = totInt_new
}

#New contribution to inertia of cells
cat("\nTotal inertia:",totInt_new)
cat("\nDiagonal's new contribution to inertia:", 
    round(100*sum(diag(tot_int_cont_new))/totInt_new,2))

#5)Perform a new CA upon the quetaltecaen table, 
#with the modified diagonal and interpret the results.
res.ca_new = CA(qData_new, graph = TRUE)
eigenvalues_new = res.ca_new$eig[,1]
#new screeplot
fviz_screeplot(res.ca_new, addlabels = TRUE, ylim = c(0, 60)) #Scree Plot
fviz_ca_biplot(res.ca_new, repel = TRUE) #Make a biplot of rows and columns. The distance between any row points or column points gives a measure of their similarity (or dissimilarity). Row points with similar profile are closed on the factor map. The same holds true for column points.
fviz_ca_row(res.ca_new, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE) 
fviz_cos2(res.ca_new, choice = "row", axes = 1:2)
fviz_ca_col(res.ca_new, col.col = "cos2", 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)  #Visualize the results for rows and columns, respectively.
fviz_cos2(res.ca_new, choice = "col", axes = 1:2)
#############################################################################################
#Practice MCA and Clustering

#6. Read the dataset
mcaData = read.table("/Users/goktugcengiz/Desktop/Datasets/mca_car.csv", 
                   header = TRUE, sep=";", dec =".", row.names = 1, strip.white=T)
mcaData[20] = NULL
summary(mcaData)
#7. Perform the MCA
res.mca = MCA(mcaData, ncp=Inf, quanti.sup = c(17), quali.sup = c(18,19))
ind = get_mca_ind(res.mca)
fviz_mca_ind(res.mca, #individuals
             label = "none", # hide individual labels
             habillage = c(19), # color by groups 
             palette = c("#0000CD", "#E7B800", "#A52A2A", "#DDA0DD"),
             addEllipses = TRUE, 
             ellipse.level=0.99,
             ggtheme = theme_minimal()) 
fviz_mca_ind(res.mca,
             gradient.cols = c("#A52A2A"),
             geom="point",
             col.ind = "cos2")
fviz_mca_biplot(res.mca, # Biplot of individuals and variable categories
                axes = 1:2,
                palette="jco",
                label=c("quali.sup", "quanti.sup"),
                habillage=c(19),
                repel =TRUE,
                ggtheme = theme_minimal())
#to highlight the correlation between variables (active & supplementary) and dimensions
#we used the function fviz_mca_var() with the argument choice = ???mca.cor???
fviz_mca_var(res.mca,
             axes = 1:2,
             choice="mca.cor",
             repel = TRUE)
#8.Interpret the first two obtained factors.
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45)) #screeplot
eig.val = get_eigenvalue(res.mca)
summary(res.mca, ncp = 2)
dimdesc(res.mca, axes = 1:2) #correlation between each variable and the principal component of rank s is calculated. correlation coefficients are sorted and significant ones are output
head(eig.val[, 1:2]) 
#9. Decide the number of significant dimensions
newEigVal = eig.val[ ,1] - mean(eig.val[,1])
barplot(newEigVal, names.arg=1:length(newEigVal), 
        main = "Scree Plot",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
sigDim = eig.val[ ,1][newEigVal > 0]
plot(sigDim, type="b", col = "red", main="Eigenvalues", xlab = "Dimensions")

#Once we have obtained the significant dimensions we will retail the ones that has more than 80% of inertia explained.
cumsum(100*sigDim/sum(sigDim))
barplot(sigDim, names.arg=1:length(sigDim), 
        main = "Scree Plot",
        xlab = "Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
#As we observe, in the first 11 dimensions there are 80.79740%.
numSigDim = 11

#10. Perform a hierarchical clustering
#First we will perform mca again with the significant dimensions previously obtained
res.mca_final = MCA(mcaData, ncp=numSigDim, quanti.sup = c(17), quali.sup = c(18,19))
hcpc <- HCPC(res.mca_final,nb.clust=-1)
plot.HCPC(hcpc, choice ="tree")

#11.Using the function catdesinterpret and name 
#the obtained clusters and represent them in the first factorial display.
#Remove the original supplementary variable columns.
dataNoSupl <-mcaData[,-17]
dataNoSupl <-dataNoSupl[,-17]
dataNoSupl <-dataNoSupl[,-17]
#Add the predicted variable column.
classifiedCluster <- hcpc$data.clust[20]
projectionsDataFrame = as.data.frame(cbind(dataNoSupl, classifiedCluster))
#projectionsDataFrame$precio_categ <- as.factor(projectionsDataFrame$precio_categ)
projectionsDataFrame$clust <- as.factor(projectionsDataFrame$clust)
catdes(projectionsDataFrame,num.var=17)

projections = res.mca$ind$coord
plot(x = projections[,1], y = projections[,2])

labls = row.names(projections)
g <- ggplot(as.data.frame(projections), aes(x=projections[,1], y=projections[,2],
                                            color = as.factor(classifiedCluster$clust))) +  geom_point() +
  labs(x = '1st Component', y = '2nd Component') + geom_text(aes(label=labls))
grid.arrange(g)
