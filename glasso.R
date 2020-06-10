# Install the packages if necessary
#install.packages("devtools")
#install.packages("bgsmtr")
#install.packages("corrplot")
#install.packages("pheatmap")
library(devtools)
library(bgsmtr)
library(corrplot)
library(pheatmap)
#install_github("monogenea/gflasso")
library(gflasso)

# load data
dat = read.table('data_total.csv',dec=',',sep=';',header=T)
# transform data to represent difference between visit 2 and 3
dat.visit2 = subset(dat, subset=(visit==2))
dat.visit3 = subset(dat, subset=(visit==3))
# Exploratory analysis of clinical data
z2 = as.matrix(dat.visit2[,7:16])
corrplot(cor(z2,use='pairwise.complete.obs'),method='ellipse')
# BMI, hdl, glucose missing in visit 3
z3 = as.matrix(dat.visit3[,7:16])
corrplot(cor(z3,use='pairwise.complete.obs'),method='ellipse')
z_diff = z3 - z2
corrplot(cor(z_diff,use='pairwise.complete.obs'),method='ellipse')

# remove id with unpaired data
id.exclude = c(3,4,58,69,87)

dat.new = subset(dat,subset= !(id %in% id.exclude),select= -c(BMI,hdl_cholesterol,glucose))

# Create subset for each visit on the new dataset
dat.visit2 = subset(dat.new, subset=(visit==2))
dat.visit3 = subset(dat.new, subset=(visit==3))

dat.diff = subset(dat.visit3,select=-c(id,visit,group,compliance,sex,age)) -
            subset(dat.visit2,select=-c(id,visit,group,compliance,sex,age))
dat.diff = cbind(subset(dat.visit3,
            select=c(id,visit,group,compliance,sex,age)), dat.diff)

# visual inspection of the observation by group
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")
panel.cor <- function(x, y){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y), digits=2)
    txt <- paste0("R = ", r)
    cex.cor <- 1.5/strwidth(txt)
    text(0.5, 0.5, txt, cex = abs(cex.cor * r))
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[as.factor(dat.new$group)])
}
# Create the plots
pairs(~n3index + total_cholesterol + ldl_cholesterol + TG + EPA + DPA + DHA, 
        dat.new ,lower.panel = panel.cor,
        upper.panel = upper.panel, oma=c(3,3,3,9))
par(xpd=TRUE)
legend("bottomright", legend = as.vector(unique(dat.new$group)),  
    fill=my_cols)

# Create corr plots for only diff b/w visit 2 and 3
# visual inspection of the observation by group
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")
panel.cor <- function(x, y){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y), digits=2)
    txt <- paste0("R = ", r)
    cex.cor <- 1.5/strwidth(txt)
    text(0.5, 0.5, txt, cex = abs(cex.cor * r))
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[as.factor(dat.diff$group)])
}
# Create the plots
pairs(~n3index + total_cholesterol + ldl_cholesterol + TG + EPA + DPA + DHA, 
        dat.diff ,lower.panel = panel.cor,
        upper.panel = upper.panel, oma=c(3,3,3,9))
par(xpd=TRUE)
legend("bottomright", legend = as.vector(unique(dat.diff$group)),  
    fill=my_cols)

# Try running gflasso
# define obs matrix
head(names(dat.diff),10)
Y = subset(dat.diff, select = c(n3index,total_cholesterol,ldl_cholesterol,TG,EPA,DPA,DHA))
X = subset(dat.diff, select = -c(group, id,visit,compliance,n3index,total_cholesterol,ldl_cholesterol,TG,EPA,DPA,DHA))
R <- ifelse(cor(Y) > .7, 1, 0)
#system.time(testCV <- cv_gflasso(as.matrix(X), as.matrix(Y), R, nCores = 2))
testCV <- cv_gflasso(as.matrix(X), as.matrix(Y), R, nCores = 2)

R2 <- function(pred, y){
      cor(as.vector(pred), as.vector(y))**2
}

R <- cor(Y)**2
testCV2 <- cv_gflasso(as.matrix(X), as.matrix(Y), R, err_fun=R2, nCores = 2, err_opt = "max")
gfMod <- gflasso(X = as.matrix(X), Y = as.matrix(Y), R = R, opts = list(lambda = testCV$optimal$lambda,
                                                                    gamma = testCV$optimal$gamma, 
                                                                    delta_conv = 1e-5,
                                                                    iter_max = 1e5))
pheatmap(testCV2$b)