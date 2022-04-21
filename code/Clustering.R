
install.packages(c("cluster","factoextra", 
                   "fpc","Rtsne"))
library(Rcpp)

library(hclust)
library(ggplot2) 
library(caret) 
library(cluster) # clustering
library(factoextra) # cluster validation, plots
library(fpc)
library(Rtsne)


data<-read.csv("cleandata.csv", 
               stringsAsFactors = FALSE)
# Obtain structure
str(data)

# Obtain basic descriptive statistics
summary(data)

noms <- c("EST_ST", "REGION", "TBIRTH_YEAR", "EGENDER", "RHISPANIC","RRACE", "MS","THHLD_NUMKID",
          "THHLD_NUMADLT", "HADCOVID", "WRKLOSSRV", "EXPCTLOSS", "ANYWORK",
          "KINDWORK", "UI_APPLYRV","SSA_RECV", "SSA_APPLYRV", "SSALIKELYRV","SSADECISN","EIPRV",
          "EXPNS_DIF", "SPNDSRC1", "SNAP_YN", "ANXIOUS", "WORRY",
          "INTEREST", "DOWN", "PRESCRIPT", "MH_SVCS", "MH_NOTGET", "INCOME")

colnames(data)          

# We use the lapply() function to convert the
# variables to unordered factor variables
data[ ,noms] <- lapply(X = data[ ,noms], 
                       FUN = factor)

data$GETVACRV  <- as.factor(data$GETVACRV) 

ords <- c("EEDUC", "MORTCONF")


data[ ,ords] <- lapply(X = data[ ,ords], 
                       FUN = factor, 
                       ordered = TRUE)


nums <- c("TSPNDFOOD", "TSPNDPRPD")

any(is.na(data))

cen_sc <- preProcess(x = data,
                     method = c("center", "scale"))
cs_sc <- predict(object = cen_sc,
                 newdata = data)


daisy(x = data[1:11000,c(1:10,12:36)], 
      metric = "gower")

dist(x = data, 
     method = "euclidean",
     diag = TRUE)

cor(x = data[ ,nums])
symnum(x = cor(data[ ,nums]), 
       corr = TRUE)



cen_yj <- preProcess(x = data,
                     method = "YeoJohnson")
cs_yj <- predict(object = cen_yj,
                 newdata = data)
hdist <- daisy(x = cs_yj[1:11000,c(1:10,12:36)], 
               metric = "gower")
summary(hdist)

wards <- hclust(d = hdist, 
                method = "ward.D2")
plot(wards, 
     xlab = NA, sub = NA, 
     main = "Ward's Method")
rect.hclust(tree = wards, 
            k = 6, 
            border = hcl.colors(6))

# Create a vector of cluster assignments
wards_clusters <- cutree(tree = wards, 
                         k = 6)

ld_dist <- Rtsne(X = hdist, 
                 is_distance = TRUE)
lddf_dist <- data.frame(ld_dist$Y)
ggplot(data = lddf_dist, 
       mapping = aes(x = X1, y = X2)) +
   geom_point(aes(color = factor(wards_clusters))) +
   labs(color = "Cluster")
A<-cs_yj[1:11000,c(1:10,12:36)]
aggregate(x = A[ ,nums], 
          by = list(wards_clusters),
          FUN = mean)

aggregate(x = A[ ,c(ords,noms)], 
          by = list(wards_clusters), 
          FUN = table)
