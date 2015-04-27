###--------------------------------------------------
### The Words the Media Industry Prefers
### Data from https://medium.com/message/the-words-the-media-industry-prefers-5d33e5b021e0
###--------------------------------------------------



###--------------------------------------------------
### options, libraries, functions
###--------------------------------------------------
library(cluster)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)

theme_set(theme_minimal())

## double center a matrix
doubleCenter <- function(x){
  n <- dim(x)[1]
  k <- dim(x)[2]
  rowMeans <- matrix(apply(x,1,mean,na.rm=TRUE),n,k,byrow=TRUE)
  colMeans <- matrix(apply(x,2,mean,na.rm=TRUE),n,k,byrow=FALSE)
  matrixMean <- matrix(mean(as.matrix(x),na.rm=TRUE),n,k)
  (x - rowMeans - colMeans + matrixMean)/-2
}



###--------------------------------------------------
### Get the Data
###--------------------------------------------------

data <- read.csv("data/searches-and-ratios.csv", header=TRUE, row.names=1)
rownames(data) <- gsub("\\.com", "", rownames(data))

head(data)

### Search ratios
ind <- str_detect(colnames(data), "\\.\\.")
data.ratios <- data[,ind]
data.ratios.t <- as.data.frame(t(data.ratios))

## Search terms
data.search <- data[,!ind]
data.search.t <- as.data.frame(t(data.search))

## Dissimilarity Matrices
d <- dist(data.ratios)
d1 <- dist(data.ratios.t)

### Check applicability
dc  <- doubleCenter(as.matrix(d))
dc.eig <- eigen(dc)
dc.eig.v <- dc.eig$values
## we'd prefer most of the eigenvalues to be positive
dc.eig.v>0
e.pos <- sum(dc.eig.v[dc.eig.v>0])
e.neg <- sum(dc.eig.v[dc.eig.v<0])

###--------------------------------------------------
### 1. Clustering Search Ratios
###--------------------------------------------------

out.by.site <- hclust(d, method="ward.D2")
out.by.search <- hclust(d1, method="ward.D2")

## Dendrograms
plot(out.by.site, hang=-1, main="Clustering Sites by Search Ratios",
     sub="", xlab="")


plot(out.by.search, hang=-1, main="Clustering Search Ratios by Site Profile",
     sub="", xlab="")


### Plot the search ratios ordered by clustering results

## Organize the data
data.m <- data.ratios
data.m$Site <- rownames(data.m)
colnames(data.m) <- gsub("\\.\\.", "isto", colnames(data.m))
data.m <- gather(data.m, Ratio, Score, dogistocat:manistowoman)
data.m$Ratio <- gsub("isto", ":", data.m$Ratio)
data.m$Ratio <- factor(data.m$Ratio, levels=unique(data.m$Ratio)[out.by.search$order], ordered=TRUE)

data.m$Site <- factor(data.m$Site, levels=unique(data.m$Site)[out.by.site$order], ordered=TRUE)

pdf(file="figures/search-ratios-clustered.pdf", height=10, width=4)
p <- ggplot(data.m, aes(x=Ratio, y=Site, fill=asinh(Score)))
p1 <- p + geom_tile() +  theme(axis.text.x=element_text(hjust=1, angle=30),
                                      axis.text.y=element_text(hjust=1), legend.position = "top") +
    scale_fill_gradient(low="white", high=muted("red")) + labs(fill="Hit ratio (logged)", y="", x="Search Comparison")
print(p1)
dev.off()

ggsave("figures/search-ratios-clustered.png", p1, height = 12, width = 4.5, dpi = 300)




###--------------------------------------------------
### 2. Clustering Search Terms
### Do all that again, but just use the raw results
### this time --- Searches only, not ratios
###--------------------------------------------------

## Dissimilarity Matrices
d <- dist(data.search)
d1 <- dist(data.search.t)
dc  <- doubleCenter(as.matrix(d))
dc.eig <- eigen(dc)
dc.eig.v <- dc.eig$values
dc.eig.v>0
e.pos <- sum(dc.eig.v[dc.eig.v>0])
e.neg <- sum(dc.eig.v[dc.eig.v<0])


out.by.site <- hclust(d, method="ward.D2")
out.by.search <- hclust(d1, method="ward.D2")


### --------------------------------------------------
### Dendrograms
### --------------------------------------------------
pdf(file="figures/site-search-clustering.pdf", height=8, width=12)
plot(out.by.site, hang=-1, main="Clustering Sites by Searches",
          sub="", xlab="")
dev.off()


png(
  "figures/site-search-clustering.png",
  width     = 6,
  height    = 4,
  units     = "in",
  res       = 1200,
  pointsize = 8
)
par(
  mar      = c(5, 5, 2, 2),
  xaxs     = "i",
  yaxs     = "i",
  cex.axis = 1.2,
  cex.lab  = 1.2
)
plot(out.by.site, hang=-1, main="Clustering Sites by Searches",
          sub="", xlab="")
dev.off()



## Not bad
pdf(file="figures/search-results-clustering.pdf", height=6, width=12)
plot(out.by.search, hang=-1, main="Clustering Searches by Site Profile",
     sub="", xlab="")
dev.off()

png(
  "figures/search-results-clustering.png",
  width     = 7,
  height    = 4,
  units     = "in",
  res       = 1000,
  pointsize = 8
)
par(
  #mar      = c(5, 5, 2, 2),
  xaxs     = "i",
  yaxs     = "i",
  cex.axis = 1.2,
  cex.lab  = 1.2
    )
plot(out.by.search, hang=-1, main="Clustering Searches by Site Profile",
     sub="", xlab="")
dev.off()


### --------------------------------------------------
### Searches ordered by clustering results
### --------------------------------------------------

### Tidy up the data
data.m <- data.search
data.m$Site <- rownames(data.m)
data.m <- gather(data.m, Search, Score, dog:woman)
data.m$Search <- factor(data.m$Search, levels=unique(data.m$Search)[out.by.search$order], ordered=TRUE)
data.m$Site <- factor(data.m$Site, levels=unique(data.m$Site)[out.by.site$order], ordered=TRUE)

pdf(file="figures/search-results-clustered.pdf", height=12, width=5)
p <- ggplot(data.m, aes(x=Search, y=Site, fill=asinh(Score)))
p1 <- p + geom_tile() +  theme(axis.text.x=element_text(hjust=1, angle=45),
                                      axis.text.y=element_text(hjust=1), legend.position = "top") +
    scale_fill_gradient(low="white", high=muted("red")) + labs(fill="Hits (logged)", y="", x="Search Term")
print(p1)
dev.off()

ggsave("figures/search-results-clustered.png", p1, height = 12, width = 4.5, dpi = 300)


###--------------------------------------------------
### 3: Try scaling the search data before
### clustering.
###
### Scaled Search Results
###--------------------------------------------------

data.search.cen <- data.search
data.search.cen <- scale(data.search.cen, center=FALSE)
data.search.cen.t <- t(scale(data.search.cen, center=FALSE))

## Dissimilarity Matrices
d <- dist(data.search.cen)
d1 <- dist(data.search.cen.t)

out.by.site <- hclust(d, method="ward.D2")
out.by.search <- hclust(d1, method="ward.D2")


### --------------------------------------------------
### Dendrograms
### --------------------------------------------------
pdf(file="figures/site-search-clustering-scaled.pdf", height=12, width=10)
plot(out.by.site, hang=-1, main="Clustering Sites by Searches (Scaled)",
          sub="", xlab="")
dev.off()

pdf(file="figures/search-results-clustering-scaled.pdf", height=6, width=12)
plot(out.by.search, hang=-1, main="Clustering Searches by Site Profile (Scaled)",
     sub="", xlab="")
dev.off()


### --------------------------------------------------
### Plot scaled searched ordered by cluster results
### --------------------------------------------------

### Gather the data
data.m <- data.frame(data.search.cen)
data.m$Site <- rownames(data.m)
data.m <- gather(data.m, Search, Score, dog:woman)
data.m$Score <- data.m$Score
data.m$Search <- factor(data.m$Search, levels=unique(data.m$Search)[out.by.search$order], ordered=TRUE)
data.m$Site <- factor(data.m$Site, levels=unique(data.m$Site)[rev(out.by.site$order)], ordered=TRUE)

pdf(file="figures/search-results-clustered-scaled.pdf", height=12, width=5)
p <- ggplot(data.m, aes(x=Search, y=Site, fill=Score))
p1 <- p + geom_tile() +  theme(axis.text.x=element_text(hjust=1, angle=45),
                                      axis.text.y=element_text(hjust=1), legend.position = "top") +
    scale_fill_gradient(low="gray98", high=muted("red")) + labs(fill="Scaled Hits", y="", x="Search Term")
print(p1)
dev.off()

ggsave("figures/search-results-clustered-scaled.png", p1, height = 12, width = 5, dpi = 300)




###--------------------------------------------------
### 4: Drop Wikipedia and Tumblr, then scale.
###--------------------------------------------------

ind <- rownames(data.search) %in% c("en.wikipedia.org", "tumblr")
data.search.cen <- data.search[!ind,]
data.search.cen <- scale(data.search.cen, center=FALSE)
data.search.cen.t <- t(scale(data.search.cen, center=FALSE))

## Dissimilarity Matrices
d <- dist(data.search.cen)
d1 <- dist(data.search.cen.t)

out.by.site <- hclust(d, method="ward.D2")
out.by.search <- hclust(d1, method="ward.D2")


### --------------------------------------------------
### Dendrograms
### --------------------------------------------------
pdf(file="figures/site-search-clustering-scaled-nowikitumb.pdf", height=12, width=10)
plot(out.by.site, hang=-1, main="Clustering Sites by Searches (Scaled)",
          sub="", xlab="")
dev.off()

pdf(file="figures/search-results-clustering-scaled-nowikitumb.pdf", height=6, width=12)
plot(out.by.search, hang=-1, main="Clustering Searches by Site Profile (Scaled)",
     sub="", xlab="")
dev.off()


### --------------------------------------------------
### Plot scaled searched ordered by cluster results
### --------------------------------------------------

### Gather the data
data.m <- data.frame(data.search.cen)
data.m$Site <- rownames(data.m)
data.m <- gather(data.m, Search, Score, dog:woman)
data.m$Score <- data.m$Score
data.m$Search <- factor(data.m$Search, levels=unique(data.m$Search)[out.by.search$order], ordered=TRUE)
data.m$Site <- factor(data.m$Site, levels=unique(data.m$Site)[rev(out.by.site$order)], ordered=TRUE)

pdf(file="figures/search-results-clustered-scaled-nowikitumb.pdf", height=12, width=5)
p <- ggplot(data.m, aes(x=Search, y=Site, fill=Score))
p1 <- p + geom_tile() +  theme(axis.text.x=element_text(hjust=1, angle=45),
                                      axis.text.y=element_text(hjust=1), legend.position = "top") +
    scale_fill_gradient(low="gray98", high=muted("red")) + labs(fill="Scaled Hits", y="", x="Search Term")
print(p1)
dev.off()

ggsave("figures/search-results-clustered-scaled-nowikitumb.png", p1, height = 12, width = 5, dpi = 300)
