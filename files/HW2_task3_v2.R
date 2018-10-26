require(jpeg)
require(rasterImage)
require(gridExtra)
require(grid)

# source("http://bioconductor.org/biocLite.R")
# biocLite("EBImage")
require(EBImage)

img = readJPEG("C:/Users/09172/Desktop/monalisa.jpg", native = FALSE)
str(img)
dim(img)

# > str(img)
# 'nativeRaster' int [1:512, 1:512] -11306389 -11634840 -10976910 -11042189 -11107469 -10712711 -10975623 -11106439 -11763855 -11961234 ...
# - attr(*, "channels")= int 3

plot(c(0, 550), c(0, 550), type = "n", xlab = "", ylab = "")
rasterImage(img,0,0, 512, 512, interpolate = FALSE)


#CHANNELS OF THE IMAGE
img.R = img
img.G = img
img.B = img

img.R[,,2:3] = 0
img.G[,,1]=0
img.G[,,3]=0
img.B[,,1:2]=0

img1 = rasterGrob(img.R)
img2 = rasterGrob(img.G)
img3 = rasterGrob(img.B)
grid.arrange(img1, img2, img3, nrow=1)

par(mfrow=c(1,1))

#ADDITION OF NOISE

noisy_img=img
for(i in 1:3){noisy_img[,,i]=noisy_img[,,i]+matrix(runif(512*512,0,0.1),nrow=512)}
# gives the error of larger than 1 value
noisy_img[noisy_img>1]=1

plot(c(0, 550), c(0, 550), type = "n", xlab = "", ylab = "")
rasterImage(noisy_img,0,0, 512, 512, interpolate = FALSE)


#CHANNELS OF THE NOISY IMAGE
noisy_img.R = noisy_img
noisy_img.G = noisy_img
noisy_img.B = noisy_img

noisy_img.R[,,2:3] = 0
noisy_img.G[,,1]=0
noisy_img.G[,,3]=0
noisy_img.B[,,1:2]=0

noisy_img1 = rasterGrob(noisy_img.R)
noisy_img2 = rasterGrob(noisy_img.G)
noisy_img3 = rasterGrob(noisy_img.B)
grid.arrange(noisy_img1, noisy_img2, noisy_img3, nrow=1)

# CONVERT THE IMAGE TO GRAYSCALE
gray_img= noisy_img[,,1]+noisy_img[,,2]+noisy_img[,,3]
gray_img = gray_img/max(gray_img)
plot(c(0, 550), c(0, 550), type = "n", xlab = "", ylab = "")
rasterImage(gray_img,0,0, 512, 512, interpolate = FALSE)

# RESIZING THE IMAGE
resize = function(img, new_width, new_height) {
  new_img = apply(img, 2, function(y){return (spline(y, n = new_height)$y)})
  new_img = t(apply(new_img, 1, function(y){return (spline(y, n = new_width)$y)}))
  new_img[new_img < 0] = 0
  new_img = round(new_img)
  return (new_img)
}

small_img = resize(gray_img,200,200)

plot(c(0, 200), c(0, 200), type = "n", xlab = "", ylab = "")
rasterImage(small_img,0,0, 200, 200, interpolate = FALSE)


# CONSTRUCT PATCHES
matrix<-matrix(0,40000,9)

for (i in 2:199) {
  for(j in 2:199) {
    matrix[198*(i-2)+j-1,1]<-small_img[i-1,j-1]
    matrix[198*(i-2)+j-1,2]<-small_img[i-1,j]
    matrix[198*(i-2)+j-1,3]<-small_img[i-1,j+1]
    matrix[198*(i-2)+j-1,4]<-small_img[i,j-1]
    matrix[198*(i-2)+j-1,5]<-small_img[i,j]
    matrix[198*(i-2)+j-1,6]<-small_img[i,j+1]
    matrix[198*(i-2)+j-1,7]<-small_img[i+1,j-1]
    matrix[198*(i-2)+j-1,8]<-small_img[i+1,j]
    matrix[198*(i-2)+j-1,9]<-small_img[i+1,j+1]
  }
}
data=matrix


#PCA
pca = princomp(data, cor=TRUE)
plot(pca)

plot(c(0, 200), c(0, 200), type = "n", xlab = "", ylab = "")
rasterImage(pca$scores[,1],0,0, 198, 198, interpolate = FALSE)


#PC1 Image
PC1 = pca$scores[,1]

PC1_img = matrix(0,198,198)

for(k in 1:198){
  for(l in 1:198){
    PC1_img[k,l]=PC1[198*(k-1)+l]
  }
} 
library(RSNNS)
PC1_img_n<-normalizeData(PC1_img,type="0_1")

plot(c(0, 200), c(0, 200), type = "n", xlab = "", ylab = "")
rasterImage(PC1_img_n,0,0, 198, 198, interpolate = FALSE)

#PC2 Image
PC2 = pca$scores[,2]

PC2_img = matrix(0,198,198)

for(k in 1:198){
  for(l in 1:198){
    PC2_img[k,l]=PC2[198*(k-1)+l]
  }
} 
library(RSNNS)
PC2_img_n<-normalizeData(PC2_img,type="0_1")

plot(c(0, 200), c(0, 200), type = "n", xlab = "", ylab = "")
rasterImage(PC2_img_n,0,0, 198, 198, interpolate = FALSE)


#PC3 Image
PC3 = pca$scores[,3]

PC3_img = matrix(0,198,198)

for(k in 1:198){
  for(l in 1:198){
    PC3_img[k,l]=PC3[198*(k-1)+l]
  }
} 
library(RSNNS)
PC3_img_n<-normalizeData(PC3_img,type="0_1")

plot(c(0, 200), c(0, 200), type = "n", xlab = "", ylab = "")
rasterImage(PC3_img_n,0,0, 198, 198, interpolate = FALSE)
# comp <- data.frame(pc$x[,1:9])
# 
# runPCA <- function(mat = 'Unadjusted matrix') eigen(cov(apply(mat, 2, function(i) i - mean(i))))
# pca <- runPCA(dat)
# 
# varExplained <- function(eigenList) {
#   
#   par(mfrow = c(1,2))
#   
#   plot(
#     eigenList$value / sum(eigenList$value), pch = 21, col = 'black',
#     bg = '#549cc4', ylim = c(0, 1), xlab = 'Principal Component',
#     ylab = 'Variance Explained'
#   ) + abline(h = 0.9)
#   
#   plot(
#     cumsum(eigenList$value) / sum(eigenList$value), pch = 21,
#     col = 'black', bg = '#549cc4', ylim = c(0, 1), xlab = 'Principal Component',
#     ylab = 'Cumulative Variance Explained'
#   ) + abline(h = 0.9)
# }
# 
# varExplained(pca)
# 
# 
# afterPCA <- function(
#   matAdjust = 'Centered matrix',
#   meanList = 'List of column means of original (unadjusted) matrix',
#   eigenList = 'List of eigenvalues and eigenvectors of adjust matrix covariance matrix',
#   n = 'selected PC\'s',
#   specific_select = 'If True: n == 1:n, if False: just n\'th columns') {
#   
#   if (length(n) > ncol(matAdjust)) stop('N is higher than the number of PC\'s')
#   if (!specific_select & length(n) > 1) stop('Use a single number when selecting up to n\'th PC')
#   if (!specific_select) n <- 1:n
#   
#   t(eigenList$vectors[,n] %*% (t(eigenList$vectors[,n]) %*% t(matAdjust))) + t(matrix(meanList, nrow = nrow(matAdjust), ncol = ncol(matAdjust)))
# }
# 
# reconstMatrix <- afterPCA(
#   matAdjust = apply(dat, 2, function(i) i - mean(i)),
#   meanList = apply(dat, 2, mean),
#   eigenList = pc,
#   n = 3,
#   specific_select = FALSE
# )