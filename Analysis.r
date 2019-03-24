data<-read.table("Log.txt", header=T)
RDW<-(0.00039*data$Major+0.00015*data$Minor-0.02274)*0.83*1000
Wide<-c()
for (i in 1:139) {
    if (is.na(RDW[i])) {
        D<-NA
    } else {
        D<-1.5
        if (RDW[i]>1.6) {D<-D+0.1}
        if (RDW[i]>12.1) {D<-D+0.1}
        if (RDW[i]>14.3) {D<-D+0.1}
        if (RDW[i]>16.7) {D<-D+0.1}
        if (RDW[i]>19.5) {D<-D+0.1}
        if (RDW[i]>21.8) {D<-D+0.1}
        if (RDW[i]>23.1) {D<-D+0.1}
        D<-(D-1.5)*10+2
        }
    Wide<-c(Wide,D)
    
}
Wide[is.na(Wide)]<-0

data2<-cbind(data,Wide)


##ホワイトボード
pdf("Fig.pdf", width=5, height=9)
plot.new()
plot.window(xlim=c(0,50), ylim=c(0,100))
##色
ccol<-c("white", "darkgray", "darkgray", "cornflowerblue", "cornflowerblue", "yellow", "darkorange", "firebrick", "firebrick")
gsize<-1.5

##基礎の枝を描く
if (nrow(na.omit(subset(data,grepl("A", data$Image))))!=0) {
    lines(c(25,25), c(96,80))
    }
if (nrow(na.omit(subset(data,grepl("B", data$Image))))!=0) {
    lines(c(25,25), c(80, 77))
    lines(c(25,2), c(80, 80))
    }
if (nrow(na.omit(subset(data,grepl("C", data$Image))))!=0) {
    lines(c(25,25), c(77, 64))
    lines(c(25,48), c(77, 77))
    }
if (nrow(na.omit(subset(data,grepl("D", data$Image))))!=0) {
    lines(c(25,25), c(64, 61))
    lines(c(25,2), c(64, 64))
    }
if (nrow(na.omit(subset(data,grepl("E", data$Image))))!=0) {
    lines(c(25,25), c(61, 48))
    lines(c(25,48), c(61, 61))
    }
if (nrow(na.omit(subset(data,grepl("F", data$Image))))!=0) {
    lines(c(25,25), c(48, 45))
    lines(c(25,2), c(48, 48))
    }
if (nrow(na.omit(subset(data,grepl("G", data$Image))))!=0) {
    lines(c(25,25), c(45, 32))
    lines(c(25,48), c(45, 45))
    }
if (nrow(na.omit(subset(data,grepl("H", data$Image))))!=0) {
    lines(c(25,25), c(32, 29))
    lines(c(25,2), c(32, 32))
    }
if (nrow(na.omit(subset(data,grepl("I", data$Image))))!=0) {
    lines(c(25,25), c(29, 16))
    lines(c(25,42), c(29, 29))
    }
if (nrow(na.omit(subset(data,grepl("J", data$Image))))!=0) {
    lines(c(25,25), c(16, 10))
    lines(c(25,16), c(16, 16))
    }


##################################################
##籾を描く
GR<-subset(data2,data2$Image=="A-1-1.tif")
if (GR$Wide!=0){

points(25,96, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="A-1-2.tif")
if (GR$Wide!=0){
lines(c(25,23), c(94,94))
points(23,94, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="A-1-3.tif")
if (GR$Wide!=0){
lines(c(25,27), c(93,93))
points(27,93, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="A-1-4.tif")
if (GR$Wide!=0){
lines(c(25,23), c(92,92))
points(23,92, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="A-1-5.tif")
if (GR$Wide!=0){
lines(c(25,27), c(91,91))
points(27,91, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="A-1-6.tif")
if (GR$Wide!=0){
lines(c(25,23), c(90,90))
points(23,90, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="A-1-7.tif")
if (GR$Wide!=0){
lines(c(25,27), c(89,89))
points(27,89, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="A-2-1.tif")
if (GR$Wide!=0){
lines(c(25,18), c(87,87))
points(18,87, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="A-2-2.tif")
if (GR$Wide!=0){
lines(c(20,20), c(89,87))
points(20,89, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="A-2-3.tif")
if (GR$Wide!=0){
lines(c(21,21), c(85,87))
points(21,85, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="A-3-1.tif")
if (GR$Wide!=0){
lines(c(25,32), c(85,85))
points(32,85, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="A-3-2.tif")
if (GR$Wide!=0){
lines(c(30,30), c(87,85))
points(30,87, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="A-3-3.tif")
if (GR$Wide!=0){
lines(c(29,29), c(83,85))
points(29,83, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-1-1.tif")
if (GR$Wide!=0){

points(2,80, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-1-2.tif")
if (GR$Wide!=0){
lines(c(4,4), c(82,80))
points(4,82, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-1-3.tif")
if (GR$Wide!=0){
lines(c(5,5), c(78,80))
points(5,78, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-1-4.tif")
if (GR$Wide!=0){
lines(c(6,6), c(82,80))
points(6,82, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-1-5.tif")
if (GR$Wide!=0){
lines(c(7,7), c(78,80))
points(7,78, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-1-6.tif")
if (GR$Wide!=0){
lines(c(8,8), c(82,80))
points(8,82, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-2-1.tif")
if (GR$Wide!=0){
lines(c(12,12), c(73,80))
points(12,73, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-2-2.tif")
if (GR$Wide!=0){
lines(c(10,12), c(75,75))
points(10,75, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-2-3.tif")
if (GR$Wide!=0){
lines(c(14,12), c(76,76))
points(14,76, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-3-1.tif")
if (GR$Wide!=0){
lines(c(15,15), c(87,80))
points(15,87, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-3-2.tif")
if (GR$Wide!=0){
lines(c(13,15), c(85,85))
points(13,85, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-3-3.tif")
if (GR$Wide!=0){
lines(c(17,15), c(84,84))
points(17,84, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-4-1.tif")
if (GR$Wide!=0){
lines(c(18,18), c(73,80))
points(18,73, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-4-2.tif")
if (GR$Wide!=0){
lines(c(16,18), c(75,75))
points(16,75, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-4-3.tif")
if (GR$Wide!=0){
lines(c(20,18), c(76,76))
points(20,76, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="B-4-4.tif")
if (GR$Wide!=0){
lines(c(16,18), c(77,77))
points(16,77, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-1-1.tif")
if (GR$Wide!=0){

points(48,77, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-1-2.tif")
if (GR$Wide!=0){
lines(c(46,46), c(79,77))
points(46,79, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-1-3.tif")
if (GR$Wide!=0){
lines(c(45,45), c(75,77))
points(45,75, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-1-4.tif")
if (GR$Wide!=0){
lines(c(44,44), c(79,77))
points(44,79, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-1-5.tif")
if (GR$Wide!=0){
lines(c(43,43), c(75,77))
points(43,75, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-1-6.tif")
if (GR$Wide!=0){
lines(c(42,42), c(79,77))
points(42,79, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-1-7.tif")
if (GR$Wide!=0){
lines(c(41,41), c(75,77))
points(41,75, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-2-1.tif")
if (GR$Wide!=0){
lines(c(37,37), c(70,77))
points(37,70, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-2-2.tif")
if (GR$Wide!=0){
lines(c(36,37), c(72,72))
points(36,72, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-2-3.tif")
if (GR$Wide!=0){
lines(c(39,37), c(73,73))
points(39,73, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-3-1.tif")
if (GR$Wide!=0){
lines(c(34,34), c(84,77))
points(34,84, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-3-2.tif")
if (GR$Wide!=0){
lines(c(32,34), c(82,82))
points(32,82, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-3-3.tif")
if (GR$Wide!=0){
lines(c(36,34), c(81,81))
points(36,81, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-4-1.tif")
if (GR$Wide!=0){
lines(c(31,31), c(70,77))
points(31,70, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-4-2.tif")
if (GR$Wide!=0){
lines(c(29,31), c(72,72))
points(29,72, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-4-3.tif")
if (GR$Wide!=0){
lines(c(33,31), c(73,73))
points(33,73, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="C-4-4.tif")
if (GR$Wide!=0){
lines(c(29,31), c(74,74))
points(29,74, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-1-1.tif")
if (GR$Wide!=0){

points(2,64, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-1-2.tif")
if (GR$Wide!=0){
lines(c(4,4), c(66,64))
points(4,66, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-1-3.tif")
if (GR$Wide!=0){
lines(c(5,5), c(62,64))
points(5,62, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-1-4.tif")
if (GR$Wide!=0){
lines(c(6,6), c(66,64))
points(6,66, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-1-5.tif")
if (GR$Wide!=0){
lines(c(7,7), c(62,64))
points(7,62, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-1-6.tif")
if (GR$Wide!=0){
lines(c(8,8), c(66,64))
points(8,66, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-1-7.tif")
if (GR$Wide!=0){
lines(c(9,9), c(62,64))
points(9,62, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-2-1.tif")
if (GR$Wide!=0){
lines(c(13,13), c(57,64))
points(13,57, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-2-2.tif")
if (GR$Wide!=0){
lines(c(11,13), c(59,59))
points(11,59, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-2-3.tif")
if (GR$Wide!=0){
lines(c(15,13), c(60,60))
points(15,60, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-3-1.tif")
if (GR$Wide!=0){
lines(c(16,16), c(71,64))
points(16,71, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-3-2.tif")
if (GR$Wide!=0){
lines(c(14,16), c(69,69))
points(14,69, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-3-3.tif")
if (GR$Wide!=0){
lines(c(18,16), c(68,68))
points(18,68, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-4-1.tif")
if (GR$Wide!=0){
lines(c(19,19), c(57,64))
points(19,57, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-4-2.tif")
if (GR$Wide!=0){
lines(c(17,19), c(59,59))
points(17,59, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-4-3.tif")
if (GR$Wide!=0){
lines(c(21,19), c(60,60))
points(21,60, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="D-4-4.tif")
if (GR$Wide!=0){
lines(c(17,19), c(61,61))
points(17,61, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-1-1.tif")
if (GR$Wide!=0){

points(48,61, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-1-2.tif")
if (GR$Wide!=0){
lines(c(46,46), c(63,61))
points(46,63, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-1-3.tif")
if (GR$Wide!=0){
lines(c(45,45), c(59,61))
points(45,59, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-1-4.tif")
if (GR$Wide!=0){
lines(c(44,44), c(63,61))
points(44,63, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-1-5.tif")
if (GR$Wide!=0){
lines(c(43,43), c(59,61))
points(43,59, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-1-6.tif")
if (GR$Wide!=0){
lines(c(42,42), c(63,61))
points(42,63, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-1-7.tif")
if (GR$Wide!=0){
lines(c(41,41), c(59,61))
points(41,59, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-2-1.tif")
if (GR$Wide!=0){
lines(c(37,37), c(54,61))
points(37,54, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-2-2.tif")
if (GR$Wide!=0){
lines(c(35,37), c(56,56))
points(35,56, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-2-3.tif")
if (GR$Wide!=0){
lines(c(39,37), c(57,57))
points(39,57, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-3-1.tif")
if (GR$Wide!=0){
lines(c(34,34), c(68,61))
points(34,68, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-3-2.tif")
if (GR$Wide!=0){
lines(c(32,34), c(66,66))
points(32,66, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-3-3.tif")
if (GR$Wide!=0){
lines(c(36,34), c(65,65))
points(36,65, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-4-1.tif")
if (GR$Wide!=0){
lines(c(31,31), c(54,61))
points(31,54, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-4-2.tif")
if (GR$Wide!=0){
lines(c(29,31), c(56,56))
points(29,56, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="E-4-3.tif")
if (GR$Wide!=0){
lines(c(33,31), c(57,57))
points(33,57, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-1-1.tif")
if (GR$Wide!=0){

points(2,48, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-1-2.tif")
if (GR$Wide!=0){
lines(c(4,4), c(50,48))
points(4,50, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-1-3.tif")
if (GR$Wide!=0){
lines(c(5,5), c(46,48))
points(5,46, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-1-4.tif")
if (GR$Wide!=0){
lines(c(6,6), c(50,48))
points(6,50, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-1-5.tif")
if (GR$Wide!=0){
lines(c(7,7), c(46,48))
points(7,46, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-1-6.tif")
if (GR$Wide!=0){
lines(c(8,8), c(50,48))
points(8,50, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-2-1.tif")
if (GR$Wide!=0){
lines(c(13,13), c(41,48))
points(13,41, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-2-2.tif")
if (GR$Wide!=0){
lines(c(11,13), c(43,43))
points(11,43, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-2-3.tif")
if (GR$Wide!=0){
lines(c(15,13), c(44,44))
points(15,44, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-3-1.tif")
if (GR$Wide!=0){
lines(c(16,16), c(55,48))
points(16,55, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-3-2.tif")
if (GR$Wide!=0){
lines(c(14,16), c(53,53))
points(14,53, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-3-3.tif")
if (GR$Wide!=0){
lines(c(18,16), c(52,52))
points(18,52, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-4-1.tif")
if (GR$Wide!=0){
lines(c(19,19), c(41,48))
points(19,41, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-4-2.tif")
if (GR$Wide!=0){
lines(c(17,19), c(43,43))
points(17,43, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-4-3.tif")
if (GR$Wide!=0){
lines(c(21,19), c(44,44))
points(21,44, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="F-4-4.tif")
if (GR$Wide!=0){
lines(c(17,19), c(45,45))
points(17,45, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-1-1.tif")
if (GR$Wide!=0){

points(48,45, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-1-2.tif")
if (GR$Wide!=0){
lines(c(46,46), c(47,45))
points(46,47, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-1-3.tif")
if (GR$Wide!=0){
lines(c(45,45), c(43,45))
points(45,43, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-1-4.tif")
if (GR$Wide!=0){
lines(c(44,44), c(47,45))
points(44,47, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-1-5.tif")
if (GR$Wide!=0){
lines(c(43,43), c(43,45))
points(43,43, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-1-6.tif")
if (GR$Wide!=0){
lines(c(42,42), c(47,45))
points(42,47, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-1-7.tif")
if (GR$Wide!=0){
lines(c(41,41), c(43,45))
points(41,43, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-2-1.tif")
if (GR$Wide!=0){
lines(c(37,37), c(38,45))
points(37,38, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-2-2.tif")
if (GR$Wide!=0){
lines(c(35,37), c(40,40))
points(35,40, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-2-3.tif")
if (GR$Wide!=0){
lines(c(39,37), c(41,41))
points(39,41, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-3-1.tif")
if (GR$Wide!=0){
lines(c(34,34), c(52,45))
points(34,52, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-3-2.tif")
if (GR$Wide!=0){
lines(c(32,34), c(50,50))
points(32,50, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-3-3.tif")
if (GR$Wide!=0){
lines(c(36,34), c(49,49))
points(36,49, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-4-1.tif")
if (GR$Wide!=0){
lines(c(31,31), c(38,45))
points(31,38, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-4-2.tif")
if (GR$Wide!=0){
lines(c(29,31), c(40,40))
points(29,40, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="G-4-3.tif")
if (GR$Wide!=0){
lines(c(33,31), c(41,41))
points(33,41, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-1-1.tif")
if (GR$Wide!=0){

points(2,32, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-1-2.tif")
if (GR$Wide!=0){
lines(c(4,4), c(34,32))
points(4,34, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-1-3.tif")
if (GR$Wide!=0){
lines(c(5,5), c(30,32))
points(5,30, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-1-4.tif")
if (GR$Wide!=0){
lines(c(6,6), c(34,32))
points(6,34, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-1-5.tif")
if (GR$Wide!=0){
lines(c(7,7), c(30,32))
points(7,30, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-1-6.tif")
if (GR$Wide!=0){
lines(c(8,8), c(34,32))
points(8,34, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-2-1.tif")
if (GR$Wide!=0){
lines(c(13,13), c(25,32))
points(13,25, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-2-2.tif")
if (GR$Wide!=0){
lines(c(10,13), c(27,27))
points(10,27, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-2-3.tif")
if (GR$Wide!=0){
lines(c(15,13), c(28,28))
points(15,28, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-3-1.tif")
if (GR$Wide!=0){
lines(c(16,16), c(39,32))
points(16,39, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-3-2.tif")
if (GR$Wide!=0){
lines(c(14,16), c(37,37))
points(14,37, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-4-1.tif")
if (GR$Wide!=0){
lines(c(19,19), c(25,32))
points(19,25, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="H-4-2.tif")
if (GR$Wide!=0){
lines(c(21,19), c(27,27))
points(21,27, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="I-1-1.tif")
if (GR$Wide!=0){

points(42,29, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="I-1-2.tif")
if (GR$Wide!=0){
lines(c(40,40), c(31,29))
points(40,31, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="I-1-3.tif")
if (GR$Wide!=0){
lines(c(39,39), c(27,29))
points(39,27, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="I-1-4.tif")
if (GR$Wide!=0){
lines(c(38,38), c(31,29))
points(38,31, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="I-1-5.tif")
if (GR$Wide!=0){
lines(c(37,37), c(27,29))
points(37,27, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="I-1-6.tif")
if (GR$Wide!=0){
lines(c(36,36), c(31,29))
points(36,31, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="I-2-1.tif")
if (GR$Wide!=0){
lines(c(31,31), c(22,29))
points(31,22, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="I-2-2.tif")
if (GR$Wide!=0){
lines(c(29,31), c(24,24))
points(29,24, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="I-2-3.tif")
if (GR$Wide!=0){
lines(c(33,31), c(25,25))
points(33,25, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="J-1-1.tif")
if (GR$Wide!=0){

points(16,16, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="J-1-2.tif")
if (GR$Wide!=0){
lines(c(18,18), c(18,16))
points(18,18, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="J-1-3.tif")
if (GR$Wide!=0){
lines(c(19,19), c(14,16))
points(19,14, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="J-1-4.tif")
if (GR$Wide!=0){
lines(c(20,20), c(18,16))
points(20,18, pch=16, cex=gsize, col=ccol[GR$Wide])
}
GR<-subset(data2,data2$Image=="J-1-5.tif")
if (GR$Wide!=0){
lines(c(21,21), c(14,16))
points(21,14, pch=16, cex=gsize, col=ccol[GR$Wide])
}
dev.off()