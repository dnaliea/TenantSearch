library(beadarray)
install.packages("Mfuzz")
library(Mfuzz)
install.packages("clValid")
library(clValid)


mash.sig.cl<- mfuzz(df, c=4, m=0)

bitmap("mash_sigNEW.png")
mfuzz.plot(mash.sig.s, mash.sig.cl, mfrow=c(2,4), time.labels=c("t0","t4","t12","t24","t50"), new.window=F, min.mem=0.55)
dev.off()


df.clVal<-clValid(df, 
                         3:7, 
                         clMethods=c('fanny'),
                         diss=FALSE,
                         stand=TRUE, 
                         maxit=5000,
                         memb.exp = 1,
                         metric = c("euclidean"),
                         maxitems=3500,
                         validation='internal')

ngn15s_t0fanny.ByProbes<-fanny(ngn.sig15.s,7,
                               diss=FALSE,
                               stand=TRUE, 
                               maxit=2000,
                               memb.exp = 1.25,
                               metric = c("euclidean"))


mash13<-fanny(mash.sig1.5.s,13,
              diss=FALSE,
              stand=TRUE, 
              maxit=3000,
              memb.exp = 1.25,
              metric = c("euclidean"))

##order according to column 1=X=probeID
ngn15Fanny_sortedX_t0 = ngn15Fanny[do.call(order, ngn15Fanny[1]),]
#annotate with reMoAt-file

annot=read.csv("Annotation_Illumina_Mouse-WG-V2_mm9_V1.0.0_Aug09.txt", sep="\t", stringsAsFactors=F)
tmp<-annot[which(annot$Array_Address_Id_0 %in% ngn15Fanny_sortedX$X),]
#ProbeId in the expression set corresponds to Array_Address_Id_0 in the annotation file

ngn15Fanny_sortedX.annot=cbind(ngn15Fanny_sortedX, tmp)

uniqueGenes=tmp3[1]
ngnFannyByGenes=aggregate.data.frame(tmp3, by=uniqueGenes, mean)

###
goodprobes=read.csv('ngn15s_fanny.ByProbes_silhcutOFF005.csv')
tmp1=ngnFanny_all_sorted[which (ngnFanny_all_sorted$X %in% goodprobes_sorted$X),]
ngnFanny_allT0_sorted=ngnFanny_allT0[do.call(order, ngnFanny_allT0[1]),]
tmp2=ngnFanny_allT0_sorted[which (ngnFanny_allT0_sorted$X %in% goodprobes_sorted$X),]
tmp3=annot[which (annot$Array_Address_Id_0 %in% goodprobes_sorted$X),]


sort.tmp1=tmp1[do.call(order, tmp1[1]),]
sort.tmp2=tmp2[do.call(order, tmp2[1]),]
sort.tmp3=tmp3[do.call(order, tmp3[96]),]
ngn2Fanny_goodprobes=cbind(tmp1,tmp2,tmp3)
tmp1=ngn2Fanny_goodprobes[1:14]
tmp2=ngn2Fanny_goodprobes[75]
tmp3=ngn2Fanny_goodprobes[77]
ngnFannyGoodprobesGENE=cbind(tmp2,tmp1)
ngnFannyGoodprobesGENE_Sorted=ngnFannyGoodprobesGENE[do.call(order,ngnFannyGoodprobesGENE[1]),]
#ngnFannyGoodprobesENSEMBL=cbind(tmp3,tmp1)
uniqueGenes=ngnFannyGoodprobesGENE[1]
ngnFannyByGenes=aggregate.data.frame(ngnFannyGoodprobesGENE, by=uniqueGenes, mean)

ngnFannyByGenes_Sorted=ngnFannyByGenes[do.call(order,ngnFannyByGenes[1]),]
#ngnFannyByGenes_SortedMatrix=ngnFannyByGenes_Sorted[4:10]
ngnFannyByGenes_SortedMatrix=as.matrix(ngnFannyByGenes_Sorted[4:10])
#head(ngnFannyByGenes_SortedMatrix)

clustercalls=max.col(ngnFannyByGenes_SortedMatrix)
ngnFannyByGenes_Sorted_called=cbind(ngnFannyByGenes_Sorted, clustercalls)
head(ngnFannyByGenes_Sorted_called)


for (i in 1:7)
{tmp1=NgnFanny[i]$Gene_symbol,
 tmp2=NgnFanny[i][12:16],
 cbind(tmp1,tmp2)=ngnFannydata[i]
}