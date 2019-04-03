###
# CKI
# Sentence analysis, comparisons, and more.	
# @author Imad Khoury - October 2013
# freq_table.r
# 	All rights reserved - Copyright Imad Khoury 2013
#
###

freq_table <- function (ss, edit=FALSE, quiet=TRUE){

  
	ss<-unlist(stopwords(ss, strip=TRUE, unique=FALSE)) #char.keep can also keep special punctuation
	sss<- paste(ss, collapse=" ")

	d<-data.frame(Words = sss)
	corp <- Corpus(DataframeSource(d))

  	if(!quiet){
		print(corp)
	}
	
	dtm <- DocumentTermMatrix(corp)
	sdtm <- sort(colSums(as.matrix(dtm)), decreasing=T)
	tcf <- data.frame(sdtm)
	tcf <- data.frame(Term=rownames(tcf), Count=tcf$sdtm)


	#corp2<- tm_map(corp, stemDocument) # Stem words
	
	#dtm2 <- DocumentTermMatrix(corp2)
	#sdtm2 <- sort(colSums(as.matrix(dtm2)), decreasing=T)
	
	tcf <- data.frame(Stem=stemDocument(as.vector(tcf$Term)), tcf)
	
	

	if(edit){
		edit(tcf)
	}
	
	if(!quiet){
		print(tcf)
 	 }
	  else{
	    tcf;
	  }
	

}