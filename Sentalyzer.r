###
# CKI
# Sentence analysis, comparisons, and more.	
# @author Imad Khoury - October 2013
# Sentalyzer.r
# 	All rights reserved - Copyright Imad Khoury 2013
#
###

Sentalyzer <- function (ss){

	#Note: in the code below never switch the append_ functions orders

  ss<-as.character(ss);
  
	if (nchar(ss)<=1){
		
		stop("Sentence is null. Cannot proceed!")
	}

	#Uses Wordnet:
	#Princeton University "About WordNet." WordNet. Princeton University. 2010. <http://wordnet.princeton.edu>

	require(tm)
	require(wordnet)
	require(RecordLinkage)
	require(qdap)
  require(openNLP)

	setDict("C:/Users/Imad/Documents/CKI/Saysome/My code/WordnetDict/dict")

  ##TODO: Aspell not working (spell-checking pre-processing) this is also slightly mitigated using the polarity lists
  #which inclusde some typos
	##aspell("I love bananas.", program="C:/Users/Imad/Documents/CKI/Saysome/My code/SpellDict(WIN)/Aspell/dict.")
  
  print(as.String(ss))
  comm("Chunking")
  ft2<-chunkify(ss)
  print(ft2)
  print(amalgamate_chunks(ft2$words))
  
  
  comm("Indexing terms")
	ft <- freq_table(ss)

  comm("Identifying lexical info")
##  	ftl<-append_lex_info(ft) ###to comment
	ftl<-ft
  comm("Identifying main concepts")
##  	ftl<-append_concept_from_stem(ftl) ###tocomment
	
  comm("Detecting negation")
  ftl<-append_negation(ftl)

  comm("Detecting emphasis")
  ftl<-append_emphasis(ftl)
  
  comm("Detecting polarity")
  ftl<-append_polarity(ftl)
	ftl<-append_net_polarity(ftl,ss)
  
  
  
	#qplot(Concept, Count, data=ftl, stat="summary", fun.y="sum")
	
  
  
#  su<-summarizer(ftl)
#print(su)  
#badger(su,ss);

  ftl

  #summary(ftl)
  
}


##########################################################################################
#Add more about amplifs: to amplify sentiment, and things inside parenthesis give less weight?), interjections, emoticons
#question types, Terms should also include expressions (n-grams)
#Try with http://sentiwordnet.isti.cnr.it/
#Reduce granularity
#Also: a spellchecker corrector pre-processor
#Also: add machine learning on previously trained data to consolidate and ameliorate snetimnt and other features
#check RTextTools and rforge-sentiment websites for more tools 