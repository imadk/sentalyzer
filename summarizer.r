###
# CKI
# Sentence analysis, comparisons, and more.	
# @author Imad Khoury - October 2013
# summarizer.r
# 	All rights reserved - Copyright Imad Khoury 2013
#
###

summarizer <- function (abc){

  sumup<-NULL
  sumup$topterms<- NULL
  sumup$topconcepts<- NULL
  sumup$topnature<-NULL
  sumup$uwpos<- NULL
  style<-NULL
  mixture<-NULL
  sentiment<-NULL
  natu<-NULL
  maxi<-NULL
  num<-NULL
  quant<-NULL
  theme<-NULL
  ppp<-NULL
  nnn<-NULL
  hasPos<-NULL
  hasNeg<-NULL
  hasEmph<-FALSE
  
  sumup$topterms<- data.frame(Top_Terms=head(as.vector(abc$Term),n=4), Count=head(as.vector(abc$Count),n=4))
  #sumup$topconcepts<- data.frame(Top_Concepts=head(as.vector(abc$Concept),n=4), Count=head(as.vector(abc$Count),n=4))
  #sumup$topnature<- data.frame(Top_Nature=head(as.vector(abc$Nature),n=4), Count=head(as.vector(abc$Count),n=4))
  sumup$uwpos<- data.frame(Top_Unweighted_POS=rownames(head(as.matrix(sort(table(abc$POS), decreasing=T)), n=4)), How_many=head(as.matrix((sort(table(abc$POS), decreasing=T))), n=4))

  #add a new count column to add weight due to emphasis words:

  abc <-data.frame(abc, Weight = abc$Count)
  abc$Weight[which(abc$Net_Polarity=="positive+")] = abc$Weight[which(abc$Net_Polarity=="positive+")]*2 + abc$Weight[which(abc$Emphasis=="yes")]
  abc$Weight[which(abc$Net_Polarity=="negative+")] = abc$Weight[which(abc$Net_Polarity=="negative+")]*2 + abc$Weight[which(abc$Emphasis=="yes")]
  
  
  #remove the + polarities if they exist
  if("negative+" %in% levels(abc$Net_Polarity) || "positive+" %in% levels(abc$Net_Polarity)){
    abc$Net_Polarity[which(abc$Net_Polarity=="negative+")] = "negative"
    abc$Net_Polarity[which(abc$Net_Polarity=="positive+")] = "positive"
    
    #note that emphasis exists:
    hasEmph<-TRUE  
  }  
  
  #don't add weight on the actual emphasis word so it doesn't affect the total score
  abc$Weight[which(abc$Emphasis=="yes")] = 1  

  
  nnn<-sum(abc$Weight[which(as.vector(abc$Negation)=="yes")])/ sum(abc$Weight)
  ppp<-(sum(1*(abc$Weight[which(as.vector(abc$Net_Polarity)=="positive")])) + sum(-1*(abc$Weight[which(as.vector(abc$Net_Polarity)=="negative")]))) / sum(abc$Weight)
  
  if(sum(1*(abc$Weight[which(as.vector(abc$Net_Polarity)=="positive")]))==0 && sum(-1*(abc$Weight[which(as.vector(abc$Net_Polarity)=="negative")]))==0){
    
    qqq<-0
  
  }else{
    
    qqq<-(sum(1*(abc$Weight[which(as.vector(abc$Net_Polarity)=="positive")])) + sum(-1*(abc$Weight[which(as.vector(abc$Net_Polarity)=="negative")]))) / (sum(1*(abc$Weight[which(as.vector(abc$Net_Polarity)=="positive")])) + sum(+1*(abc$Weight[which(as.vector(abc$Net_Polarity)=="negative")])))
    
  }    
  hasPos<-(sum(abc$Weight[which(as.vector(abc$Net_Polarity)=="positive")]))  
  hasNeg<-(sum(abc$Weight[which(as.vector(abc$Net_Polarity)=="negative")]))
  
  #linearly combine ref p sentiment (ppp) and p sentimnet (qqq) and make between -2 and 2
  ooo<-1.5*qqq + 0.5*ppp
  ooo<-round(ooo,2)
  
  
  sumup$overallsent<- data.frame(Overall_Weighted_Sent=c("Negation","Sentiment"), Score=c(nnn,ooo))
  
   natu<-trim(unlist(strsplit(as.vector(summary(abc)[,5]),":")));
    num<-suppressWarnings(as.numeric(natu));
    maxi<-max(num[which(!is.na(num))]);
    theme<-natu[which(natu==as.character(maxi))-1];
  
  theme<- gsub("\\<all\\>","TBD", theme)

  
  style<-paste(as.vector(head(sumup$uwpos$Top_Unweighted_POS, n=2)), collapse=" and ");
  
  style<-gsub("adj","adjectives",style)
  style<-gsub("verb","verbs",style)
  style<-gsub("adv","adverbs",style)
  style<-gsub("noun","nouns",style)
  
  
  if(as.vector(sumup$overallsent$Score[1])>0){
    style<-paste(style, " with presence of a negation(s)", sep="");
  }else{
    style<-paste(style, " without any negationing", sep="");
  }
  
  if(hasEmph){
    
    style<-paste(style, " and with amplification(s)", sep="");
  }else{
    style<-paste(style, " and without any amplification", sep="");    
  }
  
  
  if(hasNeg > 0 && hasPos >0){
    mixture<- "Sentiments are mixed but "    
    
  }
  if(hasNeg==0 || hasPos==0){
    mixture<- "There is no mixture of sentiments and "        
    
  }
  if(hasNeg == 0 && hasPos ==0){
    mixture<- "There are no sentiments expressed and so "    
    
  }  

  
  if(as.vector(sumup$overallsent$Score[2])>0){
    sentiment<-"positive";
  }
  if(as.vector(sumup$overallsent$Score[2])<0){
    sentiment<-"negative";
  }
  if(as.vector(sumup$overallsent$Score[2])==0){
    sentiment<-" neutral";
  }
  
  quant<-" "
  if(abs(as.vector(sumup$overallsent$Score[2]))>0 && abs(as.vector(sumup$overallsent$Score[2]))<=0.1){
    quant<-" ";
  }
  if(abs(as.vector(sumup$overallsent$Score[2]))>0.1 && abs(as.vector(sumup$overallsent$Score[2]))<=0.2){
    quant<-" ";
  }
  if(abs(as.vector(sumup$overallsent$Score[2]))>0.2 && abs(as.vector(sumup$overallsent$Score[2]))<=0.5){
    quant<-" ";
  }
  if(abs(as.vector(sumup$overallsent$Score[2]))>0.5 && abs(as.vector(sumup$overallsent$Score[2]))<=0.7){
    quant<-" ";
  }
  if(abs(as.vector(sumup$overallsent$Score[2]))>0.7 && abs(as.vector(sumup$overallsent$Score[2]))<=0.8){
    quant<-" ";
  }
  if(abs(as.vector(sumup$overallsent$Score[2]))>0.8 && abs(as.vector(sumup$overallsent$Score[2])<=1)){
    quant<-" ";
  }
  
  #maxi=1 means no dominance in theme (that is if more than one theme is present)
  if(maxi>1 || length(which(!is.na(num)))==1){
    
    sumup$special <- paste("This is mostly about ", paste(as.vector(sumup$topconcepts$Top_Concepts), collapse="_, "), "_, more specifically about ",  paste(as.vector(sumup$topterms$Top_Terms), collapse="*, "), "*. The dominant theme is ",  paste(as.vector(theme), collapse=" / "),". The author has used a style mostly based on ",  style, ". ", mixture ,"the overall sentiment involved is", quant,"",sentiment,sep="");  
  
  }else{
    
    sumup$special <- paste("This is mostly about ", paste(as.vector(sumup$topconcepts$Top_Concepts), collapse="_, "), "_, more specifically about ",  paste(as.vector(sumup$topterms$Top_Terms), collapse="*, "), "*. There is no dominant theme: themes could be any of [",  paste(as.vector(theme), collapse=", "),"]. The author has used a style mostly based on ", style,". ", mixture ,"the overall sentiment involved is", quant,"",sentiment,sep="");  
    
  }
  
  
  sumup
  
  
  
}