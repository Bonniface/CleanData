# Convert String to Integer
Transformer <-function(df,list_of_levels,plug_missing=TRUE)
{
  #loop through the columns
  for (i in 1: ncol(df))
  {
    
    #only   
    if (is.character(df[,i]) ||  is.factor(df[,i]) ){
      
      
      #deal with missing
      if(plug_missing){
        
        #if factor
        if (is.factor(df[,i])){
          df[,i] = factor(df[,i], levels=c(levels(df[,i]), 'MISSING'))
          df[,i][is.na(df[,i])] = 'MISSING' 
          
          
        }else{   #if character
          
          df[,i][is.na(df[,i])] = 'MISSING' 
          
        }
      }#end missing IF
      
      levels=list_of_levels[[colnames(df)[i]]]
      
      if (! is.null(levels)){
        df[,i] <- as.numeric(factor(df[,i], levels = levels))
      }
      
    }# character or factor
    
  }#end of loop
  
  return(df)
  
}#end of function