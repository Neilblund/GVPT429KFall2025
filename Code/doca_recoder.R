docaRecoder<-function(doca){
  required_packages<-c("dplyr", "haven", "labelled", "jsonlite", "dataverse")
  for(p in required_packages){
    if(!require(p,character.only = TRUE)) install.packages(p)
    
  }
  
  
  speclist<-jsonlite::fromJSON('https://raw.githubusercontent.com/Neilblund/GVPT429KFall2025/refs/heads/main/Code/codebook.json')
  
  labels<-speclist$labels
  # function to apply labels to variables
  codebook<-function(var, labeler){
    vals<-labeler$code
    names(vals)<-labeler$label
    labelled::val_labels(var) <-vals
    
    return(var)
  }
  
  
  
  
  # applying labels to columns: 
  for(i in 1:nrow(labels)){
    labs<-labels[i, ]
    col<-labs$`variable name`
    if(!col %in% colnames(doca)){
      next
    }
    var_label(doca[[col]]) <- labs$`variable title`
    if(!is.na(labs$Rformat)){
      if(class(doca[[col]]) == "factor"){
        next
      }
      doca[[col]] <- codebook(doca[[col]], speclist[[labs$Rformat]])
    }
    
    
  }
  
  
  
  
  # adding general claim labels
  doca<-doca|>
    dplyr::mutate(dplyr::across(matches('^claim[0-9]'), 
                                .fns=~codebook(.x, speclist$general_claims), 
                                .names='gen_{.col}'))
  
  
  
  # converting to R factors
  doca<-haven::as_factor(doca)
  
  
  return(doca)
}



doca$form_grouping<-factor(as.numeric(doca$form1), levels=group_levels, labels=group_labels)


group_levels<-c(1, 2, 5, 4, 13, 14, 8, 16, 9, 15, 18)


group_labels<- c(
  "Rallies,Demonstrations",
  "Rallies,Demonstrations",
  "Withholding obligations",
  "Withholding obligations",
  "Withholding obligations",
  "Withholding obligations",
  "Conventional",
  "Conventional",
  "Conventional",
  "Conventional",
  "Conventional"
)


doca_selected<-doca|>
  drop_na(form_grouping, smonamed)


doca_selected|>
  count(smonamed, form_grouping)|>
  group_by(smonamed)|>
  mutate(total = sum(n),
         proportion =  n/total)
         
         





