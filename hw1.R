

fmt_data <- function(x){
    dplyr::mutate(data.frame(raw=x),response=ifelse(grepl('[+]',raw),1,0),text=gsub('[+-] ','',raw),f1=substring(text,1,1),f2=substring(text,2,2))
}

test <- fmt_data(readLines('Dataset/test.data'))
train <- fmt_data(readLines('Dataset/training.data'))

entropy <- function(x){
    lcl_entropy <- -mean(x)*log(mean(x),base=2) - mean(1-x)*log(mean(1-x),base=2)
    if( mean(x) == 1 | mean(x) == 0 ){
        lcl_entropy <- 0
    }
    return(lcl_entropy)
}

feature_entropy <- function(x,feature){
    lcl_feature <- x[ , feature ]
    local <- split(x,f=lcl_feature)
    all_entropy <- lapply(local,function(y){
        entropy(y$response)
    })
    return(all_entropy)
}

expected_entropy <- function(x,feature){
    lcl_entropy <- unlist(feature_entropy(x,feature))
    lcl_feature <- x[ ,feature ]
    prop_split <- split(x,f=lcl_feature)
    prop <- unlist(lapply(prop_split,nrow)) / length(lcl_feature)
    out <- sum(lcl_entropy * prop)
    return(out)
}
