

fmt_data <- function(x){
    dplyr::mutate(data.frame(raw=x),
        response=ifelse(grepl('[+]',raw),1,0),
        text=gsub('[+-] ','',raw),
        f1=substring(text,1,1),
        f2=substring(text,2,2),
        f3=substring(text,3,3))
}

test <- fmt_data(readLines('Dataset/test.data'))
train <- fmt_data(readLines('Dataset/training.data'))

entropy <- function(x,response){
    i <- dplyr::ungroup(dplyr::summarise(dplyr::group_by_(x,response),n=n()))
    nv <- nrow(i)
    out <- 0
    if( nv > 1 ){
        tp <- dplyr::mutate(i,t=sum(n),p=n/t,e=-p*log(p,base=nv))
        out <- sum(tp$e)
    }
    return(out)
}

feature_entropy <- function(x,feature,r='response'){
    lcl_feature <- x[ , feature ]
    local <- split(x,f=lcl_feature)
    all_entropy <- lapply(local,function(y,my_r=r){
        entropy(y,my_r)
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
