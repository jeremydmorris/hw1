

fmt_data <- function(x){
    dplyr::mutate(data.frame(raw=x),
        response=ifelse(grepl('[+]',raw),1,0),
        text=gsub('[+-] ','',raw),
        f1=substring(text,1,1),
        f2=substring(text,2,2),
        f3=substring(text,3,3),
        f4=substring(text,4,5))
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
    all_entropy <- lapply(local,entropy,r)
    return(all_entropy)
}

expected_entropy <- function(feature,x){
    lcl_entropy <- unlist(feature_entropy(x,feature))
    lcl_feature <- x[ ,feature ]
    prop_split <- split(x,f=lcl_feature)
    prop <- unlist(lapply(prop_split,nrow)) / length(lcl_feature)
    out <- sum(lcl_entropy * prop)
    return(out)
}

binary_node <- function(x,features,r='response',sf='root',lvl=0,lvl_max=3,verbose=FALSE){
    #make sure everything runs correctly
    my_lvl_max <- ifelse(lvl_max > length(features) & sf == 'root',length(features),lvl_max)

    my_sv <- ifelse(sf == 'root','root',unique(x[,sf]))
    out <- list(lvl=lvl,split_feature=sf,split_value=my_sv)
    if( verbose ) cat(sf,my_sv,unlist(features),lvl,fill=T)

    #need to check if no remaining splits
    n_rem_split <- unique(as.data.frame(x[ , unlist(features) ]))

    if( length(unique(x[ , r ])) == 1 | lvl == lvl_max | nrow(n_rem_split) == 1 ){
        out$predict <- mean(x[ , r ])
    }else{
        info_gain <- entropy(x,r) - unlist(lapply(features,expected_entropy,x))
        to_split <- which(info_gain == max(info_gain))
        remaining_split <- (info_gain != max(info_gain))
        if( length(to_split) > 1 ){
            to_split <- sample(which(info_gain == max(info_gain)),1)
            remaining_split <- (seq_along(info_gain) != to_split)
        }
        split_feature <- features[[ to_split ]]
        remaining_features <- features[ remaining_split ]
        if( verbose ) cat('\t',split_feature,lvl,fill=TRUE)
        lcl_split <- split(x,f=x[ , split_feature ])
        out$subtree <- lapply(lcl_split,binary_node,features=remaining_features,r=r,lvl=lvl+1,lvl_max=my_lvl_max,sf=split_feature,verbose=verbose)
    }
    return(out)
}

classify_node <- function(node,x){
    is_root <- ( node$split_feature == 'root' )
    out <- NULL
    if( is_root ){
        out <- unlist(lapply(node$subtree,classify_node,x=x))
        # out <- lapply(node$subtree,classify_node,x=x)
    }else{
        if( !is.null(node$subtree) & x[ , node$split_feature ] == node$split_value ){
            out <- lapply(node$subtree,classify_node,x=x)
            if( is.null(unlist(out)) ){
                #if we didn't get an answer here, avg up all the predicted values at this level
                out <- mean(unlist(lapply(node$subtree,function(y){ y$predict })))
            }
        }
        if( is.null(node$subtree) & x[ , node$split_feature ] == node$split_value ){
            out <- node$predict
        }
    }
    return(out)
}

classify_ds <- function(x,tree){
    to_cl <- split(x,f=seq_along(x[,1]))
    cl <- lapply(to_cl,function(y,t=tree){
        my_cl <- classify_node(t,y)
        out <- y
        out$predict <- my_cl
        return(out)
    })
    return(dplyr::bind_rows(cl))
}

test_run <- function(){
    f <- list('f1','f2','f3','f4')
    for( i in 1:4 ){
        ct <- binary_node(train,f,lvl_max=i,verbose=F)
        test_in <- classify_ds(train,ct)
        cat(cor(test_in$response,test_in$predict),fill=T)
    }
}
