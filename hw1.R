
hd <- require(dplyr)
if( !hd ){
    install.packages('dplyr')
}

fmt_data <- function(x){
    # 1. Is their first name longer than their last name?
    # 2. Do they have a middle name?
    # 3. Does their first name start and end with the same letter? (ie "Ada")
    # 4. Does their first name come alphabetically before their last name? (ie "Dan Klein" because "d" comes before "k")
    # 5. Is the second letter of their first name a vowel (a,e,i,o,u)?
    # 6. Is the number of letters in their last name even?
    out <- dplyr::mutate(data.frame(raw=x),
        response=ifelse(grepl('[+]',raw),1,0),
        text=gsub('[+-] ','',raw),
        f1=1,
        f2=1,
        f3=1,
        f4=1,
        f5=ifelse(tolower(substring(text,2,2)) %in% c('a','e','i','o','u'),1,0),
        f6=1,
        j1=1,
        j2=1
    )
    nm <- dplyr::bind_rows(lapply(strsplit(out$text,' '),function(x){ dplyr::mutate(data.frame(first=x[1],last=x[length(x)]),ft1=ifelse(nchar(as.character(first)) > nchar(as.character(last)),1,0)) }))
    out$f1 <- nm$ft1
    out$f2 <- ifelse(sapply(gregexpr(' ',out$text),length) > 1,1,0)
    out$f3 <- ifelse(sapply(nm$first,function(y){tolower(substring(y,1,1)) == tolower(substring(y,nchar(y),nchar(y)))}),1,0)
    out$f4 <- ifelse(tolower(substring(nm$first,1,1)) < tolower(substring(nm$last,1,1)),1,0)
    out$f6 <- ifelse(nchar(nm$last) %% 2 == 0,1,0)
    out$j1 <- sapply(gregexpr(' ',out$text),length) + 1
    out$j2 <- nm$first
    return(out)
}

test <- fmt_data(readLines('Updated_Dataset/updated_test.txt'))
train <- fmt_data(readLines('Updated_Dataset/updated_train.txt'))

xv_in_files <- list('updated_training00.txt','updated_training01.txt','updated_training02.txt','updated_training03.txt')
xv_in <- lapply(xv_in_files,function(x){ fmt_data(readLines(paste0('Updated_Dataset/Updated_CVSplits/',x))) })

# test <- fmt_data(readLines('Dataset/test.data'))
# train <- fmt_data(readLines('Dataset/training.data'))

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

binary_node <- function(x,features,r='response',sf='root',lvl=0,lvl_max=3){
    #make sure everything runs correctly
    my_lvl_max <- ifelse(lvl_max > length(features) & sf == 'root',length(features),lvl_max)
    # my_lvl_max <- lvl_max

    my_sv <- ifelse(sf == 'root','root',unique(x[,sf]))
    out <- list(lvl=lvl,split_feature=sf,split_value=my_sv,avg_rate=mean(x[,r]))

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
        lcl_split <- split(x,f=x[ , split_feature ])
        out$subtree <- lapply(lcl_split,binary_node,features=remaining_features,r=r,lvl=lvl+1,lvl_max=my_lvl_max,sf=split_feature)
    }
    return(out)
}

classify_node <- function(node,x){
    is_root <- ( node$split_feature == 'root' )
    out <- NULL
    if( is_root ){
        out <- unlist(lapply(node$subtree,classify_node,x=x))
        if( is.null(unlist(out)) ){
            out <- node$avg_rate
        }
    }else{
        if( !is.null(node$subtree) & x[ , node$split_feature ] == node$split_value ){
            out <- lapply(node$subtree,classify_node,x=x)
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

cross_validate <- function(xv_data,f,h){
    n_fold <- length(xv_data)
    out <- vector(n_fold,mode='list')
    for( i in 1:n_fold ){
        ct <- binary_node(dplyr::bind_rows(xv_data[-i]),f,lvl_max=h)
        test <- classify_ds(xv_data[[i]],ct)
        out[[i]] <- data.frame(h=h,fold=i,accuracy=cor(test$response,test$predict))
    }
    return(dplyr::bind_rows(out))
}

f <- list('f1','f2','f3','f4','f5','f6')

ct <- binary_node(train,f,lvl_max=length(f))
ctr <- classify_ds(train,ct)
ctt <- classify_ds(test,ct)
cat('Correlation with training data',cor(ctr$response,ctr$predict),fill=TRUE)
cat('Correlation with test data',cor(ctt$response,ctt$predict),fill=TRUE)

tt <- dplyr::bind_rows(lapply(list(1,2,3,4,5,6,7,20),function(x){ cross_validate(xv_in,f,h=x) }))
print(final_results <- dplyr::summarise(dplyr::group_by(tt,h),mean_accuracy=mean(accuracy),sd_accuracy=sd(accuracy)))
# ggplot(final_results) +
#     geom_line(aes(x=h,y=mean_accuracy)) +
#     labs(x='Tree Depth',y='Mean Accuracy',title='Cross-validation Test') +
#     scale_y_continuous(labels=scales::percent) +
#     theme_ipsum()

ft <- binary_node(train,f,lvl_max=6)
fcast_tree <- classify_ds(test,ft)
cat('Correlation with test data',cor(fcast_tree$response,fcast_tree$predict),fill=TRUE)
