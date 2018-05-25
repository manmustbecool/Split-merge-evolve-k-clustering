

# load one of the following datasets
# k for setting number of clusters

#-------------- dataset 1 - Artificial  ----------------
# 2 classes

set.seed(1984)
x1 <- sample(seq(0.1, 0.5, by=0.01), 50, replace = T)
x2 <- sample(seq(0.3, 0.5, by=0.01), 300, replace = T) # noise of x1
x3 <- sample(seq(0.8, 1.0, by=0.01), 300, replace = T)

xx <- c(x1, x2, x3)
yy <- runif(length(xx))
zz <- cbind(xx, yy)

trainingData <- zz[,c(1:2)]
evaluationData <- c( array(1, length(x1)+length(x2)), array(2, length(x3)))

k=2

# ----------  dataset 2 - Artificial   ---------------
# 3 classes


set.seed(1986)
x1 <- matrix(rnorm(600, mean=2, sd=0.5),nc=2)

x2 <- data.frame(rnorm(600, mean=3, sd=0.5), rnorm(600, mean=5, sd=1))
names(x2) <- NULL
x2 <- as.matrix(x2)

x3 <- data.frame(rnorm(600, mean=1, sd=0.5), rnorm(600, mean=5, sd=1))
names(x3) <- NULL
x3 <- as.matrix(x3)

zz <- rbind(x1, x2, x3)


trainingData <- zz[,c(1:2)]
evaluationData <- c(array(1,nrow(x1)), array(2,nrow(x2)), array(3,nrow(x3)))

k=3

#-------------- data set 3 - iris  -----------------------
# 3 classes

# training data
trainingData <- iris[,c(1:4)]
evaluationData <- iris[, 5]

evaluationData <- as.numeric(as.factor(evaluationData))

k <-3

#----------------------  +++++++   ---------------------------

table(evaluationData)


#------------------evaluation loop------------------------------------------------

all_myCluster_result <- vector()
all_kmean_result <- vector()
all_hc_result <- vector()

# number of run to take average result for evaluation
for(evaluation_loop in 1: 20){
  
  cat("+++++++++++++ ", evaluation_loop, "++++++++++++++\n")
  
  # -----------------Strart ML ----------------------------------------
  
  #---------- configuration ---------------
  
  # 1 is centroid based
  # 2 average 
  TECH <- 2
  # if initial with random or Kmeans
  begin_random <- T 
  # number of loop to exit
  total_loop <- 30
  
  
  plotSteps <- T
  plotToFile <- F
  plot_folder <- "C:\\Users\\emiewag\\Desktop\\R_workspace\\2017_my_cluster\\images1\\"
  
  plotLoopSleep <- 15 # in seconds
  
  
  #----------------------
  
  
  # normalize the training data
  trainingData[1:2,]
  trainingData <- scale(trainingData)
  trainingData[1:2,]
  
  
  plotColumns <- c(1,2)
  
  colMeans(trainingData) 
  
  plotData <- trainingData[, plotColumns]
  plot(plotData)
  

  
  if(!plotToFile){
    par(mfrow=c(2,2),
        mar=c(3,1,3,1), oma=c(0,0,0,0) # margin: buttom left, top, right
    )
  }
  
  
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
  
  
  plot_cluster <- function(plotData, my_cluster, my_centers, main_text){
    plot(plotData, col=my_cluster+1, main=main_text, xlab = "", ylab="", pch=my_cluster)
    # point center of two attributes of plotData
    
    print(my_centers)
    
    plot_my_centers <- my_centers[, plotColumns]
    points(plot_my_centers, pch=8, cex=2
           # , col=as.numeric(row.names(my_centers))+1
    )
    plot_my_centers[1,] <- plot_my_centers[1, ]*1.1
    plot_my_centers[2,] <- plot_my_centers[2, ]*0.90
    
    text(plot_my_centers, labels=row.names(plot_my_centers), cex= 2, col=as.numeric(row.names(my_centers))+1 )
    
    # clusplot(plotData, my_cluster, color = TRUE)
  }
  
  
  
  
  opt_evl_fun_centroid <- function(k, trainingData, my_cluster, getWorst=T){
    
    all_cluster_dis <- vector()
    all_point_dis <- vector()
    
    for(cluster_index in 1:k){
      
      temp <- trainingData[my_cluster==cluster_index, , drop=FALSE]
      
      cat(cluster_index, " : ")
      cat("cluster size :", nrow(temp), " ")
      
      cluster_dis <- 0
      for(temp_index in 1:nrow(temp)){
        # cat(temp_index, " ")
        current_dis <- euc.dist(temp[temp_index, ],  my_centers[cluster_index, ])
        cluster_dis <- cluster_dis + current_dis
        all_point_dis <- c(all_point_dis, current_dis)
        
      }
      cluster_dis <- cluster_dis/nrow(temp)
      cat("opt : " , cluster_dis, "\n")
      all_cluster_dis <- c(all_cluster_dis, cluster_dis)
      
    }
    
    
    opt_v <- mean(all_point_dis)
    
    
    # min_between_cluster <- min(dist(my_centers, method = "euclidean", diag = FALSE, upper = FALSE))
    
    # opt_v <- min_between_cluster/max(all_point_dis)*(-1)
    
    
    # dunn as optimization score
    # clust_stats <- cluster.stats(d = dist(trainingData), my_cluster) 
    # opt_v <- clust_stats$dunn*(-1) # min is better
    
    if(getWorst==F){
      # print("getWorst==F")
      return(c(opt_v, NULL))
    }
    
    
    
    # selected the max distance as the wrost
    
    names(all_cluster_dis) <- c(1:k)
    all_cluster_dis <- all_cluster_dis[order(all_cluster_dis, decreasing = T)]
    
    
    
    worst_index <- 1
    
    worst <- as.numeric(names(all_cluster_dis)[worst_index])
    temp <- trainingData[my_cluster==worst, , drop=F]
    worst_index <- 1 + worst_index 
    
    # can not split a cluster has one data point
    while(nrow(temp)<2){
      worst <- as.numeric(names(all_cluster_dis)[worst_index])
      temp <- trainingData[my_cluster==worst, , drop=F]
      worst_index <- 1 + worst_index 
      
      cat("nrow(temp) for the worst ", nrow(temp), "\n")
      
    }
    
    # optimization value, the worst cluster
    return(c(opt_v, worst))
    
  }
  
  
  
  
  
  library("fpc")
  opt_evl_fun_avg <- function(k, trainingData, my_cluster, getWorst=T){
    
    
    # dunn as optimization score
    clust_stats <- cluster.stats(d = dist(trainingData), my_cluster) 
    opt_v <- clust_stats$dunn*(-1) # min is better
    
    if(getWorst==F){
      return(c(opt_v, NULL))
    }
    
    # average distance as wrost selection
    all_cluster_dis <- clust_stats$average.distance
    
    names(all_cluster_dis) <- c(1:k)
    all_cluster_dis <- all_cluster_dis[order(all_cluster_dis, decreasing = T)]
    worst <- as.numeric(names(all_cluster_dis)[1])
    
    
    worst_index <- 1
    
    worst <- as.numeric(names(all_cluster_dis)[worst_index])
    temp <- trainingData[my_cluster==worst, , drop=F]
    
    
    # can not split a cluster has one data point
    while(nrow(temp)<2){
      
      worst_index <- 1 + worst_index 
      worst <- as.numeric(names(all_cluster_dis)[worst_index])
      temp <- trainingData[my_cluster==worst, , drop=F]
      
      # this should not happen since, the average.distance is zero for one point cluster
      cat("nrow(temp) for the worst ", nrow(temp), "\n")
      
    }
    
    
    return(c(opt_v, worst))
    
  }
  
  
  
  between_cluster_distance <- function(trainingData, my_cluster){
    
    distance_df <- data.frame()
    
    n = max(unique(my_cluster))
    
    for(cluster_index_a in 1:(n-1)){
      temp_a <- trainingData[my_cluster==cluster_index_a, , drop=FALSE]
      
      for(cluster_index_b in (cluster_index_a+1):n){
        
        totol_distance <- 0
        
        if(cluster_index_a != cluster_index_b){
          
          temp_b <- trainingData[my_cluster==cluster_index_b, , drop=FALSE]
          
          for(temp_a_index in 1:nrow(temp_a)){
            temp_a_o <- temp_a[temp_a_index, ]
            
            for(temp_b_index in 1:nrow(temp_b)){
              temp_b_o <- temp_b[temp_b_index, ]
              
              totol_distance <- totol_distance + euc.dist(temp_a_o, temp_b_o)
              
            }
          }
          
          totol_distance <- totol_distance / (nrow(temp_a)*nrow(temp_b))
          
          # cat(cluster_index_a, " to ", cluster_index_b, " = ", totol_distance, "\n")
          
          distance_df <- rbind(distance_df, c(cluster_index_a, cluster_index_b,  totol_distance))
          
        }# end if
        
        
      }
      
    }
    
    names(distance_df) <- c("c1", "c2", "d")
    
    x <- distance_df[distance_df$d==min(distance_df$d), , drop=F]
    x <- x[1,] # in case two rows have a same minimal 
    x <- as.numeric(x[,c(1,2)])
    x <- x[order(x, decreasing = T)]
    return(list(x, distance_df))
    
  }
  
  
  
  #-------------- kmean ------------------------
  
  
  final_cluster <- vector()
  
  model <- kmeans(trainingData, k)
  kmean_cluster <- model$cluster
  kmean_centers <- model$centers
  
  
  my_cluster <- model$cluster
  my_centers <- model$centers
  
  if(begin_random){
    
    my_cluster <- sample(k, nrow(trainingData), replace = T)
    
    for(k_index in 1:k){
      my_centers[k_index, ] <- colMeans(trainingData[my_cluster==k_index, ])
    }
    
  }
  
  
  final_cluster <- my_cluster
  
  main_text = "loop 0"
  if(plotToFile){
    png(filename=paste0(plot_folder, gsub(" ", "", main_text, fixed = TRUE), ".png"), width = 450, height = 340)
    plot_cluster(plotData, my_cluster, my_centers, "")
    dev.off()
  }else{
    plot_cluster(plotData, my_cluster, my_centers, main_text)
    Sys.sleep(plotLoopSleep)
  }
  
  
  
  splited <- 0
  merged <- k+1
  before_opt <- 1;
  after_opt <- 0
  
  final_opt <- 10000# max
  
  loop_info_list <- list()
  
  
  # ---------- start loop ----------------------
  
  # while(length(setdiff(splited, merged))!=0){
  
  #while( after_opt < before_opt ){
  
  for(loop in 1:total_loop){
    
    cat("loop: ", loop, "\n")
    
    loop_info <- c(loop)
    
    
    #--------------- split step ------------------
    opt_evl <- vector()
    if(TECH==1){
      opt_evl <- opt_evl_fun_centroid(k, trainingData, my_cluster)
    }else{
      opt_evl <- opt_evl_fun_avg(k, trainingData, my_cluster)
    }
    
    
    before_opt <- opt_evl[1]
    cat("before_opt: ", before_opt , "\n")
 
    
    worst <- vector()
    # | (table(my_cluster)[worst] <= (nrow(trainingData)/k)) 
    if(length(setdiff(splited, merged))==0 ){
      
      print("Adjusted split")
      
      # find one of top 3 largest cluster
      
      temp <- table(my_cluster)
      temp <- names(temp[order(temp, decreasing = T)])
      temp_worst <- as.numeric(temp)
      
      random <- 1
      if(k<=3){
        random <- sample(1:k ,1)
      }else{
        random <- sample(1:3, 1)
      }
      worst <- temp_worst[random]
      
      
      # data of the worst cluster
      temp <- trainingData[my_cluster==worst, , drop=F]
      
      # if the cluster only has one data point
      if(nrow(temp)==1){
        worst <- temp_worst[1]
        # data of the worst cluster
        temp <- trainingData[my_cluster==worst, , drop=F]
      }
      
      # worst <- sample(k, 1) # "random split"
      
      
      loop_info <- c(loop_info, "Adjusted split")
    }else{
      loop_info <- c(loop_info, "")
      
      worst <- opt_evl[2]
      # data of the worst cluster
      temp <- trainingData[my_cluster==worst, , drop=F]
    }
    
    splited <- c(worst, k+1)
    
    # split the wrost cluster
    main_text <- paste0("loop ", loop, " -- split ", worst, " as ", worst, " , ", k+1 )
    cat(main_text, "\n")
    
    loop_info <- c(loop_info, paste("split ", worst, " as ", worst, " , ", k+1))
    
    
    # give error when nrow(temp) ==2
    # Error: number of cluster centres must lie between 1 and nrow(x)
    temp_model_cluster <- vector()
    if(nrow(temp)==2){
      cat("2 points\n")
      temp_model_cluster<- c(1,2)
      # temp_model$centers <- temp
    }else{
      
      if(TECH==1){
        temp_model <- kmeans(temp, 2)
        temp_model_cluster <- temp_model$cluster
      }else{
        distance <- dist(temp, method="euclidean")
        temp_model <- hclust(distance, method="average")
        temp_model_cluster <- cutree(temp_model, 2)
      }
      
    }
    
    
    
    cat( "kmean split into two, the distribution:")
    print(table(temp_model_cluster))
    
    # update cluster
    temp_model_cluster[temp_model_cluster==2] <- k+1
    temp_model_cluster[temp_model_cluster==1] <- worst
    
    my_cluster[my_cluster==worst] <- temp_model_cluster
    
    cat( "after splited cluster, point distrbution: \n")
    print(table(my_cluster))
    
    # update centers
    
    # my_centers[worst,] <- temp_model$centers[1,]
    # my_centers <- rbind(my_centers, temp_model$centers[2,])
    # rownames(my_centers) <- 1:(k+1)
    
    my_centers<-  my_centers[0,]
    for(changed_index in 1:(k+1)){
      my_centers <- rbind(my_centers, colMeans(trainingData[my_cluster==changed_index, , drop=F]))
    }
    rownames(my_centers) <- 1:(k+1)
    
    
    if(plotSteps){
      if(plotToFile){
        png(filename=paste0(plot_folder, gsub(" ", "", main_text, fixed = TRUE), ".png"), width = 450, height = 340)
        plot_cluster(plotData, my_cluster, my_centers, "")
        dev.off()
      }else{
        plot_cluster(plotData, my_cluster, my_centers, main_text)
        Sys.sleep(plotLoopSleep)
      }
    }
    
    
    #------------------ merge step --------------------------
    
    merged <- vector()
    temp_cluster <- vector()
    distance_df <- vector()
    
    if(TECH==1){
      # find two closest clusters based on Centroid linkage
      distance <- dist(my_centers, method="euclidean")
      hc_model <- hclust(distance, method="average")
      temp_cluster <- cutree(hc_model, k)
      
      # print(temp_cluster)
      
      temp <- table(temp_cluster)
      temp <- as.numeric(names(temp[temp==2]))
      print(temp)
      
      temp <- names(temp_cluster[temp_cluster==temp])
      merged <- as.numeric(temp)
      
      
      library(reshape2)
      m <-  as.matrix(distance)
      distance_df <- melt(m)[melt(upper.tri(m))$value,]
      names(distance_df) <- c("c1", "c2", "d")
      
    }else{
      
      dis_list <- between_cluster_distance(trainingData, my_cluster)
      merged <- dis_list[[1]]
      print(merged)
      
      temp_cluster <- 1:(k+1)
      names(temp_cluster) <- 1:(k+1)
      temp_cluster[names(temp_cluster)==merged[1]] <- merged[2]
      temp_cluster[temp_cluster>merged[1]] <- temp_cluster[temp_cluster>merged[1]]-1
      
      distance_df <- dis_list[[2]]
    }
    
    
    # (nrow(trainingData)/(3*k))
    #|
    # if( min(table(my_cluster)) < 10  ){
    if(length(setdiff(splited, merged))==0 ){
      # merge the smallest cluster with other cluster
      
      loop_info <- c(loop_info, "adjusted merged: smallest")
      
      random <- 1
      if(k<=3){
        random <- sample(1:(k+1) ,1)
      }else{
        random <- sample(1:3, 1)
      }
      
      smallest_cluster <- order(table(my_cluster))[random]
      
      
      distance_df <- distance_df[distance_df$c1==smallest_cluster | distance_df$c2==smallest_cluster, ]
      x <- distance_df[distance_df$d==min(distance_df$d), , drop=F]
      x <- x[1,] # in case two rows have a same minimal 
      x <- as.numeric(x[,c(1,2)])
      merged <- x[order(x, decreasing = T)]
      cat("adjuststed merged: ", merged, "\n")
      
      temp_cluster <- 1:(k+1)
      names(temp_cluster) <- 1:(k+1)
      temp_cluster[names(temp_cluster)==merged[1]] <- merged[2]
      temp_cluster[temp_cluster>merged[1]] <- temp_cluster[temp_cluster>merged[1]]-1
      
      
      # }else if(length(setdiff(splited, merged))==0 ){
      #   
      #   merged <- sample(1:(k+1), 2)
      #   merged <- merged[order(merged, decreasing = T)]
      #   
      #   
      #   temp_cluster <- 1:(k+1)
      #   names(temp_cluster) <- 1:(k+1)
      #   temp_cluster[names(temp_cluster)==merged[1]] <- merged[2]
      #   temp_cluster[temp_cluster>merged[1]] <- temp_cluster[temp_cluster>merged[1]]-1
      #   
      #   loop_info <- c(loop_info, "adjusted random")
    }else{
      loop_info <- c(loop_info, "")
    }
    
    
    # merge two clusters
    main_text <- paste0("loop ", loop, " -- merge ", paste(merged, collapse = ", ") )
    cat(main_text, "\n")
    
    loop_info <- c(loop_info, paste0("merge ", paste(merged, collapse = ", ") ))
    
    # find changed cluster
    temp_cluster <- temp_cluster[temp_cluster!=names(temp_cluster)]
    cat( "changed cluster: \n")
    print(temp_cluster)
    
    
    # update cluster
    for(changed_index in 1:length(temp_cluster)){
      my_cluster[my_cluster==names(temp_cluster)[changed_index]] <- temp_cluster[changed_index]
    }
    cat( "after megered cluster: \n")
    print(table(my_cluster))
    
    # update centers
    # for(changed_index in 1:length(temp_cluster)){
    #   my_centers[changed_index, ] <- colMeans(trainingData[my_cluster==changed_index, , drop=F])
    # }
    # my_centers <- my_centers[-(k+1), ]
    
    my_centers<-  my_centers[0,]
    for(changed_index in 1:(k)){
      my_centers <- rbind(my_centers, colMeans(trainingData[my_cluster==changed_index, , drop=F]))
    }
    rownames(my_centers) <- 1:(k)
    
    
    cat( "centers size after merge:", nrow(my_centers), "\n")
    
    # # update centers
    # my_centers <- data.frame()
    # for(cluster_index in 1:k){
    #   temp <- trainingData[my_cluster==cluster_index, ]
    #   my_centers <- rbind(my_centers, colMeans(temp))
    # }
    
    
    #-------------------------------------------------
    
    # for(cluster_index in 1:(k+1)){
    #   
    #   temp <- trainingData[my_cluster==cluster_index,]
    #   
    #   cluster_dis <- 0
    #   for(temp_index in 1:nrow(temp)){
    #     cluster_dis <- cluster_dis + euc.dist(temp[temp_index, ],  my_centers[cluster_index, ])
    #   }
    #   cluster_dis <- cluster_dis/nrow(temp)
    #   cat(cluster_index," : " , cluster_dis, "\n")
    #   
    #   
    #   all_cluster_dis <- c(all_cluster_dis, cluster_dis)
    # }
    # 
    # names(all_cluster_dis) <- c(1:k)
    # all_cluster_dis <- all_cluster_dis[order(all_cluster_dis, decreasing = T)]
    # 
    # cat("merge average: ", mean(all_cluster_dis), "\n")
    # 
    # merged <- as.numeric(names(all_cluster_dis)[1:2])
    # merged <- order(merged)
    # 
    # # merge two clusters
    # main_text <- paste("merge ", paste(merged, collapse = ", ") , "\n\n")
    # cat(main_text)
    # 
    # # update cluster
    # my_cluster[my_cluster==merged[2]] <- merged[1]
    # my_cluster[my_cluster==(k+1)] <-  merged[2]
    
    #--------------------------------------------
    
    if(plotSteps){
      if(plotToFile){
        png(filename=paste0(plot_folder, gsub(" ", "", main_text, fixed = TRUE), ".png"), width = 450, height = 340)
        plot_cluster(plotData, my_cluster, my_centers, "")
        dev.off()
      }else{
        plot_cluster(plotData, my_cluster, my_centers, main_text)
        Sys.sleep(plotLoopSleep)
      }
    }
    
    #------------------ evlove step --------------------------
    
    opt_evl <- vector()
    if(TECH==1){
      opt_evl <- opt_evl_fun_centroid(k, trainingData, my_cluster, getWorst=F)
    }else{
      opt_evl <- opt_evl_fun_avg(k, trainingData, my_cluster, getWorst=F)
    }
    
    
    after_opt <- opt_evl[1]
    cat("after_opt: ", after_opt , "\n")
    
    
    
    # if(after_opt < before_opt){
    #   final_cluster <- my_cluster
    # }
    
    
    
    
    if(after_opt < final_opt){
      final_opt <- after_opt
      final_cluster <- my_cluster
      cat("*** great *** updated at loop:", loop, "\n")
      
      loop_info <- c(loop_info, "updated")
    }else{
      loop_info <- c(loop_info, "")
    }
    cat("final_opt: ", final_opt , "\n")
    cat(" --------------", loop,  "---------------\n\n")
    
    
    loop_info_list<- append(loop_info_list, list(loop_info))
    
  }
  
  
  loop_info_df <- as.data.frame(do.call(rbind, loop_info_list))
  
  
  
  # library("fpc")
  # clust_stats_my <- cluster.stats(d = dist(trainingData), as.numeric(evaluationData), final_cluster) 
  # clust_stats_kmean <- cluster.stats(d = dist(trainingData), as.numeric(evaluationData), kmean_cluster)
  # 
  # cat("Rand index: ", clust_stats_my$corrected.rand, "  ", clust_stats_kmean$corrected.rand, "\n")
  # cat("dunn index: ", clust_stats_my$dunn, "  ", clust_stats_kmean$dunn, "\n")
  # cat("entropy: ", clust_stats_my$entropy, "  ", clust_stats_kmean$entropy, "\n")
  # 
  
  #--------------------- end split merge evlove loop -------------------------------
  
  
  distance <- dist(trainingData, method="euclidean")
  hc_model <- hclust(distance, method="average")
  hc_cluster <- cutree(hc_model, k)
  
  
  
  final_centers<-  my_centers[0,]
  for(changed_index in 1:(k)){
    final_centers <- rbind(final_centers, colMeans(trainingData[final_cluster==changed_index, , drop=F]))
  }
  rownames(final_centers) <- 1:(k)
  
  
  if(plotToFile){
    png(filename=paste0(plot_folder, "final kmean", ".png"), width = 450, height = 340)
    plot_cluster(plotData, kmean_cluster, kmean_centers[, plotColumns], "")
    dev.off()
    
    png(filename=paste0(plot_folder, "final my cluster", ".png"), width = 450, height = 340)
    plot_cluster(plotData, final_cluster, final_centers[, plotColumns], "")
    dev.off()
    
    
  }
  
  plot_cluster(plotData, kmean_cluster, kmean_centers, "final Kmean cluster")
  plot_cluster(plotData, hc_cluster, final_centers[, plotColumns], "final hc cluster")
  plot_cluster(plotData, final_cluster, final_centers[, plotColumns], "final cluster")
  
  
  
  
  library(mclust)
  kmean_ari <- adjustedRandIndex(as.numeric(evaluationData), kmean_cluster)
  kmean_ari
  all_kmean_result <- c(all_kmean_result, kmean_ari)
  
  
  
  hc_ari <- adjustedRandIndex(as.numeric(evaluationData), hc_cluster)
  hc_ari
  all_hc_result <- c(all_hc_result, hc_ari)
  
  
  my_ari <- adjustedRandIndex(as.numeric(evaluationData), final_cluster)
  my_ari
  all_myCluster_result <- c(all_myCluster_result, my_ari)
  
  
} #end evaluation loop

mean(all_kmean_result)
mean(all_hc_result)
mean(all_myCluster_result)
