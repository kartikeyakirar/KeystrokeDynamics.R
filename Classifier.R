OutlierCountTrain <- function( Y, outalpha=.05 ) {
  means <- colMeans( Y );
  sds <- apply( Y, 2, sd );
  obj <- list( type = 'OutlierCount',
               means = means,
               sds = sds,
               outalpha = outalpha );
  return( obj );
}

OutlierCountScore <- function( obj, Y ) {
  Yscale <- scale( Y, center=obj$means, scale=obj$sds );
  outcount <- rowSums( abs( Yscale ) > qnorm( 1-obj$outalpha/2 ) );
  return( outcount );
}

##################################################################################
KMeansTrain <- function( Y, k=3, iter.max=20, nstart=10,mindist=1e-10 ) {
  means <- colMeans( Y );
  sds <- apply( Y, 2, sd );
  Yscale <- scale( Y, center=means, scale=sds );
  km <- kmeans( Yscale, centers=k, iter.max=iter.max, nstart=nstart );
  obj <- list( type = 'kMeans',
               means = means,
               sds = sds,
               km = km,
               k = k,
               mindist = mindist );
    return( obj );
}


KMeansScore <- function( obj, Y ) {
    Yscale <- scale( Y, center=obj$means, scale=obj$sds );
  k <- obj$k;
  km <- obj$km;
  d <- ncol(Y);
  n <- nrow(Y);
  
  Ydists <- function( cs ) {
    Yctr <- sweep( Yscale, 2, cs );
    return( sqrt( rowSums( abs( Yctr )^2 ) ) );
  }
  
  D <- sapply( 1:k, function(i) Ydists( km$centers[i,] ) );
  D <- matrix( D, nrow=n, ncol=k );
  idxs <- apply( D, 1, which.min );
  cdist <- D[ cbind( 1:n , idxs ) ];
  cdist <- pmax( cdist, obj$mindist );
  return( cdist );
}
##################################################################################


euclideanTrain <- function( YTrain ) {
  dmod <- list ( mean  = colMeans( YTrain ) );
  return( dmod );
}

euclideanScore <- function( dmod, YScore ) {
  p <- length( dmod$mean );
  n <- nrow( YScore );
  if( ncol(YScore) != p ) stop("Training/test feature length mismatch ");
    meanMatrix <- matrix( dmod$mean, byrow=TRUE, nrow=n, ncol=p );
    scores <- rowSums( ( YScore - meanMatrix )^2 );
    return( scores );
}
#################################################################################
manhattanTrain <- function( YTrain ) {
  dmod <- list ( mean  = colMeans( YTrain ) );
  return( dmod );
}

manhattanScore <- function( dmod, YScore ) {
  p <- length( dmod$mean );
  n <- nrow( YScore );
  if( ncol(YScore) != p ) stop("Training/test feature length mismatch ");
  meanMatrix <- matrix( dmod$mean, byrow=TRUE, nrow=n, ncol=p );
  scores <- rowSums( abs( YScore - meanMatrix ) );
  return( scores );
}

################################################################################
mahalanobisTrain <- function( YTrain ) {
  dmod <- list( mean  = colMeans( YTrain ),
                covInv = ginv( cov( YTrain ) ) );
  return( dmod );
}

mahalanobisScore <- function( dmod, YScore ) {
  p <- length( dmod$mean );
  n <- nrow( YScore );
  if( ncol(YScore) != p ) stop("Training/test feature length mismatch ");
  scores <- mahalanobis( YScore, dmod$mean, dmod$covInv, inverted=TRUE );
  return( scores );
}

################################################################################
SVMTrain <- function( Y ) {
  svm <- ksvm( Y, type = 'one-svc', nu=.05 );
  obj <- list( type = 'SVM',
               svm = svm );
  return( obj );
}

SVMScore <- function( obj, Y ) {
  thescores <- -predict( obj$svm, Y, type='decision' );
  return( thescores );
}

###############################################################################
KNNTrain <- function( Y, k=1 ) {
  stopifnot( k == 1 );
  means <- colMeans( Y );
  sds <- apply( Y, 2, sd );
  Yscaled <- scale( Y, center=means, scale=sds );
  obj <- list( type = 'KNN',
               means = means,
               sds = sds,
               k = k,
               Yscaled = Yscaled );
  
  return( obj );
}


KNNScore <- function( obj, Y ) {
  stopifnot( obj$k == 1 );
  Yscaled <- scale( Y, center=obj$means, scale=obj$sds );
  nns <- nnfind( from=obj$Yscaled, to=Yscaled );
  thescores <- nns$dist;
  return( thescores );
}

#####################################################################
ScaledManhattanTrain <- function( Y ) {
obj <- list( type = 'ScaledManhattan',
               means = colMeans(Y),
               sds   = apply( Y, 2, sd ) );
  return( obj );
}

ScaledManhattanScore <- function( obj, Y ) {
  
  Y <- scale( Y, center=obj$means, scale=obj$sds );
  return( rowSums( abs(Y)));
  }
################################################################################
MahalanobisKNNTrain <- function( Y, k=1 ) {
  stopifnot( k == 1 );
  means <- colMeans( Y );
  eig <- eigen( cov( sweep( Y, 2, means ) ) );
  E <- eig$vectors;
  D <- diag( eig$values );
  Ysphere <- Y %*% E %*% sqrt( ginv( D ) ) %*% t(E);
  obj <- list( type = 'MahalanobisKNN',
               means = means,
               eig = eig,
               k = k,
               Ysphere = Ysphere );
  
  return( obj );
}

MahalanobisKNNScore <- function( obj, Y ) {
  stopifnot( obj$k == 1 );
  E <- obj$eig$vectors;
  D <- diag( obj$eig$values );
  Ysphere <- Y %*% E %*% sqrt( ginv( D ) ) %*% t(E);
  nns <- nnfind( from=obj$Ysphere, to=Ysphere );
  thescores <- nns$dist;
  return( thescores );
}

