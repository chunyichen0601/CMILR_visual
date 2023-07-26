# setting
shape <- c(1,1,1, 1,1,1, 1,1,1)
PATTERN_Shape <- matrix(shape, 3, 3)
kernel_value <- c(3, 3)
image_size <- c(10, 10)

# function
Simulate_Image <- function(n, img_h = 10, img_w = 10,
                           pattern_shape = PATTERN_Shape, random_num = runif(1000)){
  Pat_h <- nrow(pattern_shape); Pat_w <- ncol(pattern_shape) 
  W <- array(runif((n*img_h*img_w), min = 0, max = 1), dim = c(n, img_h, img_w)) 
  Y_num <- rpois(n, 0.72)
  for (count in 1:n){
    if(Y_num[count]>0){
      Y_i <- sample(1:(img_h-Pat_h+1), Y_num[count])
      Y_j <- sample(1:(img_w-Pat_w+1), Y_num[count])
      for(k in 1:Y_num[count]){
        r_p <- PATTERN_Shape * sample(random_num, 1)
        W[count,,][Y_i[k]:(Y_i[k]+(Pat_h-1)), Y_j[k]:(Y_j[k]+(Pat_w-1))]<- r_p
      }
    }else{
      NULL
    }
  }
  return(list(image = W, label = Y_num))
}

# predict weight matrix
PWM <- function(img_s, k_s){
  pwm_m <- matrix(0, img_s[1], img_s[2])
  for (i in 1:img_s[1]) {
    for (j in 1:img_s[2]) {
      i_med <- img_s[1]%/%2; j_med <- img_s[2]%/%2 
      i_index <- ifelse(i > i_med, img_s[1]+1-i, i)
      j_index <- ifelse(j > j_med, img_s[2]+1-j, j)
      if (i_index >= k_s[1] && j_index >= k_s[2]) {
        pwm_m[i, j] <- k_s[1]*k_s[2]
      } else if (i_index %in% k_s[1]:i_med | j_index %in% k_s[2]:j_med) {
        pwm_m[i, j] <- min(i_index, j_index) * ifelse(i_index>j_index, k_s[1], k_s[2])
      } else {
        pwm_m[i, j] <- ifelse(i_index*j_index > k_s[1]*k_s[2], 
                              k_s[1]*k_s[2], i_index*j_index) 
      }
    }
  }
  return(pwm_m)
}

# Topographic map
Feature_Image <- function(X_data, img_size, ksize, coef){
  W <- matrix(0, img_size[1], img_size[2])
  row <- rep(1:(img_size[1]-ksize[1]+1),
             (img_size[2]-ksize[2]+1))
  col <- rep(1:(img_size[2]-ksize[2]+1), 
             each = (img_size[1]-ksize[1]+1))
  Q <- rbind(row, col)
  for(i in 1:dim(X_data)[1]){
    w_p <- matrix(0, img_size[1], img_size[2])
    link <- exp(c(1, X_data[i,]) %*% coef)
    response <- link/(1+link)
    w_p[Q[1,i]:(Q[1,i]+ksize[1]-1), Q[2,i]:(Q[2,i]+ksize[2]-1)] <- response
    W <- W + w_p
  }
  return(W)
}