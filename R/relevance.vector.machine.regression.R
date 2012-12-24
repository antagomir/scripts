library(kernlab)
    # create data
     x <- seq(-20,20,0.1)
     y <- sin(x)/x + rnorm(401,sd=0.05)
     
     # train relevance vector machine
     foo <- rvm(x, y)
     foo
     # print relevance vectors
     alpha(foo)
     RVindex(foo)
     
     # predict and plot
     ytest <- predict(foo, x)
     plot(x, y, type ="l")
     lines(x, ytest, col="red")
   