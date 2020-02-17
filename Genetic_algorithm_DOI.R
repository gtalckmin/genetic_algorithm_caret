#tune your genetic algorithm to use PLS and a constrained search optimization (desirability package).
caretGA2<- caretGA
# index <- caret::createMultiFolds(NIR, times = 5)

caretGA2$fitness_intern <- function (object, x, y, maximize, p){
        library(desirability)
        RMSE <- object$results[max(object$bestTune), "RMSE"]
        d_RMSE <- desirability::dMin(30, 75,1)
        d_Size <- desirability::dMin(3, p, 0.5)
        overall <- desirability::dOverall(d_RMSE, d_Size)
        D <- predict(overall, data.frame(RMSE, ncol(x)))
        c(D = D, RMSE = as.vector(RMSE))
}

caret::gafs.default(x = NIR.preprocess[, c(6:75)],  y = CP, 
                    gafsControl =  gafsControl(functions = caretGA2 ,
                                               method = "repeatedcv", number = 10 , repeats = 5,
                                               returnResamp = "all",  
                                               metric = c(internal = "D", external = "RMSE"),
                                               maximize = c(internal = T, external = F), 
                                               allowParallel = T, genParallel = F, verbose = T),
                    iters = 150, 
                    popSize = 30,
                    pcrossover = .8,
                    pmutation=(1/(popSize+1)),
                    elite = max(1, round(popSize*0.05)),
                    # suggestions = start,
                    ## Now pass options to `train`
                    method="pls", metric="RMSE", verbose=T, tuneLength = 8, trControl = trainControl(method = "cv", allowParallel = T)
)