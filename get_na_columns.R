div0_level <- "#DIV/0!"

get_na_columns <- function(df) {
  r <- NULL
  for (i in 1:ncol(df)) {
    c <- df[,i]
    if (all(is.na(c))) {
      r <- c(r, i)
    }
  }
  r
}

get_div0_columns <- function(df) {
  columns <- NULL
  for (cnum in seq(1,ncol(df))) {
    col <- df[,cnum]
    if (is.factor(col) & div0_level %in% levels(col))
      columns <- c(columns, cnum)
  }
  columns
}

dummyfy_Y <- function(Y) {
  df <- data.frame(classe=Y)
  fit <- dummyVars("~ classe", data=df)
  data.frame(predict(fit, df))
}

summ_dataframe_names <- function(df) {
  paste(names(df), collapse=" + ")
}

get_nn_formula <- function(Y, X) {
  as.formula(paste(summ_dataframe_names(Y), " ~ " , summ_dataframe_names(X), sep=""))
}

train_nn <- function(Y, X, ...) {
  Y <- dummyfy_Y(Y)
  f <- get_nn_formula(Y, X)
  begin <- Sys.time()
  set.seed(234223)
  fit <- neuralnet(f, cbind(Y,X), ...)
  message(format(Sys.time() - begin))
  fit
}

predict_nn <- function(fit, data) {
  l <- c("A", "B", "C", "D", "E")
  p <- compute(fit, data)$net.result
  r <- list()
  for (i in 1:nrow(p)) {
    if (length(which.max(p[i,])) == 0) r[i] <- NA
    else r[i] <- l[which.max(p[i,])]
  }
  factor(r, levels=l)
}

multiple_train_nn <- function(Y, X, begin, end, step, ...) {
  nns <- list()
  counts <- seq(begin, end, by=step)
  i <- 1
  
  
  for (c in counts) {
    cat("Entrenando red:", c, "\n")
    nns[[i]] <- train_nn(Y, X, hidden=c, ...)
    i <- i + 1
  }
  names(nns) <- as.character(counts)
  nns
}

multiple_predict_nn <- function(multiple_nn, X) {
  res <- list()
  for (i in 1:length(multiple_nn)) { 
    res[[i]] <- predict_nn(multiple_nn[[i]], X)
  }
  names(res) <- names(multiple_nn)
  res
}

multiple_nn_error <- function(multiple_nn, X, Y) {
  pred <- multiple_predict_nn(multiple_nn, X)
  res <- 1 - sapply(pred, function(p){ mean(p == Y)})
  names(res) <- names(pred)
  res
}

plot_errors <- function(training_errors, xvalidation_errors) {
  df <- data.frame(units=as.numeric(names(training_errors)), xvalidation=xvalidation_errors, training=training_errors)
  p <- ggplot(data=df, aes(x=units, y=xvalidation, colour="black"))
  
  p <- p + geom_line()
  p <- p + geom_point()
  p <- p + geom_line(data=df, aes(x=units, y=training, colour="blue"))
  p <- p + geom_point(data=df, aes(x=units, y=training, colour="blue"))
  p <- p + ggtitle("Error comparison")
  p <- p + xlab("Number of units")
  p <- p + ylab("Generalization error")
  p <- p + scale_colour_manual(name = 'Error sets', 
                      values =c(black="black",blue="blue"), labels = c('x validation','training'))
  p
}