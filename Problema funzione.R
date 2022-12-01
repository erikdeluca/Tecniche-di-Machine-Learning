calcolaMSE = function(nodiFinali, dati, varDipendente, subsTrain, subsTest)
{
  t = prune.tree(tree(get(varDipendente) ~ .,
                      data = dati,
                      subset = subsTrain,
                      control = tree.control(nobs = length(subsTrain),minsize = nodiFinali)),
                 best = nodiFinali)
  return(sqrt(mean((unlist(dati[subsTest,colnames(dati) == varDipendente] - predict(t,newdata = dati[subsTest,])))^2)))
}


sapply(c(2:10), function(X) calcolaMSE(X,dati = tl,varDipendente = "LFACE",subsTrain = train, subsTest = train))
