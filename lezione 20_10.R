set.seed(1)
x = rnorm(200)
y = rnorm(200) + 0.2 * x**3
df = as_tibble(data.frame(x,y))
df

plot(df)

gradoMax = 6
set.seed(1)
modelli = lapply(X = c(1:gradoMax),
                 function(grado) lm(y ~ poly(x,grado),
                                    data = df))
# creo una palette di colori evitando le scale di grigi
colori = sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)],
                size = gradoMax,
                replace = F)
{
  plot(df)
  asseX = seq(from = min(df$x),
              to = max(df$x),
              by = .1)
  apply(X = c(1:gradoMax) %>% matrix,
        1,
        function(grado) lines(x = asseX,
                              predict(modelli[[grado]],
                                      data.frame(x = asseX)),
                              col = colori[grado]))
  legend("bottomright",
         stringr::str_c("grado ",c(1:gradoMax)),
         col = colori,
         lwd = 1,
         cex = .6)
}
library(ggplot2)
  ggplot(df,aes(x,y)) + 
    geom_point() + 
    geom_smooth(method = "lm",
                se = T, # bande di confidenza
                formula = y ~ poly(x,4))
  
  # forest plot per visualizzare le stime dei coefficienti di un modello

