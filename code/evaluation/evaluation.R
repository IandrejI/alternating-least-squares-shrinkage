
# Ermittlung der Ergebnisse der 10-CV -------------------------------------

quantile(eval.grid$RMSE.Valid)
summary(eval.grid)

top5 <- eval.grid %>% 
  arrange(RMSE.Valid) %>% 
  head(5)


ggplot(top20, aes(x = lambda, y = RMSE.Valid)) +
  geom_line()


adjEval.grid <- eval.grid

mean(adjEval.grid$RMSE.Valid)
median(adjEval.grid$RMSE.Valid)

adjEval.grid %>% 
  filter(RMSE.Valid != max(RMSE.Valid)) %>% 
  summarise(mean = mean(RMSE.Valid), median = median(RMSE.Valid))

adjEval.grid$type <- ifelse(adjEval.grid$alpha == 0, "Ridge", 
       ifelse(adjEval.grid$alpha == 1, "Lasso", "Elastic-Net"))

eval.info <- adjEval.grid %>% 
  group_by(type) %>% 
  summarise(RMSE = mean(RMSE.Valid), n = n(), median = median(RMSE.Valid))

## Plot
ggplot(adjEval.grid, aes(x = type, y = RMSE.Valid, fill = type, group = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  coord_cartesian(ylim = c(0.8,3.5)) +
  stat_summary(fun = mean, color = "black", shape = 4) +
  scale_fill_brewer(palette = "Set2", name = "Number of Observations",
                    labels = c("n = 102",
                               "n = 17",
                               "n = 31")) +
  theme_minimal() +
  labs(title = "Kreuzvalidierung RMSE-Boxplot", x = "Regression-Type",
       y = "RMSE")



# Laufzeit abh. k ---------------------------------------------------------
## Ermittlung der Laufzeit

# 
# k.time <- expand.grid(k = seq(5,40,5), alpha =c(0,.5,1), lambda = 0.01,time = NA)
# expand.time <- data.frame(k = seq(5,40,5), alpha = 0, lambda = 0, time = NA)
# k.time <- rbind(k.time, expand.time)
# 
# for(i in 1:nrow(k.time)){
#   loop.start <- as.numeric(Sys.time())
# 
#   ALS(trainMatrix, dim = k.time$k[i], impRate = -1, iterMax = 10,
#       alpha = k.time$alpha[i], lambda = k.time$lambda[i], seed = 1238)
# 
#   loop.end <- as.numeric(Sys.time())
#   k.time$time[i] <- loop.end - loop.start
#   print(paste0("progress: ",i/nrow(k.time)*100,"%"))
# }



k.time$type[k.time$alpha == 0 & k.time$lambda >0] <- "Ridge"
k.time$type[k.time$alpha == 0.5] <- "Elastic-Net"
k.time$type[k.time$alpha == 1] <- "Lasso"
k.time$type[k.time$alpha == 0 & k.time$lambda == 0] <- "Normal"

ggplot(k.time, aes(x = k, y = time, color = type)) +
  geom_line() +
  scale_color_brewer(palette = "Set2", name = "Regression-Type") +
  theme_minimal() +
  labs(title = "Laufzeit in Abhägigkeit von k und Regressions-Typ") +
  ylab("time(s)")
  



# Testdaten ---------------------------------------------------------------

## Bestes Modell der CV
ALS.tuned <- ALS(ratingMatrix, dim = 20, impRate = 0.01, alpha = 1, lambda = 0.7, iterMax = 30, holdout = test, seed = 8239)$iterResults
## Vergleichsmodell ohne Regularisierung
ALS.unreg <- ALS(ratingMatrix, dim = 20, impRate = 0.01, iterMax = 30, holdout = test, seed = 8239)$iterResults

##Plot Lernverhalten
ALS.Tuned <- ALS.tuned %>% 
  select(-c("SSE.Train", "SSE.Valid")) %>% 
  pivot_longer(cols = c("RMSE.Train", "RMSE.Valid"), names_to = "name", values_to = "RMSE") %>% 
  filter(Iteration != 0)

ALS.Tuned.test <- ALS.Tuned$RMSE[2]

ALS.Unreg <- ALS.unreg %>% 
  select(-c("SSE.Train", "SSE.Valid")) %>% 
  pivot_longer(cols = c("RMSE.Train", "RMSE.Valid"), names_to = "name",values_to = "RMSE") %>% 
  filter(Iteration != 0)



(gg.tuned <- ggplot(ALS.Tuned, aes(x = Iteration, y = RMSE, color = name)) +
  geom_line() +
  scale_color_brewer(palette = "Set2", name = "Set", labels = c("Train", "Test")) +
  labs(title = "RMSE Testdaten", subtitle = 
         expression(lambda == "0.7," ~ alpha == "1," ~ k == "20," ~ v == "0.01")) +
  theme_minimal())

(gg.unreg <- ggplot(ALS.Unreg, aes(x = Iteration, y = RMSE, color = name)) +
    geom_line() +
    scale_color_brewer(palette = "Set2", name = "Set", labels = c("Train", "Test")) +
    labs(title = "RMSE Testdaten", subtitle = 
           expression(lambda == "0," ~ alpha == "0," ~ k == "20," ~ v == "0.01")) +
    theme_minimal())


grid.arrange(gg.tuned, gg.unreg)
dev.off()


# Zufallsmodell -----------------------------------------------------------
posRatings <- seq(0.5,5,0.5)
N <- 1500
randomModell.results <- data.frame(RMSE = rep(NA, 1500)) 
for(i in 1:N){
  randomModell <- sample(posRatings, length(test), replace = TRUE)
  e <- test.ratings - randomModell
  randomModell.results$RMSE[i] <- sqrt(mean(e^2))
}
randomModell.mean <- mean(randomModell.results$RMSE)


compareModells <- data.frame(RMSE = c(randomModell.mean, ALS.Tuned.test),
  Modell = c("Random Modell", "Tuned Modell"))
                             

## Ohne Regularisierung

# unregModells <- data.frame(dim = seq(2,10,2), RMSE = NA) 
# 
# for(i in 1:nrow(unregModells)){
#   unreg.result <- ALS(ratingMatrix, dim = unregModells$dim[i], impRate = 0.01,iterMax = 30, holdout = test, seed = 8239)$iterResults
#   unregModells$RMSE[i] <- unreg.result$RMSE.Valid[1]
# }

ALS.noBias(trainMatrix, dim = 2, lambda = 0.001, holdout = test)

unregModells.eval <- unregModells

unregModells.eval$Modell <- paste0("Unreg. Modell (k = ",unregModells.eval$dim,")")
unregModells.eval <-  unregModells.eval %>% 
  select(-dim)
compareModells <- rbind(compareModells, unregModells.eval)

RColorBrewer::display.brewer.pal(name = "Set2", n = 3)
RColorBrewer::brewer.pal(name = "Set2", n = 3)

ggplot(data = compareModells, aes(x = reorder(Modell, RMSE), y = RMSE)) +
  geom_col(fill = "#8DA0CB", alpha = 0.9) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Modellvergleich RMSE auf den Testdaten") +
  xlab(element_blank())


