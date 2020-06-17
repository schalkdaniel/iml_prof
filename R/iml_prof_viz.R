library("iml")
library("dplyr")
library("mlr")

data("Boston", package = "MASS")

# Number of observations:
ns = c(100,500)#c(100, 500, 1000, 10000, 50000, 100000)
# ns = c(100, 500, 1000, 5000, 10000)
# Number of features:
ps = c(10)#c(10,20,30,40,50)#c(10, 15, 20)

rep = 10L

learners = c("regr.lm", "regr.randomForest", "regr.xgboost")

data_raw = Boston[,c(1:9, which(colnames(Boston) == "medv"))]
prof_list = list()

k = 1L
for (n in ns) {
  for (p in ps) {
    for (l in learners) {
      for (i in seq_len(rep)) {
        idx = sample(x = seq_len(nrow(Boston)), size = n, replace = TRUE)
        
        if (p == 10L) {
          dat = data_raw[idx,]
        } else {
          dat = cbind(data_raw[idx,], as.data.frame(matrix(rnorm((p - 10L) * n), ncol = (p - 10L))))
        }
        
        task = makeRegrTask(data = dat, target = "medv")
        
        if (l == "regr.lm") {
          lrn = makeLearner(l)
        }
        if (l == "regr.randomForest") {
          lrn = makeLearner(l)
        }
        if (l == "regr.xgboost") {
          lrn = makeLearner(l)
        }
        mod = train(lrn, task)
        
        #     rf = randomForest(medv ~ ., data = dat, ntree = 50)
        
        X = dat[which(names(dat) != "medv")]
        model = Predictor$new(mod, data = X, y = dat$medv)
        
        Rprof("temp.out", memory.profiling = TRUE)
        FeatureEffects$new(model)
        Rprof(NULL)
        a = summaryRprof("temp.out", memory = "both")$by.total
        
        a$mem.diff = c(-diff(a$mem.total), 0)
        call_names = c("\"private$run.ale\"", "\"calculate.ale.num\"")
        mem_change = a[rownames(a) %in% call_names, "mem.diff"]
        
        prof_list[[k]] = data.frame(call = call_names, mem_change = mem_change, nrows = n, ncols = p, learner = l,
                                    rep = i)
        
        k = k + 1L
      }
    }
  }
}
bm = do.call(rbind, prof_list)

library(ggplot2)
library(dplyr)
gg=bm %>%
  group_by(nrows, ncols, learner, rep) %>%
  summarize(mem_change = mem_change[call == "\"private$run.ale\""] + mem_change[call == "\"calculate.ale.num\""]) %>%
  group_by(nrows, ncols, learner) %>%
  summarize(med_mem_change = median(mem_change), se_lower = median(mem_change)-sd(mem_change), se_upper = sd(mem_change) + median(mem_change)) %>%
  ggplot(aes(x = nrows, y = med_mem_change, color = as.factor(ncols))) + 
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin = se_lower, ymax = se_upper), width = 0.2, alpha = 0.5) + 
  facet_grid(~learner)

save(prof_list, file = "prof_list.Rda")
