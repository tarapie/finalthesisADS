# ---------------------------------------------------------------
# -------------------------- Analysis file ----------------------
# ---------------------------------------------------------------

# Read in results obtained from the simulation, saved in RDS-files
# In this file we use the results from the simulation file
# to do some analyses.

pc_recover <- readRDS("results/boolean_results_pc_June2021.RDS")
kpc_recover <- readRDS("results/boolean_results_kpc_June2021.RDS")
dags_pc <- readRDS("results/dags_results_pc_June2021.RDS")
dags_kpc <- readRDS("results/dags_results_kpc_June2021.RDS")

# ---------------------------------------------------------------
# -------------------------- Analysis ---------------------------
# ---------------------------------------------------------------

# 1) Use means from simulation to make dataframes
# 2) Make plots

# Results PC
pc_model1_n100_mean <- mean(pc_recover$model1$n100)
pc_model2_n100_mean <- mean(pc_recover$model2$n100)
pc_model3_n100_mean <- mean(pc_recover$model3$n100)

pc_model1_n200_mean <- mean(pc_recover$model1$n200)
pc_model2_n200_mean <- mean(pc_recover$model2$n200)
pc_model3_n200_mean <- mean(pc_recover$model3$n200)

pc_model1_n500_mean <- mean(pc_recover$model1$n500)
pc_model2_n500_mean <- mean(pc_recover$model2$n500)
pc_model3_n500_mean <- mean(pc_recover$model3$n500)

# Results kPC
kpc_model1_n100_mean <- mean(kpc_recover$model1$n100)
kpc_model2_n100_mean <- mean(kpc_recover$model2$n100)
kpc_model3_n100_mean <- mean(kpc_recover$model3$n100)

kpc_model1_n200_mean <- mean(kpc_recover$model1$n200)
kpc_model2_n200_mean <- mean(kpc_recover$model2$n200)
kpc_model3_n200_mean <- mean(kpc_recover$model3$n200)

kpc_model1_n500_mean <- mean(kpc_recover$model1$n500)
kpc_model2_n500_mean <- mean(kpc_recover$model2$n500)
kpc_model3_n500_mean <- mean(kpc_recover$model3$n500)

# Analysis
result <- c(pc_model1_n100_mean, pc_model1_n200_mean,pc_model1_n500_mean,
            pc_model2_n100_mean, pc_model2_n200_mean,pc_model2_n500_mean,
            pc_model3_n100_mean, pc_model3_n200_mean,pc_model3_n500_mean,
            kpc_model1_n100_mean, kpc_model1_n200_mean,kpc_model1_n500_mean,
            kpc_model2_n100_mean, kpc_model2_n200_mean,kpc_model2_n500_mean,
            kpc_model3_n100_mean, kpc_model3_n200_mean,kpc_model3_n500_mean)
samplesize <- c(100, 200, 500, 100, 200, 500, 100, 200, 500, 100, 200, 500, 100, 200, 500, 100, 200, 500)
Algorithm <- c("pc", "pc","pc","pc","pc","pc","pc","pc","pc","kpc","kpc","kpc","kpc","kpc","kpc","kpc","kpc","kpc")
model <- c("main","main","main", "int","int","int", "weakmain", "weakmain", "weakmain","main","main","main", "int","int","int", "weakmain", "weakmain", "weakmain")
dataframe <- data.frame(result, samplesize, Algorithm, model)
print(dataframe)

# ---------------------------------------------------------------
# -------------------------- Figures ----------------------------
# ---------------------------------------------------------------

# Make subsets for figures
main <- filter(dataframe, model == "main") 
int <- filter(dataframe, model == "int") 
weakmain <- filter(dataframe, model == "weakmain") 

# Make figures and save as pdf's in Figures folder
mainplot <- main %>% ggplot() + geom_line(mapping = aes(x = samplesize, y = result, color = Algorithm)) + geom_point(mapping = aes(x = samplesize, y = result, color = Algorithm)) + labs(title = paste("Main effect model"), fill = "Algorithm") + xlab(label = "Sample Size") + ylab(label = "Performance") + theme(legend.background = element_rect(fill = "white")) + xlim(100,500) + ylim(0, 1) + theme(legend.background = element_rect(fill = "white")) + theme_classic()
ggsave(filename = "mainplot.pdf",
  plot = mainplot,
  path = "figures/",
  device = "pdf")
dev.off()

intplot <- int %>% ggplot() + geom_line(mapping = aes(x = samplesize, y = result, color = Algorithm)) + geom_point(mapping = aes(x = samplesize, y = result, color = Algorithm)) + labs(title = paste("Interaction effect model"), fill = "Algorithm") + xlab(label = "Sample Size") + ylab(label = "Performance") + theme(legend.background = element_rect(fill = "white")) + xlim(100,500) + ylim(0, 1) + theme(legend.background = element_rect(fill = "white")) + theme_classic()
ggsave(filename = "intplot.pdf",
       plot = intplot,
       path = "figures/",
       device = "pdf")
dev.off()

weakmainplot <- weakmain %>% ggplot() + geom_line(mapping = aes(x = samplesize, y = result, color = Algorithm)) + geom_point(mapping = aes(x = samplesize, y = result, color = Algorithm)) + labs(title = paste("Weak main effect model"), fill = "Algorithm") + xlab(label = "Sample Size") + ylab(label = "Performance") + theme(legend.background = element_rect(fill = "white")) + xlim(100,500) + ylim(0, 1) + theme(legend.background = element_rect(fill = "white")) + theme_classic()
ggsave(filename = "weakmainplot.pdf",
       plot = weakmainplot,
       path = "figures/",
       device = "pdf")
dev.off()

# ---------------------------------------------------------------
# -------------------- Miscellaneous plots ----------------------
# ---------------------------------------------------------------

# Code for plots
# Figure 1a (Chain DAG)
pdf("figures/fig1a.pdf", width = 8, height = 7)
varnames <- c("X","M","Y")
Adj <- matrix(c(0,0,1,
                0,0,0,
                0,1,0), 3,3, byrow = TRUE,
              dimnames = list(varnames,varnames))
qgraph(Adj, 
       labels = c("X","M","Y"), 
       layout = rbind(c(-1,-1),
                      c(1,-1),
                      c(0,1)),
       asize = 20,
       esize = 5,
       label.cex = 2.5)
dev.off()

# Figure 1b (Fork DAG)
pdf("figures/fig1b.pdf", width = 8, height = 7)
varnames <- c("X","M","Y")
Adj <- matrix(c(0,0,0,
                0,0,0,
                1,1,0), 3,3, byrow = TRUE,
              dimnames = list(varnames,varnames))
qgraph(Adj, 
       labels = c("X","M","Y"), 
       layout = rbind(c(-1,-1),
                      c(1,-1),
                      c(0,1)),
       asize = 20,
       esize = 5,
       label.cex = 2.5)
dev.off()

# Figure 1c (Collider DAG)
pdf("figures/fig1c.pdf", width = 8, height = 7)
varnames <- c("X","M","Y")
Adj <- matrix(c(0,0,1,
                0,0,1,
                0,0,0), 3,3, byrow = TRUE,
              dimnames = list(varnames,varnames))
qgraph(Adj, 
       labels = c("X","M","Y"), 
       layout = rbind(c(-1,-1),
                      c(1,-1),
                      c(0,1)),
       asize = 20,
       esize = 5,
       label.cex = 2.5)
dev.off()

## Figures 2 and 3  (Bowtie)
set.seed(345)
n <- 20000
b1 <- 1
b2 <- 1
b3 <- 1

mux <- 0
mum <- 0
mumain <- 0
muint <- 0
mumainint <- 0

alphavalue <- 0.05

X <- rnorm(n,mux,1) 
M <- rnorm(n,mum,1)

Y <- b3*X*M + rnorm(n,muint,1)      # only interaction effect; M = 0 (; so at its mean) yields no effect of X; should find either X indep Y or M indep Y
data <- data.frame(X,M,Y)

# Figure 2 (incorrect DAG)
# To get DAG
suffStat <- list(C = cor(data), n = nrow(data))
pc_fit1 <- pc(suffStat = suffStat, indepTest = gaussCItest, alpha = 0.05, labels = c("X", "M","Y"))
plot(pc_fit1, main = "Incorrect DAG from PC-algorithm", cex = 2)
dev.off()
# Figure used
pdf("figures/pc_fail.pdf")
varnames <- c("X","M","Y")
Adj <- matrix(c(0,0,0,
                0,0,0,
                0,0,0), 3,3, byrow = TRUE,
              dimnames = list(varnames,varnames))
qgraph(Adj, 
       labels = c("X","M","Y"), 
       layout = rbind(c(-1,1),
                      c(0,1),
                      c(1,1)),
       asize = 20,
       esize = 5,
       label.cex = 2.5)
dev.off()


# Figure 3 (bowtie plot)
bowtie <- ggplot(data = data, aes(x = X, y = Y_intonly)) + geom_point() + geom_smooth(method = "lm") + labs(title = paste("Interaction effect model")) + xlab(label = "X") + ylab(label = "Y") + theme_classic()
ggsave(filename = "bowtie.pdf",
       plot = bowtie,
       path = "figures/",
       device = "pdf")
dev.off()

