##### Code for plots

# Also as RMD file

#Figure 2.1a (Chain DAG)

varnames <- c("X","M","Y")
Adj <- matrix(c(0,0,1,
                0,0,0,
                0,1,0), 3,3, byrow = TRUE,
              dimnames = list(varnames,varnames))

qgraph(Adj, 
       labels = c("X","M","Y"), 
       layout = rbind(c(-1,-1),
                      c(1,-1),
                      c(0,1)))
#Figure 2.1b (Fork DAG)

varnames <- c("X","M","Y")
Adj <- matrix(c(0,0,0,
                0,0,0,
                1,1,0), 3,3, byrow = TRUE,
              dimnames = list(varnames,varnames))

qgraph(Adj, 
       labels = c("X","M","Y"), 
       layout = rbind(c(-1,-1),
                      c(1,-1),
                      c(0,1)))

#Figure 2.1c (Collider DAG)

varnames <- c("X","M","Y")
Adj <- matrix(c(0,0,1,
                0,0,1,
                0,0,0), 3,3, byrow = TRUE,
              dimnames = list(varnames,varnames))

qgraph(Adj, 
       labels = c("X","M","Y"), 
       layout = rbind(c(-1,-1),
                      c(1,-1),
                      c(0,1)))

## FIGURE 2.2  (BOWTIE)
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

#Y_mainonly <- b1*X + b2*M + rnorm(n,mumain,1) # only main effect, no interaction
Y_intonly <- b3*X*M + rnorm(n,muint,1)      # only interaction effect; M = 0 (; so at its mean) yields no effect of X; should find either X indep Y or M indep Y
#Y_mainint <- b1*X + b2*M + b3*X*M + rnorm(n,mumainint,1)   # main and interaction effect
data <- data.frame(Y_intonly)
# Plots
ggplot(data = data, aes(x = X, y = Y_intonly, main = "Only interaction effect")) + geom_smooth(method = "lm") + geom_point() + theme_classic()



## Figure 4.1 (main results)
mainplot <- main %>% ggplot() + geom_smooth(mapping = aes(x = samplesize, y = result, color = algorithm)) + geom_point(mapping = aes(x = samplesize, y = result, color = algorithm)) + labs(title = paste("Main effect model")) + xlim(100,500) + ylim(0, 1) + theme_classic()
## Figure 4.2 (interaction results)
intplot <- int %>% ggplot() + geom_smooth(mapping = aes(x = samplesize, y = result, color = algorithm)) + geom_point(mapping = aes(x = samplesize, y = result, color = algorithm)) + labs(title = paste("Only interaction effect model")) + xlim(100,500) + ylim(0, 1) + theme_classic()
## Figure 4.3 (weakmain results)
weakmainplot <- weakmain %>% ggplot() + geom_smooth(mapping = aes(x = samplesize, y = result, color = algorithm)) + geom_point(mapping = aes(x = samplesize, y = result, color = algorithm)) + labs(title = paste("Weak main effect model")) + xlim(100,500) + ylim(0, 1) + theme_classic()

### Showing the plots
mainplot
intplot
weakmainplot


