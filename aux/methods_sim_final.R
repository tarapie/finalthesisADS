# ---------------------------------------------------------------
# --------------------------- Methods file ----------------------
# ---------------------------------------------------------------

### METHOD 1: PC-algorithm
pc_function <- function(dataframe,a){
  suffStat <- list(C = cor(dataframe), n = nrow(dataframe))
  pc_fit <- pc(suffStat = suffStat, indepTest = gaussCItest, alpha = a, labels = colnames(dataframe))
  pc_fit@graph # NEEDED TO GET ADJ MATRIX!
  as(pc_fit@graph, "matrix")
}

### METHOD 2: kPC-algorithm
kpc_function <- function(dataframe,a){
  suffStat <- list(C = cor(dataframe), n = nrow(dataframe))
  kpc_fit <- kpc(suffStat = list(data=dataframe, ic.method="dcc.perm"),
                 indepTest = kernelCItest,
                 alpha = a,
                 labels = colnames(dataframe),
                 u2pd = "relaxed",
                 skel.method = "stable",
                 verbose = TRUE)  
  kpc_fit@graph
  as(kpc_fit@graph, "matrix")
}