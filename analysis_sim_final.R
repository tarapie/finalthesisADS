# Read in results obtained from the simulation, saved in RDS-files
source("boolean_results_pc_June2021.RDS")
source("boolean_results_kpc_June2021.RDS")
source("dags_results_pc._June2021RDS")
source("dags_results_kpc._June2021RDS")

### ANALYSIS FILE
# 1) Use means from simulation to make dataframe
# 2) Make plots

### Analysis
result <- c(pc_model1_n100_mean, pc_model1_n200_mean,pc_model1_n500_mean,
            pc_model2_n100_mean, pc_model2_n200_mean,pc_model2_n500_mean,
            pc_model3_n100_mean, pc_model3_n200_mean,pc_model3_n500_mean,
            kpc_model1_n100_mean, kpc_model1_n200_mean,kpc_model1_n500_mean,
            kpc_model2_n100_mean, kpc_model2_n200_mean,kpc_model2_n500_mean,
            kpc_model3_n100_mean, kpc_model3_n200_mean,kpc_model3_n500_mean)
samplesize <- c(100, 200, 500, 100, 200, 500, 100, 200, 500, 100, 200, 500, 100, 200, 500, 100, 200, 500)
algorithm <- c("pc", "pc","pc","pc","pc","pc","pc","pc","pc","kpc","kpc","kpc","kpc","kpc","kpc","kpc","kpc","kpc")
model <- c("main","main","main", "int","int","int", "weakmain", "weakmain", "weakmain","main","main","main", "int","int","int", "weakmain", "weakmain", "weakmain")
dataframe <- data.frame(result, samplesize, algorithm, model)
print(dataframe)

# Make subsets for figures
main <- filter(dataframe, model == "main") 
int <- filter(dataframe, model == "int") 
weakmain <- filter(dataframe, model == "weakmain") 

# Make figures
mainplot <- main %>% ggplot() + geom_smooth(mapping = aes(x = samplesize, y = result, color = algorithm)) + geom_point(mapping = aes(x = samplesize, y = result, color = algorithm)) + labs(title = paste("Main effect model")) + xlim(100,500) + ylim(0, 1) + theme_classic()
intplot <- int %>% ggplot() + geom_smooth(mapping = aes(x = samplesize, y = result, color = algorithm)) + geom_point(mapping = aes(x = samplesize, y = result, color = algorithm)) + labs(title = paste("Only interaction effect model")) + xlim(100,500) + ylim(0, 1) + theme_classic()
weakmainplot <- weakmain %>% ggplot() + geom_smooth(mapping = aes(x = samplesize, y = result, color = algorithm)) + geom_point(mapping = aes(x = samplesize, y = result, color = algorithm)) + labs(title = paste("Weak main effect model")) + xlim(100,500) + ylim(0, 1) + theme_classic()

### Showing plots (save them manually)
mainplot
intplot
weakmainplot

