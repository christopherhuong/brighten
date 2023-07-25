# Supplemental Material to “Exploring the Dynamics of Situated Expectancy-Value Theory: A Panel Network Analysis”
# R Code for Network Analysis
# Loading packages
library(psychonetrics)
library(qgraph)

#Selecting variables
vars <- c("comp", "imp", "int", "tecost", "oecost", "lvcost", "emcost")
mod <- ml_ts_lvgvar(d, beepvar = "week", idvar = "stud_id",
                    vars = vars,
                    estimator = "FIML")

# Run models for model selection:
mod <- mod %>% runmodel

Model_pruned <- mod %>% prune(adjust = "fdr", recursive = FALSE, alpha = 0.05)

Model_pruned_stepup <- Model_pruned %>% stepup(criterion = "bic")

# Compare all models:
compare(
  full = mod,
  pruned = Model_pruned
  pruned_stepup = Model_pruned_stepup)

# Save model:
saveRDS(mod, "SEVT.RDS")

# Extract matrices with significance:
contemporaneous <- mod@parameters %>% 
  filter(matrix == "omega_zeta_within") %>%  
  select(var1 = var1_id, var2 = var2_id, est, se, p)

between <- mod@parameters %>% 
  filter(matrix == "omega_zeta_between") %>% 
  select(var1 = var1_id, var2 = var2_id, est, se, p)

temporal <- mod@parameters %>% 
  filter(matrix == "beta") %>% 
  select(var1 = var2_id, var2 = var1_id, est, se, p)

vsize <- 18
asize <- 8
mar <- 5

# Labels:
groups_type <- list("Competence" = c(1), "Task Values" = c(2:3), "Costs" = c(4:7))
group_col <- c("#6495ED", "#CCCCFF", "#FF7F50") # colors

pdf("SEVT.pdf",width=15,height=5)
layout(t(1:3))
qgraph(temporal[,c("var1","var2","est")], layout = "circle", labels = vars,
       #color = group_col, 
       #groups = groups_type,
       title = "Temporal",
       theme = "colorblind", # maximum = Max,
       vsize = vsize, asize = asize,
       parallelEdge = TRUE,
       lty = ifelse(temporal$p < 0.05, 1, 2),
       mar = rep(mar, 4))

qgraph(contemporaneous[,c("var1","var2","est")], layout = "circle", labels = vars,
       #color = group_col, 
       #groups = groups_type,
       title = "Contemporaneous",
       theme = "colorblind", # maximum = Max,
       vsize = vsize, asize = asize,
       parallelEdge = TRUE,
       lty = ifelse(contemporaneous$p < 0.05, 1, 2),
       directed = FALSE,
       mar = rep(mar, 4))

qgraph(between[,c("var1","var2","est")], layout = "circle", labels = vars,
       #color = group_col, 
       #groups = groups_type,
       title = "Between-persons",
       theme = "colorblind", # maximum = Max,
       vsize = vsize, asize = asize,
       parallelEdge = TRUE,
       lty = ifelse(between$p < 0.05, 1, 2),
       directed = FALSE,
       mar = rep(mar, 4))
dev.off() 
Figure S1      
Estimated Edge Weights per Network, Surrounded by 95% Confidence Regions.          

Note. The alpha = .0005 corresponds to a Bonferroni-corrected alpha level of .05. 
