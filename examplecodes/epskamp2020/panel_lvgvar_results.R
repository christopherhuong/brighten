# Needed libraries:
library("semPlot")
library("qgraph")
library("psychonetrics")
library("xtable")
library("semPlot")
library("ggplot2")
library("dplyr")
library("tidyr")

# Helper function
source("../helperFunctions.R")

# Load results:
analysis <- readRDS("panelResults.RDS")

# Read bootstraps:
files <- list.files(path="bootstraps/",pattern="panelBootstrap",full.names = TRUE)
bootstraps <- do.call(c,lapply(files, readRDS))

### FACTOR LOADINGS ###

# Standardize at the within-subject level

# Unstandardized factor loadings:
lambda <- getmatrix(analysis, "lambda")

# Residual variances:
theta <- getmatrix(analysis, "sigma_epsilon_within")

# Latent variance-covariance:
psi <- getmatrix(analysis, "sigma_zeta_within")

# Use semPlot to standardize:
library("semPlot")
semPlot_mod <- lisrelModel(LY = lambda, TE = theta, PS = psi)
modMats <- modelMatrices(semPlot_mod, "Mplus")
lambdastd <- modMats$Lambda[[1]]$std

# Number of latents:
nLat <- ncol(lambdastd)

# Number of observed:
nObs <- nrow(lambdastd)

# Edgelist for graph:
Edgelist <- cbind(
  c(col(lambdastd)),c(row(lambdastd))+ncol(lambdastd),c(lambdastd)
)

# shape:
shape <- c(rep("ellipse",nLat),rep("rectangle",nObs))

# Size1:
size1 <-  c(rep(18,nLat),rep(5,nObs))
            
# Size2:
size2 <-  c(rep(10,nLat),rep(5,nObs))

# Edge connect points:
ECP <- Edgelist
ECP[,1] <- NA # 0.5*pi
ECP[,2] <- 0

# Labels:
# latLabels <-  c(
#   "Self\nesteem", "Pessimism", "Optimism", "Life\nsatisfaction",
#   "Positive\naffect","Negative\naffect"
# )

latLabels <-  c(
  "Self esteem\n(SE)", "Pessimism\n(Pes)", "Optimism\n(Opt)", "Life satisfaction\n(LS)",
  "Positive affect\n(PA)","Negative affect\n(NA)"
)

# Manifest labels:
manLabels <- c(
  "SE1", "SE2", "SE3", "Pes1", "Pes2", "Pes3", "Opt1", "Opt2", "Opt3", "LS1", "LS2", "LS3", "LS4","LS5",
  "PA1","PA2","PA3","PA4","NA1","NA2","NA3","NA4"
)




# Size of labels:
labelCex <- c(
  rep(1.5,nLat),
  rep(1,nObs)
)

# Starting layout:
Layout <- rbind(
  cbind(
    # seq(-1,1,length=nLat+2)[-c(1,nLat+2)],
    seq(-0.8,0.8,length=nLat),
    1
  ),
  cbind(
    seq(-1,1,length=nObs+2)[-c(1,nObs+2)],
    0
  )
)



# Plot and save to PDF:
qgraph(Edgelist,
       shape = shape,
       vsize = size1,
       vsize2 = size2,
       layout = Layout,
       mar = c(4,1,7,1),
       edgeConnectPoints = ECP,
       labels = c(latLabels, manLabels),
       label.scale = FALSE,
       label.cex = labelCex,
       asize = 5,
       theme = "colorblind",
       filetype = "pdf",
       filename = "panelloadings",
       width = 15,
       height = 3,
       cut = 0)


 ### NETWORKS ###
temporal <- getmatrix(analysis, "PDC")

contemporaneous <- getmatrix(analysis, "omega_zeta_within")

between <- getmatrix(analysis, "omega_zeta_between")


### Bootstrap inclusions probabilities ###
tempInclude <- signInclude(bootstraps, "beta", transpose = TRUE)

contInclude <- signInclude(bootstraps, "omega_zeta_within", lowertri = TRUE)

betInclude <- signInclude(bootstraps, "omega_zeta_between", lowertri = TRUE)


# Average layout:
Layout <- averageLayout(tempInclude[,1:3],contInclude[,1:3],betInclude[,1:3], repulsion = 0.9)

# # Reproducible:
Layout <- structure(c(-0.288106823857851, 1, 0.718568451360464, 0.929059698117062, 
                      -1, -0.0642115271911072, -0.958115788281574, 1, -0.124425369046546, 
                      -1, 0.0902005921385629, 0.540424666430921), .Dim = c(6L, 2L))


# Loop rotation:
loopRotation <- rep(NA, 6)
loopRotation[3] <- 0.8 * pi

latLabels <-  c(
  "SE", "Pes", "Opt", "LS",
  "PA","NA"
)

# Plot all graphs:
max <- max(c(abs(temporal),abs(contemporaneous),abs(between)))
qgraph(temporal, layout = Layout, theme = "colorblind",  vsize = 13, mar = rep(5,4), asize = 8,
       directed = TRUE, labels = latLabels, maximum = max, esize = 10, edge.labels = FALSE,
       filetype = "pdf", filename = "panelPDC",
       vTrans = 254, loopRotation = loopRotation,
       label.scale.equal = TRUE)

qgraph(contemporaneous, layout = Layout, theme = "colorblind", vsize = 13, mar = rep(5,4), 
       labels = latLabels, maximum = max, esize = 10, edge.labels = FALSE,
       filetype = "pdf", filename = "panelPCC",
       vTrans = 254, loopRotation = loopRotation,
       label.scale.equal = TRUE)

qgraph(between, layout = Layout, theme = "colorblind", vsize = 13, mar = rep(5,4), 
       labels = latLabels, maximum = max, esize = 10, edge.labels = FALSE,
       filetype = "pdf", filename = "panelBetween",
       vTrans = 254, loopRotation = loopRotation,
       label.scale.equal = TRUE)



qgraph(tempInclude[,1:3], layout = Layout, theme = "colorblind",  vsize = 13, mar = rep(5,4), asize = 8,
       directed = TRUE, labels = latLabels, maximum = 1, esize = 10, edge.labels = FALSE,
       filetype = "pdf", filename = "panelPDC_incl",edge.color = ifelse(tempInclude$type=="pos","#0000D5","#BF0000"),
       vTrans = 254, loopRotation = loopRotation, diag = TRUE,
       label.scale.equal = TRUE, parallelEdge = TRUE)

qgraph(contInclude[,1:3], layout = Layout, theme = "colorblind", vsize = 13, mar = rep(5,4), 
       labels = latLabels, maximum = 1, esize = 10, edge.labels = FALSE,
       filetype = "pdf", filename = "panelPCC_incl", edge.color = ifelse(contInclude$type=="pos","#0000D5","#BF0000"),
       vTrans = 254, loopRotation = loopRotation, directed = FALSE,
       label.scale.equal = TRUE, parallelEdge = TRUE)

qgraph(betInclude[,1:3], layout = Layout, theme = "colorblind", vsize = 13, mar = rep(5,4), 
       labels = latLabels, maximum = 1, esize = 10, edge.labels = FALSE,
       filetype = "pdf", filename = "panelBetween_incl", edge.color = ifelse(betInclude$type=="pos","#0000D5","#BF0000"),
       vTrans = 254, loopRotation = loopRotation, directed = FALSE,
       label.scale.equal = TRUE, parallelEdge = TRUE)


### Make tables ###
### Parameter estimates ###
# Sanitize function:

# Temporal:
temporal_table <- temporal
temporal_table[temporal_table==0] <- NA
rownames(temporal_table) <- colnames(temporal_table) <- latLabels
print(xtable(temporal_table,digits = 2), floating = FALSE, NA.string = ".")

# Contemporaneous:
contemporaneous_table <- contemporaneous

# Add marginal correlations:
contemporaneous_cors <- cov2cor(getmatrix(analysis, "sigma_zeta_within"))

# Add to upper tri:
contemporaneous_table[upper.tri(contemporaneous_table)] <- contemporaneous_cors[upper.tri(contemporaneous_cors)]

# Make zeroes NA:
contemporaneous_table[contemporaneous_table==0] <- NA

rownames(contemporaneous_table) <- colnames(contemporaneous_table) <- latLabels
print(xtable(contemporaneous_table,digits = 2), floating = FALSE, NA.string = ".")


# Between:
between_table <- between

# Add marginal correlations:
between_cors <- cov2cor(getmatrix(analysis, "sigma_zeta_between"))

# Add to upper tri:
between_table[upper.tri(between_table)] <- between_cors[upper.tri(between_cors)]

# Make zeroes NA:
between_table[between_table==0] <- NA

rownames(between_table) <- colnames(between_table) <- latLabels
print(xtable(between_table,digits = 2), floating = FALSE, NA.string = ".")


### Bootstrap results estimates ###
# Boldify function:
boldify <- function(x,ind){
  x[ind] <- ifelse(is.na(x[ind]),NA,paste0('\\textbf{',x[ind],'}'))
  x
}

# Inclusion over edges (disregard sign):
tempInclude_noSign <- edgeInclude(bootstraps,"beta",TRUE)
# Temporal:
rownames(tempInclude_noSign) <- colnames(tempInclude_noSign) <- latLabels
print(xtable(boldify(tempInclude_noSign, temporal!=0),digits = 0), floating = FALSE, sanitize.text.function = identity)

# Contemporaneous (lowertri)
# Inclusion over edges (disregard sign):
contInclude_noSign <- edgeInclude(bootstraps,"omega_zeta_within")
rownames(contInclude_noSign) <- colnames(contInclude_noSign) <- latLabels
contInclude_noSign[upper.tri(contInclude_noSign,diag=TRUE)] <- NA
print(xtable(boldify(contInclude_noSign, contemporaneous != 0),digits = 0), floating = FALSE, sanitize.text.function = identity)

# Between (lowertri)
# Inclusion over edges (disregard sign):
betInclude_noSign <- edgeInclude(bootstraps,"omega_zeta_between")
rownames(betInclude_noSign) <- colnames(betInclude_noSign) <- latLabels
betInclude_noSign[upper.tri(betInclude_noSign,diag=TRUE)] <- NA
print(xtable(boldify(betInclude_noSign, between != 0),digits = 0), floating = FALSE, sanitize.text.function = identity)


