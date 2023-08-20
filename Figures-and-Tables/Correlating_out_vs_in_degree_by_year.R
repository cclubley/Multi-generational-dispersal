# -------------------------------------------------------------------------
# -------------------- Temporal stability in node degree ------------------
# -------------------------------------------------------------------------
# Set working directory ---------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/Data/Data")

# Load the required packages ----------------------------------------------
library(corrplot)
library(viridis)

# Load the in degree and out degree data ----------------------------------

# Load in degree and out degree
out_deg <- read.csv("./Out_degree/Out_degree_all_years.csv")
head(out_deg)
colnames(out_deg) <- (c("Node", "Out_2000", "Out_2001", "Out_2002", "Out_2003", "Out_2004",
                      "Out_2005", "Out_2006", "Out_2007", "Out_2008", "Out_2009", "Out_2010",
                      "Out_2011", "Out_2012"))
in_deg <- read.csv("./In_degree/In_degree_all_years.csv")
head(in_deg)
colnames(in_deg) <- (c("Node", "In_2000", "In_2001", "In_2002", "In_2003", "In_2004",
                      "In_2005", "In_2006", "In_2007", "In_2008", "In_2009", "In_2010",
                      "In_2011", "In_2012"))

# Merge together
deg <- merge(out_deg, in_deg, by=c("Node"), all=TRUE)
# Remove any rows where both columns are NA
deg <- deg[rowSums(is.na(deg)) != ncol(deg), ]
# Replace any remaining NA values with 0
deg[is.na(deg)] <- 0

# Create a correlation matrix ---------------------------------------------

M <- cor(deg[, 2:27])
M[M < 0] <- 0

# Carry out a significance test for correlations
testRes <- cor.mtest(deg[, 2:27], conf.level=0.95)

# Plot a correlation matrix of only those correlations that were significant
corrplot(M, p.mat=testRes$p, method="color", type="lower", addCoef.col="black",
         number.cex=0.6, diag=FALSE, tl.col="black", insig="blank", cl.ratio=0.2, tl.srt=45, sig.level=0.05,
         col.lim=c(0, 1), col=colorRampPalette(c("white", "#54b3e7ff", "white", "#f3202cff"))(200))

# Perceptually uniform colour palette
cols <- magma(200)
cols <- cols[200:100]

corrplot(M, p.mat=testRes$p, method="color", type="lower", addCoef.col="black",
         number.cex=0.6, diag=FALSE, tl.col="black", insig="blank", cl.ratio=0.2, tl.srt=45, sig.level=0.05,
         col.lim=c(0, 1), col=cols, is.corr=FALSE)


# Below are individual plots
#out_deg <- out_deg[rowSums(is.na(out_deg)) != ncol(out_deg), ]
#out_deg[is.na(out_deg)] <- 0
#M_out <- cor(out_deg[, 2:14])
#testOut <- cor.mtest(out_deg[, 2:14], conf.level=0.95)
#corrplot(M_out, p.mat=testOut$p, method="color", type="upper", addCoef.col="black",
         #number.cex=0.8, diag=FALSE, tl.col="black", insig="blank", cl.ratio=0.2, tl.srt=45, sig.level=0.05,
         #col.lim=c(-0.2, 1), col=colorRampPalette(c("#DD8D29", "#E2D200", "#DD8D29", "#B40F20"))(100))

#in_deg <- in_deg[rowSums(is.na(in_deg)) != ncol(in_deg), ]
#in_deg[is.na(in_deg)] <- 0
#M_in <- cor(in_deg[, 2:14])
#testIn <- cor.mtest(in_deg[, 2:14], conf.level=0.95)
#corrplot(M_in, p.mat=testIn$p, method="color", type="upper", addCoef.col="black",
         #number.cex=0.8, diag=FALSE, tl.col="black", insig="blank", cl.ratio=0.2, tl.srt=45, sig.level=0.05,
         #col.lim=c(-0.2, 1), col=colorRampPalette(c("#DD8D29", "#E2D200", "#DD8D29", "#B40F20"))(100))


#M_mixed <- M[14:26, 1:13]
#testMix <- cor.mtest(M_mixed, conf.level=0.95)
#corrplot(M_mixed, p.mat=testMix$p, method="color", type="upper", addCoef.col="black",
         #number.cex=0.8, diag=FALSE, tl.col="black", insig="blank", cl.ratio=0.2, tl.srt=45, sig.level=0.05,
         #col.lim=c(-0.2, 1), col=colorRampPalette(c("#DD8D29", "#E2D200", "#DD8D29", "#B40F20"))(100))
