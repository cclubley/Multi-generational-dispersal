# Load the data
load("./Combined/Combined_2000_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2000 <- as.undirected(g)
myFreqs0 <- myFreqs

# Load the data
load("./Combined/Combined_2001_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2001 <- as.undirected(g)
myFreqs1 <- myFreqs

# Load the data
load("./Combined/Combined_2002_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2002 <- as.undirected(g)
myFreqs2 <- myFreqs

# Load the data
load("./Combined/Combined_2003_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2003 <- as.undirected(g)
myFreqs3 <- myFreqs

# Load the data
load("./Combined/Combined_2004_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2004 <- as.undirected(g)
myFreqs4 <- myFreqs

# Load the data
load("./Combined/Combined_2005_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2005 <- as.undirected(g)
myFreqs5 <- myFreqs

# Load the data
load("./Combined/Combined_2006_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2006 <- as.undirected(g)
myFreqs6 <- myFreqs

# Load the data
load("./Combined/Combined_2007_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2007 <- as.undirected(g)
myFreqs7 <- myFreqs

# Load the data
load("./Combined/Combined_2008_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2008 <- as.undirected(g)
myFreqs8 <- myFreqs

# Load the data
load("./Combined/Combined_2009_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2009 <- as.undirected(g)
myFreqs9 <- myFreqs

# Load the data
load("./Combined/Combined_2010_new.RData")
s.dat <- s.dat[-c(171), -c(171)]
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2010 <- as.undirected(g)
myFreqs10 <- myFreqs

# Load the data
load("./Combined/Combined_2011_new.RData")
s.dat <- s.dat[-c(171), -c(171)]
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2011 <- as.undirected(g)
myFreqs11 <- myFreqs

# Load the data
load("./Combined/Combined_2012_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2012 <- as.undirected(g)
myFreqs12 <- myFreqs

# Identify communities
{ceb2000 <- cluster_fast_greedy(g2000)
  ceb2001 <- cluster_fast_greedy(g2001)
  ceb2002 <- cluster_fast_greedy(g2002)
  ceb2003 <- cluster_fast_greedy(g2003)
  ceb2004 <- cluster_fast_greedy(g2004)
  ceb2005 <- cluster_fast_greedy(g2005)
  ceb2006 <- cluster_fast_greedy(g2006)
  ceb2007 <- cluster_fast_greedy(g2007)
  ceb2008 <- cluster_fast_greedy(g2008)
  ceb2009 <- cluster_fast_greedy(g2009)
  ceb2010 <- cluster_fast_greedy(g2010)
  ceb2011 <- cluster_fast_greedy(g2011)
  ceb2012 <- cluster_fast_greedy(g2012)
}

{bridge0 <- as.data.frame(igraph::crossing(ceb2000, g2000))
  bridge1 <- as.data.frame(igraph::crossing(ceb2001, g2001))
  bridge2 <- as.data.frame(igraph::crossing(ceb2002, g2002))
  bridge3 <- as.data.frame(igraph::crossing(ceb2003, g2003))
  bridge4 <- as.data.frame(igraph::crossing(ceb2004, g2004))
  bridge5 <- as.data.frame(igraph::crossing(ceb2005, g2005))
  bridge6 <- as.data.frame(igraph::crossing(ceb2006, g2006))
  bridge7 <- as.data.frame(igraph::crossing(ceb2007, g2007))
  bridge8 <- as.data.frame(igraph::crossing(ceb2008, g2008))
  bridge9 <- as.data.frame(igraph::crossing(ceb2009, g2009))
  bridge10 <- as.data.frame(igraph::crossing(ceb2010, g2010))
  bridge11 <- as.data.frame(igraph::crossing(ceb2011, g2011))
  bridge12 <- as.data.frame(igraph::crossing(ceb2012, g2012))
}

{bridge0 <- filter(bridge0, igraph::crossing(ceb2000,g2000)=="TRUE")
  bridge1 <- filter(bridge1, igraph::crossing(ceb2001,g2001)=="TRUE")
  bridge2 <- filter(bridge2, igraph::crossing(ceb2002,g2002)=="TRUE")
  bridge3 <- filter(bridge3, igraph::crossing(ceb2003,g2003)=="TRUE")
  bridge4 <- filter(bridge4, igraph::crossing(ceb2004,g2004)=="TRUE")
  bridge5 <- filter(bridge5, igraph::crossing(ceb2005,g2005)=="TRUE")
  bridge6 <- filter(bridge6, igraph::crossing(ceb2006,g2006)=="TRUE")
  bridge7 <- filter(bridge7, igraph::crossing(ceb2007,g2007)=="TRUE")
  bridge8 <- filter(bridge8, igraph::crossing(ceb2008,g2008)=="TRUE")
  bridge9 <- filter(bridge9, igraph::crossing(ceb2009,g2009)=="TRUE")
  bridge10 <- filter(bridge10, igraph::crossing(ceb2010,g2010)=="TRUE")
  bridge11 <- filter(bridge11, igraph::crossing(ceb2011,g2011)=="TRUE")
  bridge12 <- filter(bridge12, igraph::crossing(ceb2012,g2012)=="TRUE")
}

{bridge0$nodes <- rownames(bridge0)
  bridge1$nodes <- rownames(bridge1)
  bridge2$nodes <- rownames(bridge2)
  bridge3$nodes <- rownames(bridge3)
  bridge4$nodes <- rownames(bridge4)
  bridge5$nodes <- rownames(bridge5)
  bridge6$nodes <- rownames(bridge6)
  bridge7$nodes <- rownames(bridge7)
  bridge8$nodes <- rownames(bridge8)
  bridge9$nodes <- rownames(bridge9)
  bridge10$nodes <- rownames(bridge10)
  bridge11$nodes <- rownames(bridge11)
  bridge12$nodes <- rownames(bridge12)
}

{bridge0 <- bridge0 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
  bridge1 <- bridge1 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
  bridge2 <- bridge2 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
  bridge3 <- bridge3 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
  bridge4 <- bridge4 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
  bridge5 <- bridge5 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
  bridge6 <- bridge6 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
  bridge7 <- bridge7 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
  bridge8 <- bridge8 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
  bridge9 <- bridge9 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
  bridge10 <- bridge10 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
  bridge11 <- bridge11 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
  bridge12 <- bridge12 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
}

{bridge0$`igraph::crossing(ceb2000, g2000)` <- NULL
  bridge1$`igraph::crossing(ceb2001, g2001)` <- NULL
  bridge2$`igraph::crossing(ceb2002, g2002)` <- NULL
  bridge3$`igraph::crossing(ceb2003, g2003)` <- NULL
  bridge4$`igraph::crossing(ceb2004, g2004)` <- NULL
  bridge5$`igraph::crossing(ceb2005, g2005)` <- NULL
  bridge6$`igraph::crossing(ceb2006, g2006)` <- NULL
  bridge7$`igraph::crossing(ceb2007, g2007)` <- NULL
  bridge8$`igraph::crossing(ceb2008, g2008)` <- NULL
  bridge9$`igraph::crossing(ceb2009, g2009)` <- NULL
  bridge10$`igraph::crossing(ceb2010, g2010)` <- NULL
  bridge11$`igraph::crossing(ceb2011, g2011)` <- NULL
  bridge12$`igraph::crossing(ceb2012, g2012)` <- NULL
}

{bridge0 <- merge(bridge0, myFreqs0, by=c("Release_poly", "Settlement_poly"), all=FALSE)
  bridge1 <- merge(bridge1, myFreqs1, by=c("Release_poly", "Settlement_poly"), all=FALSE)
  bridge2 <- merge(bridge2, myFreqs2, by=c("Release_poly", "Settlement_poly"), all=FALSE)
  bridge3 <- merge(bridge3, myFreqs3, by=c("Release_poly", "Settlement_poly"), all=FALSE)
  bridge4 <- merge(bridge4, myFreqs4, by=c("Release_poly", "Settlement_poly"), all=FALSE)
  bridge5 <- merge(bridge5, myFreqs5, by=c("Release_poly", "Settlement_poly"), all=FALSE)
  bridge6 <- merge(bridge6, myFreqs6, by=c("Release_poly", "Settlement_poly"), all=FALSE)
  bridge7 <- merge(bridge7, myFreqs7, by=c("Release_poly", "Settlement_poly"), all=FALSE)
  bridge8 <- merge(bridge8, myFreqs8, by=c("Release_poly", "Settlement_poly"), all=FALSE)
  bridge9 <- merge(bridge9, myFreqs9, by=c("Release_poly", "Settlement_poly"), all=FALSE)
  bridge10 <- merge(bridge10, myFreqs10, by=c("Release_poly", "Settlement_poly"), all=FALSE)
  bridge11 <- merge(bridge11, myFreqs11, by=c("Release_poly", "Settlement_poly"), all=FALSE)
  bridge12 <- merge(bridge12, myFreqs12, by=c("Release_poly", "Settlement_poly"), all=FALSE)
}

perm <- as.data.frame(2000:2012)
colnames(perm) <- "Year"
perm$Permeability <- NA

perm[1, 2] <- (sum(bridge0$Count)/sum(myFreqs0$Count))
perm[2, 2] <- (sum(bridge1$Count)/sum(myFreqs1$Count))
perm[3, 2] <- (sum(bridge2$Count)/sum(myFreqs2$Count))
perm[4, 2] <- (sum(bridge3$Count)/sum(myFreqs3$Count))
perm[5, 2] <- (sum(bridge4$Count)/sum(myFreqs4$Count))
perm[6, 2] <- (sum(bridge5$Count)/sum(myFreqs5$Count))
perm[7, 2] <- (sum(bridge6$Count)/sum(myFreqs6$Count))
perm[8, 2] <- (sum(bridge7$Count)/sum(myFreqs7$Count))
perm[9, 2] <- (sum(bridge8$Count)/sum(myFreqs8$Count))
perm[10, 2] <- (sum(bridge9$Count)/sum(myFreqs9$Count))
perm[11, 2] <- (sum(bridge10$Count)/sum(myFreqs10$Count))
perm[12, 2] <- (sum(bridge11$Count)/sum(myFreqs11$Count))
perm[13, 2] <- (sum(bridge12$Count)/sum(myFreqs12$Count))

mod <- as.data.frame(2000:2012)
colnames(mod) <- "Year"
mod$Modularity <- NA

mod[1, 2] <- modularity(ceb2000)
mod[2, 2] <- modularity(ceb2001)
mod[3, 2] <- modularity(ceb2002)
mod[4, 2] <- modularity(ceb2003)
mod[5, 2] <- modularity(ceb2004)
mod[6, 2] <- modularity(ceb2005)
mod[7, 2] <- modularity(ceb2006)
mod[8, 2] <- modularity(ceb2007)
mod[9, 2] <- modularity(ceb2008)
mod[10, 2] <- modularity(ceb2009)
mod[11, 2] <- modularity(ceb2010)
mod[12, 2] <- modularity(ceb2011)
mod[13, 2] <- modularity(ceb2012)

# Plot ----------------------------------------------

perm$Year <- as.character(perm$Year)

ggplot()+
  geom_line(perm, mapping=aes(x=Year, y=Permeability, group=1), size=1)+
  ylab("Cluster permeability")+
  cc_theme()

mean(perm$Permeability)
plotrix::std.error(perm$Permeability)

mod$Year <- as.character(mod$Year)

ggplot()+
  geom_line(mod, mapping=aes(x=Year, y=Modularity, group=1), colour="orange", size=2)+
  ylab("Network modularity")+
  ylim(c(0, 1))+
  cc_theme()

mean(mod$Modularity)
plotrix::std.error(mod$Modularity)

coeff <- 1

ggplot()+
  geom_line(mod, mapping=aes(x=Year, y=Modularity, group=1), size=1, colour="#f3202cff")+
  geom_line(perm, mapping=aes(x=Year, y=Permeability*coeff, group=1), size=1, colour="#54b3e7ff") +
  scale_y_continuous(name="Network modularity",
                     sec.axis=sec_axis(~.*coeff, name="Cluster permeability"), limits=c(0, 1))+
  cc_theme()

