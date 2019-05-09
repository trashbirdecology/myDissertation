# install.packages('bibliometrix', dependencies=TRUE)
library(bibliometrix)
library(tidyverse)
library(grid)
library(gridExtra)

# HELPER FUNCTIONS --------------------------------------------------------
# A function to repalce plural keywords with singular to avoid redundancy

# Write a function for saving ggplots to permanent directory
saveMyFig <-
  function(p,
           fn,
           type = ".png",
           figDir = NULL,
           forever = FALSE, width=6,
           height=6) {
    if (forever &
        is.null(figDir))
      figDir <-
        here::here("chapterFiles/rdmReview/figures/figsCalledInDiss/")
    if (!forever &
        is.null(figDir))
      figDir <-
        here::here("chapterFiles/rdmReview/figures/tempFigs/")
    suppressWarnings(suppressMessages(dir.create(figDir)))
    
    
    if (any(c("ggplot", "gg", "grob") %in% class(p))) {
      ggsave(
        plot = p,
        filename = paste0(figDir, fn, type),
        width = 6,
        height = 6
      )
      print("gg figure saved")
    }
    
    # struggling with how to save igraph to file without having to open adn close device..
    if (any(c("igraph") %in% class(p))) {
      warning("this plot is an igraph -- please save from plot viewer.")
    }
    
  }

# Load in the WOS results -------------------------------------------------
bibDatDir <-
  here::here(
    "/chapterFiles/rdmReview/biblioMetrixAnalysis/data_regimeShifts_ecology_may2019/"
  )
files <- paste0(bibDatDir, list.files(bibDatDir, pattern = ".txt"))
bib <- NULL

for (i in seq_along(files)) {
  temp <- bibliometrix::readFiles(files[i])
  bib <- c(bib, temp)
}

# Convert plurals to singulars! (this fucks with words like analysiS, but oh well..)
bib.df <- convert2df(bib) # to dataframe
bib.df$DE <- gsub("S\\b", "", bib.df$DE)
bib.df$ID <- gsub("S\\b", "", bib.df$ID)

# Conduct analyses --------------------------------------------------------
results <- biblioAnalysis(bib.df, sep = ";")

# Simple plots ------------------------------------------------------------
# plot # avg citations per year
plot(x = results, k = 10, pause = FALSE)

# Co-citation network -----------------------------------------------------
# classic co-citation network
NetMatrix <-
  biblioNetwork(bib.df,
                analysis = "co-citation",
                network = "references",
                sep = ". ")

# Generate summary stats ----------------------------------------------------
CR <- citations(bib.df, field = "article", sep = ";")

# author dominance ranking
DF <- dominance(results, k = 10)

# Get productivity of top authors
authors = gsub(",", " ", names(results$Authors)[1:10])

indices <-
  Hindex(
    bib.df,
    field = "author",
    elements = authors,
    sep = ";",
    years = 50
  )
# indices$H
topAU <- authorProdOverTime(bib.df, k = 10, graph = TRUE)
png(
  filename = paste0(
    here::here("chapterFiles/rdmReview/figures/tempFigs/"),
    "authorProductivity",
    ".png"
  ),
  width = 800,
  height = 480
)
topAU
dev.off()


# Lotka’s law coefficients for scientific productivity (Lotka A.J., 1926)
L <- lotka(results)

# Author Productivity. Empirical Distribution
L$AuthorProd
L$Beta # Beta coefficient estimate
L$C # constant

# Observed distribution
Observed = L$AuthorProd[, 3]

# P-value of K-S two sample test
L$p.value # when p is > alpha => no difference in prod.

# Theoretical distribution with Beta = 2
Theoretical = 10 ^ (log10(L$C) - 2 * log10(L$AuthorProd[, 1]))

plot(
  L$AuthorProd[, 1],
  Theoretical,
  type = "l",
  col = "red",
  ylim = c(0, 1),
  xlab = "Articles",
  ylab = "Freq. of Authors",
  main = "Scientific Productivity"
)
lines(L$AuthorProd[, 1], Observed, col = "blue")
legend(
  x = "topright",
  c("Theoretical (B=2)", "Observed"),
  col = c("red", "blue"),
  lty = c(1, 1, 1),
  cex = 0.6,
  bty = "n"
)


# Bib network matrices ----------------------------------------------------
A <- cocMatrix(bib.df, Field = "SO", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:10]

## Bipartite networks
# cit_network <-
#   cocMatrix(bib.df, Field = "CR", sep = ".  ") # Citation network
# au_network <-
#   cocMatrix(bib.df, Field = "AU", sep = ".  ") # Author  network
# key_network <-
#   cocMatrix(bib.df, Field = "ID", sep = ";") # keyword network

# Bibliographic coupling --------------------------------------------------
M = metaTagExtraction(bib.df, "CR_SO", sep = ";")
NetMatrix <- biblioNetwork(M,
                           analysis = "co-citation",
                           network = "sources",
                           sep = ";")

figDir <- here::here("chapterFiles/rdmReview/figures/tempFigs/")
png(filename = paste0(figDir, "bibCoupling", ".png"))
networkPlot(
  NetMatrix,
  n = 15,
  Title = "Co-Citation Network",
  type = "auto",
  size.cex = TRUE,
  size = 15,
  remove.multiple = FALSE,
  labelsize = 0.7,
  edgesize = 10,
  edges.min = 5
)
dev.off()

# descriptive analysis of network ^
netstat <- networkStat(NetMatrix)
summary(netstat, k = 10)

# Country collaborations --------------------------------------------------
# Create a country collaboration network
temp <- metaTagExtraction(bib.df, Field = "AU_CO", sep = ";")
NetMatrix <-
  biblioNetwork(temp,
                analysis = "collaboration",
                network = "countries",
                sep = ";")

png(filename = paste0(figDir, "countryCollab", ".png"))
# Plot the network
networkPlot(
  NetMatrix,
  n = dim(NetMatrix)[1],
  Title = "Country Collaboration",
  type = "circle",
  size = TRUE,
  remove.multiple = FALSE,
  labelsize = 0.7,
  cluster = "none"
)
dev.off()

# Co-citation -------------------------------------------------------------
# create network
NetMatrix_cocite <-
  biblioNetwork(bib.df,
                analysis = "co-citation",
                network = "references",
                sep = ";")
# Plot the network
networkPlot(
  NetMatrix_cocite,
  n = 30,
  Title = "Co-Citation Network",
  type = "fruchterman",
  size = T,
  remove.multiple = FALSE,
  labelsize = 0.7,
  edgesize = 3
)

# Keyword co-occurrence ---------------------------------------------------

# Create keyword co-occurrences network
NetMatrix_key <-
  biblioNetwork(bib.df,
                analysis = "co-occurrences",
                network = "keywords",
                sep = ";")

# Plot the network
p_np = networkPlot(
  NetMatrix_key,
  normalize = "association",
  weighted = T,
  n = 30,
  Title = "Keyword Co-occurrences",
  type = "fruchterman",
  size = T,
  edgesize = 5,
  labelsize = 0.7
)

# Co-word analysis --------------------------------------------------------
# Conceptual Structure using keywords (method="CA")
# CS_ca <- conceptualStructure(bib.df,field="ID", method="CA",
#                              minDegree=10, k.max=8, stemming=FALSE,
#                              labelsize=10, documents=10)

# Historical direct citation  ---------------------------------------------
# Create a historical citation network
histResults <- histNetwork(bib.df, min.citations = quantile(bib.df$TC,0.75), sep = ";")
# Plot a historical co-citation network
options(width = 130)
net_hist <- histPlot(
  histResults,
  n = 20,
  # size = 2,
  # labelsize=12,
  size.cex = TRUE,
  arrowsize = 0.5,
  color = TRUE
)
saveMyFig(net_hist, "net_histCite", forever = FALSE)
## EXPORT FROM PLOT VIEWER!!

# Thematic map ------------------------------------------------------------
## Thematic map is a very intuitive plot and we can analyze themes according to the quadrant in which they are placed:
# (1) upper-right quadrant: motor-themes;
# (2) lower-right quadrant: basic themes;
# (3) lower-left quadrant: emerging or disappearing themes;
# (4) upper-left quadrant: very specialized/niche themes.
## see paper Cobo, M. J., L?pez-Herrera, A. G., Herrera-Viedma, E., & Herrera, F. (2011). An approach for detecting, quantifying, and visualizing the evolution of a research field: A practical application to the fuzzy sets theory field. Journal of Informetrics, 5(1), 146-166.

## field = "ID" (author keywords)
Map <- thematicMap(
  bib.df,
  field = "ID",
  n = 250,
  minfreq = 5,
  stemming = FALSE,
  size = 0.5,
  repel = TRUE
)
Map2 <- thematicMap(
  bib.df,
  field = "DE",
  n = 250,
  minfreq = 5,
  stemming = FALSE,
  size = 0.5,
  repel = TRUE
)

# Clusters <-
#   Map$words[order(Map$words$Cluster, -Map$words$Occurrences),]
# (CL <-
#     Clusters %>% group_by(.data$Cluster_Label) %>% top_n(5, .data$Occurrences))

# plot keywords ID (author)
p <- plot(Map$map) + theme_bw() +
  theme(legend.position = "none") +
  labs(tag = "a", title="Author keywords")
# plot keywords DE (isi)
p2 <- plot(Map2$map) + theme_bw() +
  theme(legend.position = "none") +
  labs(tag = "b", title="ISI keywords")

plot <- grid.arrange(p,p2, ncol=1,
             bottom = textGrob("`bibliometrix::thematicMap` parameters: n=250, minfreq=5", x = 1,
                               hjust = 1, gp = gpar(fontface = 3L, fontsize = 9)))
# Create a two-panel plot in file
saveMyFig(p=plot, 
fn="thematicMaps_keywords", 
forever=TRUE)


# How does the clustering of keywords vary with n -------------------------
### FOR ID (isi supplied keywords)
Map2_100 = thematicMap(
  bib.df,
  field = "ID",
  n = 100,
  minfreq = 5,
  stemming = FALSE,
  size = 0.5,
  repel = TRUE
)
Map2_200 = thematicMap(
  bib.df,
  field = "ID",
  n = 200,
  minfreq = 5,
  stemming = FALSE,
  size = 0.5,
  repel = TRUE
)
Map2_250 = thematicMap(
  bib.df,
  field = "ID",
  n = 250,
  minfreq = 5,
  stemming = FALSE,
  size = 0.5,
  repel = TRUE
)
Map2_300 = thematicMap(
  bib.df,
  field = "ID",
  n = 300,
  minfreq = 5,
  stemming = FALSE,
  size = 0.5,
  repel = TRUE
)


# Repeat plot2 for ISI keywords
p2.1 <- plot(Map2_100$map) +theme_bw() +theme(plot.margin=unit(c(.6,0,.6,0),"cm"),legend.position="none") #+ labs(tag="a (n=100)")#+theme(plot.tag.position = "topright") 
p2.2 <- plot(Map2_200$map) +theme_bw() +theme(plot.margin=unit(c(.6,0,.6,0),"cm"),legend.position="none") #+ labs(tag="b  (n=200)") 
p2.3 <- plot(Map2_250$map)+theme_bw()  +theme(plot.margin=unit(c(.6,0,.6,0),"cm"),legend.position="none") #+ labs(tag="c (n=250)")
p2.4 <- plot(Map2_300$map) +theme_bw() +theme(plot.margin=unit(c(.6,0,.6,0),"cm"),legend.position="none") #+ labs(tag="d (n=300)")

require(cowplot)
saveMyFig(
  p = plot_grid(p2.1, p2.2, p2.3, p2.4,
                hjust=-.24,
                labels = c("a (n=100)","b (n=200)","c (n=250)","d (n=300)")),
  fn = "thematicMaps_keywords_nVaries_isi",
  forever = TRUE,
  width = 25,
  height = 12)


### FOR DE (author supplied keywords)
Map2_100 = thematicMap(
  bib.df,
  field = "DE",
  n = 100,
  minfreq = 5,
  stemming = FALSE,
  size = 0.5,
  repel = TRUE
)
  Map2_200 = thematicMap(
    bib.df,
    field = "DE",
    n = 200,
    minfreq = 5,
    stemming = FALSE,
    size = 0.5,
    repel = TRUE
  )
  Map2_250 = thematicMap(
    bib.df,
    field = "DE",
    n = 250,
    minfreq = 5,
    stemming = FALSE,
    size = 0.5,
    repel = TRUE
  )
  Map2_300 = thematicMap(
    bib.df,
    field = "DE",
    n = 300,
    minfreq = 5,
    stemming = FALSE,
    size = 0.5,
    repel = TRUE
  )
  
  Map2_500 = thematicMap(
    bib.df,
    field = "DE",
    n = 500,
    minfreq = 5,
    stemming = FALSE,
    size = 0.5,
    repel = TRUE
  )
  
  # Repeat plot2 for author keywords
  p2.1 <- plot(Map2_100$map) +theme_bw() +theme(plot.margin=unit(c(.6,0,.6,0),"cm"),legend.position="none") #+ labs(tag="a (n=100)")#+theme(plot.tag.position = "topright") 
  p2.2 <- plot(Map2_200$map) +theme_bw() +theme(plot.margin=unit(c(.6,0,.6,0),"cm"),legend.position="none") #+ labs(tag="b  (n=200)") 
  p2.3 <- plot(Map2_250$map)+theme_bw()  +theme(plot.margin=unit(c(.6,0,.6,0),"cm"),legend.position="none") #+ labs(tag="c (n=250)")
  p2.4 <- plot(Map2_300$map) +theme_bw() +theme(plot.margin=unit(c(.6,0,.6,0),"cm"),legend.position="none") #+ labs(tag="d (n=300)")
  
  require(cowplot)
  saveMyFig(
    p = plot_grid(p2.1, p2.2, p2.3, p2.4,
                  hjust=-.24,
                  labels = c("a (n=100)","b (n=200)","c (n=250)","d (n=300)")),
    fn = "thematicMaps_keywords_nVaries_author",
    forever = TRUE,
    width = 25,
    height = 12)
  

