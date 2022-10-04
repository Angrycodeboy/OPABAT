library(shiny)
library(tidyverse)
library(ggpubr)
library(visNetwork)
library(png)
library(dqshiny)
library(DT)
library(gsubfn)
library(feather)

##read in necessary tables##
# (1) gene to uni lookup list
db0<-read.csv("data/final/gene_protein_list.csv",header=T,stringsAsFactors = F)

# (2) transposed protein abundance data
dft<-read_feather("data/final/dft_transposed_abundance.feather")
#dft<-read.table("data/final/dft_transposed_abundance.csv",sep=",",row.names=1,header=T,stringsAsFactors = F)

# (3) melted protein abundance data
df2<-read_feather("data/final/df2_melt_abundance.feather")
#df2<-read.delim("data/final/df2_melt_abundance.tsv",header=T,stringsAsFactors = F)

# (4)phenotypic data#
df_p<-read.csv("data/final/physiological.csv",header=T,stringsAsFactors = F)
#names for phenotypic data
names<-c(names(df_p[,13:45]),"age")

# (5) edge info for correlation plot
db<-read.delim("data/final/db_edges.tsv",header=T,stringsAsFactors = F)

# (6) edge info for visnetwork
#all_edges<-read.delim("data/final/all_edges_visnetwork.tsv",header=T,stringsAsFactors = F)

# (7) edge info for visnetwork
all_edges<-read.delim("data/final/all_edges_visnetwork.tsv",header=T,stringsAsFactors = F)

# (8) Corum filtered with accessory proteins
corum_filter<-read.delim("data/final/corum_filter.tsv",header=T,stringsAsFactors = F)
# define corum complex names
corum_names<-corum_filter$identifier

# (9) Corum filtered with accessory proteins, edge table
corum_accessor_p<-read.delim("data/final/corum_accessory_edge.tsv",header=T,stringsAsFactors = F)

#(10) phenotype-protein-correlation table
df_pcor<-read.csv("data/final/pearson_protein_phenotypic.csv",header=T,stringsAsFactors = F)

#(11) strain selection table
df_strain<-read.csv("data/final/strain_selection.csv",header=T,stringsAsFactors = F)
#names for phenotypic data
names1<-unique(df_strain$Parameter)

#(12) human BAT table
df_human_BAT<-read.delim("data/final/human_TPM_mapped.tsv",header=T,stringsAsFactors = F)
df_human_pheno<-read.csv("data/final/human_pheno.csv",header=T,stringsAsFactors = F)
df_human_cor<-read.csv("data/final/human_BAT_cor.csv",header=T,stringsAsFactors = F)
#names for phenotypic data
names_BAT<-sort(colnames(df_human_pheno))
dfp_sig<-read.csv("data/final/pearson_protein_phenotypic_only_sig.csv",header=T,stringsAsFactors = F)






### set up inactivity and authentication##
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

