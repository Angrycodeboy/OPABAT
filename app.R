library(shiny)
library(tidyverse)
library(ggpubr)
library(visNetwork)
library(png)
library(dqshiny)
library(DT)
library(gsubfn)
library(shinymanager)

##read in necessary tables##
# (1) gene to uni lookup list
db0<-read.csv("data/final/gene_protein_list.csv",header=T,stringsAsFactors = F)

# (2) transposed protein abundance data
dft<-read.table("data/final/dft_transposed_abundance.csv",sep=",",row.names=1,header=T,stringsAsFactors = F)

# (3) melted protein abundance data
df2<-read.delim("data/final/df2_melt_abundance.tsv",header=T,stringsAsFactors = F)

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



# data.frame with credentials info
credentials <- data.frame(
  user = "dobat",
  password = "dobat",
  # comment = c("protein","physiology"), %>% 
  stringsAsFactors = FALSE
)


  

#shinyServer(function(input, output, session) {})
####### shinny app below####

  ui <-    secure_app(head_auth = tags$script(inactivity), 
                      fluidPage(
    navlistPanel(#widths = c(4, 6),

      #tab 0 Homepage
      tabPanel(titlePanel("Home"),
       
      tags$h1("Welcome to OPABAT!"),
      HTML("Obesity is a polygenic disorder with variable penetrance in the general population. Brown adipose tissue (BAT) is a major regulator of energy expenditure and metabolic physiology 
           due to a specialized proteome that orchestrates futile metabolic cycles, which could be leveraged to treat obesity. However, nearly all mechanistic studies of BAT protein function 
           occur in a single inbred mouse strain, which has limited understanding of generalizable mechanisms of BAT regulation over metabolism. Here we perform deep quantitative multiplexed 
           proteomics of BAT across a cohort of 163 genetically defined Diversity Outbred (DO) mice, a model that parallels the genetic and phenotypic variation found in the human population. 
           Leveraging the high variation afforded by this model, we define the functional architecture of the outbred BAT proteome, comprising 10,479 proteins. In doing so, we assign novel 
           co-operative functions to 2,578 proteins with 780 established protein networks. We demonstrate that this analytic framework enables systematic discovery of regulators of BAT function, 
           exemplified by uncovering SFXN5 and LETMD1 as modulators of UCP1-dependent thermogenesis. We also identify 638 proteins that underlie protection from, or sensitivity to, at least one 
           parameter of metabolic disease. From this basis, we identify the Na+/K+-ATPase α2 subunit as an inhibitor of BAT energy expenditure, that increases adiposity through antagonism of 
           calcium influx-dependent activation of thermogenic effectors. We provide this Outbred Proteomic Architecture of BAT (OPABAT) as a resource to understanding conserved mechanisms of 
           BAT regulation over metabolic physiology."),
      tags$h1("Citation"),
      HTML("<p>If this website is useful to you, please consider citing <a href='https://www.sciencedirect.com/science/article/pii/S0092867420301562?via%3Dihub'> 'space holder-OPABAT' </a>!</p>"),
      
      imageOutput("image_tab1"),
      ),
    #tab 0.5 Tour guide
    tabPanel(titlePanel("Tour Guide"),
             h2("Explore what we have prepared for you on this website!"),
             h4("(Please check cookie settings or switch to Chrome/Firefox if videos cannot be loaded correctly, and use full screen mode if videos have resolution issues on PCs.)"),
             h3("Expression"),
             h5("Select a protein to see its expression level in OPABAT, and color mice by phenotypic data."),
             tags$video(id="video1", width="400px",height="400px",type = "video/mp4",src = "expression.mp4", controls = "controls"),
             h3("Correlations"),
             h5("Select any two proteins to see the correlation between their expression levels in OPABAT."),
             tags$video(id="video2", width="400px",height="400px",type = "video/mp4",src = "correlation.mp4", controls = "controls"),
             h3("Networks"),
             h5("Select a protein to explore its significant immediate correlators in OPABAT network."),
             tags$video(id="video3", width="400px",height="400px",type = "video/mp4",src = "network.mp4", controls = "controls"),
             h3("Complexes"),
             h5("Select a protein or a CORUM core complex to explore new accesory proteins to established protein complexes identified in OPABAT network."),
             tags$video(id="video4", width="400px",height="400px",type = "video/mp4",src = "complex.mp4", controls = "controls"),
             h3("Phenotypes"),
             h5("Explore the positive and negative protein correlators of each physiological parameter."),
             tags$video(id="video5", width="400px",height="400px",type = "video/mp4",src = "phenotypes.mp4", controls = "controls"),
             h3("Strain Selection"),
             h5("Explore strains that are best candidates to model phenotypes."),
             tags$video(id="video5", width="400px",height="400px",type = "video/mp4",src = "strains.mp4", controls = "controls"),
             h3("Human Data"),
             h5("Explore correlations between human phenotypes and BAT transcript abundance of all OPABAT metabolic physiology correlators."),
             tags$video(id="video5", width="400px",height="400px",type = "video/mp4",src = "human data.mp4", controls = "controls")
             #test-tags$img(width="50%",src = "image-in-app.png")
    ),
      #tab 1 protein expression
      tabPanel(
        titlePanel("Expression"),
        textOutput("expression_description"),
        # input gene
        autocomplete_input(id="gene_tab1", 
                           label="Gene", 
                           options=db0$gene, 
                           value = "", 
                           width = NULL,
                           placeholder = "Type in then select an official gene symbol", 
                           max_options = 0, 
                           hide_values = FALSE,
                           create = T),
        
        # select_phenotype
        selectInput(
          inputId="dropdown_tab1",
          label="Color mouse by",
          choices=names,
          selected = "body.weight",
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        
        actionButton("go_tab1", "see expression",class = "btn-success"),
        textOutput("expression_text"),
        plotOutput(outputId="expression_plot",
                   width = "100%",
                   height = "400px",
                   click = NULL,
                   dblclick = NULL,
                   hover = NULL,
                   brush = NULL,
                   inline = FALSE)
        ),
      
      #tab 2 protein-protein correlations
      tabPanel(
        titlePanel("Correlations"),
        autocomplete_input(id="gene1", 
                           label="Gene1", 
                           options=db0$gene, 
                           value = "", 
                           width = NULL,
                           placeholder = "Type in then select an official gene symbol", 
                           max_options = 0, 
                           hide_values = FALSE,
                           create = T),
        autocomplete_input(id="gene2", 
                           label="Gene2", 
                           options=db0$gene, 
                           value = "", 
                           width = NULL,
                           placeholder = "Type in then select an official gene symbol", 
                           max_options = 0, 
                           hide_values = FALSE,
                           create = T),
        actionButton("go", "see correlation",class = "btn-success"),
        textOutput("network"),
        plotOutput(outputId="plot",
                   width = "400px",
                   height = "400px",
                   click = NULL,
                   dblclick = NULL,
                   hover = NULL,
                   brush = NULL,
                   inline = FALSE)
      ),
      
      #tab 3 protein networks
      tabPanel(titlePanel("Networks"),
               autocomplete_input(id="gene_tab3", 
                                  label="Gene", 
                                  options=db0$gene, 
                                  value = "", 
                                  width = NULL,
                                  placeholder = "Type in then select an official gene symbol", 
                                  max_options = 0, 
                                  hide_values = FALSE,
                                  create = T),
          actionButton("go_tab3", "see correlator network",class = "btn-success"),
          textOutput("text_tab3"),
          visNetworkOutput("plot_tab3",width = "90%", height = "600px"),
          imageOutput("image_tab3"),
          ),
    
      #tab 4 complexes
      tabPanel(titlePanel("Complexes"),
               tabsetPanel( id = 'tab4',
                            tabPanel("Protein-centric view",
                                     autocomplete_input(id="gene_tab4a", 
                                                        label="Gene", 
                                                        options=db0$gene, 
                                                        value = "", 
                                                        width = NULL,
                                                        placeholder = "Type in then select an official gene symbol", 
                                                        max_options = 0, 
                                                        hide_values = FALSE,
                                                        create = T),
                                     actionButton("go_tab4a", "see complex assignment",class = "btn-success"),
                                     textOutput("text_tab4a"),
                                     tabsetPanel( 
                                       id = 'table_tab4',
                                       tabPanel("Accessory to these complexes in OPABAT", DT::dataTableOutput("accessory_table",width = '100%')),
                                       tabPanel("Subunit of these complexes in CORUM", DT::dataTableOutput("subunit_table",width = '100%'))
                                     )),
                            tabPanel("Complex-centric view",
               selectizeInput(
                 inputId="dropdown_tab4",
                 label="Select or backspace and type in a Corum complex to see new accessory proteins in  OPABAT",
                 choices=corum_names,
                 selected = "181_26S proteasome",
                 multiple = FALSE,
                 #selectize = TRUE,
                 width = NULL,
                 size = NULL
               ),
             actionButton("go_tab4", "see complex accessory proteins",class = "btn-success"),
             textOutput("text_tab4"),
             visNetworkOutput("plot_tab4",width = "90%", height = "600px"),
             imageOutput("image_tab4")
          ))),
      
      #tab 5 phenotypes
      tabPanel(titlePanel("Phenotypes"),
               tabsetPanel( id = 'tab5',
                            tabPanel("Top protein correlators",
               selectizeInput(
                 inputId="dropdown_tab5",
                 label="Select a physiological parameter to see top protein correlators",
                 choices=names[!names%in%"age"],
                 selected = "body.weight",
                 multiple = FALSE,
                 #selectize = TRUE,
                 width = NULL,
                 size = NULL
               ),
               actionButton("go_tab5", "see top protein correlators",class = "btn-success"),
               textOutput("text_tab5"),
               tabsetPanel( 
                 id = 'table_tab5',
                 tabPanel("positive correlators", DT::dataTableOutput("pos_table")),
                 tabPanel("negative correlators", DT::dataTableOutput("neg_table"))
               )),
                          tabPanel("Protein-physiological parameter correlation",
               # input gene
               autocomplete_input(id="gene_tab5a", 
                                  label="Type in then select a gene to see correaltion plots", 
                                  options=db0$gene, 
                                  value = "", 
                                  width = NULL,
                                  placeholder = "Type in then select an official gene symbol", 
                                  max_options = 0, 
                                  hide_values = FALSE,
                                  create = T),
               
               # select_phenotype
               selectInput(
                 inputId="dropdown_tab5a",
                 label="Select a physiological parameter",
                 choices=names[!names%in%"age"],
                 selected = "body.weight",
                 multiple = FALSE,
                 selectize = TRUE,
                 width = NULL,
                 size = NULL
               ),
               
               actionButton("go_tab5a", "see correlation plot",class = "btn-success"),
               textOutput("text_tab5a"),
               plotOutput(outputId="plot_tab5a",
                          width = "400px",
                          height = "400px",
                          click = NULL,
                          dblclick = NULL,
                          hover = NULL,
                          brush = NULL,
                          inline = FALSE)
               
               ))),
    
    #tab 6 strain_selection
    tabPanel(titlePanel("Strain Selection"),
             HTML("<p> Please <a href='https://churchilllab.jax.org/qtlviewer/Chouchani/OPABAT'> click here </a> for full protein and phenotype QTL mapping results."),
             tabsetPanel( id = 'tab6',
                          tabPanel("Stran selection for phenotypes",
                          selectizeInput(
                            inputId="dropdown_tab6",
                            label="Select a physiological parameter to see top mouse strain candidates",
                            choices=names1,
                            selected = "VO2 cold/day",
                            multiple = FALSE,
                            #selectize = TRUE,
                            width = NULL,
                            size = NULL
                          ),
                          actionButton("go_tab6", "see strain candidates",class = "btn-success"),
                          textOutput("text_tab6"),
                          tabsetPanel( 
                            id = 'table_tab6',
                            tabPanel("Homozygotes ranked by negative to positive contributions to a phenotype", DT::dataTableOutput("strain_table"))
                          )))),
    
    #tab 7 Human data
    tabPanel(titlePanel("Human Data"),
             tabsetPanel( id = 'tab7',
                          tabPanel("Transcripts of OPABAT correlators mapped in human SCVAT",
                                   selectizeInput(
                                     inputId="dropdown_tab7",
                                     label="Select a physiological parameter to see top transcript correlators",
                                     choices=names_BAT[!names_BAT%in%"Study.ID"],
                                     selected = "BMI",
                                     multiple = FALSE,
                                     #selectize = TRUE,
                                     width = NULL,
                                     size = NULL
                                   ),
                                   actionButton("go_tab7", "see top transcript correlators",class = "btn-success"),
                                   textOutput("text_tab7"),
                                   tabsetPanel( 
                                     id = 'table_tab7',
                                     tabPanel("human SCVAT transcript correlators", DT::dataTableOutput("human_table"))
                                   )),
                          tabPanel("Transcript-phenotype correlation plots",
                                   # input gene
                                   autocomplete_input(id="gene_tab7a", 
                                                      label="Type in then select a gene to see correaltion plots", 
                                                      options=df_human_BAT$Gene.symbol,
                                                      value = "", 
                                                      width = NULL,
                                                      placeholder = "Type in then select an official gene symbol", 
                                                      max_options = 0, 
                                                      hide_values = FALSE,
                                                      create = T),
                                   
                                   # select_phenotype
                                   selectInput(
                                     inputId="dropdown_tab7a",
                                     label="Select a physiological parameter",
                                     choices=names_BAT[!names_BAT%in%"Study.ID"],
                                     selected = "BMI",
                                     multiple = FALSE,
                                     selectize = TRUE,
                                     width = NULL,
                                     size = NULL
                                   ),
                                   
                                   actionButton("go_tab7a", "see correlation plot",class = "btn-success"),
                                   textOutput("text_tab7a"),
                                   plotOutput(outputId="plot_tab7a",
                                              width = "400px",
                                              height = "400px",
                                              click = NULL,
                                              dblclick = NULL,
                                              hover = NULL,
                                              brush = NULL,
                                              inline = FALSE)
                                   
                          ))),
    
    
      
      #tab 8 Download
      tabPanel(titlePanel("Download"),
  
               textOutput("download1"),
               tags$head(tags$style("#download1{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               a(href="(1) Protein_expression.csv", "Download protein expression", download=NA, target="_blank"),
               
               textOutput("download2"),
               tags$head(tags$style("#download2{color: black;
                                 font-size: 20px;
                                
                                 }"
               )),
               a(href="(2) Mouse_lookup_table.csv", "Download look-up table", download=NA, target="_blank"),
               
               textOutput("download3"),
               tags$head(tags$style("#download3{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               a(href="(3) Edges_in_DOBAT_network.csv", "Download edges", download=NA, target="_blank"),
               
               
               textOutput("download4"),
               tags$head(tags$style("#download4{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               a(href="(4) New_accessory_proteins_to_corum_complexes.tsv", "Download complex accessory proteins", download=NA, target="_blank"),
               
               textOutput("download5"),
               tags$head(tags$style("#download5{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               a(href="(5) New_accessory_proteins_to_KEGG_pathways.csv", "Download pathway accessory proteins", download=NA, target="_blank"),
               
               textOutput("download6"),
               tags$head(tags$style("#download6{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               a(href="(6) Physiological_data.csv", "Download phenotypic data", download=NA, target="_blank"),
               
               textOutput("download7"),
               tags$head(tags$style("#download7{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               a(href="(7) Protein_physiological_data_correlations.csv", "Download protein-phenotype correlation", download=NA, target="_blank"),
               
               textOutput("download8"),
               tags$head(tags$style("#download8{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               a(href="(8) Strains_for_phenotypes.csv", "Download strains mapping for phenotypes", download=NA, target="_blank"),
               
               textOutput("download9"),
               tags$head(tags$style("#download9{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               a(href="(9) Phenotype_QTLs.csv", "Download phenotype QTLs", download=NA, target="_blank"),
               
               textOutput("download10"),
               tags$head(tags$style("#download10{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               a(href="(10) Protein_QTLs.csv", "Download pQTLs", download=NA, target="_blank"),
               
               textOutput("download11"),
               tags$head(tags$style("#download11{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               a(href="(11) Human_SCVAT.csv", "Download human SCVAT transcript-phenotype correlations", download=NA, target="_blank"),
               
               textOutput("download12"),
               tags$head(tags$style("#download12{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               a(href="(12) Human_SAT.csv", "Download human SAT transcript-phenotype correlations", download=NA, target="_blank"),
               
               textOutput("download13"),
               tags$head(tags$style("#download13{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               a(href="(13) Disease_networks.csv", "Download disease networks", download=NA, target="_blank")
               
),
     
      
      #tab 9 Acknowledgments
tabPanel(titlePanel("Acknowledgment"),
         tags$h1("Acknowledgement"),
         HTML("<p> This project is a collaboration across the <a href='https://chouchanilab.dana-farber.org/'> Chouchani</a>, 
               <a href='https://gygi.hms.harvard.edu/index.html'> Gygi</a>,
               <a href='http://churchill-lab.jax.org/website/'> Churchill</a>,
                <a href='https://spiegelmanlab.dana-farber.org'> Spiegelman</a>,
                <a href='https://calicolabs.com/people/fiona-mcallister-ph-d'> McAllister</a>,
                <a href='https://www.calicolabs.com/people/nick-van-bruggen-ph-d'> van Bruggen</a>,
               <a href='https://nutrition.ucdavis.edu/people/maria-chondronikola'> Chondronikola</a>,
               <a href='http://evanrosenlab.com'> Rosen</a>,
               <a href='https://cohenlab.rockefeller.edu'> Cohen</a>,
               <a href='https://www.tsailab.com/research'> Tsai</a>,
               <a href='https://yhtsenglab.org'> Tseng</a>,
                    and <a href='https://bankslab.com'> Banks</a> labs at 
                    <a href='https://www.dana-farber.org'> Dana-Farber Cancer Institute</a>,
                    <a href='https://rockefeller.edu'> The Rockefeller University</a>,
                    <a href='https://www.bidmc.org'> Beth Israel Deaconess Medical Center</a>,
                    <a href='https://www.jax.org'> The Jackson Laborotory</a>,
                     <a href='https://www.ucdavis.edu'> UC Davis</a>,
                     <a href='https://www.joslin.org'> Joselin Diabetes Center</a>,
                     and <a href='https://hms.harvard.edu'> Harvard Medical School</a>,
                    along with <a href='https://www.calicolabs.com'> Calico Life Sciences LLC</a>."),
         HTML("<p> The OPABAT web application is developed by Haopeng Xiao and Jiaming Li from the <a href='https://chouchanilab.dana-farber.org/'> Chouchani</a> and
         <a href='https://gygi.hms.harvard.edu/index.html'> Gygi</a> labs. The website is maintained by Nathan Bulloch. Source codes are deposited on GitHub (<a href='https://github.com/Angrycodeboy'>https://github.com/Angrycodeboy/OPABAT</a>).
              The full visualization of <a href='https://churchilllab.jax.org/qtlviewer/Chouchani/OPABAT'> QTL mapping</a> is developed by Matthew Vincent from the <a href='http://churchill-lab.jax.org/website/'> Churchill lab</a>."),
         HTML("<p> This project is funded by <a href='https://www.calicolabs.com'> Calico Life Sciences LLC</a> and 
                    <a href='https://www.nih.gov'> National Institute of Health</a>."),
         HTML("<p> Please contact Haopeng Xiao (haopeng_xiao@dfci.harvard.edu) for bug report.")
         
               
      )
    )
  )
)

  
  ###### server below ######
  server <- function(input, output, session) {
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    output$res_auth <- renderPrint({
      reactiveValuesToList(result_auth)
    })
    # below is the traditional app server
    
    ##below is for tab0
    output$image_tab1 <- renderImage({
      list(src="data/final/OPABAT.png",width=240,height=180)
    },
    deleteFile = FALSE)
    
    ##below is for tab1
    re_tab1 <-eventReactive( input$go_tab1,{input$gene_tab1})
    output$expression_text<-renderText({
      gene_text1<-toupper(re_tab1())
      uni_text1<-db0[db0$gene==gene_text1,]$uni
      if(length(uni_text1)==0){
        print("Please click to select an official gene symbol from the typing helper menu.")
      }else{
      protein_text1<-dft[,uni_text1]
      length_text1<-length(protein_text1[!is.na(protein_text1)])
      if(length_text1!=0){
        paste0(gene_text1, " was quantified in ", length_text1," out of 163 DO mice BAT.")
      }else{
        paste0(gene_text1, " was not quantified.")}
    }})
    
    output$expression_plot<-renderPlot({
      gene_plot1<-toupper(re_tab1())
      pheno<-input$dropdown_tab1
      uni_plot1<-db0[db0$gene==gene_plot1,]$uni
      if(length(uni_plot1)==0){
        print("Please use official gene symbol found on UniProt (uniprot.org).")
      }else{
      df_plot1<-df2[df2$uni==uni_plot1,]
      df_plot1$Sample_ID1<-paste0(df_plot1$M,"_",df_plot1$ID)
      df_p_plot1<-df_p[,c("Sample_ID1",pheno)]
      data_plot1<-left_join(df_plot1,df_p_plot1,by="Sample_ID1")
      
      if (pheno=="age"){
        ggplot(data_plot1,aes(x=ID,y=value))+
        geom_point(aes(color=get(pheno)))+
        scale_color_manual(values=c("blue","gold"))+
        geom_line(data=data_plot1[!is.na(data_plot1$value),])+
        ylab("rel. protein abundance (log2 TMT S/N (sample/bridge)")+
        xlab("mouse ID")+
        labs(color=pheno)+
        theme_bw()
      }else{
        ggplot(data_plot1,aes(x=ID,y=value))+
          geom_point(aes(color=get(pheno)))+
          scale_color_gradient(low="blue",high="gold")+
          geom_line(data=data_plot1[!is.na(data_plot1$value),])+
          ylab("rel. protein abundance (log2 TMT S/N (sample/bridge)")+
          xlab("mouse ID")+
          labs(color=pheno)+
          theme_bw()
      }
        
    }}, res = 96)

    
    ## below is for tab2
    re <-eventReactive( input$go,{c(isolate(input$gene1),isolate(input$gene2))})
    output$network<-renderText({
      members<-toupper(re())
      #test members<-c("UCP1","LETMD1")
      edges<-db
      edges$count<-ifelse((edges$gene_final1 %in% members)&(edges$gene_final2 %in% members),1,0)
      edges<-edges[(edges$count!=0),]
      if(nrow(edges)!=0){
        paste0("Edge exist in OPABAT correlation network, ","FDR q=",signif(edges$qval,digits=2),", ","mouse n=",edges$n)
      }else{
        paste0("Edge not included in OPABAT network, either due to the edge not existing or not passing FDR cutoff (q>0.05, |r|>0.75, n>50)")}
    })
    
    output$plot <- renderPlot({
      x1<-toupper(re()[1])
      x2<-toupper(re()[2])
      uni1<-db0[db0$gene==x1,]$uni
      uni2<-db0[db0$gene==x2,]$uni
      ggscatter(dft, x = uni1, y = uni2, 
                add = "reg.line", conf.int = TRUE, 
                conf.int.level = 0.95,
                cor.coef = TRUE, cor.method = "pearson",
                color = "black", 
                alpha=0.65,
                add.params = list(color = "darkred",
                                  fill = "gray"),
                xlab = paste0("rel. abundance of ",x1), ylab = paste0("rel. abundance of ",x2))
    }, res = 96)
    
    ## below is for tab3
    re_tab3 <-eventReactive(input$go_tab3,{input$gene_tab3})
    output$text_tab3<-renderText({
      gene_text3<-toupper(re_tab3())
      edge_extract1<-all_edges[all_edges$from==gene_text3,]
      edge_extract2<-all_edges[all_edges$to==gene_text3,]
      length_text3<-length(c(edge_extract1$to,edge_extract2$from))
      if(length_text3==0){
        paste0(gene_text3," has no significant correlator.")
      }else{
      paste0(gene_text3,"'s first-degree neighboring network contains ",length_text3," significant correlators. 
             Zoom in and hover over nodes to see gene symbol and protein description. Hover over edges to see 
             correlation strength and evidence of protein-protein interaction in the literature. Reload the page
             if very large networks slow down the website.")}
    })
    output$plot_tab3 <- renderVisNetwork({
      gene_plot3<-toupper(re_tab3())
      edge_extract1<-all_edges[all_edges$from==gene_plot3,]
      edge_extract2<-all_edges[all_edges$to==gene_plot3,]
      edges<-rbind(edge_extract1,edge_extract2)%>%arrange(desc(value))
      if(nrow(edges)==0){
        nodes<-as.data.frame(NULL)
        edges<-as.data.frame(NULL)
        visNetwork(nodes, edges)
        }else{
      nodes<-unique(c(edges$from,edges$to))
      nodes<-as.data.frame(nodes)%>%dplyr::rename("label"="nodes")
      nodes$id<-nodes$label
      nodes$shape<-"circle"
      nodes<-left_join(nodes,db0[,4:5],by=c("label"="gene"))
      nodes<-nodes%>%dplyr::rename("title"="Description")
      nodes<-nodes%>%mutate(color=case_when(
        label==gene_plot3~"darkred",
        TRUE~"orange"
      ))
      edges<-edges[,-3]
      edges$width<-nrow(edges)/30
      nodes$value<-nrow(edges)/8
      nodes<-nodes%>%arrange(id)
      nodes$font.color<-"black"
      visNetwork(nodes, edges)%>%
        visIgraphLayout(layout = "layout_in_circle") %>%
        visNodes(size = nodes$value,label=nodes$label) %>%
        visOptions(highlightNearest = list(enabled = T, hover = T), 
                   nodesIdSelection = T)
    }
        })
    output$image_tab3 <- renderImage({
      gene_image3<-toupper(re_tab3())
      edge_extract1<-all_edges[all_edges$from==gene_image3,]
      edge_extract2<-all_edges[all_edges$to==gene_image3,]
      edges<-rbind(edge_extract1,edge_extract2)
      if(nrow(edges)==0){
        list(src="data/final/Screen Shot 2021-07-20 at 11.17.42 AM.png",width=100,height=50)
      }else{
        list(src="data/final/legends_network4.png",width=320,height=240)
      }
    },
    deleteFile = FALSE)
    
    ## below is for tab4
    re_tab4a <-eventReactive(input$go_tab4a,{input$gene_tab4a})
    output$text_tab4a<-renderText({
      #for test- gene_text4a<-"PSMC1"
      gene_text4a<-toupper(re_tab4a())
      #test gene_text4a<-"UCP1"
      corum_tab4a<-corum_filter
      corum_tab4a$subunits.Gene.name.<-str_split(corum_tab4a$subunits.Gene.name.,";")
      corum_tab4a$protein<-str_split(corum_tab4a$protein,";")
      length_subunits<-NULL
      length_protein<-NULL
      for(i in 1:nrow(corum_tab4a)){
        subunits_tab4a<-unlist(corum_tab4a[i,]$subunits.Gene.name.)
        if ( gene_text4a %in% subunits_tab4a){
          if(length(length_subunits)==0){
            length_subunits<-i
          }else{
            length_subunits<-c(length_subunits,i)
          }
        }else{
          next
        }
      }
      for(j in 1:nrow(corum_tab4a)){
        protein_tab4a<-unlist(corum_tab4a[j,]$protein)
        if ( gene_text4a %in% protein_tab4a){
          if(length(length_protein)==0){
            length_protein<-j
          }else{
            length_protein<-c(length_protein,j)
          }
        }else{
          next
        }
      }
      paste0(gene_text4a, " has been newly identified accessory to ", length(length_protein), " CORUM complexes, and it is a subunit of ", length(length_subunits), " established CORUM core complexes." )
    })
    
    output$subunit_table <- DT::renderDataTable({
      gene_text4a<-toupper(re_tab4a())
      #test gene_text4a<-"PSMC1"
      corum_tab4a<-corum_filter
      corum_tab4a$subunits.Gene.name.<-str_split(corum_tab4a$subunits.Gene.name.,";")
      corum_tab4a$protein<-str_split(corum_tab4a$protein,";")
      length_subunits<-NULL
      #length_protein<-NULL
      for(i in 1:nrow(corum_tab4a)){
        subunits_tab4a<-unlist(corum_tab4a[i,]$subunits.Gene.name.)
        if ( gene_text4a %in% subunits_tab4a){
          if(length(length_subunits)==0){
            length_subunits<-i
          }else{
            length_subunits<-c(length_subunits,i)
          }
        }else{
          next
        }
      }
      if(length(length_subunits)>0){
      sub_table<-corum_filter[length_subunits, ][,c(2,3,4,5,6,9,11)]
      sub_table$p_adj_BH<-gsubfn("([0-9.]+)", ~format(round(as.numeric(x), 2), nsmall=2), sub_table$p_adj_BH)
      sub_table$p_adj_BH<-gsubfn("([0-9.]+)", ~format(round(as.numeric(x), 2), nsmall=0), sub_table$p_adj_BH)
      rownames(sub_table)<-NULL
      sub_table$subunits.Gene.name.<-gsub(";"," ",sub_table$subunits.Gene.name.)
      sub_table$protein<-gsub(";"," ",sub_table$protein)
      sub_table$p_adj_BH<-gsub(";"," ",sub_table$p_adj_BH)
      sub_table<-sub_table%>%
        dplyr::rename("CORUM.ID"="ComplexID",
                      "New.Accessory.Count"="new_member",
                      "Subunit.Count"="subunits_count",
                      "Subunits"="subunits.Gene.name.",
                      "New.Accessory.Proteins"="protein",
                      "p.adj.BH"="p_adj_BH")
      DT::datatable( sub_table, options = list(autoWidth = TRUE,
                                               columnDefs = list(list(targets=c(3), visible=TRUE, width='300px'),
                                                                 list(targets=c(4), visible=TRUE, width='300px'),
                                                                 list(targets=c(5), visible=TRUE, width='300px'),
                                                                 list(className = 'dt-center',targets = c(1,2,6,7)))))
      }else{
        DT::datatable(NULL)
      }
    })
    
    output$accessory_table <- DT::renderDataTable({
      gene_text4a<-toupper(re_tab4a())
      #test gene_text4a<-"PSMC1"
      corum_tab4a<-corum_filter
      corum_tab4a$subunits.Gene.name.<-str_split(corum_tab4a$subunits.Gene.name.,";")
      corum_tab4a$protein<-str_split(corum_tab4a$protein,";")
      length_protein<-NULL
      for(j in 1:nrow(corum_tab4a)){
        protein_tab4a<-unlist(corum_tab4a[j,]$protein)
        if ( gene_text4a %in% protein_tab4a){
          if(length(length_protein)==0){
            length_protein<-j
          }else{
            length_protein<-c(length_protein,j)
          }
        }else{
          next
        }
      }
      if(length(length_protein)>0){
        acc_table<-corum_filter[length_protein, ][,c(2,3,4,5,6,9,11)]
        acc_table$p_adj_BH<-gsubfn("([0-9.]+)", ~format(round(as.numeric(x), 2), nsmall=2), acc_table$p_adj_BH)
        acc_table$p_adj_BH<-gsubfn("([0-9.]+)", ~format(round(as.numeric(x), 2), nsmall=0), acc_table$p_adj_BH)
        rownames(acc_table)<-NULL
        acc_table$subunits.Gene.name.<-gsub(";"," ",acc_table$subunits.Gene.name.)
        acc_table$protein<-gsub(";"," ",acc_table$protein)
        acc_table$p_adj_BH<-gsub(";"," ",acc_table$p_adj_BH)
        acc_table<-acc_table%>%
          dplyr::rename("CORUM.ID"="ComplexID",
                        "New.Accessory.Count"="new_member",
                        "Subunit.Count"="subunits_count",
                        "Subunits"="subunits.Gene.name.",
                        "New.Accessory.Proteins"="protein",
                        "p.adj.BH"="p_adj_BH")
        DT::datatable( acc_table, options = list(autoWidth = TRUE,
                                                 columnDefs = list(list(targets=c(3), visible=TRUE, width='300px'),
                                                                   list(targets=c(4), visible=TRUE, width='300px'),
                                                                   list(targets=c(5), visible=TRUE, width='300px'),
                                                                   list(className = 'dt-center',targets = c(1,2,6,7)))))
      }else{
        DT::datatable(NULL)
      }
    })
    
    
    re_tab4 <-eventReactive(input$go_tab4,{input$dropdown_tab4})
    output$text_tab4<-renderText({
      #for test- gene_text4<-"1895_mTOR complex (MTOR, RICTOR, MLST8)"
      gene_text4<-re_tab4()
      new_member_tab4<-corum_filter[corum_filter$identifier==gene_text4,]$new_member
      paste0("Corum complex ",gene_text4," has ",new_member_tab4, " new accessory proteins identified in OPABAT network. 
             Networks with over 80 members may take longer to show up. Reload the page if very large networks slow down 
             the website.")
    })
    output$plot_tab4 <- renderVisNetwork({
      gene_plot4<-re_tab4()
      #test gene_plot4<-"1895_mTOR complex (MTOR, RICTOR, MLST8)"
      complex_select<-corum_filter%>%filter(identifier==gene_plot4)%>%
        unite("components",c("subunits.Gene.name.","protein"),sep=";",remove=F)
      members_plot4<-unique(unlist(str_split(complex_select[,4],";")))
      complex_member_plot4<-unique(unlist(str_split(complex_select[,5],";")))
      new_members_plot4<-unique(unlist(str_split(complex_select[,6],";")))
      complex_name_plot4<-complex_select$identifier
      # extract edges
      ## construct corum complex edges
      pairs_plot4<-as.data.frame(expand.grid(complex_member_plot4, complex_member_plot4))%>%
        mutate(duplicate=case_when(
          Var1==Var2 ~ 1,
          TRUE ~ 2
        ))%>%filter(duplicate==2)%>%
        dplyr::select(-duplicate)
      # extract edges sorted by alphabetic orders and add features for plot
      temp<-pairs_plot4
      temp_t<-as.data.frame(t(temp))
      temp_t2<- as.data.frame(apply(temp_t, 2, sort))
      temp1<-as.data.frame(t(temp_t2))%>%unite("edge",c("V1","V2"))
      temp2<-temp1%>%distinct(edge)
      temp2<-temp2%>%
        mutate( color="darkred",
               title="interaction between corum complex members",
               smooth=TRUE,
               dashes=FALSE,
               value=1)%>%
        separate("edge",c("from","to"),sep="_",remove=F)%>%
        dplyr::rename("label"="edge")
      ## construct all edges
      pairs_plot4b<-as.data.frame(expand.grid(members_plot4, members_plot4))%>%
        mutate(duplicate=case_when(
          Var1==Var2 ~ 1,
          TRUE ~ 2
        ))%>%filter(duplicate==2)%>%
        dplyr::select(-duplicate)
      ## extract edges sorted by alphabetic orders, then join table with DO
      temp<-pairs_plot4b
      temp_t<-as.data.frame(t(temp))
      temp_t2<- as.data.frame(apply(temp_t, 2, sort))
      temp1<-as.data.frame(t(temp_t2))%>%unite("edge",c("V1","V2"))
      temp3<-inner_join(all_edges,temp1,by=c("label"="edge"))%>%
        mutate(color="orange",
               dashes=FALSE)
      ##construct final edge table
      edges_plot4<-rbind(temp2,temp3)
      edges_plot4<-edges_plot4[!duplicated(edges_plot4$label),]%>%select(-value,-label)
    ##construct node table##
      # for corum members#
      nodes_members_plot4 <- data.frame(id = complex_member_plot4,
                          label = complex_member_plot4,
                          color = "darkred")
      nodes_members_plot4_1<-left_join(nodes_members_plot4,db0,by=c("id"="gene"))%>%
        select(id,label,color,Description)%>%
        mutate(title = paste0("<p><b>", "corum complex member"," ",Description, "</b><br>"))%>%
        select(-Description)
      # for accessory proteins#
      nodes_accessory_plot4 <- data.frame(id = new_members_plot4,
                                        label = new_members_plot4,
                                        color = "darkred",
                                        identifier=complex_name_plot4)%>%
        unite("match",c("identifier","label"),remove=F)
      nodes_accessory_plot4a<-left_join(nodes_accessory_plot4,corum_accessor_p,by="match")%>%
        mutate( color="orange")%>%
        select(id,label,color,p_adj_BH)
      nodes_accessory_plot4a1<-left_join(nodes_accessory_plot4a,db0,by=c("id"="gene"))%>%
        select(id,label,color,Description,p_adj_BH)%>%
        mutate(title = paste0("<p><b>", "P_adj association= ", round(p_adj_BH,digits=3)," ",Description, "</b><br>"))%>%
        select(-Description,-p_adj_BH)
      
      node_plot4<-rbind(nodes_members_plot4_1,nodes_accessory_plot4a1)
      node_plot4$font.color<-"black"
      
      #plot
      #edges_plot4$width<-nrow(edges_plot4)/10
      #node_plot4$value<-nrow(edges_plot4)/8
      visNetwork(node_plot4, edges_plot4)%>%
        visIgraphLayout(layout = "layout_in_circle") %>%
        visNodes(size = node_plot4$value,label=node_plot4$label) %>%
        visOptions(highlightNearest = list(enabled = T, hover = T), 
                   nodesIdSelection = T)
      
    })
    output$image_tab4 <- renderImage({
      gene_image4<-re_tab4()
        list(src="data/final/legends_corum.png",width=300,height=70)
    },
    deleteFile = FALSE)
    
    #below is for tab5#
    re_tab5 <-eventReactive( input$go_tab5,{input$dropdown_tab5})
    output$text_tab5<-renderText({
      pheno_tab5<-re_tab5()
     # test pheno_tab5<-"body.weight"
      df_tab5<-df_pcor[,c("Uniprot.Accession","Gene.Symbol","Mouse.Number",pheno_tab5,"Protein.Description")]%>%
        dplyr::rename("PearsonR"=all_of(pheno_tab5))%>%
        dplyr::filter(PearsonR >=0.4|PearsonR <=-0.4)
      cor_number_tab5<-nrow(df_tab5)
      paste0("Physiological parameter ",pheno_tab5," has ",cor_number_tab5, " significant protein correlators (Pearson r ≥ 0.4 or ≤ -0.4, p<0.05, Mouse Number ≥ 81).")
      })
    output$pos_table <- DT::renderDataTable({
      pheno_tab5_a<-re_tab5()
      #test pheno_tab5_a<-"body.weight"
      pos<-df_pcor[,c("Uniprot.Accession","Gene.Symbol","Mouse.Number",pheno_tab5_a,"Protein.Description")]%>%
        dplyr::rename("PearsonR"=all_of(pheno_tab5_a))%>%
        dplyr::filter(PearsonR >=0.4)%>%
        arrange(desc(PearsonR))%>%
        mutate(PearsonR=round(PearsonR,digits=3))%>%
        dplyr::rename("Pearson r"="PearsonR")
      DT::datatable( pos)
    })
    output$neg_table <- DT::renderDataTable({
      pheno_tab5_b<-re_tab5()
      neg<-df_pcor[,c("Uniprot.Accession","Gene.Symbol","Mouse.Number",pheno_tab5_b,"Protein.Description")]%>%
        dplyr::rename("PearsonR"=all_of(pheno_tab5_b))%>%
        dplyr::filter(PearsonR <=-0.4)%>%
        arrange(PearsonR)%>%
        mutate(PearsonR=round(PearsonR,digits=3))%>%
        dplyr::rename("Pearson r"="PearsonR")
      DT::datatable( neg)
    })
    ## the bottom half of the tab below ##
    re_tab5a <-eventReactive( input$go_tab5a,{input$gene_tab5a})
    output$text_tab5a<-renderText({
      gene_text5a<-toupper(re_tab5a())
      #test gene_text5a<-"UCP1"
      uni_text5a<-db0[db0$gene==gene_text5a,]$uni
      if(length(uni_text5a)==0){
        print("Please click to select an official gene symbol from the typing helper menu.")
      }else{
        protein_text5a<-dft[,uni_text5a]
        length_text5a<-length(protein_text5a[!is.na(protein_text5a)])
        if(length_text5a>=81){
          paste0(gene_text5a, " was quantified in more than half of the DO mice BAT.")
        }else{
          paste0(gene_text5a, " was quantified in fewer than half of the DO mice BAT.")}
      }})
    
    output$plot_tab5a<-renderPlot({
      gene_plot5a<-toupper(re_tab5a())
      pheno5a<-input$dropdown_tab5a
      uni_plot5a<-db0[db0$gene==gene_plot5a,]$uni
      if(length(uni_plot5a)==0){
        print("Please use official gene symbol found on UniProt (uniprot.org).")
      }else{
        #test gene_plot5a<-"LEP" pheno5a<-"fat.percent.week8"
        df_plot5a<-df2[df2$uni==uni_plot5a,]
        df_plot5a$Sample_ID1<-paste0(df_plot5a$M,"_",df_plot5a$ID)
        df_p_plot5a<-df_p[,c("Sample_ID1",pheno5a)]
        df_p_plot5a[pheno5a] <- scale( df_p_plot5a[pheno5a]) 
        data_plot5a<-left_join(df_plot5a,df_p_plot5a,by="Sample_ID1")
        ggscatter(data_plot5a, x = "value", y = all_of(pheno5a), 
                  add = "reg.line", conf.int = TRUE, 
                  conf.int.level = 0.95,
                  cor.coef = TRUE, cor.method = "pearson",
                  color = "black", 
                  alpha=0.65,
                  add.params = list(color = "darkred",
                                    fill = "gray"),
                  xlab = paste0("rel. abundance of ",gene_plot5a), ylab = paste0("z-scored value of ",pheno5a))
      }}, res = 96)

    
    
#below is for tab6- Strain selection#
    re_tab6 <-eventReactive( input$go_tab6,{input$dropdown_tab6})
    output$text_tab6<-renderText({
      pheno_tab6<-re_tab6()
      # test pheno_tab6<-"VO2 cold/day"
      df_tab6<-df_strain%>%
        dplyr::filter(Parameter==pheno_tab6)%>%
        arrange(Allelic.contribution)
      df_tab6_sort<-na.omit(df_tab6)
      
      most_neg<-df_tab6_sort[1,]$Founder.strain
      most_pos<-df_tab6_sort[nrow(df_tab6_sort),]$Founder.strain
      
      paste0(most_neg," has the most negative founder allelic contribution to ",pheno_tab6, "; ", most_pos, " has the most positive founder allelic contribution to ",pheno_tab6, ".")
    })
    output$strain_table <- DT::renderDataTable({
      pheno_tab6_a<-re_tab6()
      #test pheno_tab6_a<-"VO2 cold/day"
      df_tab6_a<-df_strain%>%
        dplyr::filter(Parameter==pheno_tab6_a)%>%
        arrange(Allelic.contribution)
      DT::datatable(df_tab6_a,options = list(
        autoWidth = TRUE,
        columnDefs = list(list(targets=c(3), visible=TRUE, width='300px'),
                          list(targets=c(4), visible=TRUE, width='300px'),
                          list(targets=c(5), visible=TRUE, width='300px'),
                          list(className = 'dt-center',targets = c(2,3)))))
    })  
    
    #below is for tab7#
    re_tab7 <-eventReactive( input$go_tab7,{input$dropdown_tab7})
    output$text_tab7<-renderText({
      pheno_tab7<-re_tab7()
      # test pheno_tab7<-"BMI"
      df_tab7<-df_human_cor[,c("Gene.symbol",pheno_tab7,paste0(pheno_tab7,".pval"),"Description")]%>%
        dplyr::rename("PearsonR"=all_of(pheno_tab7),"Pval"=all_of(paste0(pheno_tab7,".pval")))%>%
        dplyr::filter(PearsonR >=0.4|PearsonR <=-0.4)%>%
        dplyr::filter(Pval <0.05)
      cor_number_tab7<-nrow(df_tab7)
      paste0("Human phenotype ",pheno_tab7," has ",cor_number_tab7, " significant transcript correlators (Pearson r ≥ 0.4 or ≤ -0.4, p value < 0.05) mapped as significant protein correlators of at least one of the 33 phenotypes measured in OPABAT.")
    })
    output$human_table <- DT::renderDataTable({
      pheno_tab7_a<-re_tab7()
      #test pheno_tab7_a<-"BMI"
      human<-df_human_cor[,c("Gene.symbol",pheno_tab7_a,paste0(pheno_tab7_a,".pval"),"Description")]%>%
        dplyr::rename("PearsonR"=all_of(pheno_tab7_a),"Pval"=all_of(paste0(pheno_tab7_a,".pval")))%>%
        dplyr::filter(PearsonR >=0.4|PearsonR <=-0.4)%>%
        dplyr::filter(Pval <0.05)%>%
        arrange(PearsonR)%>%
        mutate(PearsonR=round(PearsonR,digits=3))%>%
        dplyr::rename("Pearson r"="PearsonR")
      DT::datatable(human)
    })
    ## the bottom half of the tab below ##
    re_tab7a <-eventReactive( input$go_tab7a,{input$gene_tab7a})
    output$text_tab7a<-renderText({
      gene_text7a<-toupper(re_tab7a())
      #test gene_text7a<-"NDUFC2"
      uni_text7a<-db0[db0$gene==gene_text7a,]$uni
      
      if(!(gene_text7a%in%(df_human_BAT$Gene.symbol))){
        print(paste0(gene_text7a, " is not detected or not recapitulated as a human SCVAT transcript-phenotype correlator."))
      }else{
        dfp_sig_pro<-dfp_sig%>%filter(Uniprot.Accession==uni_text7a)
        dfp_sig_pro1<-dfp_sig_pro[,3:35]
        dfp_sig_pro1_t<-as.data.frame(t(dfp_sig_pro1))
        dfp_sig_pro1_t$params<-rownames(dfp_sig_pro1_t)
        dfp_sig_pro1_t1<-dfp_sig_pro1_t%>%filter(V1 >=0.4|V1 <=-0.4)
        hit_params<-toString(dfp_sig_pro1_t1$params)
        
        paste0(gene_text7a, " is a OPABAT protein-phenotype correlator of ", hit_params, ". Currently displaying its human SCVAT transcript-phenotype correlation.")}
      })
    
    output$plot_tab7a<-renderPlot({
      gene_plot7a<-toupper(re_tab7a())
      pheno7a<-input$dropdown_tab7a
      uni_plot7a<-db0[db0$gene==gene_plot7a,]$uni
      if(!(gene_plot7a%in%(df_human_BAT$Gene.symbol))){
        print(paste0(gene_plot7a, " is not detected or not recapitulated as a human SCVAT transcript-phenotype correlator."))
      }else{
        #test gene_plot7a<-"NDUFC2" pheno7a<-"BMI"
        df_plot7_transcript<-df_human_BAT%>%filter(Gene.symbol==all_of(gene_plot7a))
        df_plot7_pheno<-df_human_pheno%>%select(Study.ID,all_of(pheno7a))
        pheno_values<-df_plot7_pheno[,2]
        pheno_zscore<-as.data.frame(scale(pheno_values))
        df_plot7_pheno1<-cbind(df_plot7_pheno,pheno_zscore)
        
        
        df_plot7_transcript_t<-as.data.frame(t(df_plot7_transcript))
        df_plot7_transcript_t$"Study.ID"<-rownames(df_plot7_transcript_t)
        df_plot7_transcript_t1<-df_plot7_transcript_t[-1,]

        data_plot7a<-left_join(df_plot7_transcript_t1,df_plot7_pheno1,by="Study.ID")
        data_plot7a$V1.x<-as.numeric(data_plot7a$V1.x)
        
        ggscatter(data_plot7a, x = "V1.x", y = "V1.y", 
                  add = "reg.line", conf.int = TRUE, 
                  conf.int.level = 0.95,
                  cor.coef = TRUE, cor.method = "pearson",
                  color = "black", 
                  alpha=0.65,
                  add.params = list(color = "darkred",
                                    fill = "gray"),
                  xlab = paste0("rel. abundance (TPM) of ",gene_plot7a), ylab = paste0("z-scored value of ",pheno7a))
      }}, res = 96)   
    
        
 ###tab 8 below###
    output$download1<-renderText({
      "(1) BAT protein expression data across the whole DO cohort."
    })
    
    output$download2<-renderText({
      "(2) A look-up table for the naming/labeling system of all the mice in the DO cohort."
    })
   
    output$download3<-renderText({
      "(3) Significant protein-protein correlations found in OPABAT and evidence for correlation in the literature."
    })
    
    output$download4<-renderText({
      "(4) Proteins in OPABAT that are significantly co-operative to established CORUM complexes."
    })
    
    output$download5<-renderText({
      "(5) Proteins in OPABAT that are significantly co-operative to established KEGG pathways."
    })
    
    output$download6<-renderText({
      "(6) Phenotypic data for all DO mice."
    })
    
    output$download7<-renderText({
      "(7) Correlation between protein expression and phenotypic data."
    })
    
    output$download8<-renderText({
      "(8) Strain mapping information to model phenotypes."
    })
    
    output$download9<-renderText({
      "(9) Phenotype QTLs found in OPABAT."
    })
   
    output$download10<-renderText({
      "(10) Protein QTLs found in OPABAT."
    })
    
    output$download11<-renderText({
      "(11) Correlations between phenotypes and Human SCVAT transcripts of OPABAT metabolic phenotype correlators."
    })
    
    output$download12<-renderText({
      "(12) Correlations between phenotypes and Human SAT transcripts of OPABAT metabolic phenotype correlators."
    })
    
    output$download13<-renderText({
      "(13) Mapping of OPABAT metabolic phenotype correlators onto human disease networks."
    })
    
    
  }
  
  shinyApp(ui, server)
  


#gene_text3="CKB"
#pheno<-"age"
#x1="UCP1"
#x2="CKB"