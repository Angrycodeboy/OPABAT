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
df_p<-read.csv("data/physiological.csv",header=T,stringsAsFactors = F)
#names for phenotypic data
names<-c(names(df_p[,13:45]),"age")

# (5) edge info for correlation plot
db<-read.delim("data/final/db_edges.tsv",header=T,stringsAsFactors = F)

# (6) edge info for visnetwork
all_edges<-read.delim("data/final/all_edges_visnetwork.tsv",header=T,stringsAsFactors = F)

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


##files to download##
d1<-read.csv("download/(1) Protein_expression.csv",header=T)
d2<-read.csv("download/(2) Mouse_lookup_table.csv",header=T)
d3<-read.csv("download/(3) Edges_in_DOBAT_network.csv",header=T)
d4<-read.delim("download/(4) New_accessory_proteins_to_corum_complexes.tsv",header=T)
d5<-read.csv("download/(5) Physiological_data.csv",header=T)
d6<-read.csv("download/(6) Protein_physiological_data_correlations.csv",header=T)



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
    navlistPanel(

      #tab 0 Homepage
      tabPanel(titlePanel("Home"),
      textOutput("Welcome"),
      tags$head(tags$style("#Welcome{color: Darkred;
                                 font-size: 40px;
                                 font-weight: bold;
                                 }"
      )
      ),
      textOutput("Abstract"),
      tags$head(tags$style("#Abstract{color: Black;
                                 font-size: 15px;
                                 }"
      )
      ),
      #textOutput("Cite"),
      #tags$head(tags$style("#Cite{color: Blue;
       #                          font-size: 15px;
        #                         }"
    #  )
    #  ),
      tags$h1("Citation"),
      HTML("<p>If this website is useful to you, please consider citing <a href='https://www.sciencedirect.com/science/article/pii/S0092867420301562?via%3Dihub'> 'space holder-ProPhysDOBAT' </a>!</p>"),
      
      imageOutput("image_tab1"),
      ),
    #tab 0.5 Tour guide
    tabPanel(titlePanel("Tour Guide"),
             h2("Explore what we have prepared for you on this website!"),
             h4("(Please check cookie settings or switch to Chrome/Firefox if videos cannot be loaded correctly)"),
             h3("Expressions"),
             h5("Select a protein to see its expression level in DO BAT, and color mice by phenotypic data."),
             tags$video(id="video1", width="400px",height="400px",type = "video/mp4",src = "expression.mp4", controls = "controls"),
             h3("Correlations"),
             h5("Select any two proteins to see the correlation between their expression levels in DO BAT."),
             tags$video(id="video2", width="400px",height="400px",type = "video/mp4",src = "correlation.mp4", controls = "controls"),
             h3("Networks"),
             h5("Select a protein to explore its significant immediate correlators in DO BAT network."),
             tags$video(id="video3", width="400px",height="400px",type = "video/mp4",src = "network.mp4", controls = "controls"),
             h3("Complexes"),
             h5("Select a protein or a CORUM core complex to explore new accesory proteins to established protein complexes identified in DO BAT network."),
             tags$video(id="video4", width="400px",height="400px",type = "video/mp4",src = "complex.mp4", controls = "controls"),
             h3("Phenotypes"),
             h5("Explore the positive and negative protein correlators of each physiological parameter."),
             tags$video(id="video5", width="400px",height="400px",type = "video/mp4",src = "phenotypes.mp4", controls = "controls")
             #test-tags$img(width="50%",src = "image-in-app.png")
    ),
      #tab 1 protein expression
      tabPanel(
        titlePanel("Expressions"),
        textOutput("expression_description"),
        # input gene
        autocomplete_input(id="gene_tab1", 
                           label="Gene", 
                           options=db0$gene, 
                           value = "", 
                           width = NULL,
                           placeholder = "Type and select an official gene symbol", 
                           max_options = 0, 
                           hide_values = FALSE),
        
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
                           placeholder = "Type and select an official gene symbol", 
                           max_options = 0, 
                           hide_values = FALSE),
        autocomplete_input(id="gene2", 
                           label="Gene2", 
                           options=db0$gene, 
                           value = "", 
                           width = NULL,
                           placeholder = "Type and select an official gene symbol", 
                           max_options = 0, 
                           hide_values = FALSE),
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
                                  placeholder = "Type and select an official gene symbol", 
                                  max_options = 0, 
                                  hide_values = FALSE),
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
                                                        placeholder = "Type and select an official gene symbol", 
                                                        max_options = 0, 
                                                        hide_values = FALSE),
                                     actionButton("go_tab4a", "see complex assignment",class = "btn-success"),
                                     textOutput("text_tab4a"),
                                     tabsetPanel( 
                                       id = 'table_tab4',
                                       tabPanel("Accessory to these complexes in DO BAT", DT::dataTableOutput("accessory_table",width = '100%')),
                                       tabPanel("Subunit of these complexes in CORUM", DT::dataTableOutput("subunit_table",width = '100%'))
                                     )),
                            tabPanel("Complex-centric view",
               selectizeInput(
                 inputId="dropdown_tab4",
                 label="Select or backspace and type in a Corum complex to see new accessory proteins in  DO BAT",
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
                                  label="Type in and select a gene to see correaltion plots", 
                                  options=db0$gene, 
                                  value = "", 
                                  width = NULL,
                                  placeholder = "Type and select an official gene symbol", 
                                  max_options = 0, 
                                  hide_values = FALSE),
               
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
      
      #tab 6 Download
      tabPanel(titlePanel("Download"),
               textOutput("download1"),
               tags$head(tags$style("#download1{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               downloadButton("Download1","download protein expression"),
               textOutput("download2"),
               tags$head(tags$style("#download2{color: black;
                                 font-size: 20px;
                                
                                 }"
               )),
               downloadButton("Download2","download lookup table"),
               textOutput("download3"),
               tags$head(tags$style("#download3{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               downloadButton("Download3","download edges"),
               textOutput("download4"),
               tags$head(tags$style("#download4{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               downloadButton("Download4","download accessory proteins"),
               textOutput("download5"),
               tags$head(tags$style("#download5{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               downloadButton("Download5","download physiological data"),
               textOutput("download6"),
               tags$head(tags$style("#download6{color: black;
                                 font-size: 20px;
                                 
                                 }"
               )),
               downloadButton("Download6","download protein-physiology correlations")
),
     
      
      #tab 8 Acknowledgments
      tabPanel(titlePanel("Acknowledgment"),
               tags$h1("Acknowledgement"),
               HTML("<p> This project is a collaboration across the <a href='https://chouchanilab.dana-farber.org/'> Chouchani</a>, 
               <a href='https://gygi.hms.harvard.edu/index.html'> Gygi</a>,
                    and <a href='https://spiegelmanlab.dana-farber.org'> Spiegelman </a> labs at 
                    <a href='https://www.dana-farber.org'> Dana-Farber Cancer Institute </a> and 
                    <a href='https://hms.harvard.edu'> Harvard Medical School</a>. Please email Dr. Chouchani (edwardt_chouchani@dfci.harvard.edu) if you have questions for this website."),
               HTML("<p> The web application is developed by Haopeng Xiao and Jiaming Li from the <a href='https://chouchanilab.dana-farber.org/'> Chouchani</a> and 
               <a href='https://gygi.hms.harvard.edu/index.html'> Gygi</a> labs, the source codes are deposited in GitHub (<a href='https://github.com/Angrycodeboy'>https://github.com/Angrycodeboy</a>)."),
               HTML("<p> This project is funded by <a href='https://www.calicolabs.com'> Calico Life Sciences LLC</a> and 
                    <a href='https://www.nih.gov'> National Institute of Health</a>.")
               
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
    output$Welcome<-renderText({
      "Welcome to ProPhysDOBAT!"
    })
    output$image_tab1 <- renderImage({
        list(src="data/ProphysDOBAT.png",width=200,height=180)
      },
    deleteFile = FALSE)
    output$Abstract<-renderText({
      "Abstract space holder- Obesity is a polygenic disease with variable penetrance in the general population. Brown adipose tissue (BAT) is a major regulator of energy expenditure and metabolic disease.  However, almost all mechanistic studies of BAT function occur in a single inbred mouse strain, which has limited understanding of generalizable mechanisms for resistance to obesity. Here we perform deep quantitative multiplexed proteomics of BAT across a cohort of XX Diversity Outbred (DO) mice, a defined model that parallels the genetic and phenotypic variation found in the human population. By quantifying over 10,000 BAT proteins across the cohort, we define the functional architecture of the outbred BAT proteome. We identify protein correlation networks and use this as a basis to discover and validate novel and essential regulators of BAT thermogenic effectors. Through parallel metabolic phenotyping of the DO cohort during obesogenesis, we also identify key proteins and protein networks that underlie protection from, or sensitivity to, metabolic disease pathogenesis. We provide this outbred proteomic architecture as a framework for understanding conserved mechanisms of BAT regulation over metabolic disease.
      
      "
    })
    output$Cite<-renderText({
      "Please cite: space holder-please insert citation here"
    })
    
    ##below is for tab1
    re_tab1 <-eventReactive( input$go_tab1,{input$gene_tab1})
    output$expression_text<-renderText({
      gene_text1<-re_tab1()
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
      gene_plot1<-re_tab1()
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
      members<-re()
      #test members<-c("UCP1","LETMD1")
      edges<-db
      edges$count<-ifelse((edges$gene_final1 %in% members)&(edges$gene_final2 %in% members),1,0)
      edges<-edges[(edges$count!=0),]
      if(nrow(edges)!=0){
        paste0("Edge exist in DO BAT correlation network, ","FDR q=",signif(edges$qval,digits=2),", ","mouse n=",edges$n)
      }else{
        paste0("Edge not included in DO BAT network, either due to the edge not existing or not passing FDR cutoff (q>0.05, |r|>0.75, n>50)")}
    })
    
    output$plot <- renderPlot({
      x1<-re()[1]
      x2<-re()[2]
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
      gene_text3<-re_tab3()
      edge_extract1<-all_edges[all_edges$from==gene_text3,]
      edge_extract2<-all_edges[all_edges$to==gene_text3,]
      length_text3<-length(c(edge_extract1$to,edge_extract2$from))
      if(length_text3==0){
        paste0(gene_text3," has no significant correlator.")
      }else{
      paste0(gene_text3,"'s immediate correlation network contains ",length_text3," significant correlators. 
             Zoom in and hover over nodes to see gene symbol and protein description. Hover over edges to see 
             correlation strength and evidence of protein-protein interaction in the literature. Reload the page
             if very large networks slow down the website.")}
    })
    output$plot_tab3 <- renderVisNetwork({
      gene_plot3<-re_tab3()
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
      gene_text4a<-re_tab4a()
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
      gene_text4a<-re_tab4a()
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
      gene_text4a<-re_tab4a()
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
      paste0("Corum complex ",gene_text4," has ",new_member_tab4, " new accessory proteins identified in DO BAT network. 
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
        mutate( color="orange",
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
        mutate(color="darkred",
               dashes=FALSE)
      ##construct final edge table
      edges_plot4<-rbind(temp2,temp3)
      edges_plot4<-edges_plot4[!duplicated(edges_plot4$label),]%>%select(-value,-label)
    ##construct node table##
      # for corum members#
      nodes_members_plot4 <- data.frame(id = complex_member_plot4,
                          label = complex_member_plot4,
                          color = "orange")
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
        mutate( color="darkred")%>%
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
      paste0("Physiological parameter ",pheno_tab5," has ",cor_number_tab5, " significant protein correlators (Pearson r ≥ 0.4 or ≤ -0.4, Mouse Number ≥ 81).")
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
      gene_text5a<-re_tab5a()
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
      gene_plot5a<-re_tab5a()
      pheno5a<-input$dropdown_tab5a
      uni_plot5a<-db0[db0$gene==gene_plot5a,]$uni
      if(length(uni_plot5a)==0){
        print("Please use official gene symbol found on UniProt (uniprot.org).")
      }else{
        #test gene_plot5a<-"LEP" pheno5a<-"body.weight"
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
    
 ###tab 6 below###
    output$download1<-renderText({
      "Download BAT protein expression data across the whole DO cohort."
    })
    output$Download1 <- downloadHandler(
      filename = function() {
        "(1) Protein_expression.csv"
      },
      content = function(file) {
        write.csv(d1, file)
      }
    )
    output$download2<-renderText({
      "Download the naming/labeling system of all the mice in the DO cohort."
    })
    output$Download2 <- downloadHandler(
      filename = function() {
       "(2) Mouse_lookup_table.csv"
      },
      content = function(file) {
        write.csv(d2, file)
      }
    )
    output$download3<-renderText({
      "Download all the significant protein-protein correlations found in DO BAT and and evidence in the literature."
    })
    output$Download3 <- downloadHandler(
      filename = function() {
        "(3) Edges_in_DOBAT_network.csv"
      },
      content = function(file) {
        write.csv(d3, file)
      }
    )
    output$download4<-renderText({
      "Download all the significant proteins in DO BAT accessory to established CORUM complexes."
    })
    output$Download4 <- downloadHandler(
      filename = function() {
        "(4) New_accessory_proteins_to_corum_complexes.csv"
      },
      content = function(file) {
        write.csv(d4, file)
      }
    )
    output$download5<-renderText({
      "Download all physiological data for all DO mice."
    })
    output$Download5 <- downloadHandler(
      filename = function() {
       "(5) Physiological_data.csv"
      },
      content = function(file) {
        write.csv(d5, file)
      }
    )
    output$download6<-renderText({
      "Download the correlation table between protein expression and physiological data."
    })
    output$Download6 <- downloadHandler(
      filename = function() {
        "(6) Protein_physiological_data_correlations.csv"
      },
      content = function(file) {
        write.csv(d6, file)
      }
    )
  }
  
  shinyApp(ui, server)
  


#gene_text3="CKB"
#pheno<-"age"
#x1="UCP1"
#x2="CKB"