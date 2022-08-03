secure_app(head_auth = tags$script(inactivity), 
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