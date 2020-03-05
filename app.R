library(shiny)

ui <- fluidPage(
  
  theme = "owntheme.css",
  
  tags$head(
    tags$script( async = NA, src = "https://www.googletagmanager.com/gtag/js?id=UA-19799395-3" ),
    tags$script( HTML( "
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());
                           
        gtag('config', 'UA-19799395-3');
        " ) ),
    tags$meta( name = "description", content = paste0( "Nyugdíjbiztosítási kalkulátor.",
                                                       "Írta: Borsos Judit, Ferenci Tamás." ) ),
    tags$meta( property = "og:title", content = "Nyugdíjbiztosítási kalkulátor" ),
    tags$meta( property = "og:type", content = "website" ),
    tags$meta( property = "og:locale", content = "hu_HU" ),
    tags$meta( property = "og:url",
               content = "https://research.physcon.uni-obuda.hu/NyugdijbiztositasiKalkulator" ),
    # tags$meta( property = "og:image",
    #            content = "https://research.physcon.uni-obuda.hu/OrvosiSzures_Pelda.png" ),
    tags$meta( property = "og:description", content = paste0( "Nyugdíjbiztosítási kalkulátor.",
                                                              "Írta: Borsos Judit, Ferenci Tamás." ) ),
    tags$meta( name = "DC.Title", content = "Egészségügyi szűrőprogramok, orvosi tesztek eredményességének vizsgálata" ),
    tags$meta( name = "DC.Creator", content = "Borsos Judit, Ferenci Tamás" ),
    tags$meta( name = "DC.Subject", content = "nyugdíjbiztosítás" ),
    tags$meta( name = "DC.Description", content = paste0( "Nyugdíjbiztosítási kalkulátor." ) ),
    tags$meta( name = "DC.Publisher",
               content = "https://research.physcon.uni-obuda.hu/NyugdijbiztositasiKalkulator/" ),
    tags$meta( name = "DC.Contributor", content = "Borsos Judit, Ferenci Tamás" ),
    tags$meta( name = "DC.Language", content = "hu_HU" )
  ),
  
  tags$div( id="fb-root" ),
  tags$script( async = NA, defer = NA, crossorigin = "anonymous",
               src = "https://connect.facebook.net/hu_HU/sdk.js#xfbml=1&version=v5.0" ),
  
  tags$style( ".shiny-file-input-progress {display: none}" ),
  
  titlePanel( "Judit-féle nyugdíjbiztosítási varázs-kalkulátor" ),
  
  p( "A program használatát részletesen bemutató súgó, valamint a technikai részletek",
     a( "itt", href = "https://github.com/tamas-ferenci/NyugdijbiztositasiKalkulator",
        target = "_blank" ), "olvashatóak el." ),
  div( class="fb-like",
       "data-href"="https://research.physcon.uni-obuda.hu/NyugdijbiztositasiKalkulator",
       "data-width" = "", "data-layout"="standard", "data-action"="like", "data-size"="small",
       "data-share"="true"), p(),
  
  sidebarLayout(
    sidebarPanel(
      numericInput( "StartEv", "Mi a kezdőév?", 2019, min = 0, step = 1 ),
      numericInput( "Eletkor", "Hány éves vagy ekkor?", 32, min = 0, step = 1 ),
      numericInput( "NyugdijEv", "Hány évesen mennél nyugdíjba?", 65, min = 0, step = 1 ),
      numericInput( "EvesOsszeg", "Mekkora az éves befizetés? [Ft]", value = 650000, min = 0, step = 1000),
      sliderInput( "EvesDijemeles", "Mekkora az éves díjemelés? [Ft]", value = 4.8, min = 0, max = 100, step = 0.1),
      sliderInput( "FeltetelezettHozam", "Mekkora a feltételezett hozam? [%]", value = 6, min = 0, max = 100, step = 0.1),
      sliderInput( "Ktg1Ev", "Mennyi az 1. éves költség? [%]", value = 75, min = 0, max = 100, step = 0.1),
      sliderInput( "Ktg2Ev", "Mennyi az 2. éves költség? [%]", value = 20, min = 0, max = 100, step = 0.1),
      sliderInput( "Ktg3Ev", "Mennyi az 3. éves költség? [%]", value = 10, min = 0, max = 100, step = 0.1),
      numericInput( "EvesDijErtek", "Mennyi az éves díj? [Ft]", 20000, min = 0, step = 1000 ),
      numericInput( "AdojovairasMax", "Mennyi az adójóváírás maximuma? [Ft]", 130000, min = 0, step = 1000 ),
      sliderInput( "DiszkontErtek", "Mekkora a diszkontráta?  [%]", value = 4.8, min = 0, max = 100, step = 0.1),
      downloadButton( "DownloadFigPDF", "Az ábra letöltése (PDF)" ),
      downloadButton( "DownloadFigPNG", "Az ábra letöltése (PNG)" ),
      downloadButton( "DownloadNumCSV", "Számszerű adatok letöltése (CSV)" ),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel( "Grafikon", plotOutput( "resultPlot" ) ),
        tabPanel( "Számszerű adatok", tableOutput( "numericTable" ) ),
        tabPanel( "Eredmény", tableOutput( "resultTable" ) )
      )
    )
  ),
  
  h4( "Írta: Borsos Judit, Ferenci Tamás, v1.00" ),
  
  # tags$footer( h4( "Írta: Ferenci Tamás (Óbudai Egyetem, Élettani Szabályozások Kutatóközpont), v1.00" ),
  #              style = "position:absolute; bottom:0; width:100%; height:50px; padding: 10px; z-index: 1000;" ),
  
  tags$script( HTML( "var sc_project=11601191; 
                      var sc_invisible=1; 
                      var sc_security=\"5a06c22d\";
                      var scJsHost = ((\"https:\" == document.location.protocol) ?
                      \"https://secure.\" : \"http://www.\");
                      document.write(\"<sc\"+\"ript type='text/javascript' src='\" +
                      scJsHost+
                      \"statcounter.com/counter/counter.js'></\"+\"script>\");" ),
               type = "text/javascript" )
)

server <- function(input, output, session) {
  
  dat <- reactive({
    Evek <- input$StartEv:(input$StartEv+input$NyugdijEv-input$Eletkor)
    n <- length(Evek)
    Befizetes <- input$EvesOsszeg*(1+input$EvesDijemeles/100)^(0:(n-1))
    Koltseg <- -c( input$Ktg1Ev/100, input$Ktg2Ev/100, input$Ktg3Ev/100, rep( 0, n-3) )*Befizetes
    EvesDij <- rep( -input$EvesDijErtek, n )
    Adojovairas <- pmin( Befizetes*0.2, rep( input$AdojovairasMax, n ) )
    
    res <- data.frame( Evek, Befizetes, EvElejiEgyenleg = c( Befizetes[1], rep( 0, n-1 ) ),
                       Hozam = c( Befizetes[1]*input$FeltetelezettHozam/100, rep( 0, n-1 ) ), Koltseg,
                       EvesDij, Adojovairas,
                       EvVegiEgyenleg = c( Befizetes[1]*(1+input$FeltetelezettHozam/100)+Koltseg[1]+EvesDij[1]+Adojovairas[1],
                                           rep(0, n-1) ) )
    for( i in 2:nrow(res) ) {
      res$EvElejiEgyenleg[i] <- res$Befizetes[i]+res$EvVegiEgyenleg[i-1]
      res$Hozam[i] <- res$EvElejiEgyenleg[i]*input$FeltetelezettHozam/100
      res$EvVegiEgyenleg[i] <- res$EvElejiEgyenleg[i]+res$Hozam[i]+res$Koltseg[i]+res$EvesDij[i]+res$Adojovairas[i]
    }
    res$Kifizetes <- c( rep( 0, n-1 ), res$EvVegiEgyenleg[n] )
    res$CF <- -res$Befizetes+res$Kifizetes
    res$Diszkont <- (1/(1+input$DiszkontErtek/100))^((1:n)-1)
    res$DCF <- res$CF*res$Diszkont
    res
  })
  
  plotInput <- function() {
    print(lattice::xyplot( EvVegiEgyenleg/1e6 ~ Evek, data = dat(), type = "l", xlab = "Év",
                           ylab = "Év végi egyenleg [MFt]" ))
    grid::grid.text( "Borsos Judit, 2020", 0, 0.02, gp = grid::gpar( fontface = "bold" ), just = "left" )
    # grid::grid.text( "https://research.physcon.", 1, 0.06, gp = grid::gpar( fontface = "bold" ), just = "right" )
    # grid::grid.text( "uni-obuda.hu", 1, 0.02, gp = grid::gpar( fontface = "bold" ), just = "right" )
  }
  
  output$numericTable <- renderTable({
   res <- dat()[,c("Evek", "EvElejiEgyenleg", "Befizetes", "Hozam", "Koltseg", "EvesDij", "Adojovairas", "EvVegiEgyenleg")]
   res$EvElejiEgyenleg <- res$EvElejiEgyenleg/1e6
   res$EvVegiEgyenleg <- res$EvVegiEgyenleg/1e6
   colnames(res) <- c( "Év", "Év eleji egyenleg [MFt]", "Befizetés [Ft]", "Hozam [Ft]", "Költség [Ft]", "Éves díj [Ft]",
                       "Adójóváírás [Ft]", "Év végi egyenleg [MFt]" )
   res
  }, format.args = list( decimal.mark = "," ) )
  
  output$resultTable <- renderTable({
    res <- data.frame( Mutato = c( "Utolsó év végi egyenleg [MFt]", "Nettó jelenérték [MFt]", "Belső megtérülési ráta [%]" ),
                Ertek = c( tail( dat()$EvVegiEgyenleg, 1 )/1e6, sum(dat()$DCF)/1e6, jrvFinance::irr(dat()$CF)*100 ) )
    colnames(res) <- c( "Mutató", "Érték" )
    res
  }, format.args = list( decimal.mark = "," ) )
  
  output$resultPlot <- renderPlot({
    print( plotInput() )
  })
  
  output$DownloadFigPDF <- downloadHandler(
    filename = "NyugdijbiztositasKalkulatorPlot.pdf",
    content = function( file ) {
      cairo_pdf( file, width = 16, height = 9 )
      print( plotInput() )
      dev.off( )
    } )
  
  output$DownloadFigPNG <- downloadHandler(
    filename = "NyugdijbiztositasKalkulatorPlot.png",
    content = function( file ) {
      png( file, width = 1600, height = 900, type = "cairo-png" )
      print( plotInput() )
      dev.off( )
    } )
  
  output$DownloadNumCSV <- downloadHandler(
    filename = "NyugdijbiztositasKalkulatorPlot.csv",
    content = function( file ) {
      write.csv2( dat(), file )
    } )
  
}

shinyApp( ui = ui, server = server )