##Mark Reuter
##Copyright December 2020
##Contact mark.reuter@googlemail.com
##sfz file maker

library(shiny)
library(shinythemes)
library(tuneR)

print(Sys.time())

##colourscheme
colours <- c("brown1","brown3","coral3")
bg <- 'burlywood2'

freq.df <- read.csv("resources/freq_table.txt",header = TRUE,stringsAsFactors = FALSE)
freq.df$FreqInt <- round(freq.df$Freq,0)
freq.df$FreqUpper <- round(((freq.df$FreqInt/100)*4)+freq.df$FreqInt,0)
freq.df$FreqLower <- round(freq.df$FreqInt-((freq.df$FreqInt/100)*4),0)

#sfz_header <- readLines("resources/sfz_header.txt",n=-1L)
sfz_header <- paste0("//sfz instrument","\n",
                     "//",Sys.Date(),"\n",
                     "<control>","\n",
                     "<global>","\n")

#####functions#####
uniqueDirName <- function(){
  dirname <- vector()
  dirname[1] <- paste0(substr(Sys.Date(),1,4),"-")
  for (a in 1:12){
    dirname[length(dirname)+1] <- letters[runif(1,1,26)]
    dirname[length(dirname)+1] <- round(runif(1,0,9),0)
  }
  dirname <- paste(dirname,collapse = "")
  dirname
}

ffStats <- function(ff){
  ffStats <- data.frame()
  ffStats[1,1] <- "Median Frequency"
  ffStats[2,1] <- "Mean Frequency"
  ffStats[3,1] <- "Minimum Frequency"
  ffStats[4,1] <- "Maximum Frequency"
  
  ffStats[1,2] <- median(ff, na.rm = TRUE)
  ffStats[2,2] <- mean(ff, na.rm = TRUE)
  ffStats[3,2] <- min(ff, na.rm = TRUE)
  ffStats[4,2] <- max(ff, na.rm = TRUE)
  colnames(ffStats) <- c("Parameter","Result")
  ffStats
}

##function for finding note based on ff
getNote <- function(ff,fname){
  medianFreq <- round(median(ff, na.rm = TRUE),2)
  freq.match <- freq.df[which(medianFreq >= freq.df$FreqLower & medianFreq <= freq.df$FreqUpper),]
  freq.match$analysisFreq <- medianFreq
  freq.match$filename <- fname
  freq.match <- freq.match[,c(8,7,1,2,3)]
  
  freq.table.vertical <- data.frame()
  cn <- colnames(freq.match)
  for (i in 1:ncol(freq.match)){
    freq.table.vertical[i,1] <- cn[i]
    freq.table.vertical[i,2] <- freq.match[1,i]
  }
  colnames(freq.table.vertical) <- c("Parameter","Result")
  
  freq.table.vertical
}

##function to get FF - macro mode
getNoteMacro <- function(wavv,inputWav){
  testwav <- readWave(wavv,toWaveMC = FALSE)
  testwavmono <- mono(testwav,which = "left")
  period <- periodogram(testwavmono,width = 256)
  ff <- FF(period)
  medianFreq <- round(median(ff, na.rm = TRUE),2)
  freq.match <- freq.df[which(medianFreq >= freq.df$FreqLower & medianFreq <= freq.df$FreqUpper),]
  freq.match <- freq.match[,c(1,2,3)]
  freq.match$input <- inputWav
  freq.match$medianFundamentalFreq <- medianFreq
  freq.match <- freq.match[,c(4,1,2,3,5)]
  freq.match
}

##function to search freq.df
search.freq <- function(wavname){
  wavfn.index <- regexpr("[ABCDEFG]\\d",wavname)
  wav.note <- substr(wavname,wavfn.index[[1]],wavfn.index[[1]]+1)
  wav.note.index <- grep(wav.note,freq.df$NoteName)
  wav.number <- freq.df[wav.note.index,1]
  wav.number
}

#####functions END #####

##on start-up - cleanse www folder
previousWavs <- list.files(path = paste0(getwd(),"/www"),
                           pattern = "[.]wav$",
                           full.names = TRUE)

if (length(previousWavs)>0){
  dirdir <- uniqueDirName()
  dir.create(paste0("www/",dirdir))
  
  for (i in 1:length(previousWavs)){
    file.copy(previousWavs[i],paste0(getwd(),"/www/",dirdir))
    file.remove(previousWavs[i])
  }
}


##### UI Start ######
ui <- fluidPage(theme = shinytheme("cyborg"),
                
                # Application title
                titlePanel("SFZ creator"),
                
                
                sidebarLayout(
                  sidebarPanel(
                    fileInput(
                      inputId = "loadWavFile",
                      label = "choose one (or more) .wav file(s)",
                      multiple = TRUE,
                      accept = ".wav",
                      width = 300,
                      placeholder = ""),
                    uiOutput("wavlistdropdown"),
                    conditionalPanel("output.wavlistdropdown",
                                     actionButton("generatePlot",
                                                  "Plot & play this file",
                                                  width = 300),
                                     htmlOutput("wavplayer"),
                                     tableOutput("wavSummary"),
                                     hr(),
                                     h3("Macro mode"),
                                     radioButtons("macroPresets",
                                                  "Macro Presets:",
                                                  c("Determine pitch from wave file (SFZ)" = "a",
                                                    "Determine pitch from file name (SFZ)" = "b",
                                                    "Simple Percussion mapping (SFZ)" = "c")
                                     ),
                                     sliderInput("ampEnvAttackMacro",
                                                 "Amp envelope - Attack (s):",
                                                 min = 0, max = 5,
                                                 value = 0, step = 0.5),
                                     sliderInput("ampEnvReleaseMacro",
                                                 "Amp envelope - Release (s):",
                                                 min = 0, max = 5,
                                                 value = 0, step = 0.5),
                                     actionButton("macroGo",
                                                  "Automap all files",
                                                  width = 300),
                                     hr()
                    )
                  ),
                  
                  
                  mainPanel(
                    plotOutput("wavplot"),
                    fluidRow(
                      column(6,
                             br(),
                             actionButton("plotLeft",
                                          "Plot left channel",
                                          width = 300),
                             br(),
                             plotOutput("plotleftchannel"),
                             br(),
                             downloadButton("downloadLeft",
                                            label = "Download"),
                             hr(),
                             actionButton("getffLeft",
                                          "Get Fundamental Frequency",
                                          width = 300),
                             br(),
                             plotOutput("ffLeft"),
                             sliderInput("ffLeftSlider",
                                         "x axis:",
                                         min = 1,
                                         max = 20000,
                                         value = c(100,5000)),
                             tableOutput("ffTableStatsLeft"),
                             tableOutput("noteTableLeft"),
                             h3("Pitch mapping"),
                             radioButtons("pitchPresets",
                                          "Pitch Presets:",
                                          c("1 pitch per note" = "a",
                                            "+/- 1 octave" = "b",
                                            "Full keyboard map" = "c")),
                             uiOutput("keycentersliderLeft"),
                             uiOutput("lokeysliderLeft"),
                             uiOutput("highkeysliderLeft"),
                             h3("Amp Envelope"),
                             uiOutput("AmpEnvAttackLeft"),
                             uiOutput("AmpEnvReleaseLeft"),
                             uiOutput("opcodeActionLeft"),
                             tags$div(style="height:200px;", verbatimTextOutput("opcodeViewLeft")),
                             textInput("instrumentNameLeft",
                                       "Instrument Name",
                                       value = "",
                                       width = 300,
                                       placeholder = "sample instrument"),
                             downloadButton("downloadLeftOpCode",
                                            label = "Download sfz file")
                             
                      ),
                      column(6,
                             br(),
                             actionButton("plotRight",
                                          "Plot right channel",
                                          width = 300),
                             br(),
                             plotOutput("plotrightchannel"),
                             br(),
                             downloadButton("downloadRight",
                                            label = "Download"),
                             hr(),
                             actionButton("getffRight",
                                          "Get Fundamental Frequency",
                                          width = 300),
                             br(),
                             plotOutput("ffRight"),
                             sliderInput("ffRightSlider",
                                         "x axis:",
                                         min = 1,
                                         max = 20000,
                                         value = c(100,5000)),
                             tableOutput("ffTableStatsRight"),
                             tableOutput("noteTableRight"),
                             h3("Pitch mapping"),
                             radioButtons("pitchPresetsRight",
                                          "Pitch Presets:",
                                          c("1 pitch per note" = "a",
                                            "+/- 1 octave" = "b",
                                            "Full keyboard map" = "c")),
                             uiOutput("keycentersliderRight"),
                             uiOutput("lokeysliderRight"),
                             uiOutput("highkeysliderRight"),
                             h3("Amp Envelope"),
                             uiOutput("AmpEnvAttackRight"),
                             uiOutput("AmpEnvReleaseRight"),
                             uiOutput("opcodeActionRight"),
                             tags$div(style="height:200px;", verbatimTextOutput("opcodeViewRight")),
                             textInput("instrumentNameRight",
                                       "Instrument Name",
                                       value = "",
                                       width = 300,
                                       placeholder = "sample instrument"),
                             downloadButton("downloadRightOpCode",
                                            label = "Download sfz file")
                      )
                    )
                    ,
                    hr(),
                    hr()
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  allWavs <- reactiveValues()
  allWavs$df <- data.frame()
  allWavs$summary <- data.frame()
  
  # observeEvent(input$uploadWavs,{
  #     #wd <- choose.dir()
  #     #print(wd)
  #     
  #     wav.files <- list.files(path = wd,
  #                             pattern = "[.]wav$",
  #                             full.names = TRUE)
  #     new.folder <- paste0(getwd(),"/www")
  #     for (i in 1:length(wav.files)){
  #         file.copy(wav.files[i],new.folder) 
  #     }
  # 
  #     wav.files.local <- list.files(path = new.folder,
  #                                   pattern = "[.]wav$",
  #                                   full.names = FALSE)
  #     
  #     wav.df <- as.data.frame(wav.files.local,stringsAsFactors = FALSE)
  #     allWavs$df <- wav.df
  #     colnames(allWavs$df) <- "Samples"
  # })
  
  # output$directoryList <- renderTable({
  #     allWavs$df
  # })
  
  
  output$wavlistdropdown <- renderUI({
    req(input$loadWavFile)
    
    #print(allWavs$df[,1])
    print(input$loadWavFile)
    
    selectInput(
      inputId = "uploadedWavs",
      label = "Uploaded wav files",
      choices = input$loadWavFile$name,
      selected = NULL,
      multiple = FALSE,
      selectize = TRUE,
      width = 400,
      size = NULL
    )
  })
  
  currentWav <- reactiveValues()
  currentWav$wav <- list()
  ##slot 1 - the stereo wav
  ##slot 2 - left channel
  ##slot 3 - righ channel
  
  
  observeEvent(input$generatePlot,{
    #wfile <- input$uploadedWavs
    #print(paste0("wfile: ",wfile))
    
    #copy files to www
    new.folder <- paste0(getwd(),"/www")
    file.copy(input$loadWavFile$datapath,new.folder)
    
    # wwwfiles <- list.files(paste0(getwd(),"/www"),
    #                        pattern = "[.]wav$")
    # print(wwwfiles)
    ll <- length(input$loadWavFile$datapath)
    print(paste0("Uploaded files: ",ll))
    filenameseq <- seq(0,ll,1)
    for (i in 1:ll){
      file.rename(paste0(getwd(),"/www/",filenameseq[i],".wav"),paste0(getwd(),"/www/",input$loadWavFile$name[i]))
    }
    
    #uploadedWavs
    testwav <- readWave(paste0(getwd(),"/www/",input$uploadedWavs),toWaveMC = FALSE)
    #testwav <- readWave(input$loadWavFile$datapath,toWaveMC = FALSE)
    currentWav$wav[[1]] <- testwav
    
    output$wavplot <- renderPlot({
      par(bg = bg)
      plot(testwav,
           main = input$uploadedWavs,
           col = colours[1])
    })
    
    output$wavSummary <- renderTable({
      allWavs$summary[1,1] <- "Stereo"
      allWavs$summary[1,2] <- ifelse(testwav@stereo == TRUE,"TRUE","FALSE")
      allWavs$summary[2,1] <- "Sample Rate"
      allWavs$summary[2,2] <- testwav@samp.rate
      allWavs$summary[3,1] <- "Bit depth"
      allWavs$summary[3,2] <- testwav@bit
      colnames(allWavs$summary) <- c("Parameter","Result")
      allWavs$summary
      
    })
    
    output$wavplayer <- renderUI({
      #tags$audio(src = input$loadWavFile$name, type = "audio/wav", autoplay = NA, controls = NA)
      print(paste0("Path to wav file to play: ",input$uploadedWavs))
      #tags$audio(src = paste0(input$uploadedWavs), type = "audio/wav", autoplay = NA, controls = NA)
      
      z <- HTML(paste0('<audio controls autoplay>
                       <source src="',input$uploadedWavs,'" type="audio/wav">
                       Your browser does not support the audio element. </audio>'))
      z
    })
  })
  
  observeEvent(input$plotLeft,{
    output$plotleftchannel <- renderPlot({
      leftwavmono <- mono(currentWav$wav[[1]],which = "left")
      currentWav$wav[[2]] <- leftwavmono
      par(bg = bg)
      plot(leftwavmono,
           col = colours[2])
    })
  })
  
  observeEvent(input$plotRight,{
    output$plotrightchannel <- renderPlot({
      rightwavmono <- mono(currentWav$wav[[1]],which = "right")
      currentWav$wav[[3]] <- rightwavmono
      par(bg = bg)
      plot(rightwavmono,
           col = colours[2])
    })
  })
  
  
  output$downloadLeft <- downloadHandler(
    filename = function() {
      paste("left-", input$uploadedWavs, sep="")
    },
    content = function(file) {
      writeWave(currentWav$wav[[2]], file, extensible = FALSE)
    }
  )
  
  output$downloadRight <- downloadHandler(
    filename = function() {
      paste("right-", input$uploadedWavs, sep="")
    },
    content = function(file) {
      writeWave(currentWav$wav[[3]], file, extensible = FALSE)
    }
  )
  
  sfzoutput <- reactiveValues()
  sfzoutput$outtext <- list()
  ##slot 1 = left
  ##slot 2 = right
  
  ##fundamental frequency left
  observeEvent(input$getffLeft,{
    periodLeft <- periodogram(currentWav$wav[[2]],
                              width = 256)
    ff <- FF(periodLeft)
    
    output$ffLeft <- renderPlot({
      par(bg = bg)
      plot(periodLeft,
           col = colours[3],
           main = paste0(input$uploadedWavs,"-left-Frequencies"),
           xlim = input$ffLeftSlider)
    })
    
    output$ffTableStatsLeft <- renderTable({
      lff <- ffStats(ff)
      lff
    })
    
    no <- getNote(ff,input$uploadedWavs)
    
    
    output$noteTableLeft <- renderTable({
      no
    })
    
    output$keycentersliderLeft <- renderUI({
      sliderInput("keycenterLeft",
                  "Centre key:",
                  min = 21, max = 127,
                  value = no[3,2], step = 1)
    })
    
    output$keycentersliderLeft <- renderUI({
      sliderInput("keycenterLeft",
                  "Centre key:",
                  min = 21, max = 127,
                  value = no[3,2], step = 1)
    })
    
    output$lokeysliderLeft <- renderUI({
      lk <- as.integer(no[3,2])
      
      if (input$pitchPresets == "b"){
        lk <- lk-12
        if (lk < 21){
          lk <- 21
        }
      }
      
      if (input$pitchPresets == "c"){
        lk <- 21
      }
      sliderInput("lokeyLeft",
                  "Low key:",
                  min = 21, max = 127,
                  value = lk, step = 1)
      
    })
    
    output$highkeysliderLeft <- renderUI({
      hk <- as.integer(no[3,2])
      
      if (input$pitchPresets == "b"){
        hk <- hk+12
        if (hk > 127){
          hk <- 127
        }
      }
      if (input$pitchPresets == "c"){
        hk <- 127
      }
      
      sliderInput("highkeyLeft",
                  "High key:",
                  min = 21, max = 127,
                  value = hk, step = 1)
    })
    
    output$AmpEnvAttackLeft <- renderUI({
      sliderInput("ampEnvAttackLeft",
                  "Amp envelope - Attack (s):",
                  min = 0, max = 5,
                  value = 0, step = 0.5)
    })
    
    output$AmpEnvReleaseLeft <- renderUI({
      sliderInput("ampEnvReleaseLeft",
                  "Amp envelope - Release (s):",
                  min = 0, max = 5,
                  value = 0, step = 0.5)
    })
    
    output$opcodeActionLeft <- renderUI({
      actionButton("generateOpcodesLeft",
                   "Generate Op Codes",
                   width = 300)
    })
    
    observeEvent(input$generateOpcodesLeft,{
      output$opcodeViewLeft <- renderText({
        
        sfz <- paste0(sfz_header,"\n",
                      "<group>","\n",
                      "ampeg_attack=",input$ampEnvAttackLeft,"\n",
                      "ampeg_release=",input$ampEnvReleaseLeft,"\n",
                      "<region>",
                      " sample=samples/",input$uploadedWavs,
                      " lokey=",input$lokeyLeft,
                      " hikey=",input$highkeyLeft,
                      " pitch_keycenter=",input$keycenterLeft,
                      " seq_length=1"," seq_position=1",
                      " lovel=0"," hivel=127"
        )
        sfzoutput$outtext[[1]] <- sfz
        sfz
      })
    })
    
    
  })
  
  ##fundamental frequency right
  observeEvent(input$getffRight,{
    periodRight <- periodogram(currentWav$wav[[3]],
                               width = 256)
    ff <- FF(periodRight)
    
    output$ffRight <- renderPlot({
      par(bg = bg)
      plot(periodRight,
           col = colours[3],
           main = paste0(input$uploadedWavs,"-right-Frequencies"),
           xlim = input$ffRightSlider)
    })
    
    output$ffTableStatsRight <- renderTable({
      rff <- ffStats(ff)
      rff
    })
    
    no <- getNote(ff,input$uploadedWavs)
    
    output$noteTableRight <- renderTable({
      no
    })
    
    
    output$keycentersliderRight <- renderUI({
      sliderInput("keycenterRight",
                  "Centre key:",
                  min = 21, max = 127,
                  value = no[3,2], step = 1)
    })
    
    output$lokeysliderRight <- renderUI({
      lk <- as.integer(no[3,2])
      
      if (input$pitchPresetsRight == "b"){
        lk <- lk-12
        if (lk < 21){
          lk <- 21
        }
      }
      
      if (input$pitchPresetsRight == "c"){
        lk <- 21
      }
      sliderInput("lokeyRight",
                  "Low key:",
                  min = 21, max = 127,
                  value = lk, step = 1)
      
    })
    
    output$highkeysliderRight <- renderUI({
      hk <- as.integer(no[3,2])
      
      if (input$pitchPresetsRight == "b"){
        hk <- hk+12
        if (hk > 127){
          hk <- 127
        }
      }
      if (input$pitchPresetsRight == "c"){
        hk <- 127
      }
      
      sliderInput("highkeyRight",
                  "High key:",
                  min = 21, max = 127,
                  value = hk, step = 1)
    })
    
    output$AmpEnvAttackRight <- renderUI({
      sliderInput("ampEnvAttackRight",
                  "Amp envelope - Attack (s):",
                  min = 0, max = 5,
                  value = 0, step = 0.5)
    })
    
    output$AmpEnvReleaseRight <- renderUI({
      sliderInput("ampEnvReleaseRight",
                  "Amp envelope - Release (s):",
                  min = 0, max = 5,
                  value = 0, step = 0.5)
    })
    
    output$opcodeActionRight <- renderUI({
      actionButton("generateOpcodesRight",
                   "Generate Op Codes",
                   width = 300)
    })
    
    observeEvent(input$generateOpcodesRight,{
      output$opcodeViewRight <- renderText({
        
        sfzz <- paste0(sfz_header,"\n",
                       "<group>","\n",
                       "ampeg_attack=",input$ampEnvAttackRight,"\n",
                       "ampeg_release=",input$ampEnvReleaseRight,"\n",
                       "<region>",
                       " sample=../samples/",input$uploadedWavs,
                       " lokey=",input$lokeyRight,
                       " hikey=",input$highkeyRight,
                       " pitch_keycenter=",input$keycenterRight,
                       " seq_length=1"," seq_position=1",
                       " lovel=0"," hivel=127"
        )
        sfzoutput$outtext[[2]] <- sfzz
        sfzz
      })
    })
    
  })
  
  output$downloadLeftOpCode <- downloadHandler(
    filename = function() {
      n <- input$instrumentNameLeft
      if (nchar(n) == 0){
        n <- "sample_instrument"
      }
      paste(n,".sfz",sep="")
    },
    content = function(file) {
      writeLines(sfzoutput$outtext[[1]], file)
    }
  )
  
  output$downloadRightOpCode <- downloadHandler(
    filename = function() {
      n <- input$instrumentNameRight
      if (nchar(n) == 0){
        n <- "sample_instrument"
      }
      paste(n,".sfz",sep="")
    },
    content = function(file) {
      writeLines(sfzoutput$outtext[[2]], file)
    }
  )
  
  ##modal window for macro mode
  observeEvent(input$macroGo, {
    
    final.df <- data.frame(matrix(ncol = 5, nrow = 0))
    sfzzContent <- vector()
    
    sfzzMacroHeader <- paste0(sfz_header,"\n",
                              "<group>","\n",
                              "ampeg_attack=",input$ampEnvAttackMacro,"\n",
                              "ampeg_release=",input$ampEnvReleaseMacro,"\n"
    )
    if (input$macroPresets=='a'){
      for (i in 1:length(input$loadWavFile$name)){
        print(input$loadWavFile$name[i])
        macrowav <- paste0(input$loadWavFile$datapath[i])
        print(macrowav)
        macro.df <- getNoteMacro(macrowav,input$loadWavFile$name[i])
        final.df[nrow(final.df)+1,] <- macro.df[1,]
        sfzzMacro <- paste0("<region>",
                            " sample=../samples/",input$loadWavFile$name[i],
                            " lokey=",macro.df[1,2],
                            " hikey=",macro.df[1,2],
                            " pitch_keycenter=",macro.df[1,2],
                            " seq_length=1"," seq_position=1",
                            " lovel=0"," hivel=127","\n"
        )
        sfzzContent[i] <- sfzzMacro
      }
      
    }
    if (input$macroPresets=='b'){
      for (i in 1:length(input$loadWavFile$name)){
        f <- input$loadWavFile$name[i]
        print(f)
        note.number <- search.freq(f)
        final.df[nrow(final.df)+1,1] <- f
        final.df[nrow(final.df),2] <- note.number
        final.df[nrow(final.df),3] <- '-'
        final.df[nrow(final.df),4] <- '-'
        final.df[nrow(final.df),5] <- '-'
        
        sfzzMacro <- paste0("//",note.number,"\n",
                            "<region>",
                            " sample=../samples/",input$loadWavFile$name[i],
                            " lokey=",note.number,
                            " hikey=",note.number,
                            " pitch_keycenter=",note.number,
                            " seq_length=1"," seq_position=1",
                            " lovel=0"," hivel=127","\n"
        )
        sfzzContent[i] <- sfzzMacro
      }
      
    }
    if (input$macroPresets=='c'){
      note.number <- 36
      for (i in 1:length(input$loadWavFile$name)){
        sfzzMacro <- paste0("//",i,"\n",
                            "<region>",
                            " sample=../samples/",input$loadWavFile$name[i],
                            " lokey=",note.number,
                            " hikey=",note.number,
                            " pitch_keycenter=",note.number,
                            " seq_length=1"," seq_position=1",
                            " lovel=0"," hivel=127","\n"
        )
        sfzzContent[i] <- sfzzMacro
        final.df[nrow(final.df)+1,1] <- input$loadWavFile$name[i]
        final.df[nrow(final.df),2] <- note.number
        final.df[nrow(final.df),3] <- '-'
        final.df[nrow(final.df),4] <- '-'
        final.df[nrow(final.df),5] <- '-'
        note.number <- note.number + 1
      }
      
    }
    
    
    colnames(final.df) <- c("Input File",
                            "Note Number",
                            "Note Name",
                            "Frequency",
                            "Median Frequency - input")
    print(final.df)
    sfzzContentUnlist <- unlist(sfzzContent)
    sfzzContentUnlist <- sort(sfzzContentUnlist)
    
    sfzzFinal <- c(sfzzMacroHeader,sfzzContentUnlist)
    sfzoutput$outtext[[3]] <- sfzzFinal
    output$macroSFZoutput <- renderText(sfzzFinal)
    
    showModal(modalDialog(
      title = "Macro mode",
      renderTable(final.df),
      verbatimTextOutput("macroSFZoutput"),
      downloadButton("downloadMacroSFZ",
                     label = "Download sfz file"),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton("macromodalClose", "Close")
      )
    ))
  })
  
  observeEvent(input$macromodalClose,{
    removeModal()
  })
  
  output$downloadMacroSFZ <- downloadHandler(
    filename = function() {
      n <- "sample_instrument"
      paste(n,".sfz",sep="")
    },
    content = function(file) {
      writeLines(sfzoutput$outtext[[3]], file)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
