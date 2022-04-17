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

print(getwd())
print(paste0(getwd(),"/resources/freq_table.txt"))
fe <- file.exists(paste0(getwd(),"/resources/freq_table.txt"))
print(fe)
freq.df <- read.csv(paste0(getwd(),"/resources/freq_table.txt"),
                    header = TRUE,
                    stringsAsFactors = FALSE)
resource.files <- list.files(paste0(getwd(),"/resources"))
print(resource.files)
print("freq.df preview")
print(head(freq.df,n=5))

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
  wav.note <- regmatches(wavname,wavfn.index)
  print(paste0("sanitizer note name: ",wav.note))
  #wav.note <- substr(wavname,wavfn.index[[1]],wavfn.index[[1]]+1)
  wav.note.index <- grep(wav.note,freq.df$NoteName)
  wav.number <- freq.df[wav.note.index,1]
  wav.number
}

##function to alter bit depth
bitDepth <- function(wav,depth){
  testwav <- normalize(wav,unit = depth)
  print(testwav)
  return(testwav)
}

inameFunction <- function(){
  iname <- paste0(
    consanants[runif(1,1,21)],
    vowels[runif(1,1,5)],
    consanants[runif(1,1,21)],
    vowels[runif(1,1,5)],
    consanants[runif(1,1,21)]
  )
  return(iname)
}

autoMapperDown <- function(df){
  for (i in 1:(nrow(df)-1)){
    df[i+1,3] <- as.integer(df[i,4]+1)
    
  }
  return(df)
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
                                                  c("Determine pitch from file name (SFZ)" = "b",
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
                                     # hr(),
                                     # actionButton("manualEditGo",
                                     #              "Manually edit note mapping",
                                     #              width = 300),
                                     hr()
                    ),
                    hr(),
                    conditionalPanel("output.wavlistdropdown",
                                     h3("Bit-depth conversion"),
                                     uiOutput("batchConvertDropdown"),
                                     uiOutput("bitDepth"),
                                     actionButton("batchBitConvert",
                                                  "Convert bit depth",
                                                  width = 300)
                    )
                  ),
                  
                  ####main panel####
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
                             # actionButton("getffLeft",
                             #              "Get Fundamental Frequency",
                             #              width = 300),
                             # br(),
                             # plotOutput("ffLeft"),
                             # sliderInput("ffLeftSlider",
                             #             "x axis:",
                             #             min = 1,
                             #             max = 20000,
                             #             value = c(100,5000)),
                             # tableOutput("ffTableStatsLeft"),
                             # tableOutput("noteTableLeft"),
                             # h3("Pitch mapping"),
                             # radioButtons("pitchPresets",
                             #              "Pitch Presets:",
                             #              c("1 pitch per note" = "a",
                             #                "+/- 1 octave" = "b",
                             #                "Full keyboard map" = "c")),
                             # uiOutput("keycentersliderLeft"),
                             # uiOutput("lokeysliderLeft"),
                             # uiOutput("highkeysliderLeft"),
                             # h3("Amp Envelope"),
                             # uiOutput("AmpEnvAttackLeft"),
                             # uiOutput("AmpEnvReleaseLeft"),
                             # uiOutput("opcodeActionLeft"),
                             # tags$div(style="height:200px;", verbatimTextOutput("opcodeViewLeft")),
                             # textInput("instrumentNameLeft",
                             #           "Instrument Name",
                             #           value = "",
                             #           width = 300,
                             #           placeholder = "sample instrument"),
                             # downloadButton("downloadLeftOpCode",
                             #                label = "Download sfz file")
                             # 
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
                             # actionButton("getffRight",
                             #              "Get Fundamental Frequency",
                             #              width = 300),
                             # br(),
                             # plotOutput("ffRight"),
                             # sliderInput("ffRightSlider",
                             #             "x axis:",
                             #             min = 1,
                             #             max = 20000,
                             #             value = c(100,5000)),
                             # tableOutput("ffTableStatsRight"),
                             # tableOutput("noteTableRight"),
                             # h3("Pitch mapping"),
                             # radioButtons("pitchPresetsRight",
                             #              "Pitch Presets:",
                             #              c("1 pitch per note" = "a",
                             #                "+/- 1 octave" = "b",
                             #                "Full keyboard map" = "c")),
                             # uiOutput("keycentersliderRight"),
                             # uiOutput("lokeysliderRight"),
                             # uiOutput("highkeysliderRight"),
                             # h3("Amp Envelope"),
                             # uiOutput("AmpEnvAttackRight"),
                             # uiOutput("AmpEnvReleaseRight"),
                             # uiOutput("opcodeActionRight"),
                             # tags$div(style="height:200px;", verbatimTextOutput("opcodeViewRight")),
                             # textInput("instrumentNameRight",
                             #           "Instrument Name",
                             #           value = "",
                             #           width = 300,
                             #           placeholder = "sample instrument"),
                             # downloadButton("downloadRightOpCode",
                             #                label = "Download sfz file")
                      )#rigt column end
                    )#fluidRow end
                    ,
                    hr(),
                    HTML("<h3>Mapping Preview</h3>"),
                    fluidRow(
                      column(uiOutput("instrumentName"),
                             tableOutput("mappingDFpreview"),
                             width = 6),
                      column(uiOutput("manualEditDropdown"),
                             uiOutput("mappingSlider"),
                             textInput("instrumentName",
                                       "Name your instrument",
                                       value = "my sample instrument",
                                       width = NULL,
                                       placeholder = NULL),
                             actionButton("createName",
                                          "or, create a random instrument name",
                                          width = 300),
                             hr(),
                             uiOutput("updateButton"),
                             hr(),
                             actionButton("automapper",
                                          "AutoMapDown",
                                          width = 300),
                             width = 6)
                    ),
                    actionButton("viewFinal",
                                 "View and Save SFZ file",
                                 width = 300),
                    hr()
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #### ReactiveValues ####
  allWavs <- reactiveValues()
  allWavs$df <- data.frame()
  allWavs$summary <- data.frame()
  
  noteMapping <- reactiveValues()
  noteMapping$mapping <- vector()
  #index 1 = low note
  #index 2 = root
  #index 3 = high note
  
  sfzobject <- reactiveValues()
  sfzobject$sfz <- list()
  sfzobject$df <- data.frame(matrix(ncol = 4, nrow = 0))
  sfzobject$sfzvector <- vector()
  sfzobject$instrumentName <- vector()
  
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
  
  
  
  ##fundamental frequency left
  # observeEvent(input$getffLeft,{
  #   periodLeft <- periodogram(currentWav$wav[[2]],
  #                             width = 256)
  #   ff <- FF(periodLeft)
  #   
  #   output$ffLeft <- renderPlot({
  #     par(bg = bg)
  #     plot(periodLeft,
  #          col = colours[3],
  #          main = paste0(input$uploadedWavs,"-left-Frequencies"),
  #          xlim = input$ffLeftSlider)
  #   })
  #   
  #   output$ffTableStatsLeft <- renderTable({
  #     lff <- ffStats(ff)
  #     lff
  #   })
  #   
  #   no <- getNote(ff,input$uploadedWavs)
  #   
  #   
  #   output$noteTableLeft <- renderTable({
  #     no
  #   })
  #   
  #   output$keycentersliderLeft <- renderUI({
  #     sliderInput("keycenterLeft",
  #                 "Centre key:",
  #                 min = 21, max = 127,
  #                 value = no[3,2], step = 1)
  #   })
  #   
  #   output$keycentersliderLeft <- renderUI({
  #     sliderInput("keycenterLeft",
  #                 "Centre key:",
  #                 min = 21, max = 127,
  #                 value = no[3,2], step = 1)
  #   })
  #   
  #   output$lokeysliderLeft <- renderUI({
  #     lk <- as.integer(no[3,2])
  #     
  #     if (input$pitchPresets == "b"){
  #       lk <- lk-12
  #       if (lk < 21){
  #         lk <- 21
  #       }
  #     }
  #     
  #     if (input$pitchPresets == "c"){
  #       lk <- 21
  #     }
  #     sliderInput("lokeyLeft",
  #                 "Low key:",
  #                 min = 21, max = 127,
  #                 value = lk, step = 1)
  #     
  #   })
  #   
  #   output$highkeysliderLeft <- renderUI({
  #     hk <- as.integer(no[3,2])
  #     
  #     if (input$pitchPresets == "b"){
  #       hk <- hk+12
  #       if (hk > 127){
  #         hk <- 127
  #       }
  #     }
  #     if (input$pitchPresets == "c"){
  #       hk <- 127
  #     }
  #     
  #     sliderInput("highkeyLeft",
  #                 "High key:",
  #                 min = 21, max = 127,
  #                 value = hk, step = 1)
  #   })
  #   
  #   output$AmpEnvAttackLeft <- renderUI({
  #     sliderInput("ampEnvAttackLeft",
  #                 "Amp envelope - Attack (s):",
  #                 min = 0, max = 5,
  #                 value = 0, step = 0.5)
  #   })
  #   
  #   output$AmpEnvReleaseLeft <- renderUI({
  #     sliderInput("ampEnvReleaseLeft",
  #                 "Amp envelope - Release (s):",
  #                 min = 0, max = 5,
  #                 value = 0, step = 0.5)
  #   })
  #   
  #   output$opcodeActionLeft <- renderUI({
  #     actionButton("generateOpcodesLeft",
  #                  "Generate Op Codes",
  #                  width = 300)
  #   })
  #   
  #   observeEvent(input$generateOpcodesLeft,{
  #     output$opcodeViewLeft <- renderText({
  #       
  #       sfz <- paste0(sfz_header,"\n",
  #                     "<group>","\n",
  #                     "ampeg_attack=",input$ampEnvAttackLeft,"\n",
  #                     "ampeg_release=",input$ampEnvReleaseLeft,"\n",
  #                     "<region>",
  #                     " sample=samples/",input$uploadedWavs,
  #                     " lokey=",input$lokeyLeft,
  #                     " hikey=",input$highkeyLeft,
  #                     " pitch_keycenter=",input$keycenterLeft,
  #                     " seq_length=1"," seq_position=1",
  #                     " lovel=0"," hivel=127"
  #       )
  #       sfzoutput$outtext[[1]] <- sfz
  #       sfz
  #     })
  #   })
  #   
  #   
  # })
  
  # ##fundamental frequency right
  # observeEvent(input$getffRight,{
  #   periodRight <- periodogram(currentWav$wav[[3]],
  #                              width = 256)
  #   ff <- FF(periodRight)
  #   
  #   output$ffRight <- renderPlot({
  #     par(bg = bg)
  #     plot(periodRight,
  #          col = colours[3],
  #          main = paste0(input$uploadedWavs,"-right-Frequencies"),
  #          xlim = input$ffRightSlider)
  #   })
  #   
  #   output$ffTableStatsRight <- renderTable({
  #     rff <- ffStats(ff)
  #     rff
  #   })
  #   
  #   no <- getNote(ff,input$uploadedWavs)
  #   
  #   output$noteTableRight <- renderTable({
  #     no
  #   })
  #   
  #   
  #   output$keycentersliderRight <- renderUI({
  #     sliderInput("keycenterRight",
  #                 "Centre key:",
  #                 min = 21, max = 127,
  #                 value = no[3,2], step = 1)
  #   })
  #   
  #   output$lokeysliderRight <- renderUI({
  #     lk <- as.integer(no[3,2])
  #     
  #     if (input$pitchPresetsRight == "b"){
  #       lk <- lk-12
  #       if (lk < 21){
  #         lk <- 21
  #       }
  #     }
  #     
  #     if (input$pitchPresetsRight == "c"){
  #       lk <- 21
  #     }
  #     sliderInput("lokeyRight",
  #                 "Low key:",
  #                 min = 21, max = 127,
  #                 value = lk, step = 1)
  #     
  #   })
  #   
  #   output$highkeysliderRight <- renderUI({
  #     hk <- as.integer(no[3,2])
  #     
  #     if (input$pitchPresetsRight == "b"){
  #       hk <- hk+12
  #       if (hk > 127){
  #         hk <- 127
  #       }
  #     }
  #     if (input$pitchPresetsRight == "c"){
  #       hk <- 127
  #     }
  #     
  #     sliderInput("highkeyRight",
  #                 "High key:",
  #                 min = 21, max = 127,
  #                 value = hk, step = 1)
  #   })
  #   
  #   output$AmpEnvAttackRight <- renderUI({
  #     sliderInput("ampEnvAttackRight",
  #                 "Amp envelope - Attack (s):",
  #                 min = 0, max = 5,
  #                 value = 0, step = 0.5)
  #   })
  #   
  #   output$AmpEnvReleaseRight <- renderUI({
  #     sliderInput("ampEnvReleaseRight",
  #                 "Amp envelope - Release (s):",
  #                 min = 0, max = 5,
  #                 value = 0, step = 0.5)
  #   })
  #   
  #   output$opcodeActionRight <- renderUI({
  #     actionButton("generateOpcodesRight",
  #                  "Generate Op Codes",
  #                  width = 300)
  #   })
  #   
  #   observeEvent(input$generateOpcodesRight,{
  #     output$opcodeViewRight <- renderText({
  #       
  #       sfzz <- paste0(sfz_header,"\n",
  #                      "<group>","\n",
  #                      "ampeg_attack=",input$ampEnvAttackRight,"\n",
  #                      "ampeg_release=",input$ampEnvReleaseRight,"\n",
  #                      "<region>",
  #                      " sample=../samples/",input$uploadedWavs,
  #                      " lokey=",input$lokeyRight,
  #                      " hikey=",input$highkeyRight,
  #                      " pitch_keycenter=",input$keycenterRight,
  #                      " seq_length=1"," seq_position=1",
  #                      " lovel=0"," hivel=127"
  #       )
  #       sfzoutput$outtext[[2]] <- sfzz
  #       sfzz
  #     })
  #   })
  #   
  # })
  
  # output$downloadLeftOpCode <- downloadHandler(
  #   filename = function() {
  #     n <- input$instrumentNameLeft
  #     if (nchar(n) == 0){
  #       n <- "sample_instrument"
  #     }
  #     paste(n,".sfz",sep="")
  #   },
  #   content = function(file) {
  #     writeLines(sfzoutput$outtext[[1]], file)
  #   }
  # )
  
  # output$downloadRightOpCode <- downloadHandler(
  #   filename = function() {
  #     n <- input$instrumentNameRight
  #     if (nchar(n) == 0){
  #       n <- "sample_instrument"
  #     }
  #     paste(n,".sfz",sep="")
  #   },
  #   content = function(file) {
  #     writeLines(sfzoutput$outtext[[2]], file)
  #   }
  # )
  
  ####Automap files and view####
  ##modal window for macro mode
  observeEvent(input$macroGo, {
    
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
    #Determine pitch from file name (SFZ)
    if (input$macroPresets=='b'){
      #reset df
      sfzobject$df <- data.frame(matrix(ncol = 4, nrow = 0))
      
      for (i in 1:length(input$loadWavFile$name)){
        f <- input$loadWavFile$name[i]
        ###parse to text to get the note name
        note.number <- search.freq(f)
        noteMapping$mapping[1] <- note.number
        noteMapping$mapping[2] <- note.number
        noteMapping$mapping[3] <- note.number
        
        print(paste0('note.number: ',note.number))
        #populate ReactiveValue object
        sfzobject$df[nrow(sfzobject$df)+1,1] <- f
        sfzobject$df[nrow(sfzobject$df),2] <- noteMapping$mapping[2]
        sfzobject$df[nrow(sfzobject$df),3] <- noteMapping$mapping[1]
        sfzobject$df[nrow(sfzobject$df),4] <- noteMapping$mapping[3]
        
        sfzzMacro <- paste0("//",note.number,"\n",
                            "<region>",
                            " sample=../samples/",input$loadWavFile$name[i],
                            " lokey=",noteMapping$mapping[1],
                            " hikey=",noteMapping$mapping[3],
                            " pitch_keycenter=",noteMapping$mapping[2],
                            " seq_length=1"," seq_position=1",
                            " lovel=0"," hivel=127","\n"
        )
        sfzobject$sfzvector[i] <- sfzzMacro
      }
      
    }
    
    #Simple Percussion mapping (SFZ)
    if (input$macroPresets=='c'){
      #reset df
      sfzobject$df <- data.frame(matrix(ncol = 4, nrow = 0))
      
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
        sfzobject$sfzvector[i] <- sfzzMacro
        
        sfzobject$df[nrow(sfzobject$df)+1,1] <- input$loadWavFile$name[i]
        sfzobject$df[nrow(sfzobject$df),2] <- note.number
        sfzobject$df[nrow(sfzobject$df),3] <- note.number
        sfzobject$df[nrow(sfzobject$df),4] <- note.number
        note.number <- note.number + 1
      }
      
    }
    
    colnames(sfzobject$df) <- c("Input File",
                                "Note Number",
                                "Low note mapping",
                                "High note mapping")
    print(sfzobject$df)
    sfzzContentUnlist <- unlist(sfzobject$sfzvector)
    sfzzContentUnlist <- sort(sfzzContentUnlist)
    
    sfzzFinal <- c(sfzzMacroHeader,sfzzContentUnlist)
    sfzobject$sfz[[1]] <- sfzzFinal
    
    output$macroSFZoutput <- renderText({
      sfzobject$sfz[[1]]
    })
    
    output$dfPreviewMacro <- renderTable({
      sfzobject$df
    })
    
    
    showModal(modalDialog(
      title = "Macro mode",
      renderTable(sfzobject$df),
      #uiOutput("manualEditDropdown"),
      #uiOutput("editRadio"),
      # conditionalPanel(condition = 'input.select1=="show"',
      #                  uiOutput("showRootNote"),
      #                  sliderInput("HighLowNotes",
      #                              "Select high and low range",
      #                              min=21,
      #                              max=108,
      #                              value=c(noteMapping$mapping[1],
      #                                      noteMapping$mapping[3])
      #                              ),
      #                  uiOutput("manualButton")
      # ),
      hr(),
      verbatimTextOutput("macroSFZoutput"),
      downloadButton("downloadMacroSFZ",
                     label = "Download sfz file"),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton("macromodalClose", "Close")
      )
    )
    )
  
  })
  
  
  ####manual editting####
  # observeEvent(input$manualEditGo,{
  #   
  #   #create UI elements for editting macro
  #   ##create a dropdown for editting wavs
  #   output$manualEditDropdown <- renderUI({
  #     
  #     selectInput(
  #       inputId = "uploadedWavs3",
  #       label = "Manually edit mapping",
  #       choices = input$loadWavFile$name,
  #       selected = NULL,
  #       multiple = FALSE,
  #       selectize = TRUE,
  #       width = 400,
  #       size = NULL
  #     )
  #   })
  #   
  #   ##update sfz button
  #   output$updateButton <- renderUI({
  #     actionButton("updateGo",
  #                  "Update SFZ file",
  #                  width = 300)
  #   })
  #   
  #   
  #   # output$editRadio <- renderUI({
  #   #   radioButtons("select1",
  #   #                "Editting mode",
  #   #                c("Off" = "hide", "On" = "show")
  #   #   )
  #   # })
  #   
  #   
  #   
  #   ##show modal for editting
  #   showModal(modalDialog(
  #     title = "Edit mapping",
  #     #renderTable(final.df),
  #     tableOutput("dfPreviewMacro"),
  #     uiOutput("manualEditDropdown"),
  #     sliderInput("HighLowNotes",
  #                 "Select high and low range",
  #                 min=21,
  #                 max=108,
  #                 value=c(noteMapping$mapping[1],
  #                         noteMapping$mapping[3])
  #     ),
  #     uiOutput("updateButton"),
  #     hr(),
  #     #verbatimTextOutput("macroSFZoutput"),
  #     # downloadButton("downloadMacroSFZ",
  #     #                label = "Download sfz file"),
  #     size = "l",
  #     easyClose = TRUE,
  #     footer = tagList(
  #       actionButton("macromodalClose", "Close")
  #     )
  #   )
  #   )
  #   
  # })
  
  observeEvent(input$updateGo,{
    removeModal()
    #update high and low notes based on sliders
    noteMapping$mapping[1] <- input$HighLowNotes[1]
    noteMapping$mapping[3] <- input$HighLowNotes[2]
    
    sfz_header <- sub('//sfz instrument',
                      paste0('//',sfzobject$instrumentName),
                      sfz_header)
    
    sfzzMacroHeader <- paste0(sfz_header,"\n",
                              "<group>","\n",
                              "ampeg_attack=",input$ampEnvAttackMacro,"\n",
                              "ampeg_release=",input$ampEnvReleaseMacro,"\n"
    )

    

    ####find index of wav to update
    editWav.index <- grep(input$uploadedWavs3,sfzobject$df[,1])
    print(paste0("Edit index: ",editWav.index))
    note.number <- search.freq(input$uploadedWavs3)
    noteMapping$mapping[2] <- note.number
    
    sfzobject$df[editWav.index,1] <- input$uploadedWavs3
    sfzobject$df[editWav.index,2] <- noteMapping$mapping[2]
    sfzobject$df[editWav.index,3] <- noteMapping$mapping[1]
    sfzobject$df[editWav.index,4] <- noteMapping$mapping[3]
    
    print(sfzobject$df)
    
    #iterate through df creating sfz object
    for (i in 1:nrow(sfzobject$df)){
      sfzzMacro <- paste0("//",sfzobject$df[i,2],"\n",
                          "<region>",
                          " sample=../samples/",sfzobject$df[i,1],
                          " lokey=",sfzobject$df[i,3],
                          " hikey=",sfzobject$df[i,4],
                          " pitch_keycenter=",sfzobject$df[i,2],
                          " seq_length=1"," seq_position=1",
                          " lovel=0"," hivel=127","\n"
      )
      sfzobject$sfz[i] <- sfzzMacro
    }
    
    sfzzContentUnlist <- unlist(sfzobject$sfz)
    sfzzContentUnlist <- sort(sfzzContentUnlist)
    sfzzFinal <- c(sfzzMacroHeader,sfzzContentUnlist)
    sfzobject$sfz[[1]] <- sfzzFinal
  })
  
  
  observeEvent(input$macromodalClose,{
    removeModal()
  })
  
  observeEvent(input$viewFinal,{
    showModal(modalDialog(
      title = "Finalexport",
      renderTable(sfzobject$df),
      hr(),
      verbatimTextOutput("macroSFZoutput"),
      downloadButton("downloadMacroSFZ",
                     label = "Download sfz file"),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton("macromodalClose", "Close")
      )
    )
    )
  })
  
  output$downloadMacroSFZ <- downloadHandler(
    filename = function() {
      #n <- "sample_instrument"
      n <- sfzobject$instrumentName[1]
      paste(n,".sfz",sep="")
    },
    content = function(file) {
      writeLines(sfzobject$sfz[[1]], file)
    }
  )
  
  output$mappingDFpreview <- renderTable({
    sfzobject$df
  })
  
  ####editting on mainpage####
  output$manualEditDropdown <- renderUI({
    
    selectInput(
      inputId = "uploadedWavs3",
      label = "Manually edit mapping",
      choices = input$loadWavFile$name,
      selected = NULL,
      multiple = FALSE,
      selectize = TRUE,
      width = 400,
      size = NULL
    )
  })
  
  ##slider for mapping
  output$mappingSlider <- renderUI({
    sliderInput("HighLowNotes",
                "Select high and low range",
                min=21,
                max=108,
                value=c(noteMapping$mapping[1],
                        noteMapping$mapping[3])
    )
  })
  
  
  ##update sfz button
  output$updateButton <- renderUI({
    actionButton("updateGo",
                 "Update SFZ file",
                 width = 300)
  })
  
  
  ####batch bit conversion####
  output$batchConvertDropdown <- renderUI({
      req(input$loadWavFile)
      
      selectInput(
        inputId = "uploadedWavs2",
        label = "Uploaded wav files",
        choices = input$loadWavFile$name,
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = 400,
        size = NULL
      )
  })
  
  output$bitDepth <- renderUI({
    selectInput(
      inputId = "bitDepth",
      label = "Output Bit-depth",
      choices = c("16","8"),
      selected = "16",
      multiple = FALSE,
      selectize = TRUE,
      width = 200,
      size = NULL
    )
  })
  
  
  observeEvent(input$batchBitConvert,{
    
      print("Bit Depth conversion")
      f <- input$uploadedWavs2
      print(f)
      f.index <- grep(f,input$loadWavFile$name)
      print(f.index)
      
      wavToConvert <- readWave(input$loadWavFile$datapath[f.index],
                          toWaveMC = FALSE)
      currentBitDepth <- wavToConvert@bit
      newBitDepth <- as.character(input$bitDepth)
      wavToConvert <- bitDepth(wavToConvert,newBitDepth)
      #put convertedwave in reactiveValue object
      currentWav$wav[[4]] <- wavToConvert
      
      output$bitConvertPlot <- renderPlot({
        plot(currentWav$wav[[4]])
      })
      
      

      showModal(modalDialog(
        title = "Bit Depth Conversion",
        paste0("File is ",f,"\n",
               "Original bit depth is ",currentBitDepth,"\n",
               "New bit depth is ",newBitDepth),
        hr(),
        plotOutput("bitConvertPlot"),
        downloadButton("downloadBitConverted",
                       label = "Download Bit-converted wav"),
        easyClose = TRUE
      ))
      
      output$downloadBitConverted <- downloadHandler(
        filename = function() {
          ff <- sub('.wav','',f)
          paste0(ff,"_",newBitDepth,"bit.wav")
        },
        content = function(file) {
          writeWave(currentWav$wav[[4]], file, extensible = FALSE)
        }
      )
    
  })
  
  eventReactive(intput$instrumentName,{
    sfzobject$instrumentName[1] <- input$instrumentName
  })
  
  observeEvent(input$createName,{
    sfzobject$instrumentName[1] <- inameFunction()
  })
  
  output$instrumentName <- renderUI({
    HTML(paste0('<h3>',
                sfzobject$instrumentName[1],
                '</h3>'))
    
  })
  
  observeEvent(input$automapper,{
    sfzobject$df <- autoMapperDown(sfzobject$df)
    print(sfzobject$df)
    #recreate the SFZ object
    sfz_header <- sub('//sfz instrument',
                      paste0('//',sfzobject$instrumentName),
                      sfz_header)
    
    sfzzMacroHeader <- paste0(sfz_header,"\n",
                              "<group>","\n",
                              "ampeg_attack=",input$ampEnvAttackMacro,"\n",
                              "ampeg_release=",input$ampEnvReleaseMacro,"\n"
    )
    
    for (i in 1:nrow(sfzobject$df)){
      sfzzMacro <- paste0("//",sfzobject$df[i,2],"\n",
                          "<region>",
                          " sample=../samples/",sfzobject$df[i,1],
                          " lokey=",sfzobject$df[i,3],
                          " hikey=",sfzobject$df[i,4],
                          " pitch_keycenter=",sfzobject$df[i,2],
                          " seq_length=1"," seq_position=1",
                          " lovel=0"," hivel=127","\n"
      )
      sfzobject$sfz[i] <- sfzzMacro
    }
    
    sfzzContentUnlist <- unlist(sfzobject$sfz)
    sfzzContentUnlist <- sort(sfzzContentUnlist)
    sfzzFinal <- c(sfzzMacroHeader,sfzzContentUnlist)
    sfzobject$sfz[[1]] <- sfzzFinal
    
    
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
