#
# Author: Mitchi Kamigaki-Baron
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.

#relevant libraries used
library(shiny)
library(shinyjs)
library(tidyverse)
library(shinyWidgets)
library(ggpubr)
library(ggplot2)
library(emojifont)
library(rsconnect)

#data preparation step
sec_dat <- read_csv("sec_dat.csv")

vf_up <- sec_dat %>% mutate(status = dplyr::case_when(vowel == "á" ~ "stress", vowel == "é" ~ "stress", vowel == "í" ~ "stress", vowel == "ó" ~ "stress", vowel == "ú" ~ "stress",vowel == "a" ~ "unstress", vowel == "e" ~ "unstress", vowel == "i" ~ "unstress", vowel == "o" ~ "unstress", vowel == "u" ~ "unstress", vowel == "ə1" ~ "schwa")) %>% filter(! vowel == "ə2") %>% filter(! vowel == "ə2") %>%
  #setting up data so i can create interactive filter later part 1
  mutate(vowel = ifelse(vowel == "ə1", "schwa", vowel))%>% # R was having a hard time recognizing this symbol
  mutate(select_me= case_when(vowel == "á" ~ "non-high", vowel == "é" ~ "non-high", vowel == "í" ~ "high", vowel == "ó" ~ "non-high", vowel == "ú" ~ "high", vowel == "a" ~ "non-high", vowel == "e" ~ "non-high", vowel == "i" ~ "high", vowel == "o" ~ "non-high", vowel == "u" ~ "high", vowel == "ə1" ~ "schwa"))
#setting up data so i can create interactive filter later part 2
vf_up$status<- as.factor(vf_up$status) #make sure this is seen as a factor
schwa_dat<- vf_up  %>% filter(str_detect(vowel, "schwa")) #filter to create dataset that only contains schwa
vf_high <- vf_up %>% filter(!str_detect(vowel, "schwa")) #filter to create dataset that contains all but schwa
v_means <- vf_up %>%
  group_by(vowel) %>%
  summarise(f13 = mean(f13),
            f23 = mean(f23)) %>%
  ungroup()
#want vowels to be labeled at their mean values

no_schwa <- vf_up%>% filter(!str_detect(vowel, "ə2")) %>% mutate(pair = case_when(vowel == "á" ~ " á & a" , vowel == "é" ~ " é & e ", vowel=="í" ~ " í & i ",  vowel=="ó"~" ó & o ", vowel== "ú" ~ " ú & u ", vowel== "a" ~ " á & a", vowel =="e" ~" é & e ", vowel == "i" ~ " í & i ", vowel == "o" ~ " ó & o ", vowel == "u" ~ " ú & u ")) %>% filter(!str_detect(vowel, "ə"))
#want only non schwa data to be manipulated and keep schwa at the position.
#end of data preparation


#beginning the ui
ui <- fluidPage(
  titlePanel("The Vowel System in Secwepemctsín"), #title page
  h5("Research and webpage by Michelle Kamigaki-Baron"), #header
  a(href="mailto:michelle.baron@ubc.ca", "Click here to contact me 📧 michelle.baron@ubc.ca "), #adding an email link
  h5("The purpose of this webpage is for semi-informed linguistic audiences to interface with the findings of this study."), #caption
  br(), #line break
  sidebarPanel( #side panel positioning
    tags$h5("Kuipers (1979) describes 3 types of vowels in Secwepemctsín:"), #text
    img(width = "100%", src='3_types.png', align = "center"), #add picture
    tags$h5("Some examples:"), #text for sounds
    tags$h5("á & a: cneq̓átkwa (polluted water)"),
    tags$audio(src = "polluted water.mp3", type = "audio/mp3", autoplay = FALSE, controls = NA), #adding audio, autoplay = FALSE
    tags$h5("é & e: menmén (shade)"),
    tags$audio(src = "shade.mp3", type = "audio/mp3", autoplay = FALSE, controls = NA),
    tags$h5("í & i: i7í7tc (pocket knife)"),
    tags$audio(src = "pocket knife.mp3", type = "audio/mp3", autoplay = FALSE, controls = NA),
    tags$h5("ó & o: kóso (pig)"),
    tags$audio(src = "pig.mp3", type = "audio/mp3", autoplay = FALSE, controls = NA),
    tags$h5("ú & u: k̓ut̓múy̓e (a walk for pleasure)"),
    tags$audio(src = "walk.mp3", type = "audio/mp3", autoplay = FALSE, controls = NA),
    tags$h5("ə: mstntsútk (to try)*"),
    tags$audio(src = "to try.mp3", type = "audio/mp3", autoplay = FALSE, controls = NA),
    tags$h6("*The vowel ə is not written. Listen for it between the m and s sounds!")
    )
  ,
  mainPanel( #main panel
    tags$h5("Abstract: Secwepemctsín (Shuswap) is a Northern Interior Salish language spoken in British Columbia. Kuipers (1974, 22, 26-27) makes the claim that in Secwepemctsín, unstressed vowels will reduce to a vowel that is phonetically equivalent to epenthetic schwa (ə). Vowel reduction is a widely attested phenomena, occurring most frequently in stress-timed languages that are reliably cued by a difference in vowel duration between stressed and unstressed positions (Lehiste 1970). This study investigates unstressed vowel reduction (UVR) in Secwepemctsín by conducting an acoustic analysis of the vowel system. To test the likeness between unstressed vowels and schwa, vowel quality (F1, F2) and quantity (duration) measures were extracted and compared using Pillai score to measure the degree of overlap (threshold Pillai = 0.015) and Euclidean distance to access the relative distances between the vowel categories. Results suggest that in Secwepemctsín, partial unstressed vowel reduction occurs only in non-high vowels. Contrary to Kuipers (1974) claim, this behavior is more consistent with what is seen in languages like Bulgarian (Wood and Pettersson 1988) suggesting that Secwepemctsín may exhibit vowel reduction via jaw raising rather than the conconically known relaxation of tongue constriction."),
    pickerInput(inputId= "vowelInput", label= "Select high and non-high vowels to see this relationship for yourself!", choices = c("high", "non-high"), selected = c("high", "non-high"), options  = list(`actions-box` = TRUE, `select-all-text` = "All Vowels"), multiple = TRUE), #adding picker input
    tags$h5("Figure 1"),
    plotOutput("vowel_chart", click = "plot_click"), verbatimTextOutput("info"), #plot output position
    tags$h5("In Figure 1 you can see how each vowel category maps in F1- F2 space. F1 and F2 are measurements used in linguistics to measure vowels. This study finds a contrast between high vowels (i and u) and non-high vowels (a e o) such that unstressed vowels overlap with schwa only for non-high vowels!"),
    tags$h6("Citations: Kuipers, A. H. (1974). The Shuswap Language: Grammar, Text, Dictionary. The Hague, Netherlands: Mouton. Lehiste, I. (1970). Suprasegmentals. Cambridge (Massachusetts) and London: M.I.T. Press.
"
     )

)

)


#start of server
server <- function(input, output) {
  observe({
    print(input$vowelInput)
  })

  filtered_data <-
    reactive({ #interactive component to select different vowel classes
      vf_high %>% filter(
                      select_me == input$vowelInput )
         })

  output$vowel_chart <- #output element for the histogram plot portion of this shiny app
    renderPlot({
       filtered_data()%>%
        ggplot(aes(x= f23, y= f13, col= vowel, fill=vowel)) +
        scale_alpha(range=c(0.2,0.7)) +
        geom_text(data=v_means, aes(label=vowel), color='black', size=8) +
        scale_x_reverse() + scale_y_reverse() +
        stat_ellipse(mapping = aes(alpha=ifelse(status == "stress", 0.2, 0.6)), level = 0.67, lwd = 1.1, geom="polygon") +
        stat_ellipse(mapping= aes(alpha=0.2), level = 0.67, lwd = 1.1, geom="polygon", data=schwa_dat, linetype = 2)+
        labs(x="F2", y="F1") +

        scale_color_manual(values=c('violet','violet', 'seagreen2', 'seagreen2', 'royalblue', "royalblue", "purple", "purple", 'red', "orange", "orange"))+ scale_fill_manual(values=c('violet','violet', 'seagreen2', 'seagreen2', 'royalblue', "royalblue", "purple", "purple", 'red', "orange", "orange"))+

        theme(panel.background = element_rect(fill = 'whitesmoke', color = 'whitesmoke'),
              panel.grid = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none")+ xlim(2800, 700) + ylim(800, 200)

    })
      output$data_table <- #output placeholder for data table
        renderTable({
          filtered_data()
        })



    }




shinyApp(ui = ui, server = server)

