# Koodit datahuoneen kojelautaan.



library(shiny, warn.conflicts = F)
library(shinydashboard, warn.conflicts = F)
library(shinyWidgets, warn.conflicts = F)
library(bslib)
library(bsicons)
library(tidyverse, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)
library(markdown, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(gghighlight, warn.conflicts = F)
library(httr, warn.conflicts = F)
library(jsonlite, warn.conflicts = F)
library(plotly, warn.conflicts = F)
library(shiny.router, warn.conflicts = F)
library(magrittr, warn.conflicts = FALSE)
library(shinyjs, warn.conflicts = FALSE)

source("funktiot.R", encoding = 'UTF-8')
source("pages_r.R", encoding = 'UTF-8')

lisaa_logo <- F #lisää datahuonelogong yläoikealle

### ladataan data ----------------

#haetaan kuukaudet mistä sähkönkäyttöädataa
kuukaudet <<- feather::read_feather("data/kuukaudet_saatavilla.feather") %>%
  arrange(kuukaudet) %>%
  pull()


#kuntien nimet
kunnat <- feather::read_feather("data/kunnat.feather")
Kunnan_nimet <- kunnat %>% distinct(kunnan_nimi)

#jostain syystä tätä ei voi pitää observe eventin sisällä muut datat ladataan sertrverissä
boxplotit_asuntokunnat <- lataa_data("asuntokunnittain_boxplotit", kuukaudet)
aikasarja_data_raw <- feather::read_feather("data/aikasarjat/kulutus_kk.feather")

# URL osoitteet -----------------------------------------

#url-juuri sahkokaytto sivuille
sahk_etusivu_url <<- "sahkonkulutus"

#url-lehdet sivuille (mahdollisesti ei toimienää)
sah_kokonaiskulutus <<- paste0(sahk_etusivu_url,"/kokonaiskulutus")
sah_desiili_url <<- paste0(sahk_etusivu_url,"/sosioekonomiset")
sah_reaaliaikainen_url <<- paste0(sahk_etusivu_url,"/reaaliaikainen")
sah_tausta_url <<- paste0(sahk_etusivu_url,"/tausta")

#url-juuri tyomarkkinoille
tyomarkkinat_etusivu_url <- "tyomarkkinat"

#url-lehdet tyomarkkinoille (tämäkin saattaa olla toimimatta kun siirryttiin tabsetpanel-rakenteeseen)
tyomarkkinat_ukrainat_url <<- paste0(tyomarkkinat_etusivu_url, "/ukrainalaiset")


# ladataan ukrainadata ----------------------------------------------------

## ukraina datan lataaminen
kotikunta <-  read_csv("./data/summaries/sex_month_pop.csv")
ei_kotikuntaa <-  read_csv("./data/summaries/age_sex_month_nonpop.csv")
ikajakauma <- read_csv("./data/summaries/age_gender.csv")
toimialat <- read_csv("./data/summaries/industry.csv")
ammatit <- read_csv("./data/summaries/occupations.csv")
employed  <- read_csv("./data/summaries/employed_age_gender.csv")
employed_kotikunta <- read_csv("./data/summaries/employed_age_pop.csv")

## age group to factor
levels <- c("alle 15","15-19", "20-24", "25-54", "55-64", "yli 64")
ei_kotikuntaa <- ei_kotikuntaa %>% mutate(age_group = factor(age_group, levels = levels))

levels <- ikajakauma %>% distinct(age_group) %>% pull()
ikajakauma <- ikajakauma %>% mutate(age_group = factor(age_group, levels = levels))

## industry to factor
levels <- toimialat %>% distinct(toimiala_nimi) %>% pull()
toimialat <- toimialat %>% mutate(toimiala_nimi = factor(toimiala_nimi, levels = levels))

## profession to factor
levels <- ammatit %>% distinct(t3_nimi) %>% pull()
ammatit <- ammatit %>% mutate(t3_nimi = factor(t3_nimi, levels = levels))

#testi miten orgasnisaatiot toimii

## ukraina kuva-asetukset
alpha_u <- 0.8
font_size <- 15

# UI -------------------------------------
ui <- page_navbar(

  theme = bs_theme(
    primary = "#f16c13",
    secondary = "#8482BD",
    bg = "#f7f4e9",
    fg = "#171716",
    font_scale = 1.1
    ),

  tags$head(
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "32x32",
      href = "DH_pikkulogo.png"
      )
    ),


      # Application title
  #title =  div(img(src="Datahuone_graafi_littee.png", height = 50)),

  id = "navbarID",
  window_title = "Datahuone",
  prod_front_page(),

# sähköjutut ---------------------------
  nav_menu(
    title = "Ympäristö & energia",
    icon = img(src="Ikoni_ympäristö.svg", height = 25),

    nav_panel(
      title = 'Kotitalouksien sähkönkäyttö',
      value = sahk_etusivu_url,
      navset_pill(

    prod_ye_front_page(),


        ### aikasarjapaneeli ----------------------
    prod_energy_time_series(),


### desiilipaneeli --------------------------------
    prod_decile_page(),
 # reaaliaikainen ----------------------------------------------
    prod_real_time_energy(),

    nav_panel(
      title = "Taustaa datasta",
      #value = ,  #valueta käyteteään url muodostamiseen
      fluidPage(
        fluidRow(includeMarkdown("tekstit/dataselite.md"))
        )
      ))

)
),

# työmarkkinat ----------------------------------------------------

 ## ukrainalaiset ----------------------------------------------
  prod_ukr()

)

# Enable thematic
thematic::thematic_shiny(font = "auto")

# Change ggplot2's default "gray" theme
theme_set(theme_bw(base_size = 16))

# SERVERI ------------------------------------------------
server <- function(input, output, session) {


  # Ikonit etusivulla -------------------------------------------------------

  observeEvent(
    input$btn_ymp,{
      updateTabsetPanel(session, "navbarID", selected = sahk_etusivu_url)
    }
  )

  observeEvent(
    input$btn_tyo,{
      updateTabsetPanel(session, "navbarID", selected = tyomarkkinat_ukrainat_url)
    }
  )

  # Reaktiiviset datasetit ----------------------

  ## ladataa boxplotdatat muistille vasta kun käyttäjä menee sivulle:

  observeEvent(input$navbarID, {
    #hakee fingridin viikkodatan vain jos on sahkonkulutus/reaaliaikainen välilehdellä'
    if(input$navbarID  %in% c(sahk_etusivu_url, sah_desiili_url, sah_reaaliaikainen_url)){

      print("Loading data")

      #lataa_data_cluster <- parallel::makeCluster(parallel::detectCores() - 1)
      #parallel::clusterExport(lataa_data_cluster, kuukaudet)

      boxplotit_sopimukset   <<- lataa_data("asuntokunnittain_sopimustenlkm_boxplotit",kuukaudet)
      boxplotit_maaraik     <<- lataa_data("asuntokunnittain_maaraaik_boxplotit", kuukaudet)
      boxplotit_lammitys    <<- lataa_data("asuntokunnittain_lammitysmuoto_boxplotit", kuukaudet)
      boxplotit_taajama <<- lataa_data("asuntokunnittain_taajama_boxplotit", kuukaudet)
      boxplotit_kerrostalo <<- lataa_data("asuntokunnittain_kerrostalo_boxplotit", kuukaudet)
      boxplotit_askoko <<- lataa_data("asuntokunnittain_askoko_boxplotit", kuukaudet)

      #stopCluster(lataa_data_cluster)

      print("Loaded")

      boxplotlista <<- list(
        "-" = boxplotit_asuntokunnat,
        "sopimuksien lukumäärä" = boxplotit_sopimukset,
        "määräaikaiset sopimukset"= boxplotit_maaraik,
        "lämmitys riippuvainen sähköstä" = boxplotit_lammitys,
        "asuu taajama-alueella" = boxplotit_taajama,
        "asuu kerrostalossa" = boxplotit_kerrostalo,
        "asuntokunnan koko"= boxplotit_askoko
      )
    }
  })

  observeEvent(input$resetSahko, {

    shinyjs::reset("sahkoDate")

  })

  boxplot_data <- reactive({
    return(boxplotlista[[input$soptyyp]][[input$kk]])
  })


  kunta_data <- reactive({
    sapply(kunta_kvantiilit, hae_kunta, haettava_kunta = input$kotkunt) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("kvantiili") %>%
      as_tibble() %>%
      unnest(cols = as.character(kuukaudet))
  })

  output$boxplot_otsikko <- renderText({
    paste0("Sähkönkulutus tulokymmenyksittäin ",
           kuukaudet_suom(input$kk),"ssa ",
           format(as.Date(input$kk), "%Y")
    )
  })

  valintojen_valinnat <- reactive({

    if (input$tarktaso == 'Maakunnittain') {

      kunnat %>%
        filter(maakunta != "Ahvenanmaa") %>%
        distinct(maakunta) %>%
        pull()

    } else if (input$tarktaso == 'Kunnittain') {

      kunnat %>%
        filter(maakunta != "Ahvenanmaa") %>%
        distinct(kunnan_nimi) %>%
        pull()

    } else {

      return(c("Suomi"))

    }

  })

  observe({
    if(input$tarktaso != 'Koko maa') {
      updateSelectInput(session,
                        'valitut',
                        choices = valintojen_valinnat())

    } else {
      updateSelectInput(session,
                        'valitut',
                        selected = 'Suomi',
                        choices = valintojen_valinnat())
    }
  })

  aikasarja_data <- reactive({

    if (input$tarktaso == 'Maakunnittain') {

      aikasarja_data_raw %>%
        left_join(kunnat) %>%
        group_by(maakunta, kuukausi) %>%
        summarise(sahkonkul = sum(sahkonkul),
                  sum_ak = sum(sum_ak)) %>%
        rename(alue = maakunta)

    } else if (input$tarktaso == 'Kunnittain') {

      aikasarja_data_raw %>%
        rename(alue = kunnan_nimi)

    } else {

      aikasarja_data_raw %>%
        group_by(kuukausi) %>%
        summarise(sahkonkul = sum(sahkonkul),
                  sum_ak = sum(sum_ak)) %>%
        mutate(alue = 'Suomi')

    }

  })


  ## desiili sivu ---------------------------------------
  output$taustaotsikko <- renderText(

    if(input$soptyyp != '-'){
      return("Taustaa tulokymmenyksistä")
    } else{
      NULL
    }

  )


  output$askumaarat <- renderText({

    sum <- sum(boxplotit_asuntokunnat[[input$kk]]$n)
    tuhaterotin(sum) %>% as.character()
  })



  output$dessuhde <- renderText({
    values <- boxplotit_asuntokunnat[[input$kk]] %>%
      filter(desiili %in% c(1,10)) %>%
      group_by(desiili) %>%
      summarise(y_mean = mean(y_mean)) %>%
      ungroup() %>%
      select(y_mean) %>%
      pull()

    tuhaterotin(round(values[2]/values[1],2)) %>% as.character()
    })

  # Plotit ----------------------------------------

  ## aikasarjadata --------------------------------------
  output$aikasarjaplot <- renderPlot({

    data <- aikasarja_data()

    if(input$suure == 'per capita'){

      data <- data %>%
        mutate(sahkonkul = sahkonkul / sum_ak)

      y_akseli <-"Sähkönkulutus kWh / hlö"

    } else{
      data <- data %>%
        mutate(sahkonkul = sahkonkul / 1000)

      y_akseli <- "Sähkönkulutus MWh"
    }

    data %>%
      ungroup() %>%
      filter(kuukausi >= input$aikasarja[1],
             kuukausi <= input$aikasarja[2]) %>%
      ggplot(
        aes(x = kuukausi,
            y = sahkonkul,
            colour = alue),
        alpha = 0.2
      ) +
      geom_line(size = 1)+
      gghighlight(alue %in% input$valitut, label_key = alue) +
      scale_color_brewer(palette = "Dark2") +
      scale_y_continuous(name = y_akseli,
                         label = tuhaterotin)+
      scale_x_date(name = NULL,
                   label = formatoi_kuukaudet_plot) +
      coord_cartesian(ylim = c(0,max(data$sahkonkul))) +
      theme_light() +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14))
  })

  ## Tausta kuvaajat --------------------------------------------

  output$tausta <- renderPlot({

    if (input$soptyyp == 'määräaikaiset sopimukset') {

      boxplotit_maaraik[[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili),
          y = n,
          fill = factor(is_fixed_term_agreement)))+
        geom_col(position = 'fill')+
        scale_fill_manual(
          name = "Määräaikainen sähkösopimus",
          labels = c("Ei", "Kyllä"),
          values = c('#363197', '#F16c13')
        )+
        labs(x = 'Tulokymmennys',
             y = 'Osuus asuntokunnista')+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())+

        scale_y_continuous(label = prosenttierotin)

    } else if(input$soptyyp == 'lämmitys riippuvainen sähköstä') {

      boxplotit_lammitys[[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili),
          y = n,
          fill = is_heating_dependent_on_electricity))+
        geom_col(position = 'fill')+
        scale_fill_manual(
          name = "Lämmitys riippuvainen sähköstä",
          labels = c("Ei", "Kyllä"),
          values = c('#363197', '#F16c13')
        )+
        labs(x = 'Tulokymmennys',
             y = 'Osuus asuntokunnista')+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12))+
        scale_y_continuous(label = prosenttierotin)

    } else if (input$soptyyp == 'sopimuksien lukumäärä') {

      boxplotlista[[input$soptyyp]][[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili),
          y = n,
          fill = yli_1_sopimus))  +
        geom_col(position = 'fill') +
        scale_fill_manual(
          name = "Asuntokunnalla solmittu yli yksi sopimus",
          labels = c("Ei", "Kyllä"),
          values = c('#363197', '#F16c13')
        )+
        labs(x = 'Tulokymmennys',
             y = 'Osuus asuntokunnista')+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12))+
        scale_y_continuous(label = prosenttierotin)

    } else if (input$soptyyp == 'asuu taajama-alueella') {

      boxplotlista[[input$soptyyp]][[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili),
          y = n,
          fill = factor(asuu_taajamassa)))  +
        geom_col(position = 'fill') +
        scale_fill_manual(
          name = "Asuntokunta asuu taajama-alueella",
          labels = c("Ei", "Kyllä"),
          values = c('#363197', '#F16c13')
        )+
        labs(x = 'Tulokymmennys',
             y = 'Osuus asuntokunnista')+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12))+
        scale_y_continuous(label = prosenttierotin)
    } else if (input$soptyyp == 'asuu kerrostalossa') {

      boxplotlista[[input$soptyyp]][[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili),
          y = n,
          fill = asuu_kerrostalossa ))  +
        geom_col(position = 'fill') +
        scale_fill_manual(
          name = "Asuntokunta asuu kerrostalossa",
          labels = c("Ei", "Kyllä"),
          values = c('#363197', '#F16c13')
        )+
        labs(x = 'Tulokymmennys',
             y = 'Osuus asuntokunnista')+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12))+
        scale_y_continuous(label = prosenttierotin)
    } else if (input$soptyyp == 'asuntokunnan koko') {

      boxplotlista[[input$soptyyp]][[input$kk]] %>%
        ggplot(aes(
          x = factor(desiili),
          y = n,
          fill = yli_1_akkoko))  +
        geom_col(position = 'fill') +
        scale_fill_manual(
          name = "asuntokunnan koko yli yksi henkilö",
          labels = c("Ei", "Kyllä"),
          values = c('#363197', '#F16c13')
        )+
        labs(x = 'Tulokymmennys',
             y = 'Osuus asuntokunnista')+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12))+
        scale_y_continuous(label = prosenttierotin)
    }



    })


## boxplotit ----------------------------------------
  output$boxplot <- renderPlot({



    if (input$soptyyp == 'lämmitys riippuvainen sähköstä') {
### lämmitysboxplot ----------------------------------
      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili)
          )
        )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            position = position_nudge(
              x = rep(c(-0.2,0.2),10)
            ),
            width = 0.2)

      }
        plot <- plot +
          geom_boxplot(
            aes(
              min = y_25,
              lower = y_25,
              middle = y_median,
              upper = y_75,
              max = y_75,
              fill = is_heating_dependent_on_electricity
            ),
            colour = 'black',
            width = 0.75,
            stat = 'identity'
          )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
        plot <- plot + coord_cartesian(ylim = c(0,max_y))
      }

      plot +
        scale_y_continuous(name = "Sähkönkulutus (kWh)",
                           labels = tuhaterotin) +
        scale_x_discrete(name = "Tulokymmenys")+
        scale_fill_manual(
          name = '25 % - mediaani- 75 %',
          label = c('Lämmitys ei riippuvainen sähköstä',
                    'Lämmitys riippuvainen sähköstä'),
          values = c('#F16C13',"#234721")
        )+
        scale_colour_manual(
          name = NULL,
          label = '5 % - 95 %',
          values = 'black'
        )+
        scale_alpha_manual(
          name = NULL,
          values = 1
        )+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border =element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12),
          legend.box="vertical",
          legend.margin=margin()
        )

    } else if (input$soptyyp == 'määräaikaiset sopimukset') {

### määräaikaisboxplot --------------------------
      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili)
          )
        )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            position = position_nudge(
              x = rep(c(-0.2,0.2),10)
            ),
            width = 0.2)
      }


        plot <- plot +
          geom_boxplot(
            aes(
              min = y_25,
              lower = y_25,
              middle = y_median,
              upper = y_75,
              max = y_75,
              fill = factor(is_fixed_term_agreement)
            ),
            colour = 'black',
            width = 0.75,
            stat = 'identity'
          )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
        plot <- plot + coord_cartesian(ylim = c(0,max_y))
      }

      plot +
        scale_y_continuous(
          name = "Sähkönkulutus (kWh)",
          labels = tuhaterotin) +
        scale_x_discrete(name = "Tulokymmenys")+
        scale_fill_manual(
          name = '25 % - mediaani- 75 %',
          label = c('Ei määräaikaista','Määräaikainen'),
          values = c('#F16C13',"#234721")
        )+
        scale_colour_manual(
          name = NULL,
          label = '5 % - 95 %',
          values = 'black'
        )+
        scale_alpha_manual(
          name = NULL,
          values = 1
        )+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border =element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12),
          legend.box="vertical",
          legend.margin=margin()
        )


    } else if (input$soptyyp == "sopimuksien lukumäärä") {

### sopimusten lukumäärä ------------------------

      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili)
          )
        )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            position = position_nudge(x = rep(c(-0.2,0.2),10)),
            width = 0.2)
      }


      plot <- plot +
        geom_boxplot(
          aes(
            min = y_25,
            lower = y_25,
            middle = y_median,
            upper = y_75,
            max = y_75,
            fill = factor(yli_1_sopimus)
          ),
          colour = 'black',
          width = 0.75,
          stat = 'identity'
        )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
        plot <- plot + coord_cartesian(ylim = c(0,max_y))
      }

      plot +
        scale_y_continuous(name = "Sähkönkulutus (kWh)",
                           labels = tuhaterotin) +
        scale_x_discrete(name = "Tulokymmenys")+
        scale_fill_manual(
          name = '25 % - mediaani- 75 %',
          label = c('Yksi sopimus','Kaksi tai useampia'),
          values = c('#F16C13',"#234721")
        )+
        scale_colour_manual(
          name = NULL,
          label = '5 % - 95 %',
          values = 'black'
        )+
        scale_alpha_manual(
          name = NULL,
          values = 1
        )+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border =element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12)
        )


    }else if (input$soptyyp == "asuu kerrostalossa") {

      ### asuu kerrostalossa ------------------------
      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili)
          )
        )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            position = position_nudge(x = rep(c(-0.2,0.2),10)),
            width = 0.2)
      }


      plot <- plot +
        geom_boxplot(
          aes(
            min = y_25,
            lower = y_25,
            middle = y_median,
            upper = y_75,
            max = y_75,
            fill = factor(asuu_kerrostalossa)
          ),
          colour = 'black',
          width = 0.75,
          stat = 'identity'
        )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
        plot <- plot + coord_cartesian(ylim = c(0,max_y))
      }

      plot +
        scale_y_continuous(
          name = "Sähkönkulutus (kWh)",
          labels = tuhaterotin
        ) +
        scale_x_discrete(name = "Tulokymmenys")+
        scale_fill_manual(
          name = '25 % - mediaani- 75 %',
          label = c('Ei asu kerrostalossa','asuu kerrostalossa'),
          values = c('#F16C13',"#234721")
        )+
        scale_colour_manual(
          name = NULL,
          label = '5 % - 95 %',
          values = 'black'
        )+
        scale_alpha_manual(
          name = NULL,
          values = 1
        )+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border =element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12),
          legend.box="vertical",
          legend.margin=margin()
        )


    }else if (input$soptyyp == "asuu taajama-alueella") {

      ### Taajamassa asuminen ------------------------
      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili)
          )
        )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            position = position_nudge(x = rep(c(-0.2,0.2),10)),
            width = 0.2)
      }


      plot <- plot +
        geom_boxplot(
          aes(
            min = y_25,
            lower = y_25,
            middle = y_median,
            upper = y_75,
            max = y_75,
            fill = factor(asuu_taajamassa)
          ),
          colour = 'black',
          width = 0.75,
          stat = 'identity'
        )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
        plot <- plot + coord_cartesian(ylim = c(0,max_y))
      }

      plot +
        scale_y_continuous(name = "Sähkönkulutus (kWh)",
                           labels = tuhaterotin) +
        scale_x_discrete(name = "Tulokymmenys")+
        scale_fill_manual(
          name = '25 % - mediaani- 75 %',
          label = c('Ei asu taajamassa','asuu taajamassa'),
          values = c('#F16C13',"#234721")
        )+
        scale_colour_manual(
          name = NULL,
          label = '5 % - 95 %',
          values = 'black'
        )+
        scale_alpha_manual(
          name = NULL,
          values = 1
        )+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border =element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12),
          legend.box="vertical",
          legend.margin=margin()
        )


    }else if (input$soptyyp == "asuntokunnan koko") {

      ### asuntokunnan koko ------------------------
      plot <- boxplot_data() %>%
        ggplot(
          aes(
            x = factor(desiili)
          )
        )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            position = position_nudge(x = rep(c(-0.2,0.2),10)),
            width = 0.2)
      }


      plot <- plot +
        geom_boxplot(
          aes(
            min = y_25,
            lower = y_25,
            middle = y_median,
            upper = y_75,
            max = y_75,
            fill = factor(yli_1_akkoko)
          ),
          colour = 'black',
          width = 0.75,
          stat = 'identity'
        )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          ),
          position = position_nudge(
            x = rep(c(-0.2,0.2),10)
          ),
        )
      }

      if(input$locked_scale){
        max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
        plot <- plot + coord_cartesian(ylim = c(0,max_y))
      }

      plot +
        scale_y_continuous(name = "Sähkönkulutus (kWh)",
                           labels = tuhaterotin) +
        scale_x_discrete(name = "Tulokymmenys")+
        scale_fill_manual(
          name = '25 % - mediaani- 75 %',
          label = c('Yksi asukas','Kaksi tai useampia'),
          values = c('#F16C13',"#234721")
        )+
        scale_colour_manual(
          name = NULL,
          label = '5 % - 95 %',
          values = 'black'
        )+
        scale_alpha_manual(
          name = NULL,
          values = 1
        )+
        theme_linedraw()+
        theme(
          legend.position = 'bottom',
          panel.border =element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.caption = element_text(hjust = 0),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.text = element_text(size= 12),
          legend.box="vertical",
          legend.margin=margin()
        )


    } else {
### Normiboxplot ----------------------
    plot <- boxplot_data() %>%
      ggplot(
        aes(
          x = factor(desiili)
        )
      )

      if(input$error){
        plot <- plot +
          geom_errorbar(
            aes(
              ymin = y_min,
              ymax = y_max,
              colour = 'black'),
            width = 0.2)
      }

        plot <- plot +
          geom_boxplot(
            aes(
              min = y_25,
              lower = y_25,
              middle = y_median,
              upper = y_75,
              max = y_75,
              fill = 'median'
            ),
            colour = 'black',
            width = 0.75,
            stat = 'identity'
          )



      if(input$mean){
        plot <- plot + geom_point(
          aes(
            y = y_mean,
            x = factor(desiili),
            alpha = 'Keskiarvo'
          )
        )
      }

      if(input$locked_scale){
          max_y <- max(sapply(boxplotit_taajama, function(x) max(x$y_max)))
          plot <- plot + coord_cartesian(ylim = c(0,max_y))
        }

      plot +
      scale_y_continuous(name = "Sähkönkulutus (kWh)",
                         labels = tuhaterotin) +
      scale_x_discrete(name = "Tulokymmenys")+
      scale_fill_manual(
        name = '25 % - mediaani- 75 %',
        label = NULL,
        values = c('#F16C13',"#234721")
      )+
      scale_colour_manual(
        name = NULL,
        label = '5 % - 95 %',
        values = 'black'
      )+
      scale_alpha_manual(
        name = NULL,
        values = 1
      )+
      theme_linedraw()+
      theme(
        legend.position = 'bottom',
        panel.border =element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size= 12)
      )
    }
    })

  # ukraina ----------------------------------
  ## etusivu ---------------------------------
  output$ikaryhma <- renderPlotly({

    ## create plot
    p <- ikajakauma %>%
      rename("ikäryhmä" = "age_group") %>%
      rename("lukumäärä" = "n") %>%
      ggplot() +
      geom_col(aes(x = ikäryhmä, y =lukumäärä, fill = sukupuoli), alpha = alpha_u, position = "dodge") +
      scale_fill_manual(values = c(light_blue, orange)) +
      scale_x_discrete(name = "ikäryhmä") +
      scale_y_continuous(name = "henkilöä", labels = tuhaterotin) +
      theme_light() +
      theme(
        legend.title = element_blank(),
        text = element_text(size = font_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = font_size))

    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.5, y = -0.5, xanchor = 'center'))

  })
  ## taustatietoja ---------------------------
  ukraina_basics_data <- reactive({

    ## filter
    if(input$vaesto == "kotikunnan saaneet") {
      data <- kotikunta
    } else {
      data <- ei_kotikuntaa
    }

    return(data)

  })

  output$basic_plot <- renderPlotly({

    data <-  ukraina_basics_data()

    if(input$jaottelu == "-") {

      p <- Ukraina_kuvaaja(data, "none", FALSE, "lukumäärä", NULL, "henkilöä", alpha_u, font_size)

    } else if(input$jaottelu == "ikäryhmä") {

      if(input$vaesto == "kotikunnan saaneet") {NULL}
      else {

      if (input$osuus) {

        p <- Ukraina_kuvaaja(data, "ikäryhmä", TRUE, "prosenttia", "ikäryhmä", "prosenttia", alpha_u, font_size)

      } else{

        p <- Ukraina_kuvaaja(data, "ikäryhmä", FALSE, "lukumäärä", "ikäryhmä", "henkilöä", alpha_u, font_size)

      }
      }


    } else if (input$jaottelu == "sukupuoli"){

      if (input$osuus) {

        p <- Ukraina_kuvaaja(data, "sukupuoli", TRUE, "prosenttia", "sukupuoli", "prosenttia", alpha_u, font_size)

      } else{

        p <- Ukraina_kuvaaja(data, "sukupuoli", FALSE, "lukumäärä", "sukupuoli", "henkilöä", alpha_u, font_size)
      }


    }

  })
  ## tyollistyminen ---------------------------
  ukraina_emp_data <- reactive({

    ## filter
    if(input$employed == "kotikunnan saaneet") {
      data <- employed_kotikunta %>%
        filter(aika > dmy("01/04/2022")) %>%
        mutate(n = if_else(aika < dmy("01/03/2023"), 0, n))

    } else {
      data <- employed %>% filter(aika > dmy("01/04/2022"))
    }

    return(data)

  })

  output$emp_plot <- renderPlotly({

    data <-  ukraina_emp_data()

    if(input$jaottelu_emp == "-") {

      ## distinct
      summary <- data %>%
        distinct(aika, n_total)

      ## plot
      p <- Ukraina_kuvaaja(summary, "none", FALSE, "lukumäärä", NULL, "henkilöä", alpha_u, font_size)

    } else if(input$jaottelu_emp == "ikäryhmä") {


        if (input$osuus_emp) {

          ## plot
          p <- Ukraina_kuvaaja(data, "ikäryhmä", TRUE, "prosenttia", "ikäryhmä", "prosenttia", alpha_u, font_size)


        } else{

          ## plot
          p <- Ukraina_kuvaaja(data, "ikäryhmä", FALSE, "lukumäärä", "ikäryhmä", "henkilöä", alpha_u, font_size)

        }


    } else if (input$jaottelu_emp == "sukupuoli"){


      if (input$osuus_emp) {

        ## plot
        p <- Ukraina_kuvaaja(data, "sukupuoli", TRUE, "prosenttia", "sukupuoli", "prosenttia", alpha_u, font_size)

      } else{

        ## plot
        p <- Ukraina_kuvaaja(data, "sukupuoli", FALSE, "lukumäärä", "sukupuoli", "henkilöä", alpha_u, font_size)

      }


    }

  })

  ## alat ja ammatit -------------------------
  ukraina_alat_ja_ammatit <- reactive({

    ## filter
    if(input$alavaiammatti == "toimialat") {

      ## yleisimmät toimialat
      top <<- toimialat %>%
        group_by(toimiala_nimi) %>%
        dplyr::summarise(n = mean(n)) %>%
        arrange(desc(n)) %>%
        slice(1:input$top) %>%
        pull(toimiala_nimi)
      data <- toimialat

    } else {

      ## yleisimmät ammatit
      top <<- ammatit %>%
        group_by(t3_nimi) %>%
        dplyr::summarise(n = mean(n))  %>%
        arrange(desc(n)) %>% slice(1:input$top) %>%
        pull(t3_nimi)
      data <- ammatit
    }

    return(data)

  })
  output$ala_ammatti_plot <- renderPlotly({

    ## get data
    data <-  ukraina_alat_ja_ammatit()



    if (input$alavaiammatti == "toimialat") {

      ## plot
      p <- Ukraina_kuvaaja(data, "toimiala", FALSE, "lukumäärä", "ala", "henkilöä", alpha_u, font_size)

    } else if (input$alavaiammatti == "ammattinimikkeet") {

      ## plot
      p <- Ukraina_kuvaaja(data, "ammatti", FALSE, "lukumäärä", "ala", "henkilöä", alpha_u, font_size)

    }

  })



  ## otsikot --------------------------------
  output$toimialat_otsikko <- renderText({

    lkm <- numerolle_teksti(input$top)

    if(lkm == "Yksi"){
      if(input$alavaiammatti == "toimialat") {
        paste0( "Yleisin toimiala")
      } else {
        paste0("Yleisin ammattinimike")
      }
    } else {
      if(input$alavaiammatti == "toimialat"){
        mika_ala <- "toimialaa"
      } else {
        mika_ala <- "ammattinimikettä"
      }
      paste0(lkm, " yleisintä ", mika_ala)
    }


  })

  output$emp_otsikko <- renderText({

    # if (input$osuus_emp & input$jaottelu_emp == "-") {
    #   abs <- "lukumäärä"
    # } else if (input$osuus_emp){
    #   abs <- "osuus"
    # } else {
    #   abs <- "lukumäärä"
    # }

    if(input$employed == "kotikunnan saaneet"){
      kotikunta <- "kotikunnan saaneet"
    } else {
      kotikunta <- ""
    }

    paste0("15–64-vuotiaat ", kotikunta, " ukrainalaiset palkansaajat")
  })

  output$taustatieto_otsikko <- renderText({

    if (input$osuus & input$jaottelu == "-") {
      abs <- "lukumäärä"
    } else if (input$osuus){
      abs <- "osuus"
    } else {
      abs <- "lukumäärä"
    }

    if(input$vaesto == "kotikunnan saaneet"){
      kotikunta <- "kotikunnan saaneiden"
    } else {
      kotikunta <- ""
    }

    paste0("1.3.2022 jälkeen saapuneiden ", kotikunta, " ukrainalaisten ", abs)
  })

  ## downloaderit --------------------------------
  output$download_taustatiedot <-downloadHandler(

    filename = function(){
      if(input$vaesto == "kotiunnan saaneet"){
        return(paste0("kotikunnan_saaneet_ukrainalaiset.csv"))
      } else {
        return(paste0("kaikki_ukrainalaiset.csv"))
      }

    },
    content = function(file){

      ## get the data
      data <- ukraina_basics_data()

      if(input$jaottelu == "-") {

        ## distinct
        summary <- data %>%
          distinct(aika, n_total) %>%
          rename(c("n" = "n_total"))

      } else if(input$jaottelu == "ikäryhmä") {

        ## summarise
        summary <- data %>%
          group_by(aika, n_total, age_group) %>%
          summarise(n = sum(n)) %>%
          mutate(osuus = n/n_total*100) %>%
          mutate(osuus = round(osuus, 2)) %>%
          rename(c("ikäryhmä" = "age_group"))

      } else if (input$jaottelu == "sukupuoli"){

        ## summarise
        summary <- data %>%
          group_by(aika, n_total, sukupuoli) %>%
          summarise(n = sum(n)) %>%
          mutate(osuus = n/n_total*100) %>%
          mutate(osuus = round(osuus, 2))

      }

      write.csv(summary, file, row.names = F, fileEncoding = "ISO-8859-1")
    }

  )

  output$download_emp <-downloadHandler(

    filename = function(){
      if(input$employed == "kotikunnan saaneet"){
        return(paste0("kotikunnan_saaneet_ukrainalaiset_palkansaajat.csv"))
      } else {
        return(paste0("kaikki_ukrainalaiset_palkansaajat.csv"))
      }

    },
    content = function(file){

      ## get the data
      data <- ukraina_emp_data()

      if(input$jaottelu_emp == "-") {

        ## distinct
        summary <- data %>%
          distinct(aika, n_total) %>%
          rename(c( "n" = "n_total"))

      } else if(input$jaottelu_emp == "ikäryhmä") {

        if(input$employed == "kotikunnan saaneet") {
          data <- data %>%
            filter(aika >  dmy("01/02/2023"))
        }

        ## summarise
        summary <- data %>%
          group_by(aika, n_total, age_group) %>%
          summarise(n = sum(n)) %>%
          mutate(osuus = n/n_total*100) %>%
          mutate(osuus = round(osuus, 2)) %>%
          rename(c("ikäryhmä" = "age_group"))

      } else if (input$jaottelu_emp == "sukupuoli"){

        if(input$employed == "kotikunnan saaneet") {
          data <- data %>%
            filter(aika >  dmy("01/02/2023"))
        }

        ## summarise
        summary <- data %>%
          group_by(aika, n_total, sukupuoli) %>%
          summarise(n = sum(n)) %>%
          mutate(osuus = n/n_total*100) %>%
          mutate(osuus = round(osuus, 2))

      }

      write.csv(summary, file, row.names = F, fileEncoding = "ISO-8859-1")
    }

  )


  output$download_alat_ja_ammatit <-downloadHandler(

    filename = function(){
      if(input$alavaiammatti == "toimialat"){
        return(paste0("toimialat.csv"))
      } else {
        return(paste0("ammattinimikkeet.csv"))
      }

    },
    content = function(file){

      ## get the data
      data <- ukraina_alat_ja_ammatit()

      # ## rename variables
      # data <- data %>% select(-ala)

      write.csv(data, file, row.names = F, fileEncoding = "ISO-8859-1")
    }

  )


  # downloaderit ----------------------------------


  output$download_aikasarja <-downloadHandler(
    filename = function(){
      if(input$tarktaso == "Koko maa"){
        return(paste0("datahuone_fdh_kokonaiskulutus.csv"))
      } else if(input$tarktaso == "Maakunnittain"){
        return(paste0("datahuone_fdh_kokonaiskulutus_maakunnittain.csv"))
      } else {
        return(paste0("datahuone_fdh_kokonaiskulutus_kunnittain.csv"))
      }

    },
    content = function(file){

      data <- aikasarja_data() %>%
        rename(sahkon_kulutus_yht_kwh = sahkonkul,
               asukkaiden_lkm = sum_ak)

      write.csv(data, file, row.names = F, fileEncoding = "UTF-8")
    }
  )

  output$download <-downloadHandler(
    filename = function(){
      paste0("datahuone_fdh_",input$kk,".csv")
    },
    content = function(file){
      data <- boxplot_data() %>%
        rename(y_05 = y_min,
               y_95 = y_max,
               asuntokuntie_lkm = n,
               tulokymmenys = desiili)

      if(!input$mean){
        data <- data %>%
          select(-y_mean)
      }

      if(!input$error){
        data <- data %>%
          select(-c(y_min, y_max))
      }
      data <- data %>%
        rename_all(~str_replace_all(., "y_", "sahkonkaytto_"))

      write.csv(data, file, row.names = F)
    }
  )

  output$download_dekomponoitu <- downloadHandler(
    filename = function(){
      paste0("datahuone_fdh_dekomponoitu_", as.character(input$sahkoDate[1]), "_", as.character(input$sahkoDate[2]), ".csv")
    },
    content = function(file){
      data <- energiantuotanto_data_frame() #%>%
        #select(-c(vienti, kokonaistuotanto)) # TODO: MINE DATA UP TO 2019!
      write.csv(data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(
  ui = ui,
  server = server
  )
