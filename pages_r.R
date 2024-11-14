prod_front_page <- function(){ nav_panel(
    # Etusivu -----------------------------------------------
    title = "Etusivu",
    icon = icon('house'),
    value = 'etusivu', #valueta käyteteään url muodostamiseen
    page_fluid(
      layout_columns(
        includeMarkdown("tekstit/etusivu.md")
      ),
      tags$br(), #lisätään väli etusivun ja ikonien väliin
      layout_columns(
        tags$div(
        id = "btn_ymp",
          class = "btn btn-default action-button",
          #nappulan taustaväri ja välin suuruus nappulan ja tekstin välissä
          style = "background-color: #234721; margin-bottom: 20px;",
          tags$div(
            style = "width: 100px; height: 100px;",
            HTML('<img src="Ikoni_ympäristö.svg" width="100%" height="100%"/>')
          ),

          # alla oleva div tekee label nappiin
          tags$div(
            class="custom-label",
            style = "font-size: 125%",
            HTML("<b>Sähkönkäyttö</b> - Tietoa suomalaisten"),
            tags$br(), # tags$br() muuttaa kahdeksi riviksi
            "kotitalouksien sähkönkäytöstä")
        ),
        tags$div(
          id = "btn_tyo",
          class = "btn btn-default action-button",
          #nappulan taustaväri ja välin suuruus nappulan ja tekstin välissä
          style = "background-color: #721d41; margin-bottom: 20px;",
          tags$div(
            style = "width: 100px; height: 100px;",
            HTML('<img src="Ikoni_työmarkkinat.svg" width="100%" height="100%"/>')
          ),
          # alla oleva div tekee label nappiin
          tags$div(
            class="custom-label",
            style = "font-size: 125%",
            HTML(
              '<b>Työmarkkinat</b> - Tietoa tilapäisen'),
            tags$br(), # tags$br() muuttaa kahdeksi riviksi
            'suojelun piirissä olevista ukrainalaisista')
        ),
        col_widths = c(-1,5,5,-1),
        row_heights = "row"
      )
    )
  )

}

prod_ye_front_page <- function(){

  info_piirakka <- popover(
    bs_icon("info-circle"),
    "Tiedot ovat Fingridin avoin data  -verkkopalvelusta ja perustuvat käytönvalvontajärjestelmän reaaliaikaisiin mittauksiin. Lisätietoja sähköntuotannosta sekä kulutuksesta voi löytää \"Reaaliaikainen sähkönkäyttötilanne\"-osiosta. Ympyröiden pinta-alojen suhde kuvaa kulutuksen sekä tuotannon suhdetta.",
    h4("Sähkön tuotantomuodot"),
    "Sähkön yhteistuotanto on tuotantomuoto, jossa sähköä tuotetaan toisen prosessin yhteydessä.

Sähkön pientuotanto on paikalliseen käyttöön tarkoitettua tuotantoa, jota tuotannon ylittäessä kulutuksen voidaan myydä sähköverkkoon. Pientuotannoksi luokitellaan sellainen tuotanto, jonka nimellisteho on enintään 2 megavolttiampeeria. Yksittäiset kotitaloudet ovat tyypillisiä, mutta eivät ainoita sähkön pientuottajia.",
    title = "Tietoa kuvaajasta"
       )

  nav_panel(
    title = 'Kotitalouksien sähkönkäyttö',
    value = sahk_etusivu_url,


      ### sivu ----------------------
      nav_panel(
        title = "Etusivu",
        value = sahk_etusivu_url,  #valueta käyteteään url muodostamiseen
        page_fluid(
          h1("Kotitalouksien sähkönkulutus - Fingrid Datahubin tilastotietojen tarkastelu"),
          layout_column_wrap(
            layout_column_wrap(
            includeMarkdown("tekstit/sahko_leipateksti.md"),
            ),
            layout_column_wrap(
              height = 200,
              card(
               card_header(
                "Sähkön tämänhetkinen tuotanto sekä kulutus", info_piirakka,
                class = "d-flex justify-content-between"
                ),
              plotOutput("piirakkaplot"),
              full_screen = T,
              fill = F
              )
              )
          )
        )
      )
    )
}

prod_energy_time_series <- function(){

  info_aikasarja <- popover(
    bs_icon("info-circle"),
    h3("Asuntokuntakohtainen sähkönkäyttö"),
    "Kotitalouksien mittarikohtainen sähkönkulutus saadaan Fingrid Datahub -aineistosta. Tiedostoissa esitetään luvut joulukuun 2022 kulutuksesta. Mittaripisteen sähkösopimus yhdistetään asuntokuntiin sähkösopimuksen haltijan pseudonymisoidun henkilönumeron avulla.",
    title = "Tietoa kuvaajasta"
  )

  nav_panel(
    title = "Kokonaiskulutuksen trendit",
    value = sah_kokonaiskulutus,  #valueta käyteteään url muodostamiseen

    layout_sidebar(
      sidebar = sidebar(
        dateRangeInput(
          inputId = 'aikasarja',
          label = "Tarkasteluajanjakso",
          start = min(kuukaudet),
          end = max(kuukaudet) %m+% months(1) %m-% days(1),
          min = min(kuukaudet),
          max = max(kuukaudet) %m+% months(1) %m-% days(1), #viimeisimmän kuun viimeinen päivä
          format = "d.m.yyyy",
          separator = '-'
        ),
        selectInput(
          inputId = 'tarktaso',
          label = "Tarkastelutaso",
          choices = c('Koko maa',
                      "Maakunnittain",
                      "Kunnittain"),
          selected = 'Koko maa'
        ),
        selectInput(
          inputId = 'valitut',
          label = "Korosta",
          selected = "Suomi",
          choices = c("Suomi"),
          multiple = T
        ),
        selectInput(
          inputId = "suure",
          label = NULL,
          selected = "Kokonaiskulutus",
          choices = c("Kokonaiskulutus",
                      "per capita")
        ),
        p("Voit vaikuttaa kuvaajaan muuttamalla yllä olevia valintoja")
      ),
      card(
        full_screen = T,
        card_header(
          "Yksityishenkilöiden yhteenlaskettu sähkönkäyttö", info_aikasarja,
          class = "d-flex justify-content-between"
        ),
        plotOutput("aikasarjaplot"),
        downloadButton("download_aikasarja", "Lataa csv")
        )
    )
  )

}


prod_decile_page <- function(){

  pop_sos_settings <- popover(

    bsicons::bs_icon("info-circle"),
    "Kotitalouksien mittarikohtainen sähkönkulutus saadaan Fingrid Datahub -aineistosta. Tiedostoissa esitetään luvut joulukuun 2022 kulutuksesta. Mittaripisteen sähkösopimus yhdistetään asuntokuntiin sähkösopimuksen haltijan pseudonymisoidun henkilönumeron avulla.",
    h3("Asuntokuntien määrittely"),
    "Asuntokunnat on määritelty Tilastokeskuksen väestön ennakkotietojen mukaan. Asuntokunnan kotikunta määritellään vakituisen osoitteen sijaintikunnan perusteella. Asuntokunnan sähkönkulutus lasketaan kaikkien asuntokunnan hallussa olevien sähkösopimusten kulutuksesta, sillä asuntokunnalla voi olla useita sähkösopimuksia. Suurimmalla osalla asuntokunnista on kuitenkin vain yksi vakituiseen osoitteeseen tehty sähkösopimus. Aineistossa käsitellään vain niitä asuntokuntia, joilla on havaittu sähkösopimus ja vakituinen osoite. Aineistossa havaitaan noin 2,5 miljoonaa asuntokuntaa, kun taas noin 300 000 asuntokunnalla ei ole sähkösopimusta. Esimerkkejä sähkösopimuksettomista asuntokunnista ovat ne, joilla vuokranantaja tai joku muu taho maksaa sähkön.

Asuntokunnat on jaettu tulokymmenyksiin henkilöiden perusteella siten, että kaikille asuntokunnan henkilöille on allokoitu asuntokunnan yhteenlasketut käytettävissä olevat tulot, ja henkilöt on järjestetty tulojen mukaiseen järjestykseen ja jaettu kymmeneen yhtä suureen joukkoon. Näin yhdessä tulokymmenyksessä on kymmenesosa henkilöistä, ei asuntokunnista.

Asuntokunta määritellään sähkölämmittäjäksi, jos Fingridin tietojen mukaan sopimus, josta on havaittu eniten kulutusta, on sähkölämmitteisessä talossa. Mikäli asuntokunnalla on hallussaan useampi asunto, määritellään asuinmuoto myös sen mukaan, mistä suurin kulutus tulee. Mikäli suurin kulutus tulee kerrostaloasunnosta, valitaan asuntokunta kerrostaloasujaksi.",
title = "Tietoa kuvaajasta",

  )

  nav_panel(
    title = "Sosioekonomisten muuttujat",
    value = sah_desiili_url,  #valueta käyteteään url muodostamiseen
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = 'kk',
          label = 'Tarkastelukuukausi',
          choices = sort(kuukaudet),
          selected = max(kuukaudet)
        ),
        selectInput(
          inputId = 'soptyyp',
          label = 'Tulokymmenyksien jaottelu',
          choices = c("-" ,
                      "sopimuksien lukumäärä" ,
                      "määräaikaiset sopimukset",
                      "lämmitys riippuvainen sähköstä" ,
                      "asuu taajama-alueella" ,
                      "asuu kerrostalossa" ,
                      "asuntokunnan koko"),
          selected = "-"
        ),
        checkboxInput(
          inputId = 'mean',
          label = 'Lisää desiilien keskiarvot',
          value = TRUE
        ),
        checkboxInput(
          inputId = 'error',
          label = 'Lisää desiilien jakaumaviivat',
          value = TRUE
        ),
        checkboxInput(
          inputId = 'locked_scale',
          label = 'Lukitse y-akselin skaala',
          value = F
        ),
        p("Valinnat vaikuttavat sekä viereiseen kuvaajaan että alapuolelta ladattavaan csv-tiedostoon.")
      ),

      card(
        card_header(
          textOutput('boxplot_otsikko'), pop_sos_settings,
          class = "d-flex justify-content-between"),
        plotOutput("boxplot"),
        downloadButton("download", "Lataa csv"),
        min_height = 450,
        full_screen = T
        ),
        layout_columns(
        value_box(
          "Asuntokuntien lukumäärä",
          textOutput("askumaarat"),
          showcase = bsicons::bs_icon('houses-fill')
          ),
        value_box(
          "Korkeatuloisin desiili kulutti tässä kuussa kertaa enemmän sähköä kuin pienituloisin desiili.",
          textOutput("dessuhde"),
          showcase = bsicons::bs_icon('lightning-charge-fill')
          ),
        col_widths = c(5,7)
        ),

        card(
          card_header(textOutput("taustaotsikko")),
          plotOutput("tausta"),
          full_screen = T
        )
      )
  )


}


prod_real_time_energy <- function(){

  nav_panel(
    title = "Reaaliaikainen sähkönkäyttötilanne",
    value = sah_reaaliaikainen_url,
    shinyjs::useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        dateRangeInput(
          "sahkoDate", "Valitse aikaväli:",
          start = Sys.time()-lubridate::weeks(1),
          end = Sys.time(),
          min = lubridate::as_datetime("27-11-2019", format = "%d-%m-%Y"),
          max = Sys.time(),
          separator = "-"
        ),

        checkboxGroupInput("reaaliaikaKuvaajaAsetus", "",
                           c("Lukitse kuvaaja tuntitasolle"), selected = NA),

        p("Voit muokata esitysmuotoa yllä olevilla asetuksilla. Kuvaajan oletusasetus on muuttaa tarkasteluaikaväli päiviin, kun valittu aikaväli on pidempi kuin kuukausi.
             Tätä asetusta voi muuttaa, mutta kuvaaja saattaa tällöin latautua hitaasti. Ladattavaan dataan vaikuttaa ainoastaan valittu aikaväli."),

        actionButton("resetSahko", "Palauta oletusasetukset")
      ),

      mainPanel(
        fluidRow(
          h1("Reaaliaikainen sähkönkäyttötilanne")
        ),
        fluidRow(
          valueBoxOutput("kokonaiskulutus", width = 4),
          valueBoxOutput("kokonaistuotanto", width = 4),
          valueBoxOutput("tuulisuhde", width = 4)
        ),
        fluidRow(
          valueBoxOutput("muutoskulutus", width = 4),
          valueBoxOutput("muutostuotanto", width = 4),
          valueBoxOutput("nettovienti", width = 4)
        ),
        fluidRow(h2("Sähkön kulutus sekä tuotanto Suomessa")),
        fluidRow(
          column(plotlyOutput("viikkoplot"), width = 10)
        ),
        fluidRow(
          column(plotlyOutput("viikkoplot_dekomponoitu"), width = 10)
        ),
        fluidRow(
          column(
            p("Lähde: Fingridin avoin data -verkkopalvelu"),width = 4
          )
        ),

        fluidRow(
          downloadButton("download_dekomponoitu", "Lataa csv")
        )
      )


    )

  )

}


prod_ukr <- function(){

  nav_menu(


    title = "Työmarkkinat",
    icon = img(src="Ikoni_työmarkkinat.svg", height = 25),


    nav_panel(
      title = "Ukrainalaiset Suomessa",
      value = tyomarkkinat_ukrainat_url,
      navset_underline(
        nav_panel(
          title = "Ukrainalaiset Suomessa",
          fluidPage(
            column(includeMarkdown("tekstit/ukraina_etusivu.md"), width = 6),
            column( h3("Tilapäisen suojelun piirissä olevien ukrainalaisten ikä- ja sukupuolijakauma"),
                    plotlyOutput("ikaryhma"), width = 6)
          )
        ),
        nav_panel("Taustatietoja",
                  fluidPage(

                    sidebarLayout(
                      # sivupaneelin valinnat
                      sidebarPanel(
                        selectInput("vaesto", "Valitse kohdejoukko",
                                    choices= c("kaikki ukrainalaiset", "kotikunnan saaneet")),
                        selectInput("jaottelu", "Lisää jaottelu ",
                                    choices= c("-", "ikäryhmä", "sukupuoli")),
                        checkboxInput(inputId = "osuus",
                                      label = "prosentteina",
                                      value = FALSE),
                        p("Voit nähdä tarkan lukumäärän tai osuuden viemällä kursorin haluamasi palkin päälle."),
                        p("Valinnat vaikuttavat sekä viereiseen kuvaajaan että alapuolelta ladattavaan csv-tiedostoon."),
                        p(strong("Huom!"),"Mikäli jonkin kuukauden tiedot eivät ole näkyvissä, tiedot on jouduttu peittämään liian pienen havaintomäärän takia.")
                      ),

                      # Create a spot for the barplot
                      mainPanel(
                        fluidRow(h2( textOutput('taustatieto_otsikko'))),
                        plotlyOutput("basic_plot"),
                        fluidRow(downloadButton("download_taustatiedot", "Lataa csv"))
                      )
                    )
                  ) ## close fluid page
        ), ## close tab panel

        nav_panel("Työllistyminen",
                  fluidPage(

                    sidebarLayout(
                      # sivupaneelin valinnat
                      sidebarPanel(
                        selectInput("employed", "Valitse kohdejoukko",
                                    choices= c("kaikki ukrainalaiset", "kotikunnan saaneet")),
                        selectInput("jaottelu_emp", "Lisää jaottelu ",
                                    choices= c("-", "ikäryhmä", "sukupuoli")),
                        checkboxInput(inputId = "osuus_emp",
                                      label = "prosentteina",
                                      value = FALSE),
                        p("Voit nähdä tarkan lukumäärän tai osuuden viemällä kursorin haluamasi palkin päälle."),
                        p("Valinnat vaikuttavat sekä viereiseen kuvaajaan että alapuolelta ladattavaan csv-tiedostoon."),
                        p(strong("Huom!"),"Mikäli jonkin kuukauden tiedot eivät ole näkyvissä, tiedot on jouduttu peittämään liian pienen havaintomäärän takia.")
                      ),

                      # Create a spot for the barplot
                      mainPanel(
                        fluidRow(h2(textOutput('emp_otsikko'))),
                        plotlyOutput("emp_plot"),
                        fluidRow(downloadButton("download_emp", "Lataa csv"))
                      )
                    )
                  ) ## close fluid page
        ), ## close tab panel
        nav_panel("Toimialat ja ammatit",
                  fluidPage(

                    sidebarLayout(
                      # sivupaneelin valinnat
                      sidebarPanel(
                        selectInput("alavaiammatti", "Valitse kategoria",
                                    choices= c("toimialat", "ammattinimikkeet")),
                        selectInput("top", "Valitse tarkasteltavien alojen lkm",
                                    choices= c(1:8),
                                    selected = 5),
                        p("Voit nähdä tarkan lukumäärän tai osuuden viemällä kursorin haluamasi palkin päälle."),
                        p("Valinnat vaikuttavat sekä viereiseen kuvaajaan että alapuolelta ladattavaan csv-tiedostoon."),
                        p(strong("Huom!"),"Mikäli jonkin kuukauden tiedot eivät ole näkyvissä, tiedot on jouduttu peittämään liian pienen havaintomäärän takia.")
                      ),

                      # Create a spot for the barplot
                      mainPanel(
                        fluidRow(h2( textOutput('toimialat_otsikko'))),
                        plotlyOutput("ala_ammatti_plot"),
                        fluidRow(downloadButton("download_alat_ja_ammatit", "Lataa csv"))
                      )
                    )

                  ) ## close fluid page

        ) ## close tab panel
      ) ## close tabset panel
    ) ## close tab panel
  )

}
