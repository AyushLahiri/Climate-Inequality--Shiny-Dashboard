#############Importing Packages#############
library(shiny)
library(fullPage)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(viridis)
library(plotly)
library(ggmap)
library(rgl)
library(sf)
library(ggplot2)
library(leaflet)
library(stringi)
library(tigris)
library(plotly)
library(readxl)
library(networkD3)
library(RColorBrewer)

################Data Calls###########################
plot_nd_gains = read.csv('./Data/plot_nd_gains.csv')

sea_levels_city = read.csv('./Data/sea_levels.csv')

gni = read.csv('./Data/GNI.csv')
gni_class = read.csv('./Data/GNI_class.csv')
emissions = read.csv('./Data/wid data.csv')

transfer = read.csv('./Data/transfer.csv')
ghg = read_csv('./Data/emissions/ghg.csv')
emit_income = read_xlsx("./Data/incomeemissions.xlsx")

city_state = read.csv('./Data/Geo/worldcities.csv')

global_levels = read.csv('./Data/Climate/sea_levels_compressed.csv')
disasters  = read.csv('./Data/Climate/clim_dis.csv')
co2  = read.csv('./Data/Climate/co2_grouped.csv')
temp_change  = read.csv('./Data/Climate/temp_change_grouped.csv')

city_state = read.csv('./Data/Geo/worldcities.csv')
plot_names = read_excel('./Data/plot_names.xlsx')

########################################################################
################  Within Shiny necessary pre-processing  ###########
########################################################################

###1. Income wise emissions pre-processing #################
emit_income$Region <- gsub("East Asia", "East\nAsia", emit_income$Region)
emit_income$Region <- gsub("North America", "North\nAmerica", emit_income$Region)
emit_income$Region <- gsub("SouthAsia", "South \n&\nSouthEast\nAsia", emit_income$Region)
emit_income$Region <- gsub("Russia & Central Asia", "Russia &\nCentral\nAsia", emit_income$Region)
emit_income$Region <- gsub("Russia & Central Asia", "Russia &\nCentral\nAsia", emit_income$Region)
emit_income$Region <- gsub("MENA", "Middle East \n&\n North Africa", emit_income$Region)
emit_income$Region <- gsub("Latin America", "Latin\nAmerica", emit_income$Region)
emit_income$Region <- gsub("Sub-Saharan Africa", "Sub-Saharan\nAfrica", emit_income$Region)

top_10_order <- emit_income %>%
  filter(group == "Top 10%") %>%
  arrange(desc(c02)) %>%
  .$Region

# Convert 'Region' to a factor and specify the levels in the order obtained from 'top_10_order'
emit_income$Region <- factor(emit_income$Region, levels = top_10_order)
###########################################################################################

###2. Trade Value data pre-processing #################

transfer$Transfer = transfer$Transfer*-1
transfer$Transfer_fair  = transfer$Transfer_fair*-1

#how much each country transferred in total
transfer_group <-  
  transfer %>% 
  group_by(transfer_country, Country) %>%
  summarise(sum_transfer = sum(Transfer, na.rm = TRUE)) %>% 
  mutate(sum_transfer = sum_transfer / 1e3) %>% 
  filter(transfer_country != 'China')

#How much was transferred by all countries in total per year
annual_summary <- transfer%>%
  group_by(Year) %>%
  summarize(Total_Transfer = sum(Transfer, na.rm = TRUE),
            Total_Transfer_Fair = sum(Transfer_fair, na.rm = TRUE)) %>%
  mutate(Total_Transfer_Fair = ifelse(Year == 1999,Total_Transfer_Fair*-1,Total_Transfer_Fair))

annual_summary$Category <- rep("Unfairly Extracted Trade Value", nrow(annual_summary))
annual_summary$Category_Fair <- rep("Expected Value Exchange \nat World Average Price", nrow(annual_summary))
#####################################################################################################

###3. Create data frames for animation. Function creates new variable called frame used in plotly #################
accumulate_by_co2 <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

accumulate_by_disaster<- accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}
co2<- co2 %>%
  arrange(numer_date) %>% #data is sorted
  accumulate_by_co2(~numer_date)


global_sea_levels = global_levels%>%accumulate_by_disaster( ~Year)
disasters_grouped = disasters %>%accumulate_by_disaster( ~Year)
temp_change_grouped = temp_change %>%accumulate_by_disaster( ~Year)




############### UI ########################################################
####Final leaflet plot data. Merging  dataframes for coordinates and city level sea level danger

city_state = city_state %>% select(c(country,admin_name, city, iso3,population, lat, lng))%>%
  mutate(admin_name = stri_trans_general(admin_name, "Latin-ASCII"))

world <- readRDS("./Data/Geo/world_data.rds")
sea_levels_joined <- world %>% select(c(name,iso_a3,geometry)) %>% left_join(sea_levels_city %>% select(c(City,Country,Group,total)) , 
                                                                             by = c("name" = "Country"))%>%
  left_join(plot_names %>% select(c(ISO3,Name)) %>% group_by(Name) %>%
              summarise(ISO3 = first(ISO3)),
            by = c('iso_a3' ='ISO3')) %>% select(-Name) %>% 
  
  left_join(city_state, by = c ('iso_a3' = 'iso3', 'City' = 'city')) %>% filter(total > 20) %>%
  filter(!is.na(lng)) %>% st_as_sf(crs = 3857)





########################################################################################################################
ui <- fullPage(
  #######################################################################################################################
  fullSection(fluidRow(
    tags$head(
      tags$style(HTML("
            #loading-container {
                display: flex;
                justify-content: center;
                align-items: center;
                height: 100vh;}

            #loading-message {
                color: black;
                font-family: 'Times New Roman', Times, serif;
                font-size: 40px;
                text-align: center;}
        ")),
      tags$script(HTML("
                $(document).on('shiny:connected', function(event) {
                    $('#loading-message').text('Loading...Just a moment! ');});
                $(document).on('shiny:idle', function(event) {
                    $('#loading-message').text('All ready. Feel Free to go ahead!'); });"))),
    tags$div(id = 'loading-container',
             tags$div(id = 'loading-message')
    ))),
  
  fullSection(
    menu = "firstSection",
    center = TRUE,
    class = "first-section",
    tags$style(HTML("
      .fp-tableCell {
        vertical-align: top;  /* Align content to the top */
      }
      .section {
        padding-top: 30px;    /* Slightly reduced top padding */
      }
      .centered-header-first {
        text-align: center;
        margin-bottom: 20px; /* Space between header and content */
      }
      .plot-title {
        font-size: 18px;
        font-weight: bold;
        text-align: left;
      }
      .plot-subtitle {
        font-size: 15px;
        text-align: left;
      }
      .plot-caption-first {
        font-size: 13px;
        text-align: right;
        margin-right: 30px;
        margin-top: -30px;
      }
      .plot-caption-first-first {
        font-size: 13px;
        text-align: right;
        margin-right: 30px;
        margin-top: -45px;
      }
      .plot-container-first-a {
        height: 45vh;
        margin-bottom: 10px;
        margin-right: 30px;
        position: relative; 
      }
      .plot-container-first-b {
        height: 48vh;
        margin-right: 30px;
        position: relative; 
      }
      .dropdown-overlay-first {
        position: absolute;
        top:-20px;
        left: -110px;
        width: 100px;
        height: 1px;
        z-index: 110;
        font-weight: bold; /* Makes the font bold */
      }

      .selectize-input {
        font-size: 10px; /* Smaller font size for the dropdown */
      }
    
      .custom-text {
        font-family: 'Times New Roman', Times, serif;
        text-align: left;
        font-size: 16px; }
")),
    tags$h1("Climate Change and Carbon Emissions: A Shared Burden with Unequal Consequences", class = "centered-header-first",
            style = "font-family: 'Times New Roman', Times, serif;"), 
    fluidRow(
      column(5,
             tags$div( class = 'custom-text',style = "padding-top: 2px; height:50px;text-align: justify;margin-right:25px;",  # Placeholder box with fixed height
                       p(HTML("Climate change stands as one of the most formidable challenges facing our planet today, 
                      its reality incontrovertibly established through decades of rigorous scientific research and empirical data. 
                      At the core of this crisis lies the critical issue of greenhouse gas emissions, 
                      with carbon dioxide (CO2) being the most prominent and concerning.
                      
                      <br><br> Global carbon emissions have surged at a high rate since the 1960s, marked by exponential growth in trade, globalization, and 
                      manufacturing.The connection between rising carbon emissions and its tangible impact on the planet 
                      is starkly evident when examining three primary indicators: the increase in climate-related natural disasters, 
                      the rising sea levels, and the escalating global surface temperatures. These indicators, while only a snapshot 
                      of the broader and rapid environmental changes, are among the most visibly alarming consequences of 
                      carbon emissions. From 1960 to 2020, the global average surface temperature climbed by 1.5 degrees Celsius, 
                      By 2020, the world's average sea level had ascended by 60 millimeters 
                      compared to the early 1990s and the frequency of natural disasters has almost tripled since the 1980s, 
                      with floods becoming nearly six times more common. This evidence underscores the destructive reality of climate change,
                      a reality no longer relegated to hypothetical future scenarios but one that is present and profound"
                       )))
      ),
      column(6,offset = 1,tags$div(class = 'plot-container-first-a',
                                   tags$div(class = "plot-title", " Global Carbon Dioxide Emissions and Climate Change Indicators over time"),
                                   tags$div(class = "plot-subtitle", "All Climate Indicators show strong correlation with carbon emission patterns"),
                                   plotlyOutput("co2Plot"),
                                   tags$div(class = "plot-caption-first-first", "Source: National Oceanic and Atmospheric Administration"))
      )
    ),
    fluidRow(
      column(5,
             tags$div( class = 'custom-text',style = "padding-top: -20px; height:30px;text-align: justify;margin-right:25px;",  # Placeholder box with fixed height
                       p(HTML("
                        
                      <br><br>Yet, amidst the growing recognition of climate change's severe impacts, 
                        a critical set of questions persist which in environmental advocacy we seldom ask ourselves:
                        Is everyone equally responsible for causing climate change? 
                        Does the burden of responsibility fall equally on all? 
                        And do those contributing the most to this crisis face the consequences commensurately? 
                        <br><br>The upcoming analyses visualized in this dashboard shows a somber picture and attempts to bring to the foreground the 
                        longstanding disparity between the affluent and the impoverished. 
                        They reveal a systemic bias within our global economic and social architecture—a structure so ingrained that, 
                        even in the face of global catastrophes  like 
                        climate change, it is the lesser privileged who bear the brunt of the devastation. 
                        <br><br>Ironically (and somewhat unsurprisngly) these are the same groups that contribute the least to the environmental crisis, 
                        yet find themselves on the frontline of its impact, facing disproportionate consequences while more 
                        affluent sectors often remain insulated"))
                       
             ))
      ,
      column(6,offset =1,tags$div(class = 'plot-container-first-b',
                                  tags$div(class = "dropdown-overlay-first",
                                           selectInput("varChoice", '',
                                                       choices = c("Natural Disasters", "Sea Level Change", "Surface Temperature"))),
                                  plotlyOutput("climatePlot"),
                                  tags$div(class = "plot-caption-first", "Source: National Oceanic and Atmospheric Administration & EM-DAT"))
      )
    )
  ),
  ###################################################################################################################################
  fullSection(
    menu = "secondSection",
    center = TRUE,
    class = "second-section",
    tags$style(HTML("
  .fp-tableCell {
    vertical-align: top;
  }
  .section {
    padding-top: 30px;
    padding-left: 30px;
  }
  .centered-header {
    text-align: left;
    margin-bottom: 20px;
  }
  .plot-title {
    font-size: 18px;
    font-weight: bold;
    text-align: left;
  }
  .plot-subtitle {
    font-size: 15px;
    text-align: left;
  }
  .plot-caption {
    font-size: 13px;
    text-align: right;
    margin-top: 5px;
    margin-right: 20px;
  }
  .plot-container-thisline {
  height = 400px;
    margin-left: 8px;
    margin-right: 3px;
  
  }
  .custom-legend {
            padding: -10px;
            margin-bottom: 5px;
            display: flex; 
            justify-content: center;
        }
        .legend-title {
            font-weight: bold;
            margin-bottom: 5px;
        }
        .legend-item {
            display: flex;
            align-items: center;
            margin-bottom: 5px;
        }
        .legend-color-box {
            align-items: center;
            width: 15px;
            height:3px;
            margin-right: 5px;
            margin-left: 5px;
        }
        .legend-label {
            align-items: center;
            font-size: 14px;
        }
")),
    
    tags$h1("Should there be Global Blame for a Global Problem ?", class = "centered-header",
            style = "font-family: 'Times New Roman', Times, serif;"),
    fluidRow(
      selectInput("gasType", "Select Greenhouse Gas Emitted", 
                  choices = c("Carbon Dioxide(CO2)", "Methane(CH4)", "Nitrous Oxide(NO2)")),
      
      fluidRow(
        column(5,offset=1, tags$div(class = "plot-container-thisline", plotOutput("plotNational"))),
        column(5, tags$div(class = "plot-container-thisline", plotOutput("plotPerCapita")))
      ),
      tags$div(class = "plot-caption", "Source: Climate Watch"),
      tags$div(class = "custom-legend",
               tags$div(class = "legend-title", "Countries' Income Group"),
               tags$div(class = "legend-item",
                        tags$span(class = "legend-color-box", style = "background-color: #B53317;"),
                        tags$span(class = "legend-label", "High Income")
               ),
               tags$div(class = "legend-item",
                        tags$span(class = "legend-color-box", style = "background-color: #33B517;"),
                        tags$span(class = "legend-label", "Low Income")
               ),
               tags$div(class = "legend-item",
                        tags$span(class = "legend-color-box", style = "background-color: lightblue;"),
                        tags$span(class = "legend-label", "Middle Income"))),
    ),
    fluidRow(
      column(12,
             tags$div(class = 'custom-text',
                      style = "padding-top: 20px; height: 300px;text-align: justify;margin-right:25px;",# Add padding for spacing, adjust as needed
                      p(HTML("We begin by asking: Is everyone equally responsible for climate change? Key greenhouse gases - Carbon Dioxide, 
                      Methane, and Nitrous Oxide - are central to this question. Carbon emissions arise from activities like commuting and 
                      manufacturing, Methane from agricultural processes, and Nitrous Oxide from agricultural soil management and fossil fuel
                      combustion. It is important to note that these emissions, while harmful, are necessary byproducts of economic activity,to stay financially 
                      competitive, until significant advancements in energy generation are achieved.
                      To understand these emissions and what constitutes as 'responsibility', 
                      we consider two perspectives: total country emissions and per capita emissions, 
                      each revealing different realities.
                      
                      <br><br>Nationally, low and middle-income countries have high total emissions, linked to their reliance on fossil fuel-based, 
                      energy-intensive industries. These industries, driven by affordable labor, are essential for their economic 
                      growth and global competitiveness.Given the substantial emissions at the national level, 
                      it might be tempting to view these countries as the primary suspects in emissions. 
                      However, this perspective shifts dramatically when we analyze emissions on a per capita basis. High-income countries 
                      show starkly higher emissions across all three greenhouse gases when measured per person. This contrast indicates that, while low-income countries' emissions align with their developmental 
                      needs, high-income countries' emissions reflect excessive, unecessary production and consumption choices.This is where the crux of the issue lies.
                      Their lifestyles, characterized by heavy reliance on personal vehicles and higher living standards, lead to significantly more energy use per person, 
                      painting a picture of excess rather than necessity.
                      
                      <br><br>Therefore, in assessing climate change responsibility, high-income countries' per capita emissions tell a crucial story.
                      They reveal a lifestyle of overconsumption, in stark contrast to low and middle-income countries, where emissions are 
                      more tied to essential economic activities.
                     
                      ")))
      )
    )
  ),
  #########################################################################################################################################
  fullSection(
    menu = "thirdSection",
    center = TRUE,
    class = "third-section",
    tags$style(HTML("
      .fp-tableCell {
        vertical-align: top;  /* Align content to the top */
      }
      .section {
        padding-top: 30px;
        padding-left: 30px/* Slightly reduced top padding */
      }
      .centered-header {
        text-align: left;
        margin-bottom: 20px; /* Space between header and content */
      }
      .plot-title {
        font-size: 18px;
        font-weight: bold;
        text-align: left;
      }
      .plot-subtitle {
        font-size: 15px;
        text-align: left;
      }
      .plot-caption {
        font-size: 13px;
        text-align: right;
        margin-top: 5px;
        margin-right: 30px;
      }
      .plot-container {
        margin-bottom: 1px;
        position: relative; /* Reduced space below plot container */
      }
    ")),
    
    tags$h1("Richer countries then, must be facing the conequences of their disproportionately high emissions ?", class = "centered-header",
            style = "font-family: 'Times New Roman', Times, serif;"), 
    fluidRow(
      column(12,
             tags$div(class = 'custom-text',
                      style = "padding-top: 10px; height: 350px;text-align: justify;margin-right:25px;",# Add padding for spacing, adjust as needed
                      p(HTML(" While financial systems and lifestyles may dictate consumption, emission patterns and the inequalities that 
                      subsequently exist in these areas; climate change, natural disasters and nature in general, surely does not make these distinctions? 
                      While that may be the intuitive conclusion to arrive at, the data paints a starkly different picture.
                      
                      
                    <br><br>Using The ND-GAIN Index, developed by the University of Notre Dame, we know a country's vulnerability to climate change
                    and their readiness to absorb its impact based on projected effects by the year 2100. The scores for vulnerability  take 
                    into account factors across various metrics,including health, food and water security, ecosystem services, human habitat, and economic infrastructure. 
                    Readiness is evaluated based on a country's social, economic, and governance systems, essentially how stable and prepared a nation is to implement 
                    adaptation solutions.Both scores range between 0 and 1, with 1 implying a high degree of readiness and vulnerability respectively.
                    
                    <br><br>The results paint a stark picture: Low-income countries, with limited resources and infrastructure, have become increasingly  
                    vulnerable to climate impacts and decreasingly ready over the years. Middle-income countries fall in the middle spectrum, 
                    showing some capacity to adapt but still face significant challenges. In contrast, high-income countries majorly 
                    have decreasing vulnerability and increasing readiness over the years, benefiting from robust infrastructure, higher investments in scientific capabilities,
                    and advanced technology,greater financial resources, stable economies and strong governance and social structures .
                    
                    <br><br>Thus the average low and middle income country citizen while accounting for much lower emissions globally, in the short term 
                    faces the most severe consequences, highlighting that our pattern of even environmental destruction is such that 
                    it burdens those with the lowest financial resources. That high income countries have done little in aid and resource transfer
                    is clear from these trends.")
                        
                        
                      ))
      )
    ),
    fluidRow(
      
      column(width = 6, offset = 3,
             tags$div(class = "plot-title", "Trends in Readiness for and Vulnerabiltiy to Climate Change, by Country (1995-2021) "),
             tags$div(class = "plot-subtitle", "The disparity between high and low income countries is increasing over time"),
             plotlyOutput("scoreplot"),
             tags$div(class = "plot-caption", "Source: Notre Dame Global Adaptation Initiative ")
      )
    )
    #additional section here
  ),
  ##################################################################################################################################
  fullSection(
    menu = "FourthSection",
    center = TRUE,
    class = "fourth-section",
    tags$style(HTML("
      .fp-tableCell {
        vertical-align: top;  /* Align content to the top */
      }
      .section {
        padding-top: 30px;
        padding-left: 30px/* Slightly reduced top padding */
      }
      .centered-header {
        text-align: left; /* Space between header and content */
      }
      .plot-title-second {
        font-size: 16px;
        font-weight: bold;
        text-align: left;
      }
      .plot-subtitle-second {
        font-size: 15px;
        text-align: left;
      }
      .plot-caption-second {
        font-size: 13px;
        text-align: right;
        margin-right:20px;
        margin-top:-2px;
      }
      .plot-caption-second-second {
        font-size: 13px;
        text-align: right;
        margin-right:100px;
        margin-top:-5px;
      }
      .plot-container-second {
        margin-bottom: 15px;
        height:250px;
        position: relative; /* Reduced space below plot container */
      }


      .dropdown-overlay-second {
        position: relative;
        width: 130px;
        font-weight: bold; /* Makes the font bold */
        margin-bottom:5px;
      }

      .selectize-input {
        font-size: 12px; /* Smaller font size for the dropdown */
      }
    ")),
    
    tags$h1("Shouldn Poor Countries be Held Responsible for their Lack of Readiness?", class = "centered-header",
            style = "font-family: 'Times New Roman', Times, serif;"), 
    fluidRow(
      column(6,tags$div(class = "plot-container-second ",
                        tags$div(class = "plot-title-second", "Total Trade Value extracted by Core Countries from Periphery Countries,by Year (1998-2020)"),
                        tags$div(class = "plot-subtitle-second", "8 Trillion USD in trade value was unfairly extracted by core countries between 1998-2020 "),
                        plotOutput("tradevalue"),
                        tags$div(class = "plot-caption-second-second", "Source: IMF Direction of Trade Statistics & Penn World Table 10.01")
      )),
      column(6,
             tags$div(class = 'custom-text',style =  'margin-right:15px;text-align: justify;',
                      style = "padding-top: -5px; height: 340px;margin-bottom:52px;",# Add padding for spacing, adjust as needed
                      p(HTML("One could argue that high-income countries are not explicitly obligated to assist low-income nations 
                      in preparing for climate change impacts.Yet, examining modern value extraction practices reveals important economic dynamics.
                      Utilizing Köhler's methodology, 
                      further refined by Hickel et al. (2021), we can quantify the unfair trade exchanged between low income or  
                      'periphery' and high income or 'core' countries as Hickel et al. (2021) refer to them. Periphery countries include Bangladesh,
                      Egypt, India, Vietnam, Nigera etc. who have been historically low income but contribute significantly to globals exports
                      while core countries include The United States, France, Germany, etc; high income countries who import the most from the periphery.
                      By comparing prices low-income countries recieved for their exports from the high income, to the prices they should have recieved 
                      at global exchange rates, we can find the total trade value that was unfairly extracted from them. Subsequently, comparing the expected trade value at
                      world average prices with what they actually recieved helps us see, the hypothetical trade extraction value that would be fair under the rules of 
                      economic competition.
                      
                      <br><br> The extraction patterns by core countries are stark; between 1998 and 2020, they have extracted nearly 8 trillion 
                        dollars, and only growing over time.The lower economic status of periphery countries often results in diminished bargaining power, 
                        leaving them with little choice but to accept these unequal terms.The outcome is that they do not recieve their
                             fair economic share and are unable to build substantial wealth to invest in long term
                             climate change resilience, which is almost a luxury amidst their struggle to invest in other more immediate
                             infrastructure and social systems.")))
      )
    ),
    fluidRow(
      column(6,
             tags$div(class = 'custom-text',
                      style = "padding-top: 27px; height: 300px;text-align: justify;margin-right:15px;margin-top:55px;",
                      p(HTML("The belief that low-income countries can simply lift themselves up without help doesn't quite stand up. 
                      This perspective overlooks the ongoing and increasing value extraction by high-income countries, 
                      which take advantage of the lower bargaining power of poorer nations. 
                      This dynamic leaves low-income countries with progressively fewer resources to invest in their own development. 
                      A prime example is the United States, which has extracted nearly 4 trillion dollars from peripheral countries over 
                      the past two decades, with Mexico alone accounting for nearly half of this extraction. 
                      This issue extends beyond mere trade imbalances. It includes other forms of, more difficult to measure, practices such
                      as underpaying for labor and outsourcing high-emission activities to countries with weaker economies.
                      
                       This modern form of extraction is particularly 
                        concerning when considering the historical context – many periphery countries are former colonies or have 
                        been impacted by institutions of slavery, while core countries have historically benefited from colonization 
                        and slavery
                      
                      <br><br> Our key findings till now are clear: high-income countries, while emitting the most greenhouse gases per capita,
                      face the least vulnerability and possess the highest readiness for climate change. 
                      Instead of supporting low-income countries, they instead exacerbate the situation by unfairly extracting resources. 
                      This reality of climate inequality, highlights the urgent need for high-income countries to acknowledge their role 
                    and take responsibility")))
      ),
      
      column(6,tags$div(class = "plot-container-second ",
                        tags$div(class = "plot-title-second", "Total trade value Unfairly transferred by Peripherial Country (2000-2020) "),
                        tags$div(class = "plot-subtitle-second", "United States accounts for largest value extraction from 10 out of 11 peripheral countries"),
                        
                        tags$div(class = "dropdown-overlay-second",selectInput("countrychoice", "",
                                                                               choices = unique(transfer_group$transfer_country))),
                        
                        sankeyNetworkOutput("sankyboi",height = "300px"),
                        tags$div(class = "plot-caption-second", "Source: IMF Direction of Trade Statistics & Penn World Table 10.01"))
      )
      
    )
    #additional section here
  ),
  #####################################################################################################################
  fullSection(
    menu = "FifthSection",
    center = TRUE,
    class = "fifth-section",
    tags$style(HTML("
      .fp-tableCell {
        vertical-align: top;  /* Align content to the top */
      }
      .section {
        padding-top: 5px;
        padding-left: 30px/* Slightly reduced top padding */
      }
      .centered-header {
        text-align: left; /* Space between header and content */
      }
      .plot-title-fourth {
        font-size: 16px;
        font-weight: bold;
        text-align: left;
      }
      .plot-subtitle-fourth {
        font-size: 15px;
        text-align: left;
      }
      .plot-caption-fourth{
        font-size: 13px;
        text-align: right;
        margin-right: 20px;
        margin-top:-80px;

      }
      .plot-caption-fourth-fourth{
        font-size: 13px;
        text-align: right;
        margin-right: 20px;
        margin-top:-30px;
      }
      .plot-container-fourth-a {
        margin-right: 5px;
        margin-bottom:10px;
        position: relative; /* Reduced space below plot container */
      }
    ")),
    
    tags$h1("Even in High Income countries, is it fair for everyone be blamed ?", class = "centered-header",
            style = "font-family: 'Times New Roman', Times, serif;"), 
    fluidRow(
      column(4,
             tags$div(class = 'custom-text', 
                      style = "padding-top: 10px; height: 300px;width: 630px;text-align: justify;",# Add padding for spacing, adjust as needed
                      p(HTML("However, it's not entirely fair to blame every individual in high-income countries. The scenario of the 
                      wealthy prospering at the expense of the poor is a recurring theme, observed both in the context of national and 
                      personal incomes. 
                      <br><br>Before discussing our results, it is important to highlight our methodology. It is often difficult to measure 
                      each country's per capita emissions by income groups, especially for lower income countries due to lack of measurement 
                      capabilities and resources. However, it is more feasible to do so when considering larger global regions. Interestingly,
                      globally, there is a clear pattern in how per capita income is distributed across regions. The Global North and West 
                      generally include countries of high income and the South and East generally include middle to lower income countries.
                      <br><br>Knowing these patterns allows us to use per capita emissions of different income groups by region as a representative 
                      measure of what these emission patterns by income groups would look across high, middle and low income countries. For eg.
                      emissions of the top 10% income earners in North America and Europe can be thought of as representative of  
                      high income countries, while South Asia and Africa include more countries that are representative of middle to low 
                      income countries")))
      ),
      column(7,offset =1, tags$div(class = "plot-container-fourth-a",
                                    tags$div(class = "plot-title-fourth", "Global Income Distribution by Country  (2022)"),
                                    tags$div(class = "plot-subtitle-fourth", "Majority of the global south and east tends to be poorer than the global north and west"),
                                    plotOutput("map1"),
                                    tags$div(class = "plot-caption-fourth", "Source: World Bank")
                                    
                                    
      ))
    ),
    fluidRow(
      column(4,
             tags$div(class = 'custom-text',  
                      style = "padding-top: 30px; height: 300px;width: 630px; text-align: justify;",# Add padding for spacing, adjust as needed
                      p(HTML("Our results remain consistent: The affluent, whether viewed in terms of countries or individuals, 
                        emit significantly more than their lower-income counterparts globally. 
                        <br><br>The difference in emission rates is striking when comparing the bottom 50% in South and Southeast Asia and 
                        Sub-Saharan Africa to the top 10% in wealthier regions. In fact,
                        The top 10% in North America emit nearly 6 times more than the region's bottom 50%, 
                        and an astounding 140 times more than the bottom 50% of Sub-Saharan Africa.
                        
                        
                        <br><br>These findings add the final layer to the narrative of climate inequality, 
                        highlighting a pattern where high-income 'entities'—to use a broad term—indulge in consumption without considering 
                        the consequences. This behavior disproportionately affects those with lower incomes, 
                        both people and countries, who bear the brunt of climate change impacts sooner than everyone while being responsible
                        the least for it.")))
      ),
      column(7,offset =1, tags$div(class = "plot-container-fourth-a",
                                    tags$div(class = "plot-title-fourth", "Yearly Per-Capita Carbon Emissions by Income group, Globally(2019)"),
                                    tags$div(class = "plot-subtitle-fourth", "Globally, on average, the top 10% earners emit nearly 4x more carbon than the bottom 50% "),
                                    plotOutput("stats"),
                                    tags$div(class = "plot-caption-fourth-fourth", 'Source: Chancel,Lucas(2022)."Global carbon inequality over 1990–2019"')
      )
      )
      #additional section here
    )),
  ###################################################################################################################################
  fullSection(
    menu = "FifthSection",
    center = TRUE,
    class = "fifth-section",
    tags$style(HTML("
      .fp-tableCell {
        vertical-align: top;  /* Align content to the top */
      }
      .section-two {
        padding-top: -10px;
        padding-left: 30px/* Slightly reduced top padding */
      }
      .centered-header-last {
        text-align: left; /* Space between header and content */
        margin-bottom: -30px;
      }
      .plot-title-fifth {
        font-size: 16px;
        font-weight: bold;
        text-align: left;
      }
      .plot-subtitle-fifth {
        font-size: 15px;
        text-align: left;
      }
      .plot-caption-fifth {
        font-size: 12px;
        text-align: right;
        margin-bottom: 5px;
      }
      .plot-container-fifth {
        margin-bottom: 5px;
        margin-right: 5px;
        width: 100%;
        height = 150px;
        position: relative; /* Reduced space below plot container */
      }
    ")),
    
    tags$h1("Consequences and Conclusion", class = "centered-header-last",
            style = "font-family: 'Times New Roman', Times, serif;"), 
    fluidRow(
      column(12,
             tags$div(class = 'custom-text',  
                      style = "margin-bottom: 50px; text-align: justify; margin-right:15px;",
                      p(HTML(" <br><br>Overall, in assessing the global landscape of climate change, a stark inequality emerges, underscored by unambiguous data. 
                      The affluent, both as countries and individuals, are disproportionately responsible for greenhouse gas emissions, with its source
                      being in the lack of consequential accountability of their over-consumption. The intention of this narrative is not to persuade that
                      climate change is a looming catastrophe; rather, it operates under the hopeful assumption that the severity of climate
                      change is already an accepted reality. It hopes to contribute in efforts to hold the wealthy
                      accountable for the benefits they recieve from disproportionate emissions while reaping substantial benefits through the unfair extraction of value from low-income 
                      nations.
                      
                      <br><br>It is vital that high-income countries shift from a pattern of extraction to one of investment, 
                      focusing on enhancing the resilience of lower-income countries.Understanding these wealth based patterns is key in identifying those accountable and 
                    targeting national and global policies to combat climate change most effectively by holding those most responsible, acountable for their actions.At the end,
                    the effects of climate change is a shared burden and high income entities' climate cognizance 
                      benefits everyone in the long term. 
                      
                      <br><br>Finally, if the time frames of discussion thusfar, ranging from the 1960s to 2100, appeared 
                      too distant to visualize, below you can look at cities that are predicted with a high degree of confidence 
                      to become either uninhabitable or completely under the sea by, as early as, 2050 . For most of those under the age of 50, loss of these cities 
                      is very much going to occur within our lifetimes. These might be your hometowns, destinations you have travelled to or 
                      are just generally interested in. Regardless, each of these cities represent 100s to 1000s of years of cultural history and 
                      most importantly millions of real people and their future generations.
                      
                     ")))
      )),
    fluidRow(
      column(8,offset = 2, tags$div(class = "plot-container-fifth",
                                    tags$div(class = "plot-title-fifth", "Which cities will Become Uninhabitable by 2050 due to Climate Change"),
                                    
                                    fluidRow(
                                      column(2, # Adjust the width as needed
                                             selectInput("countryInput", "Select at Risk Country", 
                                                         choices = unique(sea_levels_joined$country), 
                                                         selected = "Australia")
                                      ),
                                      column(2,offset = 1,  # Adjust the width as needed
                                             uiOutput("cityInputUI")
                                      )
                                    ),
                                    
                                    leafletOutput("map2"),
                                    tags$div(class = "plot-caption-fifth", "Source: Nestpick")
      ))
    )
  )
)    
###################################################################################################################################
server <- function(input, output, session) {
  
  # CO2 Plot
  output$co2Plot <- renderPlotly({
    plot_ly(
      data = co2, 
      x = ~numer_date, 
      y = ~avg_co2, 
      frame = ~frame, 
      type = 'scatter',
      mode = 'lines', 
      line = list(simplify = FALSE)
    ) %>%
      layout(
        xaxis = list(title = "Year", zeroline = FALSE),
        yaxis = list(title = "CO2 (parts per millions)", zeroline = FALSE),
        height = 350
      ) %>%
      animation_opts(frame = 50, transition = 0, redraw = FALSE)
  })
  
  # Subset the data based on selection
  data_to_plot <- reactive({
    switch(input$varChoice,
           "Natural Disasters" = disasters_grouped,
           "Sea Level Change" = global_sea_levels,
           "Surface Temperature" = temp_change_grouped
    )
  })
  
  # Plotting logic for climate data
  output$climatePlot <- renderPlotly({
    plot_data <- data_to_plot()  # Access the reactive data
    
    
    # Determine the y-axis variable
    y_var <- switch(input$varChoice,
                    "Natural Disasters" = ~Disaster_Count,
                    "Sea Level Change" = ~avg_sea_level_change,
                    "Surface Temperature" = ~Avg_temp_change
    )
    
    # Base plot
    p <- plot_ly(
      data = plot_data, 
      x = ~Year, 
      y = y_var,
      frame = ~frame, 
      type = 'scatter',
      mode = 'lines',
      line = list(simplify = FALSE)
    ) 
    
    # Conditional additions to the plot
    if (input$varChoice == "Natural Disasters") {
      p <- plot_ly(
        data = plot_data, 
        x = ~Year, 
        y = y_var,
        frame = ~frame, 
        type = 'scatter',
        mode = 'lines',
        split = ~Indicator,
        line = list(simplify = FALSE)
      )
      p <- p %>% layout(
        xaxis = list(title = "Year", zeroline = FALSE),
        yaxis = list(title = "Disaster Count", zeroline = FALSE),
        annotations = list(
          x = 1970,
          y = 130,
          xref = 'x',
          yref = 'y',
          text = 'Data available from 1980',
          showarrow = FALSE,
          ax = 0,
          ay = -40
        ),
        legend = list(
          x = 0.5, 
          y = 1.1,
          xanchor = 'center',
          yanchor = 'top',
          orientation = 'h'
        ),height = 350
      )
    } else if (input$varChoice == "Sea Level Change") {
      p <- p %>% layout(
        xaxis = list(title = "Year", zeroline = FALSE),
        yaxis = list(title = 'sea level change (in millimeters)', zeroline = FALSE),
        annotations = list(
          x = 1970,
          y = 20,
          xref = 'x',
          yref = 'y',
          text = 'Data available from 1992',
          showarrow = FALSE,
          ax = 0,
          ay = -40
        ),
        showlegend = FALSE,height = 350
      )
    } else {
      p <- p %>% layout(
        xaxis = list(title = "Year", zeroline = FALSE,range = c(min(plot_data$Year), max(plot_data$Year))),
        yaxis = list(title = 'Temperature change (in Celsius)', zeroline = FALSE),
        showlegend = FALSE,height = 350
      )
    }
    
    p <- p %>% animation_opts(frame = 70, transition = 0, redraw = FALSE)
    p
  })
  
  custom_colors <- c(
    "High income" = "#B53317", "Low income" = "#33B517", "Middle income" = "lightblue"
  )
  
  create_plot <- function(data, emission_type, ylabel, title, subtitle) {
    
    p <- ggplot(data, aes(x = Year, y = !!sym(emission_type), group = group, color = group)) +
      geom_line() +
      scale_color_manual(values = custom_colors) +
      labs(x = "Year", y = ylabel, title = title, subtitle = subtitle) +
      scale_x_continuous(breaks = c(1990, 2000, 2010, 2019))+
      theme_minimal() +
      theme(
        
        legend.position = "none",
        axis.text.x = element_text(size = 13, face = 'bold'),
        axis.text.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.caption = element_text(size = 14),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 14)
      )
    
    return(p)
  }
  # Render different plots based on switch. Multipe if else conditions for three gases for 
  #labels, titles and subtitles
  
  output$plotNational <- renderPlot({
    gas <- switch(input$gasType,
                  "Carbon Dioxide(CO2)" = "co2_total",
                  "Methane(CH4)" = "ch4_total",
                  "Nitrous Oxide(NO2)" = "no2_total")
    
    if (input$gasType == 'Carbon Dioxide(CO2)'){
      ylabel <- "Emission in Metric Tonnes"
      title <- "Total National Carbon Dioxide Emissions, by Income Group (1990-2019)"
      subtitle <- "Middle income countries emit more than 2x carbon than low income"
    } 
    else if (input$gasType == 'Methane(CH4)'){
      ylabel <- "Emission in Metric Tonnes"
      title <- "Total National Methane emissions, by Income Group (1990-2019)"
      subtitle <- "Middle and Low Income countries' methane emissions are the highest"
    }
    else {
      ylabel <- "Emission in Metric Tonnes"
      title <- "Total National Nitrous Oxide emissions, by Income Group (1990-2019)"
      subtitle <- "Middle and high income countries emit 5x more NO2 than low income"
    }
    
    create_plot(ghg, gas, ylabel, title, subtitle)
  }, height = 350)
  
  
  output$plotPerCapita <- renderPlot({
    gas <- switch(input$gasType,
                  "Carbon Dioxide(CO2)" = "co2_pc",
                  "Methane(CH4)" = "ch4_pc",
                  "Nitrous Oxide(NO2)" = "no2_pc")
    if (input$gasType == 'Carbon Dioxide(CO2)'){
      ylabel <-  "Emission in Tonnes"
      title <- "Total Per-Capita Carbon Dioxide Emissions, by Income Group (1990-2019)"
      subtitle <- "High income citizens emit 2x more carbon than low income."
    } 
    else if (input$gasType == 'Methane(CH4)'){
      ylabel <- "Emission in Tonnes"
      title <- "Total Per-Capita Methane emissions, by Income Group (1990-2019)"
      subtitle <- "High income country citizens emit the most methane"
    }
    else {
      ylabel <- "Emission in Tonnes"
      title <- "Total Per-Capita Nitrous Oxide emissions, by Income Group (1990-2019)"
      subtitle <- "High income citizens emit 7x more NO2 than low income"
    }
    create_plot(ghg, gas, ylabel, title, subtitle)
  }, height = 350)
  
  
  
  x_min <- min(plot_nd_gains$readiness, na.rm = TRUE)
  x_max <- max(plot_nd_gains$readiness, na.rm = TRUE)
  y_min <- min(plot_nd_gains$vulnerability, na.rm = TRUE)
  y_max <- max(plot_nd_gains$vulnerability, na.rm = TRUE)
  
  # Find the common range for the 45-degree line
  common_min <- max(x_min, y_min)
  common_max <- min(x_max, y_max)
  t1 <- c(common_min, common_max)
  y1 <- c(common_min, common_max)
  
  output$scoreplot <- renderPlotly({plot_ly() %>%
      add_trace(data = plot_nd_gains, x = ~readiness, y = ~vulnerability, text = ~Name, ids = ~Name, color = ~group, 
                colors = custom_colors,frame = ~year, marker = list(size = 15), mode = "markers") %>%
      add_trace( x = t1, y = y1, mode = "lines", line = list(color = 'grey'),
                 showlegend = FALSE  # Hide this trace from the legend
      ) %>%
      add_annotations(x = t1[2]+0.08,  # X position of the annotation (end of the line)
                      y = y1[2]-0.026,  # Y position of the annotation (end of the line)
                      text = "Proportionately prepared \nfor climate caused \nvulnerability",  # Your annotation text
                      showarrow = FALSE,
                      xref = "x",
                      yref = "y"
      ) %>%layout(
        xaxis = list(title = "Readiness Score (0-1)"),
        yaxis = list(title = "Vulnerability Score(0-1)")
      )
  },)
  
  
  
  output$tradevalue <- renderPlot({
    
    
    ggplot(annual_summary) +
      geom_bar(aes(y = Total_Transfer / 1e3, x = Year, fill = "Unfairly Extracted Trade Value"), 
               stat = "identity", position = "stack") +
      geom_bar(aes(y = Total_Transfer_Fair / 1e3, x = Year, fill = "Expected Value Exchange \nat World Average Price"), 
               stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Unfairly Extracted Trade Value" = "#B53317", 
                                   "Expected Value Exchange \nat World Average Price" = "#6BAED6")) +
      labs(
        x = "Year",
        y = "Total Trade Transfer (USD, Billion)",
      ) +
      theme_minimal() +
      theme(
        text = element_text(family = 'serif'),
        legend.text = element_text(size = 13, face = 'bold'),
        legend.title = element_blank(),
        legend.key.height = unit(3, "cm"),
        axis.text.x = element_text(size = 14, face = 'bold'),
        axis.text.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.caption = element_text(size = 15)
      )
    
    
    
    
  })
  
  df_filtered <- reactive ({transfer_group %>% filter(transfer_country == input$countrychoice)})
  choice  = reactive ({input$countrychoice})
  output$sankyboi <- renderSankeyNetwork({
    
    # Create a vector of all unique countries in this subset
    all_countries <- unique(c(df_filtered()$transfer_country, df_filtered()$Country))
    
    country_totals <- df_filtered() %>%
      group_by(Country) %>%
      summarise(total_transfer = sum(sum_transfer)) %>%
      arrange(desc(total_transfer))
    
    # Create a nodes data frame
    nodes <- data.frame(name = c(choice(), country_totals$Country), 
                        group = as.factor(c(choice(), country_totals$Country)))
    
    #assign colors to each node using javascript base 
    my_color <- 'd3.scaleOrdinal() .domain(["0", "1", "2", "3", 
                                    "4", "5", "6", "7", 
                                      "8"]) .range(["#B53317","#3288BD","#9ECAE1", "#66C2A5", 
                                                                      "#ABDDA4","#FD8D3C", 
                                                                        "#FEB24C", "#FED976", 
                                                                        "#FFEDA0" ])'
    
    # Create a links data frame
    links <- df_filtered() %>%
      mutate(source = match(transfer_country, nodes$name) - 1,
             target = match(Country, nodes$name) - 1) %>%
      select(source, target, sum_transfer)
    
    links$group <- as.factor(links$target)
    
    nodes$group <- 0:(nrow(nodes) - 1)
    nodes$group  <- as.character(nodes$group)
    
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                  Target = "target", Value = "sum_transfer", NodeID = "name",LinkGroup = 'group', NodeGroup = 'group',
                  fontSize = 14,fontFamily = "Times New Roman", nodeWidth = 40, units = "Billion USD",colourScale=my_color,)
    
  })
  
  output$map1 <- renderPlot({
    world2 <- world %>%
      left_join(gni, by = c('iso_a3' = 'Code'))
    
    world2 %>%
      st_transform(crs = "+proj=wintri") %>%
      ggplot() +
      geom_sf(aes(fill = gni), color = "black", size = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +  # Equator
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + 
      scale_fill_gradientn(colours = plasma(n = 256)) +
      labs(
        fill = "Yearly Income per capita (USD)",
        x ='',
        y = ''
      ) +coord_sf(datum = NA)+
      theme_minimal()+theme(legend.spacing.y = unit(1, 'cm'),
                            legend.text = element_text(size = 13),
                            legend.title = element_text(size = 14, face = 'bold'),
                            plot.caption = element_text(size = 15))
    
    
  }, height = 340)
  
  output$stats <- renderPlot({
    ggplot(emit_income, aes(x = Region, y = group, size = c02)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(2, 38)) +  # Adjust the size range for bubbles
      labs(
        x = "Region",
        y = "Income Group",
        size = "Tonnes of CO2"
      ) +
      theme_minimal() +
      theme(
        text = element_text(family = 'serif'),
        legend.text = element_text(size = 13, face = 'bold'),
        legend.title = element_text(size = 13, face = 'bold'),
        axis.text.x = element_text(size = 13, face = 'bold'),
        axis.text.y = element_text(size = 13, face = 'bold'),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.caption = element_text(size = 15)
      )
    
    
  }, height = 360, width = 920)
  
  country_df <- reactive({
    sea_levels_joined %>% filter(country == input$countryInput)
  })
  
  # Dynamic UI for city selection based on chosen country
  output$cityInputUI <- renderUI({
    # Ensure a country is selected before populating city choices
    if (!is.null(input$countryInput) && input$countryInput != "") {
      selectInput("cityInput", "Select City at high risk", 
                  choices = unique(country_df()$City), 
                  selected = "MELBOURNE")
    }
  })
  
  # Reactive expression for city selection
  city <- reactive({ input$cityInput })
  
  # Render the leaflet map
  output$map2 <- renderLeaflet({
    # Ensure both country and city are selected
    if (!is.null(input$countryInput) && input$countryInput != "" &&
        !is.null(input$cityInput) && input$cityInput != "") {
      
      selected_data <- country_df() %>% filter(City == city())
      if(nrow(selected_data) > 0) {
        selected_row <- selected_data[1, ]
        
        # Extract the country polygon and city coordinates
        country_polygon <- selected_row$geometry
        
        m <- leaflet() %>%
          addProviderTiles(providers$OpenStreetMap) %>%
          addPolygons(data = country_polygon, color = "red", weight = 1, fillOpacity = 0.1) %>%
          addMarkers(lng = selected_row$lng, lat = selected_row$lat, popup = selected_row$City)
        m <- setView(m, lng = selected_row$lng, lat = selected_row$lat, zoom = 9.5)
        m
        return(m)
      }
    }
  })
}


shinyApp(ui, server)