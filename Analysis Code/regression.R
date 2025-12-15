# %% load in libraries and source data #####
library(dplyr) # data wrangling
library(readr) # reading csv file
library(broom) # regression
library(texreg)

# data source
source = "buses.csv" 
dat = source %>% read_csv()

source("tim_functions_process_control.R")

dat = dat %>% mutate(
  # convert from excel serial dates if date is available
  date_published_or_updated = date_published_or_updated %>% as.numeric() %>% as.Date(origin = "1899-12-30"),
  price_per_seat = round(base_price / seating_capacity, 2)
  )

# dat %>% write_csv("buses_with_price_per_seat_and_converted_dates.csv")

# remove NAs from the dataset
dat = dat[!is.na(dat$price_per_seat), ]

# turn categorical variables into factors
dat_f = dat %>% mutate(
  bus_type = factor(bus_type),
  bus_manufacturer = factor(bus_manufacturer),
  state = factor(state)
)

## tidier() #####
# helper function to display model info
tidier = function(model){
  model %>% tidy(conf.int = TRUE, conf.level = 0.95) %>% 
    select(term, estimate, p.value, conf.low, conf.high) %>%
    arrange(p.value)
}


## strip_se() 
# helper function to remove se from htmlreg() #####
strip_se <- function(m) {
  e <- texreg::extract(m)
  e@se <- rep(NA_real_, length(e@se))  # NA -> nothing printed
  e
}

## format_htmlreg() 
# helper function to format htmlreg() #####
format_htmlreg = function(input_html, output_html, sci_digits, note_text){

  # input_html  <- "regression_type_a.html"
  # output_html <- "regression_type_a_sci.html"
  # sci_digits  <- 3
  # note_text   <- paste(
  #   "Statistical Significance: *** p < 0.001; ** p < 0.01; * p < 0.05.",
  #   "Collins Bus is the baseline manufacturer, Arkansas is the baseline state, and 2022 is the baseline year.",
  #   "Purchase year is treated as a categorical variable."
  # )

  to_sci <- function(x, digits = sci_digits) formatC(as.numeric(x), format = "e", digits = digits)
  is_summary_label <- function(x) {
    x <- trimws(x)
    grepl("^R\\s*2\\b", x) || grepl("^Adj\\.", x) || grepl("^Num\\. obs\\.", x)
  }

  doc <- read_html(input_html)

  ## 1) Convert only coefficient cells to scientific notation
  rows <- xml_find_all(doc, "//table//tr")
  for (row in rows) {
    first_td <- xml_find_first(row, ".//td[1]")
    if (is.na(first_td)) next
    if (is_summary_label(xml_text(first_td))) next

    cells <- xml_find_all(row, ".//td[position()>1]")
    for (cell in cells) {
      target <- xml_find_first(cell, ".//b"); if (is.na(target)) target <- cell
      txt <- xml_text(target)
      m <- regexec("^\\s*([-+]?(?:\\d+\\.?\\d*|\\d*\\.\\d+))\\s*(\\*+)?\\s*$", txt)
      hit <- regmatches(txt, m)[[1]]
      if (length(hit)) {
        sci <- to_sci(hit[2]); stars <- if (length(hit) >= 3) hit[3] else ""
        xml_set_text(target, paste0(sci, stars))
      }
    }
  }

  ## 2) Remove any in-table note row and old outside notes
  xml_remove(xml_find_all(doc, "//table//tr[td[contains(., 'Statistical Significance')]]"))
  xml_remove(xml_find_all(doc, "//p[contains(., 'Statistical Significance')]"))

  ## 3) Insert a fresh note AFTER the table (escaped safely)
  tbl <- xml_find_first(doc, "//table")
  if (!is.na(tbl)) {
    new_note <- read_xml("<p class='texreg-note' style='margin-top:0.6em;'></p>")
    xml_set_text(new_note, note_text)  # xml2 escapes < correctly
    xml_add_sibling(tbl, new_note, .where = "after")
  }

  write_html(doc, output_html)

}

# %%

dat_f %>% lm(formula = base_price ~ bus_type) %>% tidier()

# Reference category: Type A
# # A tibble: 3 × 3
#   term           estimate  p.value
#   <chr>             <dbl>    <dbl>
# 1 (Intercept)     272120. 1.91e-98
# 2 bus_typeType C   95084. 1.38e-30
# 3 bus_typeType D  120697. 1.64e-25



dat_f %>% lm(formula = base_price ~ seating_capacity) %>% tidier()

# # A tibble: 2 × 5
#   term             estimate  p.value conf.low conf.high
#   <chr>               <dbl>    <dbl>    <dbl>     <dbl>
# 1 (Intercept)       237471. 1.14e-63  220637.   254305.
# 2 seating_capacity    1869. 1.01e-28    1599.     2139.

fit = dat %>% lm(formula = base_price ~ seating_capacity)
eq <- substitute(
  italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,
  list(
    a = format(as.numeric(coef(fit)[1]), digits = 2),
    b = format(as.numeric(coef(fit)[2]), digits = 2),
    r2 = format(as.numeric(summary(fit)$r.squared), digits = 3)
  )
)

g = dat %>% ggplot(mapping = aes(x = seating_capacity, y = base_price))+
  geom_point() +
  labs(x = "Seating Capacity", 
      y = "Base Price ($)",
      subtitle = "Base Price vs Seating Capacity"
  )  +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE) + # se = FALSE removes extra stuff 
  geom_text(
    x = Inf, y = -Inf,
    label = as.character(as.expression(eq)),
    hjust = 1.1, vjust = -1,
    parse = TRUE
  )

g

ggsave('price_vs_seating.png', plot = g, width = 6.5, height = 4, dpi = 300)

# %% type a htmlreg() #####

type_a = dat %>% filter(bus_type == "Type A")
type_a = type_a %>% mutate(
  bus_type = factor(bus_type),
  bus_manufacturer = factor(bus_manufacturer),
  state = factor(state),
  purchase_year = factor(purchase_year)
)

m1a = type_a %>% lm(formula = base_price ~ bus_manufacturer) 
m2a = type_a %>% lm(formula = base_price ~ bus_manufacturer + state) 
m3a = type_a %>% lm(formula = base_price ~ bus_manufacturer + state + purchase_year) 

levels(type_a$bus_manufacturer)[1] # Collins Bus
levels(type_a$state)[1] # AR
levels(type_a$purchase_year)[1] # 2022

mods_a <- lapply(list(m1a, m2a, m3a), strip_se)

htmlreg(
  mods_a,
  bold = 0.05, 
  include.fstat = TRUE,
  file = "regression_type_a.html",
  # Add column labels
  custom.model.names = c(
    "Model 1", "Model 2", "Model 3"
    ),
  custom.coef.map = list(
    "(Intercept)" = "Intercept",
    "bus_manufacturerEndera" = "Endera",
    "bus_manufacturerGreenPower" = "GreenPower",
    "bus_manufacturerLightning eMotors/Collins Bus" = "Lightening eMotors/Collins Bus",
    "bus_manufacturerMagellan" = "Magellan",
    "bus_manufacturerMicro Bird" = "Micro Bird",
    "bus_manufacturerPegasus/Zeus Electric" = "Pegasus/Zeus Electric",
    "bus_manufacturerThomas Built Buses" = "Thomas Built Buses",
    "bus_manufacturerTrans Tech" = "Trans Tech",
    "stateFL" = "Florida",
    "stateGA" = "Georgia",
    "stateKY" = "Kentucky",
    "stateLA" = "Louisiana",
    "stateMS" = "Mississippi",
    "stateNY" = "New York",
    "stateWA" = "Washington",
    "stateWV" = "West Virginia",
    "purchase_year2023" = "Purchase year: 2023"
  ),
  custom.note = "Statistical Significance: *** p < 0.001; ** p < 0.01; * p < 0.05."
)

input_html  <- "regression_type_a.html"
output_html <- "regression_type_a_sci.html"
sci_digits  <- 2
note_text   <- paste(
  "Statistical Significance: *** p < 0.001; ** p < 0.01; * p < 0.05.",
  "Collins Bus is the baseline manufacturer, Arkansas is the baseline state, and 2022 is the baseline year.",
  "Purchase year is treated as a categorical variable."
)

format_htmlreg(input_html, output_html, sci_digits, note_text)


# %% type c htmlreg() #####

type_c = dat %>% filter(bus_type == "Type C")
type_c = type_c %>% mutate(
  bus_type = factor(bus_type),
  bus_manufacturer = factor(bus_manufacturer),
  state = factor(state),
  purchase_year = factor(purchase_year)
)

m1c = type_c %>% lm(formula = base_price ~ bus_manufacturer) 
m2c = type_c %>% lm(formula = base_price ~ bus_manufacturer + state) 
m3c = type_c %>% lm(formula = base_price ~ bus_manufacturer + state + purchase_year) 

levels(type_c$bus_manufacturer)[1] # Blue Bird
levels(type_c$state)[1] # AR
levels(type_c$purchase_year)[1] # 2021

mods_c <- lapply(list(m1c, m2c, m3c), strip_se)

htmlreg(
  mods_c,
  bold = 0.05, 
  include.fstat = TRUE,
  file = "regression_type_c.html",
  # Add column labels
  custom.model.names = c(
    "Model 1", "Model 2", "Model 3"
    ),
  custom.coef.map = list(
    "(Intercept)" = "Intercept",
    "bus_manufacturerIC Bus" = "IC Bus",
    "bus_manufacturerLion Electric" = "Lion Electric",
    "bus_manufacturerThomas Built Buses" = "Thomas Built Buses",
    "stateAZ" = "Arizona",
    "stateFL" = "Florida",
    "stateGA" = "Georgia",
    "stateKY" = "Kentucky",
    "stateLA" = "Louisiana",
    "stateME" = "Maine",
    "stateMS" = "Mississippi",
    "stateNC" = "North Carolina",
    "stateNY" = "New York",
    "stateOH" = "Ohio",
    "stateUT" = "Utah",
    "stateVA" = "Vermont",
    "stateWA" = "Washington",
    "stateWV" = "West Virginia",
    "purchase_year2022" = "Purchase year: 2022",
    "purchase_year2023" = "Purchase year: 2023"
  ),
  custom.note = "Statistical Significance: *** p < 0.001; ** p < 0.01; * p < 0.05."
)

input_html  <- "regression_type_c.html"
output_html <- "regression_type_c_sci.html"
sci_digits  <- 2
note_text   <- paste(
  "Statistical Significance: *** p < 0.001; ** p < 0.01; * p < 0.05.",
  "Blue Bird is the baseline manufacturer, Arkansas is the baseline state, and 2021 is the baseline year.",
  "Purchase year is treated as a categorical variable."
)

format_htmlreg(input_html, output_html, sci_digits, note_text)

# %% type d htmlreg() #####

type_d = dat %>% filter(bus_type == "Type D")
type_d = type_d %>% mutate(
  bus_type = factor(bus_type),
  bus_manufacturer = factor(bus_manufacturer),
  state = factor(state),
  purchase_year = factor(purchase_year)
)

m1d = type_d %>% lm(formula = base_price ~ bus_manufacturer) 
m2d = type_d %>% lm(formula = base_price ~ bus_manufacturer + state) 
m3d = type_d %>% lm(formula = base_price ~ bus_manufacturer + state + purchase_year) 

levels(type_d$bus_manufacturer)[1] # Blue Bird
levels(type_d$state)[1] # FL
levels(type_d$purchase_year)[1] # 2020

mods_d <- lapply(list(m1d, m2d, m3d), strip_se)

htmlreg(
  mods_d,
  bold = 0.05, 
  include.fstat = TRUE,
  file = "regression_type_d.html",
  # Add column labels
  custom.model.names = c(
    "Model 1", "Model 2", "Model 3"
    ),
  custom.coef.map = list(
    "(Intercept)" = "Intercept",
    "bus_manufacturerGreenPower" = "GreenPower",
    "bus_manufacturerLion Electric" = "Lion Electric",
    "stateGA" = "Georgia",
    "stateKY" = "Kentucky",
    "stateNY" = "New York",
    "stateUT" = "Utah",
    "stateWA" = "Washington",
    "stateWV" = "West Virginia",
    "purchase_year2021" = "Purchase year: 2021",
    "purchase_year2022" = "Purchase year: 2022",
    "purchase_year2023" = "Purchase year: 2023"
  ),
  custom.note = "Statistical Significance: *** p < 0.001; ** p < 0.01; * p < 0.05."
)

input_html  <- "regression_type_d.html"
output_html <- "regression_type_d_sci.html"
sci_digits  <- 2
note_text   <- paste(
  "Statistical Significance: *** p < 0.001; ** p < 0.01; * p < 0.05.",
  "Blue Bird is the baseline manufacturer, Florida is the baseline state, and 2020 is the baseline year.",
  "Purchase year is treated as a categorical variable."
)

format_htmlreg(input_html, output_html, sci_digits, note_text)
