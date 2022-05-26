
# Dependências dos slides/aula
library(knitr)          # CRAN v1.37
library(rmarkdown)      # CRAN v2.11
library(xaringan)       # CRAN v0.22
library(xaringanthemer) # CRAN v0.4.1
library(xaringanExtra)  # [github::gadenbuie/xaringanExtra] v0.5.5
library(RefManageR)     # CRAN v1.3.0
library(ggplot2)        # CRAN v3.3.5
library(fontawesome)    # CRAN v0.1.0

# Opções de chunks
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(
  echo       = TRUE,
  warning    = FALSE,
  message    = FALSE,
  fig.retina = 3,
  fig.width  = 6,
  fig.asp    = 0.618,
  out.width  = "100%",
  fig.align  = "center",
  comment    = "#"
  )

# Cores para gráficos
colors <- c(
  blue       = "#282f6b",
  red        = "#b22200",
  yellow     = "#eace3f",
  green      = "#224f20",
  purple     = "#5f487c",
  orange     = "#b35c1e",
  turquoise  = "#419391",
  green_two  = "#839c56",
  light_blue = "#3b89bc",
  gray       = "#666666"
)

# Tema da apresentação
xaringanthemer::style_mono_light(
  base_color                      = unname(colors["blue"]),
  title_slide_background_image    = params$logo_slides,
  title_slide_background_size     = 12,
  title_slide_background_position = "bottom 10px right 20px",
  title_slide_background_color    = "white",
  title_slide_text_color          = unname(colors["blue"]),
  footnote_position_bottom        = "15px"
  )

# Opções extras do tema
xaringanExtra::use_scribble(pen_color = unname(colors["blue"]))
xaringanExtra::use_search(
  position  = "bottom-left",
  show_icon = TRUE
  )
xaringanExtra::use_clipboard(
  button_text  = "Copiar",
  success_text = "Copiado!",
  error_text   = "Pressione Ctrl+C para copiar"
  )
xaringanExtra::use_progress_bar(
  color    = colors["blue"],
  location = "bottom"
  )
xaringanExtra::use_extra_styles(
  hover_code_line         = TRUE,
  mute_unhighlighted_code = FALSE
  )
xaringanExtra::use_panelset()



