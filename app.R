library(shiny)
library(dplyr)

setwd('C:/Users/USUARIO/Documents/GitHub/modelo_tfg')

# ── Coeficientes del modelo ────────────────────────────────────────────────────
coefs <- c(
  "(Intercept)"                                             = -3.856547528,
  "ORIENTACIONHomosexual"                                   = -0.461205440,
  "CONVIVENCIAVivo con mi padre y/o madre"                  = -0.181482527,
  "CONVIVENCIAVivo con mi pareja e hijo/a/s"                =  0.007060126,
  "CONVIVENCIAOtra forma de convivencia"                    = -0.134567632,
  "PAREJAHe tenido pareja en el pasado, pero no actualmente"= -0.033378919,
  "REL_SEXUALESNo"                                          =  0.233917471,
  "SEXOHombres"                                             =  0.227540293,
  "ESTUDIOSSuperiores"                                      = -0.071241171,
  "CLASESOCIALMedia Baja y Baja"                            = -0.317093705,
  "CARENCIAMLeve"                                           =  0.102124392,
  "LIMITACon alguna limitacion"                             =  0.527738744,
  "PRESION_PORN_MIO"                                        =  0.234127892,
  "SEXTING_PIDO"                                            =  0.174012355,
  "REDES_COMENT"                                            =  0.682936716,
  "REDES_DINERO"                                            =  0.548566398,
  "DIFUSION_NO_CONSENTIDA"                                  =  0.377934725,
  "EDUC.SEX_CLASE"                                          = -0.120389815,
  "EDUC.SEX_PROP"                                           = -0.037772326,
  "CONSUMO_ES_INFIDELIDAD"                                  =  0.030516951,
  "COMUNICACION_POCO_IMP"                                   =  0.024407320,
  "ORGASMO_MAS_IMP"                                         =  0.001074133,
  "TAMANO_PENE_IMP"                                         =  0.019344044,
  "INTENSO_MEJOR"                                           =  0.014486265,
  "SEXO_ES_PENETRACION"                                     =  0.027871653,
  "HOM_SABEN_MAS"                                           =  0.077615328,
  "FORZAR_SEXO"                                             =  0.057454340
)

# ── Prediccion ─────────────────────────────────────────────────────────────────
predict_prob <- function(v) {
  lp <- coefs["(Intercept)"]
  
  if (!is.null(v$ORIENTACION) && v$ORIENTACION == "Homosexual")
    lp <- lp + coefs["ORIENTACIONHomosexual"]
  
  conv <- v$CONVIVENCIA
  if (!is.null(conv) && conv != "") {
    if      (conv == "Vivo con mi padre y/o madre")
      lp <- lp + coefs["CONVIVENCIAVivo con mi padre y/o madre"]
    else if (conv == "Vivo con mi pareja e hijo/a/s")
      lp <- lp + coefs["CONVIVENCIAVivo con mi pareja e hijo/a/s"]
    else if (conv %in% c("Vivo con mi pareja (sin hijo/a/s)", "Otra forma de convivencia"))
      lp <- lp + coefs["CONVIVENCIAOtra forma de convivencia"]
  }
  
  if (!is.null(v$PAREJA) &&
      v$PAREJA == "He tenido pareja en el pasado, pero no actualmente")
    lp <- lp + coefs["PAREJAHe tenido pareja en el pasado, pero no actualmente"]
  
  if (!is.null(v$REL_SEXUALES) && v$REL_SEXUALES == "No")
    lp <- lp + coefs["REL_SEXUALESNo"]
  
  if (!is.null(v$SEXO) && v$SEXO == "Hombre")
    lp <- lp + coefs["SEXOHombres"]
  
  if (!is.null(v$ESTUDIOS) && v$ESTUDIOS == "Superiores")
    lp <- lp + coefs["ESTUDIOSSuperiores"]
  
  if (!is.null(v$CLASESOCIAL) && v$CLASESOCIAL == "Media Baja y Baja")
    lp <- lp + coefs["CLASESOCIALMedia Baja y Baja"]
  
  if (!is.null(v$CARENCIAM) && v$CARENCIAM == "Leve")
    lp <- lp + coefs["CARENCIAMLeve"]
  
  if (!is.null(v$LIMITA) && v$LIMITA == "Con alguna limitacion")
    lp <- lp + coefs["LIMITACon alguna limitacion"]
  
  # Ordinales 0-3
  for (nm in c("PRESION_PORN_MIO","SEXTING_PIDO","REDES_COMENT",
               "REDES_DINERO","DIFUSION_NO_CONSENTIDA",
               "EDUC.SEX_CLASE","EDUC.SEX_PROP")) {
    val <- v[[nm]]
    if (!is.null(val) && !is.na(val))
      lp <- lp + coefs[nm] * as.numeric(val)
  }
  
  # Numericas 0-10
  for (nm in c("CONSUMO_ES_INFIDELIDAD","COMUNICACION_POCO_IMP","ORGASMO_MAS_IMP",
               "TAMANO_PENE_IMP","INTENSO_MEJOR","SEXO_ES_PENETRACION",
               "HOM_SABEN_MAS","FORZAR_SEXO")) {
    val <- v[[nm]]
    if (!is.null(val) && !is.na(val))
      lp <- lp + coefs[nm] * as.numeric(val)
  }
  
  1 / (1 + exp(-lp))
}

# ── Helpers UI ─────────────────────────────────────────────────────────────────

# Select con etiqueta
sel_q <- function(id, num, label, choices, width = "380px") {
  div(class = "q-block",
      tags$label(class = "q-label", label),
      selectInput(id, NULL, choices = c("— Selecciona una opción —" = "", choices),
                  width = width)
  )
}

# Numeric con etiqueta
num_q <- function(id, num, label, ...) {
  div(class = "q-block",
      tags$label(class = "q-label", label),
      numericInput(id, NULL, value = NA, ...)
  )
}

# Slider 0-10
slider010 <- function(id, label,
                      left = "0 — Nada de acuerdo",
                      right = "10 — Totalmente de acuerdo") {
  div(class = "q-block",
      tags$label(class = "q-label", label),
      sliderInput(id, NULL, min = 0, max = 10, value = 5, step = 1,
                  ticks = TRUE, width = "100%"),
      div(class = "slider-ends", span(left), span(right))
  )
}

# Slider 0-3
slider03 <- function(id, label,
                     left  = "0 — Totalmente en desacuerdo",
                     right = "3 — Totalmente de acuerdo") {
  div(class = "q-block",
      tags$label(class = "q-label", label),
      sliderInput(id, NULL, min = 0, max = 3, value = 1, step = 1,
                  ticks = TRUE, width = "100%"),
      div(class = "slider-ends", span(left), span(right))
  )
}

# Radio Likert 4 puntos, 0-3, sin valor por defecto (obligatorio)
likert_req <- function(id, label) {
  div(class = "q-block",
      tags$label(class = "q-label", label),
      radioButtons(id, NULL,
                   choices  = c("Nunca" = 0, 'De forma ocasional' = 1,
                                "A menudo" = 2, "Mucha frecuencia" =3),
                   selected = character(0), inline = TRUE
      )
  )
}

# Checkbox item
chk <- function(id, label) {
  div(style = "margin-bottom:5px;",
      checkboxInput(id, label, value = FALSE)
  )
}

# Nota de bloque
block_note <- function(...) div(class = "block-note", ...)

# Divisor
qdiv <- function() hr(class = "q-divider")

# Mensaje validacion
val_msg <- function(id) {
  div(id = id, class = "val-msg", style = "display:none;",
      "Por favor, responde a todas las preguntas antes de continuar.")
}

# Navegacion
nav_btns <- function(...) div(class = "nav-buttons", ...)
btn_p <- function(id, label = "Anterior") actionButton(id, label, class = "btn-sec")
btn_n <- function(id, label = "Siguiente") actionButton(id, label, class = "btn-pri")

# Encabezado de seccion
sec_hdr <- function(title, subtitle = NULL) {
  div(class = "sec-header",
      h3(title),
      if (!is.null(subtitle)) p(class = "sec-subtitle", subtitle)
  )
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "Cuestionario consumo de pornografia",
  
  tags$head(tags$style(HTML("
    * { box-sizing: border-box; }

    body {
      font-family: Arial, Helvetica, sans-serif;
      font-size: 14px;
      background: #f2f2f2;
      color: #222;
      margin: 0; padding: 0;
    }

    /* Cabecera */
    .app-hdr {
      background: #7c429e;
      color: #fff;
      padding: 18px 24px 14px;
      border-bottom: 3px solid #57306e;
    }
    .app-hdr h1 { font-size: 2rem; font-weight: bold; margin: 0 0 3px; }
    .app-hdr p  { font-size: 1rem; color: #b8cedf; margin: 0; }

    /* Barra progreso */
    .prog-wrap {
      background: #fff; border-bottom: 1px solid #ccc;
      padding: 9px 24px; display: flex; align-items: center; gap: 12px;
      position: sticky; top: 0; z-index: 100;
    }
    .prog-track { flex:1; height:5px; background:#ddd; border-radius:3px; overflow:hidden; }
    .prog-fill  { height:100%; background:#6c29c4; border-radius:3px; transition:width .3s ease; }
    .prog-lbl   { font-size:.9rem; color:#666; white-space:nowrap; }

    /* Contenedor */
    .main-wrap { max-width:720px; margin:0 auto; padding:22px 14px 60px; }

    /* Tarjeta */
    .sec-card {
      background:#fff; border:1px solid #ccc; border-radius:3px;
      padding:22px 24px 16px; margin-bottom:14px;
    }

    /* Encabezado seccion */
    .sec-header { margin-bottom:16px; border-bottom:1px solid #844ea3; padding-bottom:10px; }
    .sec-header h3 { font-size:1.5rem; font-weight:bold; margin:0 0 3px; color:#29063d; }
    .sec-subtitle  { font-size:1.2rem; color:#666; margin:0; line-height:1.45; }

    /* Pregunta */
    .q-block { margin-bottom:18px; }
    .q-label { display:block; font-weight:bold; font-size:1.2rem; color:#222;
                margin-bottom:5px; line-height:1.4; }

    /* Select */
    select.form-control {
      border:1px solid #aaa; border-radius:2px; padding:5px 8px;
      font-size:.87rem; background:#fafafa;
    }
    select.form-control:focus { border-color:#2e6da4; outline:none; }

    /* Radios */
    .radio label, .radio-inline label { font-size:1rem; color:#333; }

    /* Sliders */
    .irs--shiny .irs-bar        { background:#8e48cf; border-color:#8e48cf; }
    .irs--shiny .irs-handle     { background:#643491; border-color:#643491; }
    .irs--shiny .irs-from,
    .irs--shiny .irs-to,
    .irs--shiny .irs-single     { background:#925ac7; font-size:1rem; }
    .slider-ends { display:flex; justify-content:space-between;
                   font-size:1rem; color:#888; margin-top:-2px; }

    /* Checkboxes */
    .checkbox label { font-size:1.1rem; color:#333; }

    /* Divisor */
    .q-divider { border:none; border-top:1px solid #eee; margin:16px 0; }

    /* Nota de bloque */
    .block-note {
      font-size:1.1rem; color:#444; background:#f7f7f7;
      border-left:3px solid #612ea4; padding:8px 12px;
      margin-bottom:14px; line-height:1.5;
    }

    /* Validacion */
    .val-msg {
      font-size:1.1rem; font-weight:bold; color:#a00;
      background:#fff0f0; border:1px solid #e0b0b0;
      border-radius:2px; padding:6px 10px; margin-top:6px;
    }

    /* Botones de nav */
    .nav-buttons {
      display:flex; gap:8px; justify-content:flex-end;
      margin-top:12px; padding-top:12px; border-top:1px solid #eee;
    }
    .btn-pri {
      background:#612ea4; color:#fff; border:none; border-radius:2px;
      padding:7px 20px; font-size:1rem; font-weight:bold; cursor:pointer;
    }
    .btn-pri:hover { background:#5417a3; }
    .btn-sec {
      background:#fff; color:#333; border:1px solid #aaa;
      border-radius:2px; padding:7px 16px; font-size:1rem; cursor:pointer;
    }
    .btn-sec:hover { background:#5417a3; }

    /* Nota intro */
    .intro-note {
      background:#e0cdfa; border:1px solid #b0cce0; border-radius:2px;
      padding:10px 14px; font-size:1rem; color:#1d0042;
      line-height:1.55; margin-bottom:16px;
    }

    /* Resultado */
    .res-card {
      background:#fff; border:1px solid #ccc; border-radius:3px;
      padding:28px 24px; text-align:center;
    }
    .res-card h2 { font-size:1.3rem; color:#1a3a5c; margin-bottom:20px; }
    .gauge-wrap  { width:200px; height:110px; margin:0 auto 12px; }
    .prob-pct    { font-size:2.1rem; font-weight:bold; margin:0 0 4px; }
    .prob-desc   { font-size:1rem; color:#666; margin-bottom:12px; }
    .risk-pill   {
      display:inline-block; padding:3px 16px; border-radius:99px;
      font-size:.8rem; font-weight:bold; margin-bottom:20px;
    }
    .risk-low    { background:#d4edda; color:#155724; }
    .risk-med    { background:#fff3cd; color:#856404; }
    .risk-high   { background:#f8d7da; color:#721c24; }
    .res-note {
      background:#f7f7f7; border:1px solid #ddd; border-radius:2px;
      padding:12px 16px; font-size:1.2rem; color:#555;
      line-height:1.7; text-align:left; margin-bottom:18px;
    }
    .btn-restart {
      background:#612ea4; color:#fff; border:none; border-radius:2px;
      padding:8px 22px; font-size:1rem; font-weight:bold; cursor:pointer;
    }
    .btn-restart:hover { background:#5417a3; }

    @media(max-width:520px){
      .sec-card { padding:14px 12px; }
      .radio-inline { display:block; margin-bottom:3px; }
    }
    
        /* FAQ */
    
    .faq-wrap {
      margin-top: 22px;
      text-align: left;
    }
    
    .faq-item {
      border: 1px solid #ddd;
      border-radius: 4px;
      background: #fafafa;
      margin-bottom: 10px;
      overflow: hidden;
    }
    
    .faq-item summary {
      cursor: pointer;
      padding: 12px 14px;
      font-weight: bold;
      font-size: 1.3rem;
      color: #2a1045;
      list-style: none;
    }
    
    .faq-item summary:hover {
      background: #f0e8fa;
    }
    
    .faq-item[open] summary {
      border-bottom: 1px solid #ddd;
    }
    
    .faq-item p {
      padding: 12px 14px;
      margin: 0;
      line-height: 1.6;
      color: #444;
    }
  "))),
  
  # Cabecera
  div(class = "app-hdr",
      h1("Predicción del consumo problemático de pornografía"),
      p("Cuestionario sobre prácticas sexuales y percepciones de sexualidad")
  ),
  
  # Progreso
  div(class = "prog-wrap",
      div(class = "prog-track", div(class = "prog-fill", id = "prog-fill", style = "width:0%")),
      span(class = "prog-lbl", id = "prog-lbl", "Inicio")
  ),
  
  div(class = "main-wrap",
      
      # ── Pantalla inicio ──────────────────────────────────────────────────────────
      conditionalPanel("output.sec == 0",
                       div(class = "sec-card",
                           p("Buenas. Esta encuesta trata de predecir el consumo problemático de pornografía.
                           Las preguntas están orientadas a este fin y a jugar con la estadística.
          Si quieres un resultado fiable, responde con la mayor sinceridad posible leyendo detenidamente cada pregunta.
          El cuestionario consta de", strong("5 secciones"), "relacionadas con preguntas sociodemográficas, prácticas sexuales
          y de percepción de sexualidad, y tarda aproximadamente
          8 minutos en completarse."),
                           p("Al finalizar se calculará la probabilidad de presentar un patrón de
          consumo problemático de pornografía según un modelo estadístico predictivo (regresión logística binaria, para los entendidos y/o desquiciados).
          ", strong('Esta puntuación asume que ves pornografía'),"
                             (o, como poco, que has consumido alguna vez en los últimos años), de modo que una probabilidad
                             de 0 indica que tu consumo es estándar y una de 1 indica que es problemático. En cualquier caso,
                             ninguna pregunta requiere que seas consumidor(a) (se trata de predecir hábitos problemáticos sin conocerlos),
                             así que si no lo eres puedes tirar del «¿Y si lo
                             fuera?» si te da curiosidad."),
                           p("Si te estás preguntando",strong('«¿Qué es el consumo problemático?»'),', te contestaré que es el
                             resultado de una perfilado de elaboración propia sobre datos de hábitos de consumo. Puede resumirse rápidamente en
                             un consumo frecuente, habitualmente con gastos asociados y con consecuencias negativas en la autopercepción
                             (mala imagen corporal propia, sentimiento de culpa o vergüenza por el consumo), la vida sexual (pérdida de deseo de tener
                             relaciones con otras personas, atracción exclusiva a los cuerpos del porno...) e incluso la vida cotidiana (bajo
                             rendimiento académico/laboral como consecuencia del consumo, problemas con familiares o amigos por él...).'),
                           p('Finalmente, decirte que los datos sobre los que se han desarrollado el perfilado y el modelo predictivo, así como el cuestionario que vas a contestar
                           provienen del estudio hecho
                             por Gómez Miguel, A, Kuric Kardelis, S. y Sanmartí Ortí, A.,',a(tags$em("Juventud y pornografía en la era digital: consumo, percepción y efectos"),
                                                            href = "https://www.centroreinasofia.org/publicacion/juventud-y-pornografia-en-la-era-digital-consumo-percepcion-y-efectos/",
                                                            target = "_blank"),'. Te animo a leerlo si tienes curiosidad sobre la situación de la relación de los jóvenes españoles con la llamada «nueva pornografía».'),
                           div(class = "intro-note",
                               strong("Aviso: "),
                               "Por motivos obvios, algunas preguntas giran en torno a la sexualidad, la
          percepcion sobre el sexo y otros temas sensibles. Si no te interesa
          contestar o te resulta incómodo, arriba a la derecha está tu botón de salida. Tus datos no se guardan, por si eso te anima
                               a quedarte"
                           ),
                           nav_btns(btn_n("btn_start", "Comenzar"))
                       )
      ),
      
      # ── Seccion 1: Sociodemografica ───────────────────────────────────────────────
      conditionalPanel("output.sec == 1",
                       div(class = "sec-card",
                           sec_hdr("Sección 1. Preguntas sociodemográficas"),
                           
                           sel_q("SEXO", 1, "Eres...",
                                 c("Mujer", "Hombre", "Otro"), width = "360px"),
                           
                           num_q("EDAD_NUM", 2, "Edad exacta en años",
                                 min = 14, max = 35, width = "360px"),
                           
                           div(class = "q-block",
                               tags$label(class = "q-label",
                                          "Independientemente de si continúas estudiando o no, ¿cuál es el nivel
            de estudios mas alto que has finalizado?"),
                               selectInput("ESTUDIOS", NULL, width = "360px",
                                           choices = c(
                                             "— Selecciona una opción —" = "",
                                             "Primaria o menos que primaria"                        = "primaria",
                                             "Secundaria obligatoria 1a etapa (1o, 2o, 3o ESO)"    = "eso1",
                                             "Secundaria obligatoria 2a etapa (4o ESO, FP basica)" = "eso2",
                                             "Secundaria postobligatoria (Bachillerato)"           = "bach",
                                             "FP grado medio"                                      = "fpgm",
                                             "FP grado superior"                                   = "fpgs",
                                             "Grado universitario"                                         = "universidad",
                                             "Postgrado, Máster, Doctorado"                        = "postgrado",
                                             "Otro titulo oficial"                                 = "otro"
                                           ))
                           ),
                           
                           sel_q("ORIENTACION", 4, "¿Cuál es tu orientacion sexual?",
                                 c("Heterosexual", "Homosexual", "Bisexual",
                                   "Otro"), width = "360px"),
                           
                           sel_q("CONVIVENCIA", 5, "¿Con quién vives actualmente?",
                                 c("Vivo solo/a"                       = "Vivo solo",
                                   "Vivo con mi padre y/o madre"       = "Vivo con mi padre y/o madre",
                                   "Vivo con mi pareja (sin hijo/a/s)" = "Vivo con mi pareja (sin hijo/a/s)",
                                   "Vivo con mi pareja e hijo/a/s"     = "Vivo con mi pareja e hijo/a/s",
                                   "Otra forma de convivencia (otro/s familiar/es,\n
                                   amigo/a/s, otra/s persona/s, etc.)"         = "Otra forma de convivencia"),
                                 width = "360px"),
                           
                           sel_q("PAREJA", 6, "¿Tienes pareja o una relación estable en la actualidad?",
                                 c("Si, tengo pareja en la actualidad"                      = "Si, tengo pareja en la actualidad",
                                   "He tenido pareja en el pasado, pero no actualmente"     = "He tenido pareja en el pasado, pero no actualmente",
                                   "Nunca he tenido pareja estable"                         = "Nunca he tenido pareja estable"),
                                 width = "360px"),
                           
                           sel_q("REL_SEXUALES", 7,
                                 "¿Has mantenido alguna vez relaciones sexuales (entendiendo por ellas, cualquier actividad sexual con otra persona)?",
                                 c("Sí" = "Si", "No" = "No"), width = "360px"),
                           
                           div(class = "q-block",
                               tags$label(class = "q-label",
                                          "Muchas personas se definen por su clase social, es decir, por su profesión, sus
ingresos económicos, por los estudios que tienen, etc. En tu caso, y si piensas en tu
profesión y estudios o en los de tus padres (si estás dependiendo de ellos), ¿en qué
clase social te incluirías?"),
                               selectInput("CLASESOCIAL", NULL, width = "360px",
                                           choices = c(
                                             "— Selecciona una opcion —" = "",
                                             "Alta"       = "alta",
                                             "Media-alta" = "media_alta",
                                             "Media"      = "media",
                                             "Media-baja" = "media_baja",
                                             "Baja"       = "baja"
                                           ))
                           ),
                           
                           div(class = "q-block",
                               tags$label(class = "q-label",
                                          "Independientemente de que profeses o no una religión, ¿podrías decir cuál es tu grado
de religiosidad?"),
                               sliderInput("RELIGIOSIDAD_NUM", NULL, min = 0, max = 10, value = 5,
                                           step = 1, ticks = TRUE, width = "100%"),
                               div(class = "slider-ends",
                                   span("0 — Nada religioso/a"), span("10 — Totalmente religioso/a"))
                           ),
                           
                           div(class = "q-block",
                               tags$label(class = "q-label",
                                          "Habitualmente se habla de la izquierda y la derecha política. En una escala de 0 a 10,
siendo el 0 la «extrema izquierda» y el 10 la «extrema derecha», ¿dónde te situarías?"),
                               sliderInput("IDEOLOGIA_NUM", NULL, min = 0, max = 10, value = 5,
                                           step = 1, ticks = TRUE, width = "100%"),
                               div(class = "slider-ends",
                                   span("0 — Extrema izquierda"), span("10 — Extrema derecha"))
                           ),
                           
                           div(class = "q-block",
                               tags$label(class = "q-label",
                                          "¿Tienes alguna enfermedad, condición crónica o problema de salud que
            te genere limitaciones para realizar actividades diarias básicas
            (comer, lavarse, desplazarse, leer u oir, comunicarse, etc.)?"),
                               selectInput("LIMITA", NULL, width = "420px",
                                           choices = c(
                                             "— Selecciona una opcion —"                    = "",
                                             "No tengo ninguna enfermedad o condición crónica" = "Sin limitaciones",
                                             "Sí, muy leve"  = "muyleve",
                                             "Sí, leve"      = "leve",
                                             "Sí, grave"     = "grave",
                                             "Sí, muy grave" = "muygrave"
                                           ))
                           ),
                           
                           qdiv(),
                           
                           block_note(
                             strong(
                               "Teniendo en cuenta los ingresos de tu unidad familiar, indica si en el
          último año has podido realizar cada una de las siguientes cosas.
          Marca las que SÍ has podido permitirte:")
                           ),
                           
                           chk("car_vacaciones",  "Ir de vacaciones al menos una semana al año"),
                           chk("car_temperatura", "Mantener la casa a una temperatura adecuada"),
                           chk("car_imprevistos", "Afrontar gastos imprevistos en un mes"),
                           chk("car_recibos",     "Afrontar sin retrasos el pago de recibos, prestamos, hipotecas, alquiler, etc."),
                           chk("car_ahorro",      "Ahorrar parte de los ingresos mensuales"),
                           chk("car_capricho",    "Darte algun capricho al menos una vez al mes (ir de compras, renovar tecnologia, etc.)"),
                           chk("car_ordenador",   "Disponer de ordenador (de cualquier tipo) en el hogar"),
                           chk("car_ocio",        "Participar regularmente en actividades de ocio (cenar fuera, cine, conciertos, salir de copas, etc.)"),
                           
                           
                           
                           val_msg("val1"),
                           nav_btns(btn_p("btn_prev1"), btn_n("btn_next1"))
                       )
      ),
      
      # ── Seccion 2: Inicios ───────────────────────────────────────────────────────
      conditionalPanel("output.sec == 2",
                       div(class = "sec-card",
                           sec_hdr("Sección 2. Inicios en la pornografia",
                                   "Por pornografía se entiende cualquier material (vídeo, audio, foto…) que tenga como
objetivo principal provocar excitación sexual en quien lo recibe, abarcando un amplio espectro de
contenidos: desde aquellos que presentan situaciones eróticas y de desnudez —pero sin mostrar actos
sexuales explícitos—, hasta aquellos que muestran actos sexuales de manera explícita e, incluso,
violenta.\n

Para realizar esta encuesta ten en cuenta esta definición, considerando como pornografía cualquier
material que presente contenido erótico o sexual, independientemente de cómo de explícito, violento o
detallado sea, o si su consumo se ha realizado de manera intencionada o de forma imprevista"),
                           
                           num_q("EDAD_PORN", 14,
                                 "¿A qué edad viste pornografía o algún contenido pornográfico por primera vez?",
                                 min = 5, max = 35, width = "360px"),
                           
                           sel_q("FACIL_PORN", 16,
                                 "¿Te resultó fácil acceder a contenidos pornográficos las primeras veces que lo hiciste?",
                                 c("Sí" = "Si", "No" = "No"), width = "360px"),
                           
                           val_msg("val2"),
                           nav_btns(btn_p("btn_prev2"), btn_n("btn_next2"))
                       )
      ),
      
      # ── Seccion 3: Comportamientos online ────────────────────────────────────────
      conditionalPanel("output.sec == 3",
                       div(class = "sec-card",
                           sec_hdr("Sección 3. Prácticas sexuales y comportamientos online"),
                           
                           block_note(
                             "Por favor, indica la frecuencia con la que te ocurren las siguientes
          situaciones."
                           ),
                           
                           likert_req("PRESION_PORN_MIO",
                                      "Siento presión para enviar imágenes o vídeos eróticos míos"),
                           likert_req("SEXTING_DOY",
                                      "Comparto imágenes o vídeos eróticos míos con mis parejas
sexuales o ligues"),
                           likert_req("SEXTING_PIDO",
                                      "Pido imágenes o vídeos eróticos a mis parejas sexuales o
ligues"),
                           likert_req("REDES_COMENT",
                                      "Subo contenido de carácter erótico o sexual a mis redes
sociales o plataformas online para conseguir seguidores o
comentarios positivos"),
                           likert_req("REDES_DINERO",
                                      "Subo contenido de carácter erótico o sexual a mis redes
sociales o plataformas online para conseguir beneficios
económicos"),
                           likert_req("DIFUSION_NO_CONSENTIDA",
                                      "Comparto imágenes o vídeos eróticos de personas cercanas
o conocidas sin su consentimiento"),
                           
                           val_msg("val3"),
                           nav_btns(btn_p("btn_prev3"), btn_n("btn_next3"))
                       )
      ),
      
      # ── Seccion 4: Pornografia y educacion sexual ────────────────────────────────
      conditionalPanel("output.sec == 4",
                       div(class = "sec-card",
                           sec_hdr("Sección 4. Pornografía y educación sexual"),
                           
                           block_note(
                             "Con relación a la educación afectivo-sexual que has recibido, ¿podrías valorar tu
grado de acuerdo con las siguientes afirmaciones? Utilizar una escala de 0 a 3, siendo 0
«totalmente en desacuerdo», 1 «en desacuerdo», 2 «de acuerdo» y 3 «totalmente de acuerdo»"
                           ),
                           
                           slider03("EDUC_SEX_CLASE",
                                    "He recibido educación afectivo-sexual de calidad en mi centro educativo"),
                           slider03("EDUC_SEX_FAM",
                                    "Mi familia me ha proporcionado educación afectivo-sexual de calidad"),
                           slider03("EDUC_SEX_PROP",
                                    "He buscado información afectivo-sexual por mi cuenta"),
                           
                           qdiv(),
                           
                           block_note(
                             "Tengas pareja o no, valora tu grado de acuerdo con las siguientes
          afirmaciones sobre relaciones de pareja.
          (0 = nada de acuerdo, 10 = totalmente de acuerdo)"
                           ),
                           
                           slider010("PAREJA_COMO_ACT",
                                     "Me gustaría que mi pareja actúe sexualmente como una actriz o actor porno"),
                           slider010("CONSUMO_POR_PAREJA",
                                     "Veo (o vería) porno para saber lo que le gusta a mi pareja en el sexo"),
                           slider010("CONSUMO_ES_INFIDELIDAD",
                                     "Ver porno es una forma de infidelidad "),
                           
                           nav_btns(btn_p("btn_prev4"), btn_n("btn_next4"))
                       )
      ),
      
      # ── Seccion 5: Percepciones sobre la sexualidad ───────────────────────────────
      conditionalPanel("output.sec == 5",
                       div(class = "sec-card",
                           sec_hdr("Sección 5. Percepciones sobre la sexualidad"),
                           
                           block_note(
                             "Valora tu grado de acuerdo con las siguientes afirmaciones sobre la
          sexualidad. (0 = nada de acuerdo, 10 = totalmente de acuerdo)"
                           ),
                           
                           slider010("COMUNICACION_POCO_IMP",
                                     "En el sexo no se habla, se actúa (la comunicación no es tan importante)"),
                           slider010("PENETRACION_MEJOR",
                                     "La penetración vaginal es siempre la práctica sexual que da más placer"),
                           slider010("ORGASMO_MAS_IMP",
                                     "Lo más importante del sexo es el orgasmo"),
                           slider010("DIFICIL_MUJ_ORG",
                                     "A las mujeres les cuesta más llegar al orgasmo "),
                           slider010("TAMANO_PENE_IMP",
                                     "El tamaño del pene es importante para tener relaciones sexuales satisfactorias"),
                           slider010("INTENSO_MEJOR",
                                     "El sexo duro excita más y da más placer"),
                           slider010("SEXO_ES_PENETRACION",
                                     "Solo es sexo si hay penetración "),
                           slider010("HOM_SABEN_MAS",
                                     "Los chicos saben más de sexo porque ven más porno"),
                           slider010("FORZAR_SEXO",
                                     "A veces las chicas no están seguras de querer tener relaciones, pero cuando lo hacen les gusta; un poco de presión ayuda"),
                           
                           nav_btns(btn_p("btn_prev5"), btn_n("btn_next5"))
                       )
      ),
      
      # ── Resultado ─────────────────────────────────────────────────────────────────
      conditionalPanel("output.sec == 6",
                       div(class = "res-card",
                           h2("Resultado del cuestionario"),
                           uiOutput("result_ui"),
                           
                           actionButton("btn_restart", "Volver a empezar", class = "btn-restart"),
                           tags$h2(
                             strong("Si te interesa saber cómo funciona la aplicación\n
                                    tienes estas secciones donde intento explicarlo de la manera menos técnica posible")),
                           
                           div(class = "faq-wrap",
                                   
                               tags$details(
                                 class = "faq-item",
                                 
                                 tags$summary("¿Cómo se ha calculado esta probabilidad?"),
                                 
                                 tags$p(
                                   "Como se mencionaba al principio, para dar la probabilidad se utiliza un modelo estadístico
    predictivo llamado ",
                                   strong("regresión logística binaria"),
                                   ". Imagina que estás preocupado porque crees que te va a dar un infarto (Dios no lo quiera) y vas
                                   al médico a quedarte tranquilo.
    Te miden el colesterol, la presión arterial y el signo del zodiaco. ¿Cómo lo hacen? Pues meten los datos a un modelo como el siguiente,
                                   que les devuelve mágicamente una probabilidad"
                                 ),
                                 
                                 withMathJax(
                                   helpText("$$
    \\text{Probabilidad infarto} =
    \\beta_{col} \\times \\text{Colesterol} +
    \\beta_{pres} \\times \\text{Presión arterial} +
    \\beta_{zod} \\times \\text{Signo zodiaco}
    $$")),
                                            
                                   tags$p("donde \\(\\beta_{col}\\), \\(\\beta_{pres}\\) y \\(\\beta_{zod}\\) son los «pesos» o la «fuerza» de cada
                                            variable para determinar la probabilidad. Faltaria hacer el estudio, pero me aventuro a decir
                                            que el colesterol tiene más fuerza para predecir infartos que el signo del zodiaco, así que su
                                            \\(\\beta\\) debería ser más grande (incluso es posible que la \\(\\beta\\) del signo del zodiaco sea 0
                                            , lo que significa que no aporta nada para predecir la probabilidad). Y, como es natural, si un peso es negativo, significa que reduce
                                            la probabilidad."),
                                 
                                 tags$p(
                                   "Esto es en realidad una simplificación, principalmente porque si metes valores a la fórmula anterior
                                   te puede dar un valor que no esté entre 0 y 1, que es lo básico de lo básico para cualquier probabilidad. Lo único
                                   que cambia es que en lugar de la probabilidad, se predice el", tags$em("✨logaritmo natural de la fracción de la probabilidad
                                                                                                             entre 1 menos la probabilidad✨"),"
                                   , pero si no te gustan las mates no te asustes, porque por lo demás es lo mismo, y la probabilidad también puede sacarse de predecir esa cosa fea."
                                   
                                 ),
                                 tags$p(
                                   "En resumen, el modelo de esta aplicación es igual al de predecir los infartos; algunas respuestas que has dado te han aumentado o bajado poco o
                                   mucho tu probabilidad, y otras no han tenido ningún efecto (por ejemplo la edad de primer acceso a la pornografía no influye, así que si te has quedado
                                   un rato haciendo flashback para dar una respuesta, no ha servido de mucho, sorry). ¿Cuáles son las variables que más influyen? Pues la que infuye una barbaridad
                                   es la de subir contenidos eróticos a redes para recibir comentarios positivos, si vuelves y pruebas a poner otra cosa el cambio puede ser algo drástico. Más no te cuento,
                                   que se arruina la magia."
                                 )
                               ),
                               
                               tags$details(
                                 class = "faq-item",
                                 tags$summary("¿Qué representa la probabilidad?"),
                                 tags$p("
        Como se decía al principio, la probabilidad es resultado de un modelo predictivo. Estos modelos no caen de los árboles, se entrenan y se generan en base al comportamiento registrado de varios individuos. Por ejemplo, el modelo
        ve que en el grupo de consumo problemático de pornografía crece el número de hombres (por lo que sea) y dice «ah, los hombres tienen más probabilidad de tener consumo problemático». Esto implica
        que la probabilidad resultante asume que podrías ser parte de la muestra con la que se entrenó al modelo. Esto es, un joven español de 16 a 29 años que ha consumido pornografía alguna vez en los últimos años."
                                 ),
                                        tags$p("La
        probabilidad estimada no es una certeza, es el modelo diciendo «con lo que he visto en la muestra original, este individuo con estás características tendría una probabilidad de tanto». Pero no deja de ser una estimación, un
        estadístico diciéndote «seh, esto puede ser así o no ser así, yo q sé».
      ")
                               ),
                               
                               tags$details(
                                 class = "faq-item",
                                 tags$summary("¿Qué probabilidad es clasificable como problemática? ¿Qué es una probabilidad baja, media o alta?"),
                                 tags$p("
        Hablemos de pruebas diagnósticas. ¿Te acuerdas del COVID-19 y los tests de antígenos que te decían si lo tenías o no? Eso es una prueba diagnóstica. De ella esperas lo más básico, que si tienes
        el virus te dé un positivo y si no lo tienes te dé un negativo. Alerta de terminología: la probabilidad de que la prueba de positivo teniendo el virus se llama ",strong('sensibilidad de la prueba'), " mientras que la
        probabilidad de dar negativo cuando no estas contagiado se llama", strong('especificidad'), ". Idealmente, ambos valores valdrían 100%, pero muchas veces no es así, así que se busca maximizar
                                        ambas probabilidades (o solo una de ellas, si es más importante confirmar casos o descartar enfermedades)."
      ),
      tags$p("
        Este modelo podría considerarse una prueba diagnóstica. Si somos estrictos, el modelo solo puede predecir si tu consumo es problemático o no. «¿Entonces si mi probabilidad está por debajo del 50%
        mi consumo es normal?» Estadísticamente hablando, no exactamente. Si un doctor le dijese a una persona que cree que va a tener un infarto: «No se preocupe usted, la prueba ha dado negativo, solo tiene una probabilidad del 49%»,
        no creo que esa persona se quedase muy tranquila. El umbral del 50% muchas veces se mueve, habitualmente porque es necesario dar una señal de alarma aunque haya una baja probabilidad, o porque los casos positivos
        son muy poco frecuentes y hay que «ayudar» al modelo a que los detecte. Nuestro caso es el segundo; el consumo problemático en nuestra muestra es mucho menos frecuente que el estándar (17% frente a 83%), por lo que hemos bajado
        el umbral. Esto hace que detectemos más casos positivos, aunque con el coste de tener más falsos positivos ¿A cuánto hemos bajado el umbral?
        Al 16.46%. Sí, es un valor muy concreto, pero ¿te acuerdas de la sensibilidad y la especificidad? Pues tras varias pruebas, este es el valor que, en media, tiende a maximizar y equilibrar ambos valores. Que por cierto,
        los valores medianos de ambas probabilidades son del 82.14% y del 84.82%, respectivamente.
                               Para que te fies del resultado.
      "),
      
      tags$p("
        ¿Qué es entonces probabilidad baja, alta y media? Pues una decisión propia porque quedaba bonito que tuviese un comportamiento de semáforo. Se considera probabilidad media si el valor es superior al 16.46% y alta si empieza a ser más probable que
        pertenezcas al consumo problemático que al no problemático, o sea, a partir del 50%. Pero no significan mucho, es un poco decorativo.
      ")
                               ),
                               
                               tags$details(
                                 class = "faq-item",
                                 tags$summary("Quiero saber más sobre el consumo problemático"),
                                 tags$p("
        Como ya he mencionado, la etiqueta «consumo problemático» es basada en un perfilado propio. Las personas del estudio original contestaron a una encuesta completa
        que incluye las preguntas que te he hecho a ti, además de otras sobre su consumo de pornografía. En base a ellas, se hizo una segmentación y se descubrió que había
        un grupo con tendencias globales «sanas» y otro que presentaba problemas por su relación con la pornografía. De modo que no se está prediciendo una realidad objetiva
        como la ocurrencia futura o no de un infarto, si tienes o no mascotas o si eres o no homosexual, sino que se esta prediciendo si podrías compartir tendencias con estas
        personas que yo he clasificado como problemáticas. En cualquier caso, hay muchos estudios acerca de los efectos de la pornografía digital. Algunos son",
                                        a(tags$em("Nueva pornografía y cambios en las relaciones interpersonales"),
                                          href = "https://cdn.20m.es/adj/2019/06/10/4007.pdf",
                                          target = "_blank")," de Ballester y Orte, ",
                                        a(tags$em("Impact of pornography consumption on children and adolescents: a trauma-informed approach"),
                                          href = "https://www.frontiersin.org/journals/child-and-adolescent-psychiatry/articles/10.3389/frcha.2025.1567649/full",
                                          target = "_blank"),", de Alvarez-Segura y otros y, de nuevo, el propio estudio de donde se sacan los datos",
                                        a(tags$em("Juventud y pornografía en la era digital: consumo, percepción y efectos"),
                                          href = "https://www.centroreinasofia.org/publicacion/juventud-y-pornografia-en-la-era-digital-consumo-percepcion-y-efectos/",
                                          target = "_blank"),".
      "),
                                 
                                 tags$p("
        Aun así, si quieres saber más sobre las tendencias medias de los grupos de este análisis, te dejo unos cuantos gráficos con las proporciones de respuesta según
        el grupo de consumo. El primero es las respuestas a la pregunta «En los últimos doce meses, ¿cuántas veces has visto pornografía?»
      "),
                                 tags$img(src = "perfil_frecconsumo.png",
                                          width = "60%",
                                          style = "display:block; margin:auto; border-radius:10px;"),
                                 
                                 tags$p("
        Estos siguientes son sobre frecuencia de tipos de contenidos vistos. De izquierda a derecha, de arriba a abajo:"),
                                 
                                 tags$ul(
                                   
                                   tags$li("Contenidos eróticos donde no se ven desnudos integrales ni actos sexuales explícitos"),
                                   
                                   tags$li("Contenido donde se ven desnudos integrales, pero sin actos sexuales explícitos"),
                                   
                                   tags$li("Contenidos que muestran desnudos integrales y actos sexuales explícitos"),
                                   
                                   tags$li("Contenidos que muestran desnudos integrales, actos sexuales explícitos y violencia física y/o verbal"),
                                   
                                   tags$li("Contenidos pornográficos que muestran una alta violencia física y/o verbal y/o humillaciones")
                                   
                                 )
                                 ,
                                 tags$img(src = "mosaiccontenidonv.png",
                                          width = "100%",
                                          style = "display:block; margin:auto; border-radius:10px;"),
                                 tags$img(src = "mosaictipocontenidosv.png",
                                          width = "85%",
                                          style = "display:block; margin:auto; border-radius:10px;"),
                                 tags$p("
        El siguiente es sobre frecuencia de gasto en contenidos pornográficos
      "),
                                 tags$img(src = "mosaicgasto.png",
                                          width = "60%",
                                          style = "display:block; margin:auto; border-radius:10px;"),
                                 
                                 tags$p("
        Los siguientes responden a la pregunta «¿En qué medida consideras que ver pornografía te ha influido en las siguientes
cuestiones?». De izquierda a derecha, de arriba a abajo:"),
                                 
                                 tags$ul(
                                   
                                   tags$li("Hace que pierda el interés en tener relaciones sexuales con otras personas"),
                                   
                                   tags$li("Hace que me sienta culpable o avergonzado/a"),
                                   
                                   tags$li("Hace que tenga una imagen corporal negativa de mí mismo/a"),
                                   
                                   tags$li("Hace que sea más exigente con las prácticas sexuales que espero"),
                                   
                                   tags$li("Hace que solo me atraigan personas con un físico parecido al de actores/actrices porno")
                                   
                                 ),
                                 
                                 tags$img(src = "mosaicinfluencias1.png",
                                          width = "100%",
                                          style = "display:block; margin:auto; border-radius:10px;"),
                                 tags$img(src = "mosaicimplicaciones2.png",
                                          width = "85%",
                                          style = "display:block; margin:auto; border-radius:10px;"),
                                 
                                 tags$p("
        Y, finalmente, tenemos las respuestas asociadas a un consumo de riesgo. Se responde a la pregunta «Por favor, ¿puedes indicar la frecuencia con la que te ocurren las siguientes
cuestiones?» De izquierda a derecha, de arriba a abajo:"),
                                 
                                 tags$ul(
                                   
                                   tags$li("Siento que veo demasiada pornografía "),
                                   
                                   tags$li("He intentado reducir o controlar el consumo de pornografía sin éxito"),
                                   
                                   tags$li("He dejado de hacer cosas que me interesan por ver pornografía (deporte, aficiones, etc.) "),
                                   
                                   tags$li("Ver pornografía afecta negativamente a ámbitos importantes de mi vida como el rendimiento educativo/laboral, dormir, etc."),
                                   
                                   tags$li("Tengo problemas con mi familia o amistades por ver pornografía"),
                                   
                                   tags$li('Me siento mal (con ansiedad, irritable, etc.,) cuando no
puedo ver pornografía')
                                   
                                 ),
                                 
                                 tags$img(src = "mosaicconsumoprobl1.png",
                                          width = "100%",
                                          style = "border-radius:10px;"),
                                 tags$img(src = "mosaicconsumoprobl2.png",
                                          width = "100%",
                                          style = "border-radius:10px;")
                               
                               )
      
                           )
                       )
      )
  ),
  
  tags$script(HTML("
    Shiny.addCustomMessageHandler('set_progress', function(d) {
      document.getElementById('prog-fill').style.width = d.pct + '%';
      document.getElementById('prog-lbl').textContent  = d.label;
    });
    Shiny.addCustomMessageHandler('show_val', function(d) {
      var el = document.getElementById(d.id);
      if (el) el.style.display = d.show ? 'block' : 'none';
    });
    Shiny.addCustomMessageHandler('scroll_top', function(d) {
      window.scrollTo({ top: 0, behavior: 'smooth' });
    });
  "))
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  sec <- reactiveVal(0)
  
  prog_labels <- list(
    list(pct = 0,   label = "Inicio"),
    list(pct = 14,  label = "Seccion 1 de 5 — Datos sociodemograficos"),
    list(pct = 28,  label = "Seccion 2 de 5 — Inicios en la pornografia"),
    list(pct = 43,  label = "Seccion 3 de 5 — Comportamientos online"),
    list(pct = 57,  label = "Seccion 4 de 5 — Educacion sexual"),
    list(pct = 71,  label = "Seccion 5 de 5 — Percepciones sobre sexualidad"),
    list(pct = 100, label = "Completado")
  )
  
  observe({
    info <- prog_labels[[sec() + 1]]
    session$sendCustomMessage("set_progress", info)
    session$sendCustomMessage("scroll_top", list())
  })
  
  output$sec <- reactive(sec())
  outputOptions(output, "sec", suspendWhenHidden = FALSE)
  
  # Carencias: contar items NO marcados (no se ha podido permitir)
  n_car <- reactive({
    items <- c(input$car_vacaciones, input$car_temperatura, input$car_imprevistos,
               input$car_recibos,    input$car_ahorro,      input$car_capricho,
               input$car_ordenador,  input$car_ocio)
    sum(!unlist(lapply(items, isTRUE)))
  })
  
  carencia_cat <- reactive({
    n <- n_car()
    if (n <= 1)      "Ninguna carencia"
    else if (n <= 3) "Leve"
    else             "Severa"
  })
  
  output$carencia_ui <- renderUI({
    div(style = "margin-top:6px; font-size:1rem; color:#666;",
        sprintf("Situaciones no cubiertas: %d — Clasificacion carencia material: %s",
                n_car(), carencia_cat())
    )
  })
  
  # Helpers de validacion
  show_val <- function(id, show) {
    session$sendCustomMessage("show_val", list(id = id, show = show))
  }
  
  req_sel <- function(id) !is.null(input[[id]]) && input[[id]] != ""
  req_num <- function(id) !is.null(input[[id]]) && !is.na(input[[id]])
  req_rad <- function(id) {
    v <- input[[id]]
    !is.null(v) && length(v) > 0 && v != ""
  }
  
  val1 <- function() {
    ok <- all(c(
      req_sel("SEXO"), req_num("EDAD_NUM"), req_sel("ESTUDIOS"),
      req_sel("ORIENTACION"), req_sel("CONVIVENCIA"), req_sel("PAREJA"),
      req_sel("REL_SEXUALES"), req_sel("CLASESOCIAL"), req_sel("LIMITA")
    ))
    show_val("val1", !ok)
    ok
  }
  
  val2 <- function() {
    ok <- all(c(req_num("EDAD_PORN"), req_sel("FACIL_PORN")))
    show_val("val2", !ok)
    ok
  }
  
  val3 <- function() {
    ids <- c("PRESION_PORN_MIO","SEXTING_DOY","SEXTING_PIDO",
             "REDES_COMENT","REDES_DINERO","DIFUSION_NO_CONSENTIDA")
    ok  <- all(sapply(ids, req_rad))
    show_val("val3", !ok)
    ok
  }
  
  # Navegacion
  observeEvent(input$btn_start, sec(1))
  observeEvent(input$btn_next1, { if (val1()) sec(2) })
  observeEvent(input$btn_prev1, sec(0))
  observeEvent(input$btn_prev2, sec(1))
  observeEvent(input$btn_next2, { if (val2()) sec(3) })
  observeEvent(input$btn_prev3, sec(2))
  observeEvent(input$btn_next3, { if (val3()) sec(4) })
  observeEvent(input$btn_prev4, sec(3))
  observeEvent(input$btn_next4, sec(5))
  observeEvent(input$btn_prev5, sec(4))
  observeEvent(input$btn_next5, sec(6))
  observeEvent(input$btn_restart, sec(0))
  
  # Resultado
  output$result_ui <- renderUI({
    req(sec() == 6)
    
    vals <- list(
      ORIENTACION            = input$ORIENTACION,
      CONVIVENCIA            = input$CONVIVENCIA,
      PAREJA                 = input$PAREJA,
      REL_SEXUALES           = input$REL_SEXUALES,
      SEXO                   = input$SEXO,
      ESTUDIOS = case_when(
        input$ESTUDIOS %in% c("primaria", "eso1", "eso2") ~
          "Hasta secundarios obligatorios",
        
        input$ESTUDIOS %in% c("bach", "fpgm", "otro") ~
          "Secundarios post-obligatorios",
        
        input$ESTUDIOS %in% c("fpgs", "universidad", "postgrado") ~
          "Superiores",
        
        TRUE ~ NA_character_
      ),
      CLASESOCIAL = case_when(
        input$CLASESOCIAL %in% c('media_alta', 'alta') ~ 'Media Alta y Alta',
        
        input$CLASESOCIAL %in% c('media_baja', 'baja') ~ 'Media Baja y Baja',
        
        input$CLASESOCIAL == 'media' ~ 'Media',
        
        TRUE ~ NA_character_
      ),
      CARENCIAM              = carencia_cat(),
      LIMITA  = case_when(
        input$LIMITA %in% c('muyleve', 'leve','grave','muygrave') ~ 'Con alguna limitacion',
        
        input$LIMITA == 'Sin limitaciones' ~ 'Sin limitaciones',
        
        TRUE ~ NA_character_
      ),
      PRESION_PORN_MIO       = as.numeric(input$PRESION_PORN_MIO),
      SEXTING_PIDO           = as.numeric(input$SEXTING_PIDO),
      REDES_COMENT           = as.numeric(input$REDES_COMENT),
      REDES_DINERO           = as.numeric(input$REDES_DINERO),
      DIFUSION_NO_CONSENTIDA = as.numeric(input$DIFUSION_NO_CONSENTIDA),
      EDUC.SEX_CLASE       = input$EDUC_SEX_CLASE,
      EDUC.SEX_PROP        = input$EDUC_SEX_PROP,
      CONSUMO_ES_INFIDELIDAD = input$CONSUMO_ES_INFIDELIDAD,
      COMUNICACION_POCO_IMP  = input$COMUNICACION_POCO_IMP,
      ORGASMO_MAS_IMP        = input$ORGASMO_MAS_IMP,
      TAMANO_PENE_IMP        = input$TAMANO_PENE_IMP,
      INTENSO_MEJOR          = input$INTENSO_MEJOR,
      SEXO_ES_PENETRACION    = input$SEXO_ES_PENETRACION,
      HOM_SABEN_MAS          = input$HOM_SABEN_MAS,
      FORZAR_SEXO            = input$FORZAR_SEXO
    )
    
    prob <- predict_prob(vals)
    pct  <- round(prob * 100, 4)
    
    rc  <- if (prob < 0.1646) "risk-low" else if (prob < 0.5) "risk-med" else "risk-high"
    rt  <- if (prob < 0.1646) "Probabilidad baja (consumo estándar)"
    else if (prob < 0.5) "Probabilidad moderada (consumo problemático)"
    else "Probabilidad elevada (consumo problemático)"
    gc  <- if (prob < 0.1646) "#27ae60" else if (prob < 0.5) "#e67e22" else "#c0392b"
    
    angle <- prob * 180
    rad   <- angle * pi / 180
    xe    <- 100 + 80 * cos(pi - rad)
    ye    <- 100 - 80 * sin(rad)
    la    <- if (angle > 180) 1 else 0
    
    svg_g <- tags$svg(
      viewBox = "0 0 200 110", width = "200", height = "110",
      tags$path(d = "M 20 100 A 80 80 0 0 1 180 100",
                fill = "none", stroke = "#ddd", `stroke-width` = "14",
                `stroke-linecap` = "round"),
      tags$path(d = sprintf("M 20 100 A 80 80 0 %d 1 %.2f %.2f", la, xe, ye),
                fill = "none", stroke = gc, `stroke-width` = "14",
                `stroke-linecap` = "round")
    )
    
    tagList(
      div(class = "gauge-wrap", svg_g),
      div(class = "prob-pct",  style = paste0("color:", gc), paste0(pct, "%")),
      div(class = "prob-desc", "Probabilidad estimada de consumo problemático"),
      div(class = paste("risk-pill", rc), rt)
    )
  })
}



shinyApp(ui, server)


