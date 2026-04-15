library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(htmltools)
library(xml2)
library(stringi)
library(curl)

# --- GEODATOS ---
if (!file.exists("data/geodatos.RData")) source("preparar_geodatos.R")
load("data/geodatos.RData")

# --- UTILIDADES ---
norm_txt <- function(x) tolower(stri_trans_general(x, "Latin-ASCII"))

# --- CLASIFICADOR DE PRIORIDAD ---
kw_alta <- c("muerto","muere","murio","fallecio","fallecido","homicidio","accidente",
             "incendio","heridos","herido","explosion","crisis","emergencia","tiroteo",
             "asesinato","fatal","tragedia","rescate","desaparecido","choque","ataque",
             "evacuacion","derrumbe","siniestro","victima","victimas","femicidio")
kw_media <- c("robo","policia","protesta","marcha","paro","corte","denuncia","investigacion",
              "operativo","violencia","conflicto","detenido","incidente","allanamiento",
              "arrestado","hurto","amenaza","fraude","rapina","procesado")

detectar_prioridad <- function(title, desc = "") {
  tryCatch({
    if (is.na(title)) title <- ""
    if (is.na(desc))  desc  <- ""
    txt <- norm_txt(paste(title, desc))
    if (any(sapply(kw_alta,  function(k) grepl(paste0("\\b", k, "\\b"), txt)))) return("ALTA")
    if (any(sapply(kw_media, function(k) grepl(paste0("\\b", k, "\\b"), txt)))) return("MEDIA")
    "BAJA"
  }, error = function(e) "BAJA")
}

# --- DICCIONARIO DE CIUDADES ---
ciudades_uy <- list(
  "Artigas"      = c("artigas", "bella union", "tomas gomensoro"),
  "Canelones"    = c("canelones", "pando", "las piedras", "ciudad de la costa", "atlantida",
                     "santa lucia", "la paz", "progreso", "paso carrasco", "sauce",
                     "joaquin suarez", "tala", "san ramon", "migues"),
  "Cerro Largo"  = c("melo", "rio branco", "fraile muerto", "isidoro noblia"),
  "Colonia"      = c("colonia", "colonia del sacramento", "carmelo", "juan lacaze",
                     "nueva helvecia", "rosario", "tarariras", "florencio sanchez", "ombues de lavalle"),
  "Durazno"      = c("durazno", "sarandi del yi", "carmen", "blanquillo"),
  "Flores"       = c("trinidad", "ismael cortinas"),
  "Florida"      = c("florida", "sarandi grande", "casupa", "25 de mayo"),
  "Lavalleja"    = c("minas", "jose pedro varela", "solis de mataojo", "mariscala"),
  "Maldonado"    = c("maldonado", "punta del este", "san carlos", "piriapolis",
                     "pan de azucar", "pinares"),
  "Montevideo"   = c("montevideo", "pocitos", "cerro", "carrasco", "malvin", "prado",
                     "buceo", "ciudad vieja", "casavalle", "aguada"),
  "Paysandu"     = c("paysandu", "guichon", "quebracho"),
  "Rio Negro"    = c("rio negro", "fray bentos", "young", "nuevo berlin", "san javier"),
  "Rivera"       = c("rivera", "tranqueras", "vichadero", "minas de corrales"),
  "Rocha"        = c("rocha", "chuy", "castillos", "lascano", "la paloma", "punta del diablo"),
  "Salto"        = c("salto", "constitucion", "belen"),
  "San Jose"     = c("san jose", "san jose de mayo", "ciudad del plata", "libertad",
                     "rodriguez", "ecilda paullier"),
  "Soriano"      = c("soriano", "mercedes", "dolores", "cardona", "palmitas"),
  "Tacuarembo"   = c("tacuarembo", "paso de los toros", "san gregorio de polanco", "ansina"),
  "Treinta y Tres" = c("treinta y tres", "treintaytres", "vergara", "santa clara de olimar")
)

# --- FUENTES RSS ---
rss_urls <- list(
  # NACIONALES
  "Mdeo Portal"   = "https://www.montevideo.com.uy/anxml.aspx?58",
  "La Diaria"     = "https://ladiaria.com.uy/feeds/articulos/",
  "Telemundo"     = "https://www.teledoce.com/telemundo/feed/",
  "El Pais"       = "https://www.elpais.com.uy/rss/",
  "Subrayado"     = "https://www.subrayado.com.uy/rss/pages/home.xml",
  "Portal 180"    = "https://www.180.com.uy/feed.php",
  "Uypress"       = "https://www.uypress.net/anxml.aspx?13",
  "LR21"          = "https://www.lr21.com.uy/feed",
  "Caras y Caretas" = "https://www.carasycaretas.com.uy/rss/pages/home.xml",
  "Carve"         = "https://www.radiocarve.uy/feed/",
  "Universal"     = "https://970universal.com/feed/",
  "Sarandi 690"   = "https://www.sarandi690.com.uy/feed/",
  "El Observador" = "https://www.elobservador.com.uy/rss/pages/home.xml",
  # ARTIGAS
  "Todo Artigas"  = "https://todoartigas.uy/feed/",
  "ClicRegional"  = "https://www.clicregional.com/feed/",
  # CANELONES
  "Hoy Canelones" = "https://hoycanelones.com.uy/feed/",
  "Canelones Ciudad" = "https://canelonesciudad.com.uy/feed/",
  "Progreso Al Dia" = "https://progresoaldia.com.uy/blog/rss/all.rss",
  # CERRO LARGO
  "El Profesional" = "https://elprofesional.uy/?feed=rss2",
  "Cerro L. Portal" = "https://cerrolargoportal.com/feed/",
  # COLONIA
  "Carmelo Portal" = "https://www.carmeloportal.com/feed",
  "Colonia Noticias" = "https://www.colonianoticias.com.uy/feed/",
  "La Colonia Portal" = "https://lacoloniaportal.com.uy/feed/",
  # DURAZNO
  "El Acontecer"  = "https://elacontecer.com.uy/feed/",
  "Durazno Digital" = "https://duraznodigital.uy/feeds/posts/default",
  # FLORES
  "Ecos Regionales" = "http://www.ecosregionales.uy/sitio/feed/",
  # FLORIDA
  "Florida Noticias" = "https://floridanoticias.com.uy/feed/",
  "Florida Diario" = "https://floridadiario.com.uy/feed/",
  "Diario Cambios" = "https://diariocambios.com/feed/",
  # LAVALLEJA
  "Diario La Union" = "https://www.diariolaunion.com/feed/",
  # MALDONADO
  "Maldonado Notic." = "https://maldonadonoticias.com/beta/index.php?format=feed&type=rss",
  "Correo Punta E." = "https://correopuntadeleste.com/feed/",
  # PAYSANDU
  "El Telegrafo"  = "https://www.eltelegrafo.com/feed/",
  # RIO NEGRO
  "InfoRio"       = "https://inforio.com.uy/feed/",
  "El Rionegrense" = "https://elrionegrense.com.uy/feed/",
  # ROCHA
  "Rocha Noticias" = "https://rochanoticias.com/feed/",
  "La Paloma Hoy"  = "https://lapalomahoy.uy/rss.xml",
  # SALTO
  "Diario Cambio"  = "https://diariocambio.com.uy/rss.xml",
  "La Prensa"      = "https://laprensa.com.uy/?format=feed&type=rss",
  # SAN JOSE
  "San Jose Ahora" = "https://sanjoseahora.com.uy/feed/",
  "Vision Ciudadana" = "https://visionciudadana.uy/feed/",
  # SORIANO
  "Soriano Total"  = "https://www.sorianototal.com/feed/",
  "Cronicas"       = "https://cronicas.com.uy/feed/",
  # TACUAREMBO
  "Tacuarembo.net" = "https://tacuarembo.net/feed/",
  "El Avisador"    = "https://avisador.com.uy/feed/",
  # TREINTA Y TRES
  "Pagina 33"      = "https://pagina33.com/feed/"
)

rss_origins <- c(
  "Todo Artigas" = "Artigas",    "ClicRegional" = "Artigas",
  "Hoy Canelones" = "Canelones", "Canelones Ciudad" = "Canelones", "Progreso Al Dia" = "Canelones",
  "El Profesional" = "Cerro Largo", "Cerro L. Portal" = "Cerro Largo",
  "Carmelo Portal" = "Colonia",  "Colonia Noticias" = "Colonia",   "La Colonia Portal" = "Colonia",
  "El Acontecer" = "Durazno",    "Durazno Digital" = "Durazno",
  "Ecos Regionales" = "Flores",
  "Florida Noticias" = "Florida", "Florida Diario" = "Florida",    "Diario Cambios" = "Florida",
  "Diario La Union" = "Lavalleja",
  "Maldonado Notic." = "Maldonado", "Correo Punta E." = "Maldonado",
  "El Telegrafo" = "Paysandu",
  "InfoRio" = "Rio Negro",       "El Rionegrense" = "Rio Negro",
  "Rocha Noticias" = "Rocha",    "La Paloma Hoy" = "Rocha",
  "Diario Cambio" = "Salto",     "La Prensa" = "Salto",
  "San Jose Ahora" = "San Jose", "Vision Ciudadana" = "San Jose",
  "Soriano Total" = "Soriano",   "Cronicas" = "Soriano",
  "Tacuarembo.net" = "Tacuarembo", "El Avisador" = "Tacuarembo",
  "Pagina 33" = "Treinta y Tres"
)

# ======================================================================
# UI
# ======================================================================
ui <- fluidPage(
  tags$head(
    includeCSS("www/style.css"),

    # --- JAVASCRIPT: clock, countdown, panel toggle, splash ---
    tags$script(HTML("
      // ---- RELOJ EN TIEMPO REAL ----
      (function() {
        function pad(n) { return String(n).padStart(2, '0'); }
        function tick() {
          var el = document.getElementById('nb_clock');
          if (!el) return;
          var n = new Date();
          el.textContent = pad(n.getHours()) + ':' + pad(n.getMinutes()) + ':' + pad(n.getSeconds());
        }
        setInterval(tick, 1000);
        document.addEventListener('DOMContentLoaded', tick);
      })();

      // ---- CUENTA REGRESIVA (hasta el 1 ene del proximo anio) ----
      (function() {
        function pad(n) { return String(n).padStart(2, '0'); }
        var now = new Date();
        var DEADLINE = new Date(now.getFullYear() + 1, 0, 1, 0, 0, 0);
        function tick() {
          var diff = DEADLINE - new Date();
          var el = document.getElementById('nb_countdown');
          if (!el) return;
          if (diff <= 0) { el.textContent = 'EXPIRADO'; return; }
          var d = Math.floor(diff / 86400000);
          var h = Math.floor((diff % 86400000) / 3600000);
          var m = Math.floor((diff % 3600000) / 60000);
          var s = Math.floor((diff % 60000) / 1000);
          el.textContent = d + 'D ' + pad(h) + 'H ' + pad(m) + 'M ' + pad(s) + 'S';
        }
        setInterval(tick, 1000);
        document.addEventListener('DOMContentLoaded', tick);
      })();

      // ---- TOGGLE DE PANELES ----
      function togglePanel(id) {
        var panel = document.getElementById('panel_' + id);
        var btn   = document.getElementById('tbtn_' + id);
        if (!panel) return;
        var nowHidden = panel.classList.toggle('panel-hidden');
        if (btn) {
          if (nowHidden) btn.classList.remove('active');
          else           btn.classList.add('active');
        }
      }

      // ---- SPLASH ----
      document.addEventListener('DOMContentLoaded', function() {
        window.startCompile = function() {
          document.getElementById('btn_compilar').style.display = 'none';
          document.getElementById('splash_progress').style.display = 'block';
          var msgs = [
            'CARGANDO MODULOS GEOESPACIALES...',
            'SINCRONIZANDO CANALES DE INTELIGENCIA...',
            'VERIFICANDO FEEDS DEPARTAMENTALES...',
            'COMPILANDO MATRIZ DE MONITOREO...',
            'ENLAZANDO RED NACIONAL DE VIGILANCIA...',
            'SISTEMA OPERATIVO. BIENVENIDO.'
          ];
          var step = 0;
          function next() {
            if (step < msgs.length) {
              document.getElementById('splash_msg').textContent = '> ' + msgs[step];
              document.getElementById('splash_bar_fill').style.width =
                ((step + 1) / msgs.length * 100) + '%';
              step++;
              setTimeout(next, 400 + Math.random() * 300);
            } else {
              setTimeout(function() {
                document.getElementById('splash_screen').classList.add('hidden');
                setTimeout(function() {
                  document.getElementById('splash_screen').remove();
                  var app = document.getElementById('main_app');
                  app.style.display = 'flex';
                  setTimeout(function() {
                    window.dispatchEvent(new Event('resize'));
                    try {
                      var mw = HTMLWidgets.find('#mapa');
                      if (mw && mw.getMap) {
                        var m = mw.getMap();
                        m.invalidateSize(true);
                        setTimeout(function() { m.invalidateSize(true); }, 400);
                      }
                    } catch(e) { console.error('map resize error', e); }
                  }, 300);
                }, 600);
              }, 500);
            }
          }
          next();
        };
      });
    ")),

    # --- HANDLERS DE PROGRESO (solo barra, feed siempre visible) ---
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle_progress', function(data) {
        var wrap = document.getElementById('console_progress_wrapper');
        if (data.show) {
          if (wrap) wrap.style.display = 'block';
        } else {
          if (wrap) wrap.style.display = 'none';
        }
      });
      Shiny.addCustomMessageHandler('update_progress', function(data) {
        var msg = document.getElementById('console_msg');
        var bar = document.getElementById('console_bar_fill');
        if (msg) msg.textContent = '> ' + data.msg;
        if (bar) bar.style.width = data.percent + '%';
      });
    "))
  ),

  # ----------------------------------------------------------------
  # PANTALLA SPLASH
  # ----------------------------------------------------------------
  tags$div(id = "splash_screen", class = "splash-screen",
    tags$div(class = "splash-content",
      tags$span(class = "splash-logo", "◉"),
      tags$div(class = "splash-title",    "SISTEMA DE MONITOREO"),
      tags$div(class = "splash-subtitle", "REPUBLICA ORIENTAL DEL URUGUAY"),
      tags$div(class = "splash-version",  "v3.0 // CLASIFICADO // NIVEL 5"),
      tags$button(id = "btn_compilar", class = "connect-btn",
                  onclick = "startCompile()", "◉ INICIAR SISTEMA"),
      tags$div(id = "splash_progress", class = "splash-progress", style = "display:none;",
        tags$p(id = "splash_msg", class = "console-msg", "> INICIALIZANDO..."),
        tags$div(class = "console-bar-border",
          tags$div(id = "splash_bar_fill", class = "console-bar-fill", style = "width:0%;")
        )
      )
    )
  ),

  # ----------------------------------------------------------------
  # APLICACION PRINCIPAL
  # ----------------------------------------------------------------
  tags$div(
    id = "main_app",
    style = "display:none; flex-direction:column; height:100vh; overflow:hidden;",

    # ---- BARRA DE NAVEGACION SUPERIOR ----
    tags$nav(class = "sys-navbar",

      # Marca / título
      tags$div(class = "nb-brand",
        tags$span(class = "nb-dot", "◉"),
        tags$span("MONITOR NACIONAL")
      ),

      # Sección de relojes
      tags$div(class = "nb-clocks",
        tags$div(class = "nb-section",
          tags$span(class = "nb-label", "HORA LOCAL"),
          tags$span(id = "nb_clock", class = "nb-value", "00:00:00")
        ),
        tags$div(class = "nb-divider"),
        tags$div(class = "nb-section",
          tags$span(class = "nb-label", "OPERACION ACTIVA"),
          tags$span(id = "nb_countdown", class = "nb-value nb-countdown", "--D --H --M --S")
        )
      ),

      # Botones de selección de paneles
      tags$div(class = "nb-toggles",
        tags$span(class = "nb-label", style = "margin-right:4px;", "MONITORES:"),
        tags$button(id = "tbtn_mapa",   class = "btn-toggle active",
                    onclick = "togglePanel('mapa')",   "[ MAPA ]"),
        tags$button(id = "tbtn_feed",   class = "btn-toggle active",
                    onclick = "togglePanel('feed')",   "[ NOTICIAS ]"),
        tags$button(id = "tbtn_intel",  class = "btn-toggle",
                    onclick = "togglePanel('intel')",  "[ INTEL ]"),
        tags$button(id = "tbtn_ticker", class = "btn-toggle",
                    onclick = "togglePanel('ticker')", "[ TICKER ]")
      )
    ),

    # ---- AREA DE CONTENIDO: MAPA + COLUMNA DERECHA ----
    tags$div(class = "content-area",

      # == PANEL: MAPA DE SITUACION ==
      tags$div(id = "panel_mapa", class = "monitor-panel",
        tags$div(class = "monitor-header",
          tags$div(class = "mh-left",
            tags$span(class = "mh-dot"),
            tags$span(class = "mh-title", "MAPA DE SITUACION"),
            tags$span(class = "mh-status", "● EN LINEA")
          ),
          tags$button(class = "btn-panel-close", onclick = "togglePanel('mapa')", "✖")
        ),
        tags$div(class = "monitor-body",
          tags$div(class = "map-title", "Monitor de Noticias · Uruguay"),
          leafletOutput("mapa", width = "100%", height = "100%")
        )
      ),

      # == COLUMNA DERECHA: FEED + INTEL ==
      tags$div(class = "right-column",

        # -- PANEL: FEED DE INCIDENCIAS --
        tags$div(id = "panel_feed", class = "monitor-panel feed-panel-inner",
          tags$div(class = "monitor-header",
            tags$div(class = "mh-left",
              tags$span(class = "mh-dot"),
              tags$span(class = "mh-title", "FEED DE INCIDENCIAS"),
              uiOutput("feed_counter", inline = TRUE)
            ),
            tags$button(class = "btn-panel-close", onclick = "togglePanel('feed')", "✖")
          ),
          tags$div(class = "monitor-controls",
            actionButton("btn_recopilar", "> RECOPILAR",    class = "btn-tactical"),
            actionButton("btn_buscar_mas", "+ MAS NOTICIAS", class = "btn-tactical"),
            actionButton("btn_todos",     "◎ TODOS",         class = "btn-tactical"),
            actionButton("btn_borrar",    "✕ BORRAR",        class = "btn-tactical danger")
          ),
          uiOutput("depto_badge"),
          # Barra de progreso: fuera del feed, se muestra encima al cargar
          tags$div(id = "console_progress_wrapper", style = "display:none; flex-shrink:0;",
            tags$div(style = "padding: 10px 12px; border-bottom: 1px solid rgba(0,255,0,0.2);
                              background: rgba(0,25,0,0.9);",
              tags$p(id = "console_msg", class = "console-msg",
                     style = "margin:0 0 8px 0; font-size:1.2rem;",
                     "> CALIBRANDO SISTEMAS..."),
              tags$div(class = "console-bar-border", style = "margin-top:0;",
                tags$div(id = "console_bar_fill", class = "console-bar-fill", style = "width:0%;")
              )
            )
          ),
          tags$div(class = "monitor-body scrollable",
            uiOutput("news_feed_ui")
          )
        ),

        # -- PANEL: INTELIGENCIA (oculto por defecto) --
        tags$div(id = "panel_intel", class = "monitor-panel intel-panel-inner panel-hidden",
          tags$div(class = "monitor-header",
            tags$div(class = "mh-left",
              tags$span(class = "mh-dot dot-yellow"),
              tags$span(class = "mh-title", "INTELIGENCIA")
            ),
            tags$button(class = "btn-panel-close", onclick = "togglePanel('intel')", "✖")
          ),
          tags$div(class = "monitor-body scrollable",
            uiOutput("intel_ui")
          )
        )
      )
    ),

    # ---- TICKER DE NOTICIAS (oculto por defecto) ----
    tags$div(id = "panel_ticker", class = "ticker-bar panel-hidden",
      tags$div(class = "ticker-header",
        tags$span(class = "ticker-label", "◉ TRANSMISION EN VIVO"),
        tags$button(class = "btn-panel-close", onclick = "togglePanel('ticker')", "✖")
      ),
      tags$div(class = "ticker-body",
        tags$div(class = "ticker-scroll",
          uiOutput("ticker_ui", inline = TRUE)
        )
      )
    )
  )
)

# ======================================================================
# SERVER
# ======================================================================
server <- function(input, output, session) {

  depto_seleccionado <- reactiveVal("URUGUAY (GLOBAL)")
  news_data          <- reactiveVal(data.frame())
  max_per_feed       <- 5

  # ------------------------------------------------------------------
  # MOTOR DE SCRAPING RSS
  # ------------------------------------------------------------------
  extrar_lote <- function(overwrite = FALSE) {
    session$sendCustomMessage("toggle_progress", list(show = TRUE))
    session$sendCustomMessage("update_progress", list(msg = "CONECTANDO CON FUENTES...", percent = 5))

    current_db        <- if (overwrite) data.frame() else news_data()
    titulos_existentes <- if (nrow(current_db) > 0) current_db$titulo else character(0)

    # Descarga paralela
    pool          <- curl::new_pool()
    responses_env <- new.env()

    for (src_name in names(rss_urls)) {
      local({
        n   <- src_name
        url <- rss_urls[[n]]
        h   <- curl::new_handle()
        curl::handle_setopt(h,
          useragent      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
          accept_encoding = "",
          followlocation = TRUE,
          ssl_verifypeer = FALSE,
          connecttimeout = 10,
          timeout        = 20
        )
        curl::curl_fetch_multi(url,
          done = function(res) {
            if (!is.null(res$status_code) && res$status_code >= 200 && res$status_code < 400)
              responses_env[[n]] <- res$content
          },
          fail = function(msg) {},
          pool = pool, handle = h
        )
      })
    }

    session$sendCustomMessage("update_progress", list(msg = "DESCARGANDO FEEDS EN PARALELO...", percent = 20))
    curl::multi_run(timeout = 20, pool = pool)

    # Parseo
    new_rows           <- list()
    feeds_ok           <- 0
    feeds_fail         <- 0
    feeds_noresponse   <- 0
    total_feeds        <- length(rss_urls)

    session$sendCustomMessage("update_progress", list(msg = "PROCESANDO SENALES...", percent = 40))

    for (src_name in names(rss_urls)) {
      if (is.null(responses_env[[src_name]])) {
        feeds_noresponse <- feeds_noresponse + 1
        next
      }
      tryCatch({
        doc   <- xml2::read_xml(responses_env[[src_name]])
        items <- xml_find_all(doc, "//item")
        if (length(items) == 0) items <- xml_find_all(doc, "//*[local-name()='entry']")
        if (length(items) == 0) { feeds_ok <- feeds_ok + 1; next }

        items_taken <- 0
        for (i in seq_along(items)) {
          if (items_taken >= max_per_feed) break

          title <- xml_text(xml_find_first(items[i], "title | *[local-name()='title']"))
          if (is.na(title) || trimws(title) == "") next
          if (title %in% titulos_existentes) next

          desc <- xml_text(xml_find_first(items[i], "description | *[local-name()='summary']"))
          if (is.na(desc)) desc <- ""
          desc <- gsub("<p>The post .*? first appeared on .*?</p>", "", desc, ignore.case = TRUE)
          desc <- gsub("The post .*? first appeared on .*",         "", desc, ignore.case = TRUE)

          # Clasificación geográfica
          if (src_name %in% names(rss_origins)) {
            depto_match <- rss_origins[[src_name]]
          } else {
            corp <- paste(norm_txt(title), norm_txt(desc))
            depto_match <- "URUGUAY (GLOBAL)"
            for (d in names(ciudades_uy)) {
              if (any(sapply(ciudades_uy[[d]], function(ter) grepl(paste0("\\b", ter, "\\b"), corp)))) {
                depto_match <- d; break
              }
            }
          }

          link_xml <- xml_find_first(items[i], "link | *[local-name()='link']")
          link     <- xml_text(link_xml)
          if (is.na(link) || trimws(link) == "") link <- xml_attr(link_xml, "href")
          if (is.na(link)) link <- ""

          prioridad <- detectar_prioridad(title, desc)
          id_corto  <- toupper(sprintf("%04X", sample.int(65535, 1)))

          new_rows[[length(new_rows) + 1]] <- data.frame(
            news_id      = paste0("nid", as.integer(Sys.time()), "r", sample.int(99999, 1)),
            id_corto     = id_corto,
            departamento = depto_match,
            tipo         = src_name,
            titulo       = title,
            cuerpo       = desc,
            enlace       = link,
            hora         = format(Sys.time(), "%H:%M:%S"),
            prioridad    = prioridad,
            stringsAsFactors = FALSE
          )
          titulos_existentes <- c(titulos_existentes, title)
          items_taken <- items_taken + 1
        }
        feeds_ok <- feeds_ok + 1

      }, error = function(e) {
        feeds_fail <<- feeds_fail + 1
      })

      pct <- min(95, 40 + (feeds_ok + feeds_fail + feeds_noresponse) / total_feeds * 55)
      session$sendCustomMessage("update_progress", list(
        msg     = paste0("PROCESANDO: ", src_name, " [", feeds_ok, "/", total_feeds, "]"),
        percent = pct
      ))
    }

    if (length(new_rows) > 0)
      current_db <- bind_rows(current_db, bind_rows(new_rows))

    news_data(current_db)

    msg_final <- paste0(
      "BARRIDO COMPLETO: ", length(new_rows), " NUEVAS | ",
      "OK: ", feeds_ok, " | ERRORES: ", feeds_fail, " | TIMEOUT: ", feeds_noresponse
    )
    session$sendCustomMessage("update_progress", list(msg = msg_final, percent = 100))
    session$sendCustomMessage("toggle_progress", list(show = FALSE))
  }

  # ------------------------------------------------------------------
  # BOTONES DE CONTROL
  # ------------------------------------------------------------------
  observeEvent(input$btn_recopilar,  { extrar_lote(overwrite = TRUE) })
  observeEvent(input$btn_buscar_mas, { extrar_lote(overwrite = FALSE) })
  observeEvent(input$btn_todos,      { depto_seleccionado("URUGUAY (GLOBAL)") })
  observeEvent(input$btn_borrar, {
    news_data(data.frame())
    depto_seleccionado("URUGUAY (GLOBAL)")
    leafletProxy("mapa") %>%
      clearGroup("sm_rings") %>%
      clearGroup("sm_cores") %>%
      clearGroup("sm_labels") %>%
      clearMarkers()
  })

  # Badge que muestra el sector activo
  output$depto_badge <- renderUI({
    d <- depto_seleccionado()
    if (d == "URUGUAY (GLOBAL)") return(NULL)
    tags$div(
      style = paste0(
        "margin: 4px 12px 0 12px; padding: 4px 10px;",
        "border: 1px solid #ffc400; color: #ffc400;",
        "font-size: 1rem; letter-spacing: 2px;",
        "text-shadow: 0 0 6px #ffc400; background: rgba(255,196,0,0.08);",
        "display: inline-block;"
      ),
      paste0("▶ SECTOR ACTIVO: ", toupper(stri_trans_general(d, "Latin-ASCII")),
             "  — clic en [ TODOS ] para ver todos")
    )
  })

  # ------------------------------------------------------------------
  # MAPA BASE
  # ------------------------------------------------------------------
  output$mapa <- renderLeaflet({
    leaflet(uruguay_map, options = leafletOptions(
      zoomControl      = FALSE,
      attributionControl = FALSE,
      dragging         = FALSE,
      minZoom = 7, maxZoom = 7,
      scrollWheelZoom  = FALSE,
      doubleClickZoom  = FALSE,
      boxZoom          = FALSE,
      keyboard         = FALSE
    )) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      setView(lng = -56.0, lat = -32.5, zoom = 7) %>%
      addPolygons(
        fillColor   = "#000000", fillOpacity = 0.6,
        color       = "#0f0",   weight = 2, opacity = 0.8, dashArray = "3",
        highlightOptions = highlightOptions(
          weight = 4, color = "#ffffff", dashArray = "", fillOpacity = 0.9, bringToFront = TRUE
        ),
        label = ~NAME_1, layerId = ~NAME_1,
        labelOptions = labelOptions(style = list(
          "background-color" = "black", "color" = "#0f0", "border-color" = "#0f0",
          "font-family" = "VT323", "font-size" = "16px"
        ))
      )
  })
  outputOptions(output, "mapa", suspendWhenHidden = FALSE)

  # Marcadores de incidentes sobre el mapa
  observe({
    nd <- news_data()
    req(nrow(nd) > 0)
    tryCatch({
      if (is.null(nd$prioridad)) nd$prioridad <- "BAJA"
      nd$prioridad[is.na(nd$prioridad) | nd$prioridad == ""] <- "BAJA"

      dept_counts <- nd %>%
        filter(departamento != "URUGUAY (GLOBAL)") %>%
        group_by(departamento) %>%
        summarise(
          n       = dplyr::n(),
          max_pri = ifelse(any(prioridad == "ALTA"), "ALTA",
                    ifelse(any(prioridad == "MEDIA"), "MEDIA", "BAJA")),
          .groups = "drop"
        ) %>%
        mutate(sev_color = ifelse(max_pri == "ALTA",  "#ff3030",
                           ifelse(max_pri == "MEDIA", "#ffc400", "#00ff66")))

      if (nrow(dept_counts) > 0) {
        dept_counts$match_id <- toupper(stri_trans_general(dept_counts$departamento, "Latin-ASCII"))
        uc           <- uruguay_centroids
        uc$match_id  <- toupper(stri_trans_general(uc$NAME_1, "Latin-ASCII"))
        poi_data     <- uc %>% inner_join(dept_counts, by = "match_id")

        proxy <- leafletProxy("mapa", data = poi_data) %>%
          clearGroup("sm_rings") %>% clearGroup("sm_cores") %>% clearGroup("sm_labels")

        # Anillo pulsante (radar)
        proxy <- proxy %>% addCircleMarkers(
          lng = ~lng, lat = ~lat, radius = 22,
          color = ~sev_color, weight = 2, opacity = 0.9, fill = FALSE,
          group = "sm_rings", options = pathOptions(className = "sm-ring")
        )
        # Marcador central clickeable
        proxy <- proxy %>% addCircleMarkers(
          lng = ~lng, lat = ~lat, radius = 14,
          color = ~sev_color, weight = 3, opacity = 1,
          fillColor = "#001a00", fillOpacity = 0.95,
          layerId = ~paste0("poi_", NAME_1), group = "sm_cores",
          options = pathOptions(className = "sm-core"),
          label = ~paste0("SECTOR::", toupper(NAME_1),
                          " | INCIDENTES::", n,
                          " | PRIORIDAD::", max_pri),
          labelOptions = labelOptions(style = list(
            "background-color" = "#000", "color" = "#0f0",
            "border" = "1px solid #0f0", "font-family" = "VT323",
            "font-size" = "15px", "text-shadow" = "0 0 6px #0f0", "padding" = "4px 8px"
          ))
        )
        # Etiqueta con el número
        proxy %>% addLabelOnlyMarkers(
          lng = ~lng, lat = ~lat, label = ~as.character(n), group = "sm_labels",
          labelOptions = labelOptions(
            noHide = TRUE, direction = "center", textOnly = FALSE, offset = c(0, 0),
            style = list(
              "background-color" = "transparent", "color" = "#0f0", "border" = "0",
              "font-family" = "VT323", "font-size" = "16px", "font-weight" = "bold",
              "text-shadow" = "0 0 6px #0f0, 0 0 10px #0f0",
              "padding" = "0", "pointer-events" = "none"
            )
          )
        )
      }
    }, error = function(e) message("[MAPA] Error: ", conditionMessage(e)))
  })

  # Clicks en departamentos
  observeEvent(input$mapa_shape_click, {
    click <- input$mapa_shape_click
    if (!is.null(click$id)) depto_seleccionado(click$id)
  })
  observeEvent(input$mapa_marker_click, {
    click <- input$mapa_marker_click
    if (!is.null(click$id)) depto_seleccionado(sub("poi_", "", click$id))
  })

  # ------------------------------------------------------------------
  # CHIPS CONTADORES
  # ------------------------------------------------------------------
  output$feed_counter <- renderUI({
    df <- news_data()
    if (nrow(df) == 0) return(tags$span(class = "fc-chip fc-chip-idle", "◌ STANDBY"))
    n_alta  <- sum(df$prioridad == "ALTA",  na.rm = TRUE)
    n_media <- sum(df$prioridad == "MEDIA", na.rm = TRUE)
    n_baja  <- sum(df$prioridad == "BAJA",  na.rm = TRUE)
    tags$div(class = "fc-row",
      tags$span(class = "fc-chip fc-chip-total", paste0("TOTAL ", nrow(df))),
      tags$span(class = "fc-chip fc-chip-alta",  paste0("● ALTA ",  n_alta)),
      tags$span(class = "fc-chip fc-chip-media", paste0("● MEDIA ", n_media)),
      tags$span(class = "fc-chip fc-chip-baja",  paste0("● BAJA ",  n_baja))
    )
  })

  # ------------------------------------------------------------------
  # FEED DE NOTICIAS
  # ------------------------------------------------------------------
  output$news_feed_ui <- renderUI({
    tryCatch({
      current_news <- news_data()

      if (nrow(current_news) == 0) return(tags$p(
        "> A LA ESPERA DE ORDENES PARA INICIAR BARRIDO...",
        style = "color:#0f0; margin-top:20px; text-align:center; font-size:1.5rem; text-shadow:0 0 5px #0f0;"
      ))

      if (depto_seleccionado() != "URUGUAY (GLOBAL)")
        current_news <- current_news[
          norm_txt(current_news$departamento) == norm_txt(depto_seleccionado()), , drop = FALSE
        ]

      if (nrow(current_news) == 0) return(tags$p(
        paste0("> ANALIZANDO SECTOR ", toupper(depto_seleccionado()), "... (0 DETECTADAS)"),
        style = "color:#0f0; margin-top:20px; text-align:center; font-size:1.5rem; text-shadow:0 0 5px #0f0;"
      ))

      if (is.null(current_news$prioridad)) current_news$prioridad <- "BAJA"
      if (is.null(current_news$id_corto))  current_news$id_corto  <- "----"
      current_news$prioridad[is.na(current_news$prioridad) | current_news$prioridad == ""] <- "BAJA"
      current_news$id_corto[is.na(current_news$id_corto)] <- "----"

      pri_ord <- match(current_news$prioridad, c("ALTA", "MEDIA", "BAJA"))
      pri_ord[is.na(pri_ord)] <- 3L
      ord <- order(pri_ord, -xtfrm(as.character(current_news$hora)))
      current_news <- current_news[ord, , drop = FALSE]

      banner <- tags$div(class = "feed-banner",
        tags$span(class = "fb-dot"),
        tags$span(class = "fb-text",
          if (depto_seleccionado() == "URUGUAY (GLOBAL)")
            paste0("TRANSMISION NACIONAL · ", nrow(current_news), " SENALES DECODIFICADAS")
          else
            paste0("FILTRO SECTOR :: ", toupper(depto_seleccionado()), " · ", nrow(current_news), " SENALES")
        )
      )

      feed_items <- lapply(seq_len(nrow(current_news)), function(i) {
        row      <- current_news[i, ]
        pri      <- as.character(row$prioridad); if (is.na(pri) || pri == "") pri <- "BAJA"
        id_corto <- as.character(row$id_corto);  if (is.na(id_corto) || id_corto == "") id_corto <- "----"

        tags$div(
          class   = paste0("feed-item priority-", tolower(pri)),
          onclick = sprintf("Shiny.setInputValue('news_clicked','%s',{priority:'event'});", row$news_id),
          title  = "Haz clic para ver el artículo completo",
          tags$div(class = "fi-head",
            tags$span(class = "fi-id",       paste0("#", id_corto)),
            tags$span(class = "fi-priority",
              tags$span(class = "fi-dot"),
              paste0("PRI::", pri)
            ),
            tags$span(class = "fi-time", paste0("[", row$hora, "]"))
          ),
          tags$div(class = "fi-meta",
            tags$span(class = "fi-sector", paste0("[SECTOR: ", toupper(row$departamento), "]")),
            tags$span(class = "fi-src",    paste0("SRC::",     toupper(row$tipo)))
          ),
          tags$h4(class = "fi-title", row$titulo),
          tags$div(class = "fi-foot",
            tags$span(class = "fi-decode", "[ CLICK PARA LEER ARTICULO COMPLETO → ]")
          )
        )
      })

      do.call(tagList, c(list(banner), feed_items))

    }, error = function(e) {
      message("[FEED] Error: ", conditionMessage(e))
      tags$p(paste("> ERROR EN FEED:", conditionMessage(e)),
             style = "color:#f00; margin-top:20px; text-align:center; font-size:1.3rem;")
    })
  })

  # ------------------------------------------------------------------
  # PANEL INTELIGENCIA
  # ------------------------------------------------------------------
  output$intel_ui <- renderUI({
    df <- news_data()

    if (nrow(df) == 0) return(tags$p(
      "> SIN DATOS :: RECOPILAR PRIMERO",
      style = "color:#0f0; text-align:center; margin-top:25px; font-size:1.3rem; text-shadow:0 0 5px #0f0;"
    ))

    n_alta  <- sum(df$prioridad == "ALTA",  na.rm = TRUE)
    n_media <- sum(df$prioridad == "MEDIA", na.rm = TRUE)
    n_baja  <- sum(df$prioridad == "BAJA",  na.rm = TRUE)
    total   <- nrow(df)

    top_depts <- df %>%
      filter(departamento != "URUGUAY (GLOBAL)") %>%
      group_by(departamento) %>%
      summarise(n = dplyr::n(), .groups = "drop") %>%
      arrange(desc(n)) %>%
      head(8)

    top_srcs <- df %>%
      group_by(tipo) %>%
      summarise(n = dplyr::n(), .groups = "drop") %>%
      arrange(desc(n)) %>%
      head(6)

    dept_rows <- if (nrow(top_depts) > 0) {
      lapply(seq_len(nrow(top_depts)), function(i) {
        d   <- top_depts[i, ]
        pct <- max(3, round(d$n / max(top_depts$n) * 100))
        tags$div(class = "intel-bar-row",
          tags$div(class = "intel-bar-label", toupper(d$departamento)),
          tags$div(class = "intel-bar-track",
            tags$div(class = "intel-bar-fill", style = paste0("width:", pct, "%;"))
          ),
          tags$div(class = "intel-bar-count", d$n)
        )
      })
    } else {
      list(tags$p("> SIN DATOS DEPARTAMENTALES", style = "color:#0a0; font-size:0.95rem;"))
    }

    src_rows <- lapply(seq_len(nrow(top_srcs)), function(i) {
      s <- top_srcs[i, ]
      tags$div(class = "intel-src-row",
        tags$span(class = "intel-src-name", toupper(s$tipo)),
        tags$span(class = "intel-src-count", paste0("[", s$n, "]"))
      )
    })

    tagList(
      # Resumen de prioridades
      tags$div(class = "intel-section",
        tags$div(class = "intel-section-title", "◉ RESUMEN OPERACIONAL"),
        tags$div(class = "intel-stats-grid",
          tags$div(class = "intel-stat",
            tags$div(class = "is-value is-total", total),
            tags$div(class = "is-label", "TOTAL")
          ),
          tags$div(class = "intel-stat",
            tags$div(class = "is-value is-alta", n_alta),
            tags$div(class = "is-label", "ALTA")
          ),
          tags$div(class = "intel-stat",
            tags$div(class = "is-value is-media", n_media),
            tags$div(class = "is-label", "MEDIA")
          ),
          tags$div(class = "intel-stat",
            tags$div(class = "is-value is-baja", n_baja),
            tags$div(class = "is-label", "BAJA")
          )
        )
      ),
      # Sectores activos
      tags$div(class = "intel-section",
        tags$div(class = "intel-section-title", "◉ SECTORES ACTIVOS"),
        do.call(tagList, dept_rows)
      ),
      # Fuentes principales
      tags$div(class = "intel-section",
        tags$div(class = "intel-section-title", "◉ FUENTES PRINCIPALES"),
        do.call(tagList, src_rows)
      )
    )
  })

  # ------------------------------------------------------------------
  # TICKER DE NOTICIAS
  # ------------------------------------------------------------------
  output$ticker_ui <- renderUI({
    df <- news_data()

    if (nrow(df) == 0) return(tags$span(
      "> SISTEMA EN STANDBY :: SIN SENAL ACTIVA :: RECOPILAR NOTICIAS PARA ACTIVAR TRANSMISION ",
      style = "color:#0f0; font-size:1.1rem;"
    ))

    df_sorted <- df[order(match(df$prioridad, c("ALTA", "MEDIA", "BAJA"))), , drop = FALSE]
    n_show    <- min(nrow(df_sorted), 40)

    headlines <- paste(
      sapply(seq_len(n_show), function(i) {
        row     <- df_sorted[i, ]
        pri_sym <- switch(as.character(row$prioridad),
          ALTA  = "[ALTA]",
          MEDIA = "[MEDIA]",
          BAJA  = "[BAJA]",
          "[---]"
        )
        paste0(" ", pri_sym, " ", toupper(row$departamento), " :: ", toupper(row$titulo), "  ◆  ")
      }),
      collapse = ""
    )

    tags$span(
      class = "ticker-text",
      headlines
    )
  })

  # ------------------------------------------------------------------
  # MODAL DE DETALLE
  # ------------------------------------------------------------------
  observeEvent(input$news_clicked, {
    clicked_id  <- input$news_clicked
    target_news <- news_data() %>% filter(news_id == clicked_id) %>% head(1)

    if (nrow(target_news) > 0) {
      showModal(modalDialog(
        title = paste(">> INTERCEPCION:", toupper(target_news$tipo)),
        tags$p(class = "status-indicator", style = "display:inline-block;",
               paste("SECTOR:", toupper(target_news$departamento))),
        tags$h3(target_news$titulo),
        tags$hr(style = "border-top: 1px dashed #0f0;"),
        tags$div(style = "max-height:40vh; overflow-y:auto;", HTML(target_news$cuerpo)),
        tags$br(),
        tags$span(class = "status-indicator", style = "display:block; margin-bottom:10px;",
                  paste("FUENTE DEL REPORTE:", toupper(target_news$tipo))),
        tags$a(
          href = target_news$enlace, target = "_blank", class = "btn-default",
          style = "padding:10px; display:block; text-align:center; text-decoration:none; margin-bottom:10px;",
          paste(">> IR A LA PUBLICACION ORIGINAL EN", toupper(target_news$tipo), "<<")
        ),
        tags$span(class = "feed-date", paste("HORA DE REGISTRO LOCAL:", target_news$hora)),
        easyClose = TRUE,
        footer    = modalButton("CERRAR [X]")
      ))
    }
  })

  # ------------------------------------------------------------------
  # EVITAR SUSPENSION DE OUTPUTS EN CONTENEDOR display:none
  # ------------------------------------------------------------------
  outputOptions(output, "news_feed_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "feed_counter", suspendWhenHidden = FALSE)
  outputOptions(output, "intel_ui",     suspendWhenHidden = FALSE)
  outputOptions(output, "ticker_ui",    suspendWhenHidden = FALSE)
  outputOptions(output, "depto_badge",  suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
