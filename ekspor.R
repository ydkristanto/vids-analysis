#' Export the ShinyApp from "app" to "docs".
shinylive::export(appdir = "app",
                  destdir = "docs")

# Menguji aplikasi dalam folder "docs".
httpuv::runStaticServer("docs")
