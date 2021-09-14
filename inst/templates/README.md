# {{shinywc.pkgname}}

A <{{name}}> web component for Shiny apps, using the {shinywc} framework.

```
library(shiny)
library({{shinywc.pkgname}})

ui <- fluidPage(
  {{shinywc.clean}}(
    id = "test"
  )
)
server <- function(input, output, session) {
  proxy <- {{shinywc.clean}}_proxy("test")
}
shinyApp(ui, server)
```

## Developer setup (you can delete this section)

Remember to run `devtools::document()` and `devtools::build()` immediately after creating this package.
