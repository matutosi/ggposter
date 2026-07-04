# Package index

## Build a poster

Assemble and render a poster from a spec.

- [`poster()`](https://matutosi.github.io/ggposter/reference/poster.md)
  : Build a poster from a declarative spec
- [`read_poster_yaml()`](https://matutosi.github.io/ggposter/reference/read_poster_yaml.md)
  : Read a poster spec from a YAML file
- [`render_poster()`](https://matutosi.github.io/ggposter/reference/render_poster.md)
  : Render a poster to PDF or PNG at true size

## Theming

Colours, fonts, and spacing.

- [`poster_theme()`](https://matutosi.github.io/ggposter/reference/poster_theme.md)
  : Theme (visual settings) for a ggposter poster
- [`theme_green()`](https://matutosi.github.io/ggposter/reference/theme_green.md)
  : A green preset theme resembling the bundled sample poster
- [`poster_font()`](https://matutosi.github.io/ggposter/reference/poster_font.md)
  : Find an available font family for a poster
- [`poster_register_font()`](https://matutosi.github.io/ggposter/reference/poster_register_font.md)
  : Register a font so its exact file is used by graphics devices

## Content builders

Build the body of a section card.

- [`card_text()`](https://matutosi.github.io/ggposter/reference/card_text.md)
  : Text card content: a bullet list rendered as markdown
- [`card_table()`](https://matutosi.github.io/ggposter/reference/card_table.md)
  : Table card content: a booktab-style table grob
- [`card_figure()`](https://matutosi.github.io/ggposter/reference/card_figure.md)
  : Figure card content: a ggplot styled for poster font size
- [`card_image()`](https://matutosi.github.io/ggposter/reference/card_image.md)
  : Image card content: one or more photos arranged in a row, with
  optional labels

## Cards and titles

- [`poster_card()`](https://matutosi.github.io/ggposter/reference/poster_card.md)
  : Build a section card: a rounded box with a header tab and content
- [`poster_title()`](https://matutosi.github.io/ggposter/reference/poster_title.md)
  : Build the full-width title band of a poster
