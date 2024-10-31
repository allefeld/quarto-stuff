# Custom Format for City Moodle

An HTML-based format suitable for uploading to City's Moodle.

The format inserts a header showing the logo and the programme name, and adjusts the font and primary and link colours.

## Installing

```bash
quarto add allefeld/quarto-stuff/city-moodle
```

## Using

Add to the document or project metadata:

```yaml
format: city-moodle-html
programme: My Programme
```

## Example

Source code for a minimal example: [example.qmd](example.qmd).
