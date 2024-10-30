-- mark compact definition lists
function DefinitionList(elem)
  -- Determine whether the definition list contains only `Plain` definitions,
  -- i.e. its contents are not wrapped in Para elements.
  local allplain = true
  for _, item in pairs(elem.content) do
    for _, definition in pairs(item[2]) do
      allplain = allplain and (definition[1].tag == "Plain")
    end
  end
  -- If so, wrap the definition list in a Div element with class "compactdef."
  -- This is styled by CSS for a more compact, bullet-list-like layout.
  -- The wrapping is necessary because `DefinitionList` have no classes.
  if allplain then
    return pandoc.Div(elem, pandoc.Attr("", {"compactdef"}, {}))
  else
    return elem
  end
end

-- link to CSS which provides the styling of compact definition lists
function Meta(meta)
  if quarto.doc.is_format("html") then
    quarto.doc.add_html_dependency({
      name = "compactdef",
      stylesheets = {"compactdef.css"}
    })
  end
end
