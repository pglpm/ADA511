return {
  {Quoted = function (elem)
    for _, el in ipairs(elem.content) do
      if el.t == "Math" then
        return pandoc.Span(elem, {style = "display:inline-block;" })
      end
    end
    return elem
  end},
  {Para = function (elem)
    -- quarto.log.output(elem)
    content = elem.content
    for i, el in ipairs(content) do
      if el.t == "Math" then
        if i > 1 then
          -- prev = content[i-1] -- not sure previous string element should be append ...
          -- if prev.t == "Str" then
          --   current_elem = {prev, el}
          --   table.remove(content, i-1)
          -- else
            current_elem = {el}
          -- end
          if i < #content then
            next = content[i+1]
            if next.t == "Str" then
              table.insert(current_elem, next)
              table.remove(content, i+1)
            end
          end
          content[i] = pandoc.Span(current_elem, {style = "display:inline-block;" })
        end
      end
    end
    return pandoc.Para(content)
  end}
}