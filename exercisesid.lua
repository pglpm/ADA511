local id = 1
function Callout(callout)
  if callout.type == "caution" then
    if callout.attr.identifier ~= "" then
      -- callout already has an id
      return nil
    end
    -- callout.title = "Exercise " .. tostring(id)
    callout.icon = true
    callout.attr.identifier = "cau-unique" .. tostring(id)
    id = id + 1
  end
  return quarto.Callout(callout)
end