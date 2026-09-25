-- Keep internal navigation in the current tab; open external web resources separately.
function Link(link)
  if link.target:match("^https?://") then
    link.attributes["target"] = "_blank"
    link.attributes["rel"] = "noopener noreferrer"
  end
  return link
end
