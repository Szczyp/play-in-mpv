HTMLCollection.prototype.toArray = Array.prototype.slice

function sendLink(href) {
  return () => {
    chrome.runtime.sendMessage({"link": href})
  }
}

function addButton(node) {
  let btn = document.createElement("button")
  btn.textContent = "\u{25b6}"
  btn.addEventListener("click", sendLink(node.href))
  node.parentNode.insertBefore(btn, node)
}

function replaceDuplicate(nodes, node) {
  if (nodes.length > 0 && node.href == nodes[nodes.length - 1].href) {
    nodes[nodes.length - 1] = node
    return nodes
  } else return nodes.concat(node)
}

const urls = ["(www|m)\\.youtube\\.com/(watch|playlist)",
              "youtu\\.be",
              "player\\.vimeo\\.com",
              "vimeo\\.com",
              "www\\.twitch\\.tv/\\w+$",
              "www\\.twitch\\.tv/.+/v/\\d+$",
              "clips.twitch.tv/.+/.+",
              "streamable\\.com",
              "vid\\.me",
              "www.liveleak.com/view"
             ]

document
  .getElementsByTagName("a")
  .toArray()
  .filter(a => new RegExp("https?:\/\/(" + urls.join("|") + ")").test(a.href))
  .reduce(replaceDuplicate, [])
  .forEach(addButton)
