HTMLCollection.prototype.toArray = Array.prototype.slice;

function sendLink(node) {
  return (e) => {
    e.stopPropagation();
    e.preventDefault();
    chrome.runtime.sendMessage({"link": node.href});
  };
}

function addButton(node) {
  let btn = document.createElement("button");
  btn.textContent = "▶";
  btn.addEventListener("click", sendLink(node));
  node.parentNode.insertBefore(btn, node);
}

function replaceDuplicate(nodes, node) {
  node.setAttribute("play-in-mpv", "");
  if (nodes.length > 0 && node.href == nodes[nodes.length - 1].href) {
    nodes[nodes.length - 1] = node;
    return nodes;
  } else return nodes.concat(node);
}

const urls = ["(www|m)\\.youtube\\.com/(watch|playlist)",
              "youtu\\.be",
              "player\\.vimeo\\.com",
              "vimeo\\.com",
              "(www|clips)\\.twitch\\.tv/\\w+$",
              "www\\.twitch\\.tv/videos/\\d+$",
              "streamable\\.com",
              ".*\\.streamable\\.com/video",
              "vid\\.me",
              "www.liveleak.com/view",
              "twitter\\.com/.+/video/",
              "www.facebook\\.com/.+/videos/"
             ];

function main (node) {
  node
    .getElementsByTagName("a")
    .toArray()
    .filter(a => new RegExp("https?://(" + urls.join("|") + ")").test(a.href) &&
            !a.hasAttribute("play-in-mpv"))
    .reduce(replaceDuplicate, [])
    .forEach(addButton);
}

new MutationObserver(mutations => mutations.forEach(main(document)))
  .observe(document, {childList: true, subtree: true});

main(document);