#!/usr/bin/env node

const spawn = require("child_process").spawn,
      url = require("url"),
      mpv = "mpv",
      defaultOptions = ["--no-terminal",
                        "--volume 70",
                        "--ytdl-format bestvideo+bestaudio"
                       ]

function youtube (href) {
  let {query: {list, index=1}} = href

  if (list) {
    delete href.query.v
    delete href.query.index
    delete href.search
    href.pathname = "playlist"

    return defaultOptions
      .concat("--playlist-start " + (parseInt(index) - 1))
      .concat(url.format(href))
  }
  else
    return generic(href)
}

function generic (href) {
  return defaultOptions.concat(href.href)
}

const handlers = {"youtube.com": youtube}

function read () {
  let buf = process.stdin.read()
  if (buf) {
    let message = JSON.parse(buf.slice(4).toString("utf8")),
        href = url.parse(message.link, true),
        hostname = href.hostname.split(".").slice(-2).join("."),
        options = handlers[hostname] ? handlers[hostname] : generic,
        player = spawn(mpv, options(href), {shell: true})

    player.on("exit", process.exit)

    process.stdin.removeListener("readable", read)
  }
}

process.stdin.on("readable", read)
