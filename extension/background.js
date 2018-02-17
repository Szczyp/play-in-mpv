var port = browser.runtime.connectNative("com.szczyp.play_in_mpv");

function play(message) {
  port.postMessage(message);
}

chrome.runtime.onMessage.addListener(play);
chrome.runtime.onMessageExternal.addListener(play);
