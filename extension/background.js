var port = browser.runtime.connectNative("com.szczyp.play_in_mpv");

chrome.runtime.onMessage.addListener(message => port.postMessage(message));
