chrome.runtime.onMessage.addListener(
  message => chrome.runtime.sendNativeMessage("com.szczyp.play_in_mpv", message))
