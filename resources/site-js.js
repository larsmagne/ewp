function getOffsetPos(elem) {
  var offsetLeft = 0;
  var offsetTop = 0;
  while (elem) {
    offsetLeft += elem.offsetLeft;
    offsetTop += elem.offsetTop;
    elem = elem.offsetParent;
  }
  return [offsetLeft, offsetTop];
}

function hoverLink(e) {
  var elem = e.target;
  var link = elem.getAttribute("data-cached-image");
  if (! link)
    return;
  // Already exists.
  if (document.getElementById(link))
    return;
  elem.style.position = "relative";
  var img = document.createElement("img");
  // Just about any character is allowed in IDs in HTML5. 
  img.id = link;
  var thumb = link.replace(/[.][^.]+$/, "-150x150$&");
  img.onload = function() {
    img.classList.add("link-hover-fade-in");
  };
  img.src = thumb;
  var pos = getOffsetPos(elem);
  var left = pos[0];
  img.style.top = "-150px";
  img.style.left = e.pageX - left - 75 + "px";
  var time = elem.getAttribute("data-cached-time");
  var timeParsed = new Date(time);
  let options = {
    year: "numeric", month: "long",
    day: "numeric", hour: "2-digit", minute: "2-digit", hour12: false
  };
  let fTime = timeParsed.toLocaleTimeString("en-us", options);
  img.title = "Click to view static version of the web page" +
    (time? " (cache-time " + fTime + ")": "");
  img.className = "link-hover";
  var go = function() {
    window.history.pushState("site-js", "", link);
    for (var i = 0; i < document.styleSheets.length; i++)
      document.styleSheets[i].disabled = true;
    document.getElementsByTagName("body")[0].innerHTML =
      "<style> body { background: #d0d0d0; font-family: sans-serif; margin: 20px; text-align: center; } </style>" +
      "Original Link: <a href='" + elem.href + "'>" + elem.href +
      "</a>; <a href='" + link + "'>page cached " + fTime + "</a>.<p>" +
      "<img src='" + link + "'>";
    window.scrollTo(0, 0);
    window.addEventListener("popstate", function() {
      window.location.reload();
    });
  };
  img.onclick = function(ev) {
    ev.preventDefault();
    go();
    return false;
  };
  elem.appendChild(img);
  elem.onmouseleave = function() {
    img.classList.add("link-hover-fade-out");
    // Allow a new hovering to happen before this one is removed.
    img.id = "";
    setTimeout(function() {
      if (img.parentNode)
	img.parentNode.removeChild(img);
    }, 500);
  };
}
