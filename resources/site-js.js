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
  img.title = "Click to view static version of the web page" +
    (time? " (cache-time " + time + ")": "");
  img.className = "link-hover";
  img.onclick = function() {
    window.location = link;
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
