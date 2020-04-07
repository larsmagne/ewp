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
  /* Already exists. */
  if (document.getElementById(link))
    return;
  elem.style.position = "relative";
  var img = document.createElement("img");
  /* Just about any character is allowed in IDs in HTML5. */
  img.id = link;
  var thumb = link.replace(/[.][^.]+$/, "-150x150$&");
  img.onload = function() {
    img.classList.add("link-hover-fade-in");
  };
  img.src = thumb;
  var pos = getOffsetPos(elem);
  var bottom = pos[1] + elem.offsetHeight;
  var left = pos[0];
  img.style.bottom = "15px";
  img.style.left = e.pageX - left - 75 + "px";
  img.title = "Click to view static version of the web page (cache-time " +
    elem.getAttribute("data-cached-time") + ")";
  img.className = "link-hover";
  img.onclick = function() {
    window.location = link;
    return false;
  };
  elem.appendChild(img);
  elem.onmouseleave = function() {
    img.classList.add("link-hover-fade-out");
    setTimeout(function() {
      if (img.parentNode)
	img.parentNode.removeChild(img);
    }, 500);
  };
}
