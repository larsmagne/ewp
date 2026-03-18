function r(selector) {
  document.querySelectorAll(selector).forEach(el => el.remove());
}

function d(domain, selector) {
  if (window.location.hostname == domain ||
      window.location.hostname.endsWith("." + domain)) {
    r(selector);
  }
}
