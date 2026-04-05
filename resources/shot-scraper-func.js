function r(selector) {
  document.querySelectorAll(selector).forEach(el => el.remove());
}

function d(domain, selector) {
  if (window.location.hostname == domain ||
      window.location.hostname.endsWith("." + domain)) {
    r(selector);
  }
}

function e(domain, selector) {
  if (window.location.hostname == domain ||
      window.location.hostname.endsWith("." + domain)) {
    findExtended(selector).forEach(el => el.remove());
  }
}

function findExtended(selector) {
  // Extract :has-text(...) parts
  const procedures = [];
  const regex = /:has-text\((.*?)\)/g;

  let baseSelector = selector.replace(regex, (_, value) => {
    procedures.push({
      type: "has-text",
      value: value.trim().toLowerCase()
    });
    return "";
  }).trim();

  if (!baseSelector) baseSelector = "*";

  let elements;
  try {
    elements = document.querySelectorAll(baseSelector);
  } catch (e) {
    console.warn("Invalid selector:", baseSelector);
    return [];
  }

  // Apply procedural filters
  return [...elements].filter(el => {
    for (const proc of procedures) {
      if (proc.type === "has-text") {
        const text = (el.textContent || "").toLowerCase();
        if (!text.includes(proc.value)) return false;
      }
    }
    return true;
  });
}
