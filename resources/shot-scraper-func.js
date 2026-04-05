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
    const { elements, watchAttrs } = queryProceduralSelector(
      selector
    );
    elements.forEach(el => el.remove());
  }
}

/* Code below written by ChatGPT to emulate uBlock origin extended selectors
   https://github.com/gorhill/ublock/wiki/Procedural-cosmetic-filters */

/**
 * Evaluate the selector text that appears after #?# in a uBO-style procedural
 * cosmetic filter and return matching elements.
 *
 * Supported operators from the uBO "Procedural cosmetic filters" page:
 *   :has(...)
 *   :has-text(...)
 *   :matches-attr(...)
 *   :matches-css(...)
 *   :matches-css-before(...)
 *   :matches-css-after(...)
 *   :matches-media(...)
 *   :matches-path(...)
 *   :matches-prop(...)
 *   :min-text-length(...)
 *   :not(...)
 *   :others()
 *   :upward(...)
 *   :watch-attr(...)
 *   :xpath(...)
 *
 * Usage:
 *   const { elements, watchAttrs } = queryProceduralSelector(
 *     '.foo[style]:has-text("data protection"):upward(2)'
 *   );
 *   elements.forEach(el => el.remove());
 */
function queryProceduralSelector(selectorText, options = {}) {
  const ctx = {
    root: options.root || document,
    location: options.location || window.location,
    window: options.window || window,
    document: options.document || document,
  };

  const ast = parseProceduralSelector(selectorText);
  const result = evaluateProgram(ast, [ctx.root], ctx);

  return {
    elements: uniqueElements(result.elements),
    watchAttrs: [...result.watchAttrs],
  };
}

/* -------------------------------------------------------------------------- */
/* Parser                                                                      */
/* -------------------------------------------------------------------------- */

function parseProceduralSelector(input) {
  const s = input.trim();
  let i = 0;

  function eof() {
    return i >= s.length;
  }

  function peek() {
    return s[i];
  }

  function consume(ch) {
    if (s[i] !== ch) {
      throw new Error(`Expected "${ch}" at position ${i}, got "${s[i] || "EOF"}"`);
    }
    i++;
  }

  function skipWs() {
    while (!eof() && /\s/.test(peek())) i++;
  }

  function readQuoted() {
    const quote = peek();
    if (quote !== '"' && quote !== "'") return null;
    i++;
    let out = "";
    while (!eof()) {
      const ch = s[i++];
      if (ch === "\\") {
        if (eof()) break;
        out += s[i++];
        continue;
      }
      if (ch === quote) return out;
      out += ch;
    }
    throw new Error("Unterminated quoted string");
  }

  function readRegexLiteral() {
    if (peek() !== "/") return null;
    i++;
    let body = "";
    let inClass = false;

    while (!eof()) {
      const ch = s[i++];
      if (ch === "\\") {
        if (eof()) break;
        body += ch + s[i++];
        continue;
      }
      if (ch === "[") inClass = true;
      if (ch === "]") inClass = false;
      if (ch === "/" && !inClass) {
        let flags = "";
        while (!eof() && /[gimsuyd]/.test(peek())) flags += s[i++];
        return new RegExp(body, flags);
      }
      body += ch;
    }
    throw new Error("Unterminated regex literal");
  }

  function readBalancedParenContent() {
    consume("(");
    let depth = 1;
    let out = "";

    while (!eof()) {
      const ch = s[i];

      if (ch === '"' || ch === "'") {
        const q = readQuotedRaw();
        out += q;
        continue;
      }

      if (ch === "/") {
        const rx = readRegexRaw();
        out += rx;
        continue;
      }

      i++;
      if (ch === "(") depth++;
      if (ch === ")") depth--;

      if (depth === 0) {
        return out;
      }
      out += ch;
    }
    throw new Error("Unterminated parenthesized argument");
  }

  function readQuotedRaw() {
    const start = i;
    const quote = s[i++];
    while (!eof()) {
      const ch = s[i++];
      if (ch === "\\") {
        if (!eof()) i++;
        continue;
      }
      if (ch === quote) break;
    }
    return s.slice(start, i);
  }

  function readRegexRaw() {
    const start = i;
    i++; // leading /
    let inClass = false;
    while (!eof()) {
      const ch = s[i++];
      if (ch === "\\") {
        if (!eof()) i++;
        continue;
      }
      if (ch === "[") inClass = true;
      if (ch === "]") inClass = false;
      if (ch === "/" && !inClass) {
        while (!eof() && /[gimsuyd]/.test(peek())) i++;
        break;
      }
    }
    return s.slice(start, i);
  }

  function readBaseSelector() {
    let out = "";
    while (!eof()) {
      const ch = peek();
      if (ch === ":" && isProceduralStart(i)) break;
      out += ch;
      i++;
    }
    return out.trim();
  }

  function isProceduralStart(pos) {
    const names = [
      ":has(",
      ":has-text(",
      ":matches-attr(",
      ":matches-css(",
      ":matches-css-before(",
      ":matches-css-after(",
      ":matches-media(",
      ":matches-path(",
      ":matches-prop(",
      ":min-text-length(",
      ":not(",
      ":others(",
      ":upward(",
      ":watch-attr(",
      ":xpath(",
    ];
    return names.some(name => s.startsWith(name, pos));
  }

  function parseStep() {
    if (peek() !== ":") return null;

    const names = [
      "has-text",
      "matches-attr",
      "matches-css-before",
      "matches-css-after",
      "matches-css",
      "matches-media",
      "matches-path",
      "matches-prop",
      "min-text-length",
      "others",
      "upward",
      "watch-attr",
      "xpath",
      "has",
      "not",
    ];

    const name = names.find(n => s.startsWith(`:${n}(`, i));
    if (!name) return null;

    i += name.length + 1; // skip :name
    const rawArg = readBalancedParenContent();

    return { type: name, rawArg };
  }

  const baseSelector = readBaseSelector() || "*";
  const steps = [];

  while (!eof()) {
    skipWs();
    const step = parseStep();
    if (!step) {
      // Support trailing native CSS after procedural operators
      const tail = s.slice(i).trim();
      if (tail) {
        steps.push({ type: "__css_tail__", rawArg: tail });
        i = s.length;
        break;
      }
      break;
    }
    steps.push(step);
  }

  return { baseSelector, steps };
}

/* -------------------------------------------------------------------------- */
/* Evaluator                                                                   */
/* -------------------------------------------------------------------------- */

function evaluateProgram(ast, roots, ctx) {
  let current = queryBase(ast.baseSelector, roots, ctx);
  let watchAttrs = new Set();

  for (const step of ast.steps) {
    switch (step.type) {
      case "has":
        current = opHas(current, step.rawArg, ctx);
        break;
      case "has-text":
        current = opHasText(current, step.rawArg);
        break;
      case "matches-attr":
        current = opMatchesAttr(current, step.rawArg);
        break;
      case "matches-css":
        current = opMatchesCss(current, step.rawArg, null, ctx);
        break;
      case "matches-css-before":
        current = opMatchesCss(current, step.rawArg, "::before", ctx);
        break;
      case "matches-css-after":
        current = opMatchesCss(current, step.rawArg, "::after", ctx);
        break;
      case "matches-media":
        current = opMatchesMedia(current, step.rawArg, ctx);
        break;
      case "matches-path":
        current = opMatchesPath(current, step.rawArg, ctx);
        break;
      case "matches-prop":
        current = opMatchesProp(current, step.rawArg);
        break;
      case "min-text-length":
        current = opMinTextLength(current, step.rawArg);
        break;
      case "not":
        current = opNot(current, step.rawArg, ctx);
        break;
      case "others":
        current = opOthers(current, ctx);
        break;
      case "upward":
        current = opUpward(current, step.rawArg);
        break;
      case "watch-attr": {
        const attrs = parseCsvArgs(step.rawArg);
        if (attrs.length === 0) {
          watchAttrs.add("*");
        } else {
          for (const a of attrs) watchAttrs.add(a);
        }
        break;
      }
      case "xpath":
        current = opXPath(current, step.rawArg, ctx);
        break;
      case "__css_tail__":
        current = queryBase(step.rawArg, current, ctx);
        break;
      default:
        throw new Error(`Unsupported operator: ${step.type}`);
    }
  }

  return { elements: current, watchAttrs };
}

/* -------------------------------------------------------------------------- */
/* Core querying                                                               */
/* -------------------------------------------------------------------------- */

function queryBase(selector, roots, ctx) {
  const out = [];
  const seen = new Set();

  for (const root of roots) {
    if (!root) continue;

    if (isDocument(root) || isElement(root) || isDocumentFragment(root)) {
      // Match root itself if it is an Element and selector matches.
      if (isElement(root)) {
        try {
          if (root.matches(selector) && !seen.has(root)) {
            seen.add(root);
            out.push(root);
          }
        } catch (_) {}
      }

      let nodeList = [];
      try {
        nodeList = root.querySelectorAll(selector);
      } catch (e) {
        throw new Error(`Invalid CSS selector "${selector}": ${e.message}`);
      }
      for (const el of nodeList) {
        if (!seen.has(el)) {
          seen.add(el);
          out.push(el);
        }
      }
    }
  }

  return out;
}

/* -------------------------------------------------------------------------- */
/* Operators                                                                   */
/* -------------------------------------------------------------------------- */

function opHas(elements, arg, ctx) {
  return elements.filter(el => {
    const nested = parseProceduralSelector(arg);
    return evaluateProgram(nested, [el], ctx).elements.length > 0;
  });
}

function opHasText(elements, arg) {
  const matcher = parseTextOrRegex(arg.trim());
  return elements.filter(el => testTextMatcher(matcher, el.textContent || ""));
}

function opMatchesAttr(elements, arg) {
  const { nameMatcher, valueMatcher, requireValue } = parseMatchesAttrArg(arg);
  return elements.filter(el => {
    for (const attr of el.getAttributeNames()) {
      if (!testTextMatcher(nameMatcher, attr)) continue;
      if (!requireValue) return true;
      const value = el.getAttribute(attr) ?? "";
      if (testTextMatcher(valueMatcher, value)) return true;
    }
    return false;
  });
}

function opMatchesCss(elements, arg, pseudo, ctx) {
  const idx = splitOnceRespectingSyntax(arg, ":");
  if (idx < 0) throw new Error(`Invalid matches-css arg: ${arg}`);
  const prop = arg.slice(0, idx).trim();
  const rawValue = arg.slice(idx + 1).trim();
  const matcher = parseTextOrRegex(rawValue);

  return elements.filter(el => {
    const style = ctx.window.getComputedStyle(el, pseudo);
    const actual = style.getPropertyValue(prop) || style[prop] || "";
    return testTextMatcher(matcher, String(actual).trim(), { exactLiteral: true });
  });
}

function opMatchesMedia(elements, arg, ctx) {
  if (!ctx.window.matchMedia(String(arg).trim()).matches) return [];
  return elements;
}

function opMatchesPath(elements, arg, ctx) {
  const path = `${ctx.location.pathname || ""}${ctx.location.search || ""}`;
  const matcher = parseTextOrRegex(arg.trim());
  return testTextMatcher(matcher, path) ? elements : [];
}

function opMatchesProp(elements, arg) {
  const { chain, matcher, requireValue } = parseMatchesPropArg(arg);

  return elements.filter(el => {
    let cur = el;
    for (const key of chain) {
      if (cur == null || !(key in cur)) return false;
      cur = cur[key];
    }
    if (!requireValue) return true;
    return testAnyMatcher(matcher, cur);
  });
}

function opMinTextLength(elements, arg) {
  const n = Number(arg.trim());
  if (!Number.isFinite(n) || n < 0) throw new Error(`Invalid min-text-length: ${arg}`);
  return elements.filter(el => (el.textContent || "").length >= n);
}

function opNot(elements, arg, ctx) {
  // If arg is plain CSS and not procedural, use native CSS semantics relative to subject.
  const procedural = looksProcedural(arg);
  if (!procedural) {
    return elements.filter(el => !matchesWithinContext(el, arg));
  }

  return elements.filter(el => {
    const nested = parseProceduralSelector(arg);
    return evaluateProgram(nested, [el], ctx).elements.length === 0;
  });
}

function opOthers(elements, ctx) {
  const subjects = uniqueElements(elements);
  if (subjects.length === 0) return [];

  const excluded = new Set(subjects);

  for (const el of subjects) {
    for (const d of el.querySelectorAll("*")) excluded.add(d);
    let p = el.parentElement;
    while (p) {
      excluded.add(p);
      p = p.parentElement;
    }
  }

  const all = Array.from((ctx.document || document).querySelectorAll("*"));
  return all.filter(el => !excluded.has(el));
}

function opUpward(elements, arg) {
  const trimmed = arg.trim();
  const out = [];
  const seen = new Set();

  if (/^\d+$/.test(trimmed)) {
    const n = Number(trimmed);
    if (n < 1 || n >= 256) throw new Error(`Invalid upward(n): ${arg}`);
    for (const el of elements) {
      let cur = el;
      for (let k = 0; k < n && cur; k++) cur = cur.parentElement;
      if (cur && !seen.has(cur)) {
        seen.add(cur);
        out.push(cur);
      }
    }
    return out;
  }

  for (const el of elements) {
    let cur = el.parentElement;
    while (cur) {
      if (cur.matches(trimmed)) {
        if (!seen.has(cur)) {
          seen.add(cur);
          out.push(cur);
        }
        break;
      }
      cur = cur.parentElement;
    }
  }
  return out;
}

function opXPath(elements, arg, ctx) {
  const out = [];
  const seen = new Set();

  const roots = elements.length ? elements : [ctx.document];
  for (const root of roots) {
    const doc = isDocument(root) ? root : root.ownerDocument || ctx.document;
    const res = doc.evaluate(
      arg,
      root,
      null,
      XPathResult.ORDERED_NODE_SNAPSHOT_TYPE,
      null
    );
    for (let i = 0; i < res.snapshotLength; i++) {
      const node = res.snapshotItem(i);
      if (isElement(node) && !seen.has(node)) {
        seen.add(node);
        out.push(node);
      }
    }
  }
  return out;
}

/* -------------------------------------------------------------------------- */
/* Helpers                                                                     */
/* -------------------------------------------------------------------------- */

function parseTextOrRegex(raw) {
  const t = raw.trim();
  const rx = tryParseRegex(t);
  if (rx) return { type: "regex", value: rx };

  if ((t.startsWith('"') && t.endsWith('"')) || (t.startsWith("'") && t.endsWith("'"))) {
    return { type: "literal", value: unquote(t) };
  }

  return { type: "literal", value: t };
}

function tryParseRegex(s) {
  if (!s.startsWith("/")) return null;
  let i = 1;
  let inClass = false;
  while (i < s.length) {
    const ch = s[i];
    if (ch === "\\") {
      i += 2;
      continue;
    }
    if (ch === "[") inClass = true;
    if (ch === "]") inClass = false;
    if (ch === "/" && !inClass) {
      const body = s.slice(1, i);
      const flags = s.slice(i + 1);
      if (!/^[gimsuyd]*$/.test(flags)) return null;
      return new RegExp(body, flags);
    }
    i++;
  }
  return null;
}

function unquote(s) {
  const q = s[0];
  let out = "";
  for (let i = 1; i < s.length - 1; i++) {
    const ch = s[i];
    if (ch === "\\" && i + 1 < s.length - 1) {
      out += s[++i];
    } else {
      out += ch;
    }
  }
  return out;
}

function testTextMatcher(matcher, text, opts = {}) {
  if (matcher.type === "regex") return matcher.value.test(text);
  return opts.exactLiteral ? text === matcher.value : text.includes(matcher.value);
}

function testAnyMatcher(matcher, value) {
  const text = String(value);
  if (matcher.type === "regex") return matcher.value.test(text);
  return text === matcher.value;
}

function parseMatchesAttrArg(arg) {
  const idx = findTopLevelEquals(arg);
  if (idx < 0) {
    return {
      nameMatcher: parseTextOrRegex(arg.trim()),
      valueMatcher: null,
      requireValue: false,
    };
  }
  return {
    nameMatcher: parseTextOrRegex(arg.slice(0, idx).trim()),
    valueMatcher: parseTextOrRegex(arg.slice(idx + 1).trim()),
    requireValue: true,
  };
}

function parseMatchesPropArg(arg) {
  const idx = findTopLevelEquals(arg);
  if (idx < 0) {
    return {
      chain: arg.trim().split(".").filter(Boolean),
      matcher: null,
      requireValue: false,
    };
  }
  return {
    chain: arg.slice(0, idx).trim().split(".").filter(Boolean),
    matcher: parseTextOrRegex(arg.slice(idx + 1).trim()),
    requireValue: true,
  };
}

function findTopLevelEquals(s) {
  let quote = null;
  let inRegex = false;
  let inClass = false;

  for (let i = 0; i < s.length; i++) {
    const ch = s[i];

    if (quote) {
      if (ch === "\\") {
        i++;
        continue;
      }
      if (ch === quote) quote = null;
      continue;
    }

    if (inRegex) {
      if (ch === "\\") {
        i++;
        continue;
      }
      if (ch === "[") inClass = true;
      if (ch === "]") inClass = false;
      if (ch === "/" && !inClass) inRegex = false;
      continue;
    }

    if (ch === '"' || ch === "'") {
      quote = ch;
      continue;
    }
    if (ch === "/") {
      inRegex = true;
      continue;
    }
    if (ch === "=") return i;
  }
  return -1;
}

function splitOnceRespectingSyntax(s, delimChar) {
  let quote = null;
  let inRegex = false;
  let inClass = false;

  for (let i = 0; i < s.length; i++) {
    const ch = s[i];

    if (quote) {
      if (ch === "\\") {
        i++;
        continue;
      }
      if (ch === quote) quote = null;
      continue;
    }

    if (inRegex) {
      if (ch === "\\") {
        i++;
        continue;
      }
      if (ch === "[") inClass = true;
      if (ch === "]") inClass = false;
      if (ch === "/" && !inClass) inRegex = false;
      continue;
    }

    if (ch === '"' || ch === "'") {
      quote = ch;
      continue;
    }
    if (ch === "/") {
      inRegex = true;
      continue;
    }
    if (ch === delimChar) return i;
  }
  return -1;
}

function parseCsvArgs(s) {
  const parts = [];
  let cur = "";
  let quote = null;
  let inRegex = false;
  let inClass = false;

  for (let i = 0; i < s.length; i++) {
    const ch = s[i];

    if (quote) {
      cur += ch;
      if (ch === "\\") {
        if (i + 1 < s.length) cur += s[++i];
        continue;
      }
      if (ch === quote) quote = null;
      continue;
    }

    if (inRegex) {
      cur += ch;
      if (ch === "\\") {
        if (i + 1 < s.length) cur += s[++i];
        continue;
      }
      if (ch === "[") inClass = true;
      if (ch === "]") inClass = false;
      if (ch === "/" && !inClass) inRegex = false;
      continue;
    }

    if (ch === '"' || ch === "'") {
      quote = ch;
      cur += ch;
      continue;
    }
    if (ch === "/") {
      inRegex = true;
      cur += ch;
      continue;
    }
    if (ch === ",") {
      const trimmed = cur.trim();
      if (trimmed) parts.push(trimmed);
      cur = "";
      continue;
    }
    cur += ch;
  }

  if (cur.trim()) parts.push(cur.trim());
  return parts.map(x => stripOptionalQuotes(x));
}

function stripOptionalQuotes(s) {
  const t = s.trim();
  if (
    (t.startsWith('"') && t.endsWith('"')) ||
    (t.startsWith("'") && t.endsWith("'"))
  ) {
    return unquote(t);
  }
  return t;
}

function looksProcedural(s) {
  return /:(?:has|has-text|matches-attr|matches-css(?:-before|-after)?|matches-media|matches-path|matches-prop|min-text-length|not|others|upward|watch-attr|xpath)\(/.test(s);
}

function matchesWithinContext(subject, selector) {
  if (!subject || !subject.querySelector) return false;
  try {
    return subject.querySelector(selector) != null;
  } catch {
    // Fallback: if selector itself matches subject, treat it as matched
    try {
      return subject.matches(selector);
    } catch {
      return false;
    }
  }
}

function uniqueElements(arr) {
  const out = [];
  const seen = new Set();
  for (const el of arr) {
    if (isElement(el) && !seen.has(el)) {
      seen.add(el);
      out.push(el);
    }
  }
  return out;
}

function isElement(node) {
  return !!node && node.nodeType === 1;
}

function isDocument(node) {
  return !!node && node.nodeType === 9;
}

function isDocumentFragment(node) {
  return !!node && node.nodeType === 11;
}
