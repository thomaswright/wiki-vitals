export const divisionsPageTitle = "Wikipedia:Vital_articles/Level/5";
export const divisionsHtmlUrl =
  "https://en.wikipedia.org/w/rest.php/v1/page/" +
  encodeURIComponent(divisionsPageTitle) +
  "/html";

const buildPageHtmlUrl = (pageTitle) =>
  "https://en.wikipedia.org/w/rest.php/v1/page/" +
  encodeURIComponent(pageTitle) +
  "/html";

const scrubDocument = (doc) => {
  doc.querySelectorAll(".navbox").forEach((el) => el.remove());
};

const normalizeHref = (href) => {
  if (!href) return "";
  if (href.startsWith("./")) {
    return "/wiki/" + href.slice(2);
  }
  if (href.startsWith("../")) {
    return "/wiki/" + href.replace(/^\.\.\/+/, "");
  }
  try {
    const url = new URL(href, "https://en.wikipedia.org");
    const path = url.pathname + url.search;
    return path.startsWith("/wiki/") ? path : href;
  } catch {
    return href;
  }
};

const pageTitleFromHref = (href) => {
  if (!href) return "";
  if (href.startsWith("/wiki/")) {
    return decodeURIComponent(href.slice(6));
  }
  if (href.startsWith("./")) {
    return decodeURIComponent(href.slice(2));
  }
  if (href.startsWith("../")) {
    return decodeURIComponent(href.replace(/^\.\.\/+/, ""));
  }
  try {
    const url = new URL(href);
    if (url.pathname.startsWith("/wiki/")) {
      return decodeURIComponent(url.pathname.slice(6));
    }
  } catch {}
  return decodeURIComponent(href);
};

const levelFromTag = (tag) => {
  switch (tag) {
    case "H2":
      return 2;
    case "H3":
      return 3;
    case "H4":
      return 4;
    case "H5":
      return 5;
    default:
      return 6;
  }
};

const levelFromText = (text) => {
  const match = text.match(/\b(?:[Ll]evel|L)\s*(\d)\b/);
  if (!match) return null;
  return Number(match[1]);
};

const levelFromHref = (href) => {
  const match = href.match(/\/Level\/(\d)\b/);
  if (!match) return null;
  return Number(match[1]);
};

const isLevelLink = (anchor) => {
  const href = normalizeHref(anchor.getAttribute("href") || "");
  const hrefLevel = levelFromHref(href);
  if (hrefLevel) return true;
  const textLevel = levelFromText((anchor.textContent || "").trim());
  return Boolean(textLevel);
};

const levelFromNode = (node) => {
  const anchors = Array.from(node.querySelectorAll("a[href]"));
  for (const anchor of anchors) {
    const href = normalizeHref(anchor.getAttribute("href") || "");
    const hrefLevel = levelFromHref(href);
    if (hrefLevel) return hrefLevel;
    const textLevel = levelFromText((anchor.textContent || "").trim());
    if (textLevel) return textLevel;
  }
  return levelFromText((node.textContent || "").trim());
};

const stripLevelSuffix = (title) =>
  title
    .replace(/\s*[–-]\s*\b[Ll]evel\s*\d\b\s*$/, "")
    .replace(/\s*\(?\b[Ll]evel\s*\d\b\)?\s*$/, "")
    .trim();

const parseSections = (doc) => {
  const nodes = doc.querySelectorAll("h2,h3,h4,h5,ul,ol,p");

  const sections = [];
  const stack = [];

  const pushSection = (section) => {
    stack.push(section);
  };

  const popSection = () => {
    stack.pop();
  };

  const currentSection = () => stack[stack.length - 1];

  nodes.forEach((el) => {
    const tag = el.tagName;

    if (tag === "H2" || tag === "H3" || tag === "H4" || tag === "H5") {
      const level = levelFromTag(tag);
      const title = (el.textContent || "").trim();
      const section = {
        title,
        level,
        items: [],
        children: [],
      };

      while (stack.length > 0 && currentSection().level >= level) {
        popSection();
      }

      const parent = currentSection();
      if (parent) {
        parent.children.push(section);
      } else {
        sections.push(section);
      }

      pushSection(section);
      return;
    }

    if (tag === "UL" || tag === "OL" || tag === "P") {
      const current = currentSection();
      if (!current) {
        return;
      }

      if (tag === "P") {
        const links = el.querySelectorAll("a[href]");
        links.forEach((a) => {
          const title = (a.textContent || "").trim();
          const href = normalizeHref(a.getAttribute("href"));
          if (!title || !href) return;
          current.items.push({ title, href, level: 5, children: [] });
        });
        return;
      }

      const pickPrimaryLink = (li) => {
        const anchors = Array.from(li.querySelectorAll("a[href]"));
        const primary = anchors.find((a) => {
          const rawHref = a.getAttribute("href") || "";
          const href = normalizeHref(rawHref);
          return (
            href.startsWith("/wiki/") &&
            !a.classList.contains("mw-file-description") &&
            !href.startsWith("/wiki/File:") &&
            !isLevelLink(a)
          );
        });
        return primary || null;
      };

      const itemFromLi = (li) => {
        const clone = li.cloneNode(true);
        clone.querySelectorAll("ul,ol").forEach((child) => child.remove());
        const rawLevel = levelFromNode(clone);
        const primaryLink = pickPrimaryLink(li);
        const titleFromLink = primaryLink
          ? (primaryLink.textContent || "").trim()
          : "";
        let title = titleFromLink || (clone.textContent || "").trim();
        if (rawLevel) {
          title = stripLevelSuffix(title);
        }
        if (!title) return null;
        const href = primaryLink
          ? normalizeHref(primaryLink.getAttribute("href"))
          : "";
        const childLists = li.querySelectorAll(":scope > ul, :scope > ol");
        const children = [];
        childLists.forEach((list) => {
          list.querySelectorAll(":scope > li").forEach((childLi) => {
            const childItem = itemFromLi(childLi);
            if (childItem) children.push(childItem);
          });
        });
        const level = rawLevel || 5;
        return { title, href: href || "", level, children };
      };

      const listItems = el.querySelectorAll(":scope > li");
      listItems.forEach((li) => {
        const item = itemFromLi(li);
        if (item) current.items.push(item);
      });
    }
  });

  return sections;
};

const fetchSectionsFromHref = async (href) => {
  if (!href) return [];
  const pageTitle = pageTitleFromHref(href);
  if (!pageTitle) return [];
  const html = await fetch(buildPageHtmlUrl(pageTitle)).then((r) => r.text());
  const doc = new DOMParser().parseFromString(html, "text/html");
  scrubDocument(doc);
  return parseSections(doc);
};

const parseDivisions = (doc) => {
  const table = doc.querySelector("table.wikitable");
  if (!table) return [];
  const rows = Array.from(table.querySelectorAll("tbody > tr"));
  const divisions = [];
  const stack = [];

  rows.forEach((row) => {
    if (row.classList.contains("sortbottom")) return;
    const cell = row.querySelector("td");
    if (!cell) return;
    const cellText = cell.textContent || "";
    const title = cellText.replace(/^\u00A0+/, "").trim();
    if (!title) return;
    if (title.toLowerCase() === "total") return;
    const link = cell.querySelector("a[href]");
    const href = link ? normalizeHref(link.getAttribute("href")) : "";
    const leading = cellText.match(/^\u00A0+/);
    const indent = leading ? Math.floor(leading[0].length / 3) : 0;
    const division = {
      title,
      href,
      sections: [],
      children: [],
    };

    while (stack.length > indent) {
      stack.pop();
    }

    if (stack.length === 0) {
      divisions.push(division);
    } else {
      stack[stack.length - 1].children.push(division);
    }

    stack.push(division);
  });

  return divisions;
};

const hydrateDivision = async (division) => {
  const sections = division.href ? await fetchSectionsFromHref(division.href) : [];
  const children = await Promise.all(
    division.children.map((child) => hydrateDivision(child)),
  );
  return { ...division, sections, children };
};

export const fetchDivisions = async () => {
  const html = await fetch(divisionsHtmlUrl).then((r) => r.text());
  const doc = new DOMParser().parseFromString(html, "text/html");
  scrubDocument(doc);
  const divisions = parseDivisions(doc);
  return Promise.all(divisions.map((division) => hydrateDivision(division)));
};
