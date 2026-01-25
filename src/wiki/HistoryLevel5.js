export const pageTitle = "Wikipedia:Vital_articles/Level/5/History";
export const pageHtmlUrl =
  "https://en.wikipedia.org/w/rest.php/v1/page/" +
  encodeURIComponent(pageTitle) +
  "/html";

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

export const fetchSections = async () => {
  const html = await fetch(pageHtmlUrl).then((r) => r.text());

  const doc = new DOMParser().parseFromString(html, "text/html");

  const nodes = doc.querySelectorAll("h2,h3,h4,h5,ul,ol,p");

  const sections = [];
  const stack = [];
  const debug = false;

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
        if (debug) {
          const anchors = Array.from(li.querySelectorAll("a[href]")).map(
            (a) => ({
              href: a.getAttribute("href"),
              text: (a.textContent || "").trim(),
              classes: a.getAttribute("class"),
            }),
          );
          console.log(
            "li",
            (li.textContent || "").trim().slice(0, 120),
            anchors,
          );
        }
        const item = itemFromLi(li);
        if (item) current.items.push(item);
      });
    }
  });

  return sections;
};
