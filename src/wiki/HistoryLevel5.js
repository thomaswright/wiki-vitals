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
        const links = el.querySelectorAll("a[href^='/wiki/']");
        links.forEach((a) => {
          const title = (a.textContent || "").trim();
          const href = a.getAttribute("href");
          if (!title || !href) return;
          current.items.push({ title, href, children: [] });
        });
        return;
      }

      const itemFromLi = (li) => {
        const clone = li.cloneNode(true);
        clone.querySelectorAll("ul,ol").forEach((child) => child.remove());
        const title = (clone.textContent || "").trim();
        if (!title) return null;
        const link = li.querySelector("a[href^='/wiki/']");
        const href = link ? link.getAttribute("href") : "";
        const childLists = li.querySelectorAll(":scope > ul, :scope > ol");
        const children = [];
        childLists.forEach((list) => {
          list.querySelectorAll(":scope > li").forEach((childLi) => {
            const childItem = itemFromLi(childLi);
            if (childItem) children.push(childItem);
          });
        });
        return { title, href: href || "", children };
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
