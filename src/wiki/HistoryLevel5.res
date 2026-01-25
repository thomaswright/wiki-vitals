type rec link = {
  title: string,
  href: string,
  children: array<link>,
}

type rec section = {
  title: string,
  level: int,
  items: array<link>,
  children: array<section>,
}

@module("./HistoryLevel5.js") external pageTitle: string = "pageTitle"
@module("./HistoryLevel5.js") external pageHtmlUrl: string = "pageHtmlUrl"
@module("./HistoryLevel5.js")
external fetchSections: unit => promise<array<section>> = "fetchSections"
