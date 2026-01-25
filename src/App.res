open HistoryLevel5

@react.component
let make = () => {
  let (sections, setSections) = React.useState(() => None)
  let (error, setError) = React.useState(() => None)
  let (filterText, setFilterText) = React.useState(() => "")
  let (expanded, setExpanded) = React.useState(() => Belt.Set.String.empty)
  let (expandedItems, setExpandedItems) = React.useState(() => Belt.Set.String.empty)

  let rec collectKeys = (sections, prefix, acc) =>
    sections->Belt.Array.reduce(acc, (acc, section) => {
      let key = prefix ++ "/" ++ section.title
      let nextAcc = acc->Belt.Set.String.add(key)
      collectKeys(section.children, key, nextAcc)
    })

  let rec collectItemKeys = (items: array<HistoryLevel5.link>, prefix, acc) =>
    items->Belt.Array.reduce(acc, (acc, item) => {
      let key = prefix ++ "/item/" ++ item.title
      let nextAcc = acc->Belt.Set.String.add(key)
      collectItemKeys(item.children, key, nextAcc)
    })

  let rec collectAllItemKeys = (sections, prefix, acc) =>
    sections->Belt.Array.reduce(acc, (acc, section) => {
      let nextAcc = collectItemKeys(section.items, prefix ++ "/" ++ section.title, acc)
      collectAllItemKeys(section.children, prefix ++ "/" ++ section.title, nextAcc)
    })

  React.useEffect0(() => {
    HistoryLevel5.fetchSections()
    ->Promise.then(sections => {
      setSections(_ => Some(sections))
      setExpanded(_ => collectKeys(sections, "root", Belt.Set.String.empty))
      setExpandedItems(_ => collectAllItemKeys(sections, "root", Belt.Set.String.empty))
      Promise.resolve()
    })
    ->Promise.catch(_ => {
      setError(_ => Some("Failed to load the History page."))
      Promise.resolve()
    })
    ->ignore

    None
  })

  let rec renderLink = (link: HistoryLevel5.link, keyPrefix) => {
    let hasHref = link.href != ""
    let href = "https://en.wikipedia.org" ++ link.href
    let key = keyPrefix ++ "/item/" ++ link.title
    let isOpen = expandedItems->Belt.Set.String.has(key)
    <li key={link.title} className="my-1">
      <div className="flex items-center gap-2">
        {switch hasHref {
        | true =>
          <a
            href
            target="_blank"
            rel="noreferrer"
            className="text-sky-700 hover:text-sky-900 underline decoration-sky-300"
          >
            {React.string(link.title)}
          </a>
        | false =>
          <span className="text-stone-700">{React.string(link.title)}</span>
        }}
        {switch link.children->Belt.Array.length > 0 {
        | true =>
          <button
            className="rounded border border-stone-200 px-2 py-1 text-[10px] font-semibold text-stone-600 hover:border-stone-300"
            onClick={_ => {
              setExpandedItems(prev =>
                prev->Belt.Set.String.has(key)
                  ? prev->Belt.Set.String.remove(key)
                  : prev->Belt.Set.String.add(key)
              )
            }}
          >
            {React.string(isOpen ? "−" : "+")}
          </button>
        | false => React.null
        }}
      </div>
      {switch link.children->Belt.Array.length > 0 {
      | true =>
        {switch isOpen {
        | true =>
          <ul className="ml-5 mt-2 list-disc text-sm text-stone-600">
            {link.children->Belt.Array.map(child => renderLink(child, key))->React.array}
          </ul>
        | false => React.null
        }}
      | false => React.null
      }}
    </li>
  }

  let query = filterText->String.toLowerCase

  let rec filterSection = (section: HistoryLevel5.section) => {
    if query == "" {
      Some(section)
    } else {
      let titleMatch = section.title->String.toLowerCase->String.includes(query)
      if titleMatch {
        Some(section)
      } else {
        let rec filterItem = (item: HistoryLevel5.link) => {
          let itemMatch = item.title->String.toLowerCase->String.includes(query)
          let children = item.children->Belt.Array.keepMap(filterItem)
          if itemMatch {
            Some({...item, children})
          } else {
            switch children->Belt.Array.length > 0 {
            | true => Some({...item, children})
            | false => None
            }
          }
        }

        let items = section.items->Belt.Array.keepMap(filterItem)
        let children = section.children->Belt.Array.keepMap(filterSection)
        if items->Belt.Array.length > 0 || children->Belt.Array.length > 0 {
          Some({...section, items, children})
        } else {
          None
        }
      }
    }
  }

  let toggleExpanded = key => {
    setExpanded(prev =>
      prev->Belt.Set.String.has(key)
        ? prev->Belt.Set.String.remove(key)
        : prev->Belt.Set.String.add(key)
    )
  }

  let expandAll = () =>
    switch sections {
    | Some(sections) =>
      setExpanded(_ => collectKeys(sections, "root", Belt.Set.String.empty))
      setExpandedItems(_ => collectAllItemKeys(sections, "root", Belt.Set.String.empty))
    | None => ()
    }

  let collapseAll = () => {
    setExpanded(_ => Belt.Set.String.empty)
    setExpandedItems(_ => Belt.Set.String.empty)
  }

  let rec renderSection = (section, keyPrefix) => {
    let key = keyPrefix ++ "/" ++ section.title
    let isOpen = expanded->Belt.Set.String.has(key)

    <div className="my-4">
      <div className="flex items-center gap-2 text-lg font-semibold text-stone-800">
        <span> {React.string(section.title)} </span>
        <button
          className="rounded border border-stone-200 px-2 py-1 text-xs font-semibold text-stone-600 hover:border-stone-300"
          onClick={_ => toggleExpanded(key)}
        >
          {React.string(isOpen ? "−" : "+")}
        </button>
      </div>
      {switch isOpen {
      | true =>
        <div>
          {switch section.items->Belt.Array.length > 0 {
          | true =>
            <ul className="ml-8 mt-2 list-disc text-sm text-stone-700">
              {section.items->Belt.Array.map(item => renderLink(item, key))->React.array}
            </ul>
          | false => React.null
          }}
          {switch section.children->Belt.Array.length > 0 {
          | true =>
            <div className="ml-6 mt-3 border-l border-stone-200 pl-4">
              {section.children->Belt.Array.map(child => renderSection(child, key))->React.array}
            </div>
          | false => React.null
          }}
        </div>
      | false => React.null
      }}
    </div>
  }

  <div className="mx-auto max-w-5xl px-6 py-10">
    <div className="mb-8">
      <p className="text-sm uppercase tracking-widest text-stone-500">
        {React.string("Wikipedia Vital Articles")}
      </p>
      <h1 className="mt-2 text-4xl font-semibold text-stone-900">
        {React.string("Level 5 • History")}
      </h1>
      <p className="mt-2 text-stone-600">
        {React.string("Browse and expand sections from the Vital Articles list.")}
      </p>
      <div className="mt-4 flex flex-col gap-3 md:flex-row md:items-center">
        <input
          value={filterText}
          placeholder="Filter sections or articles"
          onChange={event => setFilterText(_ => (event->ReactEvent.Form.target)["value"])}
          className="w-full rounded-xl border border-stone-200 bg-white px-4 py-3 text-sm text-stone-700 shadow-sm focus:border-sky-300 focus:outline-none"
        />
        <div className="flex gap-2">
          <button
            className="rounded-lg border border-stone-200 bg-white px-3 py-2 text-xs font-semibold uppercase tracking-wider text-stone-600 hover:border-stone-300"
            onClick={_ => expandAll()}
          >
            {React.string("Expand all")}
          </button>
          <button
            className="rounded-lg border border-stone-200 bg-white px-3 py-2 text-xs font-semibold uppercase tracking-wider text-stone-600 hover:border-stone-300"
            onClick={_ => collapseAll()}
          >
            {React.string("Collapse all")}
          </button>
        </div>
      </div>
    </div>
    {switch (error, sections) {
    | (Some(message), _) =>
      <div className="rounded border border-red-200 bg-red-50 px-4 py-3 text-sm text-red-700">
        {React.string(message)}
      </div>
    | (_, None) =>
      <div className="text-sm text-stone-500"> {React.string("Loading sections…")} </div>
    | (_, Some(sections)) =>
      <div className="rounded-2xl border border-stone-200 bg-white px-6 py-4 shadow-sm">
        {sections
        ->Belt.Array.keepMap(filterSection)
        ->Belt.Array.map(section => renderSection(section, "root"))
        ->React.array}
      </div>
    }}
  </div>
}
