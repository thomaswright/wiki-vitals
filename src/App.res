open VitalLevel5

module Data = {
  type response
  @val external fetch: string => promise<response> = "fetch"
  @get external ok: response => bool = "ok"
  type promiseReturn = {divisions: array<VitalLevel5.division>}
  @send external json: response => promise<promiseReturn> = "json"
}

@react.component
let make = () => {
  let levelMatchesSelection = (selectedLevels, levelOpt: option<int>) =>
    switch levelOpt {
    | Some(level) => selectedLevels->Belt.Set.Int.has(level)
    | None => false
    }
  let (divisions, setDivisions) = React.useState(() => None)
  let (error, setError) = React.useState(() => None)
  let (filterText, setFilterText) = React.useState(() => "")
  let (expanded, setExpanded) = React.useState(() => Belt.Set.String.empty)
  let (expandedItems, setExpandedItems) = React.useState(() => Belt.Set.String.empty)
  let (selectedLevels, setSelectedLevels) = React.useState(() =>
    Belt.Set.Int.fromArray([1, 2, 3, 4, 5])
  )
  let (showHeaders, setShowHeaders) = React.useState(() => true)
  Console.log(divisions)
  let rec collectSectionKeys = (sections: array<section>, prefix, acc) =>
    sections->Belt.Array.reduce(acc, (acc, section) => {
      let key = prefix ++ "/section/" ++ section.title
      let nextAcc = acc->Belt.Set.String.add(key)
      collectSectionKeys(section.children, key, nextAcc)
    })

  let rec collectItemKeys = (items: array<VitalLevel5.link>, prefix, acc) =>
    items->Belt.Array.reduce(acc, (acc, item) => {
      let key = prefix ++ "/item/" ++ item.title
      let nextAcc = acc->Belt.Set.String.add(key)
      collectItemKeys(item.children, key, nextAcc)
    })

  let rec collectAllItemKeys = (sections: array<section>, prefix, acc) =>
    sections->Belt.Array.reduce(acc, (acc, section) => {
      let key = prefix ++ "/section/" ++ section.title

      let nextAcc = collectItemKeys(section.items, key, acc)
      collectAllItemKeys(section.children, key, nextAcc)
    })

  let rec collectDivisionKeys = (divisions: array<division>, prefix, acc) =>
    divisions->Belt.Array.reduce(acc, (acc, division: division) => {
      let key = prefix ++ "/division/" ++ division.title
      let nextAcc = acc->Belt.Set.String.add(key)
      let nextSections = collectSectionKeys(division.sections, key, nextAcc)
      collectDivisionKeys(division.children, key, nextSections)
    })

  let rec collectDivisionItemKeys = (divisions, prefix, acc) =>
    divisions->Belt.Array.reduce(acc, (acc, division) => {
      let key = prefix ++ "/division/" ++ division.title
      let nextAcc = collectAllItemKeys(division.sections, key, acc)
      collectDivisionItemKeys(division.children, key, nextAcc)
    })

  React.useEffect0(() => {
    Data.fetch("/vitals-level5.json")
    ->Promise.then(response =>
      if Data.ok(response) {
        Data.json(response)
      } else {
        Promise.reject(JsExn.anyToExnInternal("Failed to load vitals-level5.json"))
      }
    )
    ->Promise.then(payload => {
      let divisions = payload.divisions
      setDivisions(_ => Some(divisions))
      setExpanded(_ => collectDivisionKeys(divisions, "root", Belt.Set.String.empty))
      setExpandedItems(_ => collectDivisionItemKeys(divisions, "root", Belt.Set.String.empty))
      Promise.resolve()
    })
    ->Promise.catch(_ => {
      setError(_ => Some("Failed to load the Vital Articles list."))
      Promise.resolve()
    })
    ->ignore

    None
  })

  let rec renderLink = (link: VitalLevel5.link, keyPrefix) => {
    let hasHref = link.href != ""
    let href = "https://en.wikipedia.org" ++ link.href
    let key = keyPrefix ++ "/item/" ++ link.title
    let isOpen = expandedItems->Belt.Set.String.has(key)

    <li key={link.title} className="my-1">
      <div className="flex items-center gap-2">
        <span>
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
          | false => <span className="text-stone-700"> {React.string(link.title)} </span>
          }}
          {switch link.level {
          | None => React.null
          | Some(level) =>
            level == 5
              ? React.null
              : <span className="ml-1"> {React.string(Int.toString(level))} </span>
          }}
        </span>

        {switch link.children->Belt.Array.length > 0 {
        | true =>
          <button
            className="rounded border-stone-200 px-2 py-1 text-[10px] font-semibold text-stone-600 hover:border-stone-300"
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
        switch isOpen {
        | true =>
          <ul className="ml-5 mt-2 list-disc text-sm text-stone-600">
            {link.children->Belt.Array.map(child => renderLink(child, key))->React.array}
          </ul>
        | false => React.null
        }
      | false => React.null
      }}
    </li>
  }

  let query = filterText->String.toLowerCase

  let rec filterSection = (section: VitalLevel5.section) => {
    let titleMatch = section.title->String.toLowerCase->String.includes(query)

    let rec filterItem = (item: VitalLevel5.link) => {
      let levelMatch = levelMatchesSelection(selectedLevels, item.level)
      let itemMatch = item.title->String.toLowerCase->String.includes(query)
      let children = item.children->Belt.Array.keepMap(filterItem)
      if levelMatch && (query == "" || itemMatch) {
        Some({...item, children})
      } else if children->Belt.Array.length > 0 {
        Some({...item, children})
      } else {
        None
      }
    }

    let items = section.items->Belt.Array.keepMap(filterItem)
    let children = section.children->Belt.Array.keepMap(filterSection)

    if query == "" {
      if items->Belt.Array.length > 0 || children->Belt.Array.length > 0 {
        Some({...section, items, children})
      } else {
        None
      }
    } else if titleMatch || items->Belt.Array.length > 0 || children->Belt.Array.length > 0 {
      Some({...section, items, children})
    } else {
      None
    }
  }

  let rec filterDivision = (division: VitalLevel5.division) => {
    let titleMatch = division.title->String.toLowerCase->String.includes(query)
    let sections = division.sections->Belt.Array.keepMap(filterSection)
    let children = division.children->Belt.Array.keepMap(filterDivision)

    if query == "" {
      if sections->Belt.Array.length > 0 || children->Belt.Array.length > 0 {
        Some({...division, sections, children})
      } else {
        None
      }
    } else if titleMatch || sections->Belt.Array.length > 0 || children->Belt.Array.length > 0 {
      Some({...division, sections, children})
    } else {
      None
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
    switch divisions {
    | Some(divisions) =>
      setExpanded(_ => collectDivisionKeys(divisions, "root", Belt.Set.String.empty))
      setExpandedItems(_ => collectDivisionItemKeys(divisions, "root", Belt.Set.String.empty))
    | None => ()
    }

  let collapseAll = () => {
    setExpanded(_ => Belt.Set.String.empty)
    setExpandedItems(_ => Belt.Set.String.empty)
  }

  let rec renderSection = (section: section, keyPrefix) => {
    let key = keyPrefix ++ "/section/" ++ section.title
    let isOpen = expanded->Belt.Set.String.has(key)

    showHeaders
      ? <div className="ml-4">
          <div className="flex items-center gap-2 font-semibold text-stone-800">
            <span> {React.string(section.title)} </span>
            <button
              className="rounded border-stone-200 px-2 py-1 text-xs font-semibold text-stone-600 hover:border-stone-300"
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
                <ul className="ml-8 list-disc text-sm text-stone-700">
                  {section.items->Belt.Array.map(item => renderLink(item, key))->React.array}
                </ul>
              | false => React.null
              }}
              {switch section.children->Belt.Array.length > 0 {
              | true =>
                <div className=" border-stone-200">
                  {section.children
                  ->Belt.Array.map(child => renderSection(child, key))
                  ->React.array}
                </div>
              | false => React.null
              }}
            </div>
          | false => React.null
          }}
        </div>
      : <div>
          {switch section.items->Belt.Array.length > 0 {
          | true =>
            <ul className="ml-4 list-disc text-sm text-stone-700">
              {section.items->Belt.Array.map(item => renderLink(item, key))->React.array}
            </ul>
          | false => React.null
          }}
          {switch section.children->Belt.Array.length > 0 {
          | true =>
            <div className=" border-stone-200">
              {section.children->Belt.Array.map(child => renderSection(child, key))->React.array}
            </div>
          | false => React.null
          }}
        </div>
  }

  let rec renderDivision = (division, keyPrefix) => {
    let key = keyPrefix ++ "/division/" ++ division.title
    let isOpen = expanded->Belt.Set.String.has(key)

    showHeaders
      ? <div className="ml-2">
          <div className="flex items-center gap-2 font-semibold text-stone-900">
            <span> {React.string(division.title)} </span>
            <button
              className="rounded border-stone-200 px-2 py-1 text-xs font-semibold text-stone-600 hover:border-stone-300"
              onClick={_ => toggleExpanded(key)}
            >
              {React.string(isOpen ? "−" : "+")}
            </button>
          </div>
          {switch isOpen {
          | true =>
            <div>
              {switch division.sections->Belt.Array.length > 0 {
              | true =>
                <ul className="ml-6 list-disc text-sm text-stone-700">
                  {division.sections
                  ->Belt.Array.map(section => renderSection(section, key))
                  ->React.array}
                </ul>
              | false => React.null
              }}
              {switch division.children->Belt.Array.length > 0 {
              | true =>
                <div className="border-stone-200">
                  {division.children
                  ->Belt.Array.map(child => renderDivision(child, key))
                  ->React.array}
                </div>
              | false => React.null
              }}
            </div>
          | false => React.null
          }}
        </div>
      : <div>
          {switch division.sections->Belt.Array.length > 0 {
          | true =>
            <ul className="ml-4 list-disc text-sm text-stone-700">
              {division.sections
              ->Belt.Array.map(section => renderSection(section, key))
              ->React.array}
            </ul>
          | false => React.null
          }}
          {switch division.children->Belt.Array.length > 0 {
          | true =>
            <div className="border-stone-200">
              {division.children
              ->Belt.Array.map(child => renderDivision(child, key))
              ->React.array}
            </div>
          | false => React.null
          }}
        </div>
  }

  <div className="mx-auto max-w-5xl p-6">
    <div className="mb-8">
      <p className="text-sm uppercase tracking-widest text-stone-500">
        {React.string("Wikipedia Vital Articles")}
      </p>
      <h1 className="mt-2 text-4xl font-semibold text-stone-900">
        {React.string("Level 5 • Vital Articles")}
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
        <div className="flex flex-wrap gap-2">
          {[1, 2, 3, 4, 5]
          ->Belt.Array.map(level => {
            let isSelected = selectedLevels->Belt.Set.Int.has(level)
            <button
              key={Int.toString(level)}
              className={"rounded-lg border px-3 py-2 text-xs font-semibold uppercase tracking-wider " ++ (
                isSelected
                  ? "border-sky-300 bg-sky-50 text-sky-800"
                  : "border-stone-200 bg-white text-stone-600 hover:border-stone-300"
              )}
              onClick={_ =>
                setSelectedLevels(prev =>
                  prev->Belt.Set.Int.has(level)
                    ? prev->Belt.Set.Int.remove(level)
                    : prev->Belt.Set.Int.add(level)
                )}
            >
              {React.string("Level " ++ Int.toString(level))}
            </button>
          })
          ->React.array}
          <button
            className={"rounded-lg border px-3 py-2 text-xs font-semibold uppercase tracking-wider " ++ (
              showHeaders
                ? "border-stone-200 bg-white text-stone-600 hover:border-stone-300"
                : "border-sky-300 bg-sky-50 text-sky-800"
            )}
            onClick={_ => setShowHeaders(prev => !prev)}
          >
            {React.string(showHeaders ? "Hide headers" : "Show headers")}
          </button>
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
    {switch (error, divisions) {
    | (Some(message), _) =>
      <div className="rounded border border-red-200 bg-red-50 p-3 text-sm text-red-700">
        {React.string(message)}
      </div>
    | (_, None) =>
      <div className="text-sm text-stone-500"> {React.string("Loading divisions…")} </div>
    | (_, Some(divisions)) =>
      <div className="  bg-white">
        {divisions
        ->Belt.Array.keepMap(filterDivision)
        ->Belt.Array.map(division => renderDivision(division, "root"))
        ->React.array}
      </div>
    }}
  </div>
}
