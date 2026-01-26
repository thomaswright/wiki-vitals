open VitalLevel5

let divisionKey = (prefix, division: division) => {
  let slug = division.href != "" ? division.href : division.title
  prefix ++ "/division/" ++ slug
}

let sectionKey = (prefix, section: section) =>
  prefix ++ "/section/" ++ section.title ++ "#" ++ Int.toString(section.level)

let itemKey = (prefix, link: VitalLevel5.link) => {
  let slug = link.href != "" ? link.href : link.title
  prefix ++ "/item/" ++ slug
}

let rec collectSectionKeys = (sections: array<section>, prefix, acc) =>
  sections->Belt.Array.reduce(acc, (acc, section) => {
    let key = sectionKey(prefix, section)
    let nextAcc = acc->Belt.Set.String.add(key)
    collectSectionKeys(section.children, key, nextAcc)
  })

let rec collectItemKeys = (items: array<VitalLevel5.link>, prefix, acc) =>
  items->Belt.Array.reduce(acc, (acc, item) => {
    let key = itemKey(prefix, item)
    let nextAcc = acc->Belt.Set.String.add(key)
    collectItemKeys(item.children, key, nextAcc)
  })

let rec collectAllItemKeys = (sections: array<section>, prefix, acc) =>
  sections->Belt.Array.reduce(acc, (acc, section) => {
    let key = sectionKey(prefix, section)

    let nextAcc = collectItemKeys(section.items, key, acc)
    collectAllItemKeys(section.children, key, nextAcc)
  })

let rec collectDivisionKeys = (divisions: array<division>, prefix, acc) =>
  divisions->Belt.Array.reduce(acc, (acc, division: division) => {
    let key = divisionKey(prefix, division)
    let nextAcc = acc->Belt.Set.String.add(key)
    let nextSections = collectSectionKeys(division.sections, key, nextAcc)
    collectDivisionKeys(division.children, key, nextSections)
  })

let rec collectDivisionItemKeys = (divisions, prefix, acc) =>
  divisions->Belt.Array.reduce(acc, (acc, division) => {
    let key = divisionKey(prefix, division)
    let nextAcc = collectAllItemKeys(division.sections, key, acc)
    collectDivisionItemKeys(division.children, key, nextAcc)
  })

module ListView = {
  @react.component
  let make = (
    ~error,
    ~divisions,
    ~debouncedFilterText,
    ~includeChildrenOnMatch,
    ~expanded,
    ~expandedItems,
    ~selectedLevels,
    ~showHeaders,
    ~focusedDivisionKey,
    ~showAllDivisions,
    ~setExpanded,
    ~setExpandedItems,
    ~setFocusedDivisionKey,
    ~setShowAllDivisions,
  ) => {
    let levelMatchesSelection = (selectedLevels, levelOpt: option<int>) =>
      switch levelOpt {
      | Some(level) => selectedLevels->Belt.Set.Int.has(level)
      | None => false
      }

    let query = debouncedFilterText->String.toLowerCase
    let isAllLevelsSelected = selectedLevels->Belt.Set.Int.size == 5

    let itemMatches = (item: VitalLevel5.link) =>
      levelMatchesSelection(selectedLevels, item.level) &&
      item.title->String.toLowerCase->String.includes(query)

    let rec itemHasMatch = (item: VitalLevel5.link) =>
      itemMatches(item) || item.children->Belt.Array.some(child => itemHasMatch(child))

    let sectionMatches = (section: VitalLevel5.section) =>
      section.title->String.toLowerCase->String.includes(query)

    let rec sectionHasMatch = (section: VitalLevel5.section) =>
      sectionMatches(section) ||
      section.items->Belt.Array.some(item => itemHasMatch(item)) ||
      section.children->Belt.Array.some(child => sectionHasMatch(child))

    let divisionMatches = (division: VitalLevel5.division) =>
      division.title->String.toLowerCase->String.includes(query)

    let rec divisionHasMatch = (division: VitalLevel5.division) =>
      divisionMatches(division) ||
      division.sections->Belt.Array.some(section => sectionHasMatch(section)) ||
      division.children->Belt.Array.some(child => divisionHasMatch(child))

    let rec renderLink = (link: VitalLevel5.link, keyPrefix) => {
      let hasHref = link.href != ""
      let href = "https://en.wikipedia.org" ++ link.href
      let key = itemKey(keyPrefix, link)
      let isOpen =
        expandedItems->Belt.Set.String.has(key) ||
          (includeChildrenOnMatch && query != "" && itemHasMatch(link))

      <li key={key} className="my-1">
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

    let rec filterSection = (section: VitalLevel5.section) => {
      let titleMatch = sectionMatches(section)

      let rec filterItem = (item: VitalLevel5.link) => {
        let levelMatch = levelMatchesSelection(selectedLevels, item.level)
        let itemMatch = item.title->String.toLowerCase->String.includes(query)
        let children = if includeChildrenOnMatch && query != "" && itemMatch {
          item.children
        } else {
          item.children->Belt.Array.keepMap(filterItem)
        }
        if levelMatch && (query == "" || itemMatch) {
          Some({...item, children})
        } else if children->Belt.Array.length > 0 {
          Some({...item, children})
        } else {
          None
        }
      }

      let items = if includeChildrenOnMatch && query != "" && titleMatch {
        section.items
      } else {
        section.items->Belt.Array.keepMap(filterItem)
      }
      let children = if includeChildrenOnMatch && query != "" && titleMatch {
        section.children
      } else {
        section.children->Belt.Array.keepMap(filterSection)
      }

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
      let titleMatch = divisionMatches(division)
      let sections = if includeChildrenOnMatch && query != "" && titleMatch {
        division.sections
      } else {
        division.sections->Belt.Array.keepMap(filterSection)
      }
      let children = if includeChildrenOnMatch && query != "" && titleMatch {
        division.children
      } else {
        division.children->Belt.Array.keepMap(filterDivision)
      }

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

    let rec renderSection = (section: section, keyPrefix, depth) => {
      let key = sectionKey(keyPrefix, section)
      let isOpen =
        expanded->Belt.Set.String.has(key) ||
          (includeChildrenOnMatch && query != "" && sectionHasMatch(section))

      showHeaders
        ? <div key className={depth == 0 ? "" : "ml-4"}>
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
                    ->Belt.Array.map(child => renderSection(child, key, depth + 1))
                    ->React.array}
                  </div>
                | false => React.null
                }}
              </div>
            | false => React.null
            }}
          </div>
        : <div key>
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
                {section.children
                ->Belt.Array.map(child => renderSection(child, key, depth + 1))
                ->React.array}
              </div>
            | false => React.null
            }}
          </div>
    }

    let rec renderDivision = (division, keyPrefix, depth) => {
      let key = divisionKey(keyPrefix, division)
      let isOpen =
        expanded->Belt.Set.String.has(key) ||
          (includeChildrenOnMatch && query != "" && divisionHasMatch(division))

      showHeaders
        ? <div key className={depth == 0 ? "" : "ml-4"}>
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
                  <ul className="ml-2 list-disc text-sm text-stone-700">
                    {division.sections
                    ->Belt.Array.map(section => renderSection(section, key, 0))
                    ->React.array}
                  </ul>
                | false => React.null
                }}
                {switch division.children->Belt.Array.length > 0 {
                | true =>
                  <div className="border-stone-200">
                    {division.children
                    ->Belt.Array.map(child => renderDivision(child, key, depth + 1))
                    ->React.array}
                  </div>
                | false => React.null
                }}
              </div>
            | false => React.null
            }}
          </div>
        : <div key>
            {switch division.sections->Belt.Array.length > 0 {
            | true =>
              <ul className="ml-4 list-disc text-sm text-stone-700">
                {division.sections
                ->Belt.Array.map(section => renderSection(section, key, 0))
                ->React.array}
              </ul>
            | false => React.null
            }}
            {switch division.children->Belt.Array.length > 0 {
            | true =>
              <div className="border-stone-200">
                {division.children
                ->Belt.Array.map(child => renderDivision(child, key, depth + 1))
                ->React.array}
              </div>
            | false => React.null
            }}
          </div>
    }

    let renderFocusedDivision = (division, keyPrefix) => {
      let key = divisionKey(keyPrefix, division)

      <div>
        {switch division.sections->Belt.Array.length > 0 {
        | true =>
          <ul className={"list-disc text-sm text-stone-700"}>
            {division.sections
            ->Belt.Array.map(section => renderSection(section, key, 0))
            ->React.array}
          </ul>
        | false => React.null
        }}
        {switch division.children->Belt.Array.length > 0 {
        | true =>
          <div className="border-stone-200">
            {division.children
            ->Belt.Array.map(child => renderDivision(child, key, 0))
            ->React.array}
          </div>
        | false => React.null
        }}
      </div>
    }

    let rec renderDivisionNav = (division, keyPrefix) => {
      let key = divisionKey(keyPrefix, division)
      let hasChildren = division.children->Belt.Array.length > 0

      <li key={key} className="my-1  text-sm text-stone-800">
        <div className="flex items-center gap-2">
          <button
            className="text-left text-sm font-semibold text-stone-800 hover:text-sky-700"
            onClick={_ => {
              setFocusedDivisionKey(_ => Some(key))
              setShowAllDivisions(_ => false)
            }}
          >
            {React.string(division.title)}
          </button>
        </div>
        {switch hasChildren {
        | true =>
          <ul className="ml-4 list-disc text-sm text-stone-800">
            {division.children
            ->Belt.Array.map(child => renderDivisionNav(child, key))
            ->React.array}
          </ul>
        | false => React.null
        }}
      </li>
    }

    let rec findDivisionByKey = (divisions, targetKey, prefix) =>
      divisions->Belt.Array.reduce(None, (acc, division) =>
        switch acc {
        | Some(_) => acc
        | None =>
          let key = divisionKey(prefix, division)
          if key == targetKey {
            Some((division, prefix))
          } else {
            findDivisionByKey(division.children, targetKey, key)
          }
        }
      )

    switch (error, divisions) {
    | (Some(message), _) =>
      <div className="rounded border border-red-200 bg-red-50 p-3 text-sm text-red-700">
        {React.string(message)}
      </div>
    | (_, None) =>
      <div className="text-sm text-stone-500"> {React.string("Loading divisions…")} </div>
    | (_, Some(divisions)) =>
      let visibleDivisions = if query == "" && isAllLevelsSelected {
        divisions
      } else {
        divisions->Belt.Array.keepMap(filterDivision)
      }
      switch focusedDivisionKey {
      | None =>
        showAllDivisions
          ? <div className="rounded border-stone-100 bg-white p-4">
              <div className="mb-4 flex items-center gap-3">
                <button
                  className="rounded border border-stone-200 bg-white px-3 py-1 text-xs font-semibold uppercase tracking-wider text-stone-600 hover:border-stone-300"
                  onClick={_ => setShowAllDivisions(_ => false)}
                >
                  {React.string("Divisions")}
                </button>
                <span className="text-sm font-semibold text-stone-800">
                  {React.string("All divisions")}
                </span>
              </div>
              {visibleDivisions
              ->Belt.Array.map(division => renderDivision(division, "root", 0))
              ->React.array}
            </div>
          : <div className="rounded border-stone-100 bg-white p-4">
              <p className="mb-3 text-xs uppercase tracking-widest text-stone-500">
                {React.string("Divisions")}
              </p>
              <ul className="list-disc">
                {visibleDivisions
                ->Belt.Array.map(division => renderDivisionNav(division, "root"))
                ->React.array}
              </ul>
              <div className="mt-4">
                <button
                  className="rounded border border-stone-200 bg-white px-3 py-2 text-xs font-semibold uppercase tracking-wider text-stone-600 hover:border-stone-300"
                  onClick={_ => {
                    setShowAllDivisions(_ => true)
                    setFocusedDivisionKey(_ => None)
                  }}
                >
                  {React.string("All (slow)")}
                </button>
              </div>
            </div>
      | Some(focusedKey) =>
        switch findDivisionByKey(visibleDivisions, focusedKey, "root") {
        | None =>
          <div className="rounded border-stone-100 bg-white p-4 text-sm text-stone-600">
            {React.string("That division is no longer available.")}
          </div>
        | Some((division, prefix)) =>
          <div className="rounded border-stone-100 bg-white">
            <div className="mb-4 flex items-center gap-3">
              <button
                className="rounded border border-stone-200 bg-white px-3 py-1 text-xs font-semibold uppercase tracking-wider text-stone-600 hover:border-stone-300"
                onClick={_ => {
                  setFocusedDivisionKey(_ => None)
                  setShowAllDivisions(_ => false)
                }}
              >
                {React.string("All divisions")}
              </button>
              <span className="text-sm font-semibold text-stone-800">
                {React.string(division.title)}
              </span>
            </div>
            {renderFocusedDivision(division, prefix)}
          </div>
        }
      }
    }
  }
}

module MemoListView = {
  let make = React.memo(ListView.make)
}

module Data = {
  type response
  @val external fetch: string => promise<response> = "fetch"
  @get external ok: response => bool = "ok"
  type promiseReturn = {divisions: array<VitalLevel5.division>}
  @send external json: response => promise<promiseReturn> = "json"
}

@react.component
let make = () => {
  let (divisions, setDivisions) = React.useState(() => None)
  let (error, setError) = React.useState(() => None)
  let (filterText, setFilterText) = React.useState(() => "")
  let (debouncedFilterText, setDebouncedFilterText) = React.useState(() => "")
  let (isPending, startTransition) = React.useTransition()
  let (showSlowFilterLabel, setShowSlowFilterLabel) = React.useState(() => false)
  let (includeChildrenOnMatch, setIncludeChildrenOnMatch) = React.useState(() => true)
  let (expanded, setExpanded) = React.useState(() => Belt.Set.String.empty)
  let (expandedItems, setExpandedItems) = React.useState(() => Belt.Set.String.empty)
  let (selectedLevels, setSelectedLevels) = React.useState(() =>
    Belt.Set.Int.fromArray([1, 2, 3, 4, 5])
  )
  let (showHeaders, setShowHeaders) = React.useState(() => true)
  let (focusedDivisionKey, setFocusedDivisionKey) = React.useState(() => None)
  let (showAllDivisions, setShowAllDivisions) = React.useState(() => false)
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
      setExpanded(_ => Belt.Set.String.empty)
      setExpandedItems(_ => Belt.Set.String.empty)
      setFocusedDivisionKey(_ => None)
      setShowAllDivisions(_ => false)
      Promise.resolve()
    })
    ->Promise.catch(_ => {
      setError(_ => Some("Failed to load the Vital Articles list."))
      Promise.resolve()
    })
    ->ignore

    None
  })

  React.useEffect1(() => {
    if filterText == "" {
      setDebouncedFilterText(_ => "")
      None
    } else {
      let timeoutId = setTimeout(
        () => startTransition(() => setDebouncedFilterText(_ => filterText)),
        500,
      )
      Some(() => clearTimeout(timeoutId))
    }
  }, [filterText])

  React.useEffect1(() => {
    if isPending {
      let timeoutId = setTimeout(() => setShowSlowFilterLabel(_ => true), 1000)
      Some(() => clearTimeout(timeoutId))
    } else {
      setShowSlowFilterLabel(_ => false)
      None
    }
  }, [isPending])

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

  let rec findDivisionByKey = (divisions, targetKey, prefix) =>
    divisions->Belt.Array.reduce(None, (acc, division) =>
      switch acc {
      | Some(_) => acc
      | None =>
        let key = divisionKey(prefix, division)
        if key == targetKey {
          Some((division, prefix))
        } else {
          findDivisionByKey(division.children, targetKey, key)
        }
      }
    )

  React.useEffect3(() => {
    switch (focusedDivisionKey, divisions, showAllDivisions) {
    | (_, Some(divisions), true) =>
      setExpanded(_ => collectDivisionKeys(divisions, "root", Belt.Set.String.empty))
      setExpandedItems(_ => collectDivisionItemKeys(divisions, "root", Belt.Set.String.empty))
    | (Some(key), Some(divisions), false) =>
      switch findDivisionByKey(divisions, key, "root") {
      | None => ()
      | Some((division, prefix)) =>
        setExpanded(_ => collectDivisionKeys([division], prefix, Belt.Set.String.empty))
        setExpandedItems(_ => collectDivisionItemKeys([division], prefix, Belt.Set.String.empty))
      }
    | (None, _, _) => ()
    | (_, None, _) => ()
    }
    None
  }, (focusedDivisionKey, divisions, showAllDivisions))

  <div className="mx-auto max-w-5xl p-6">
    <div className="mb-8">
      <p className="text-sm uppercase tracking-widest text-stone-500">
        {React.string("Wikipedia Vital Articles")}
      </p>

      <div className="mt-4 flex flex-col gap-3 md:flex-row md:items-center">
        <input
          value={filterText}
          placeholder="Filter sections or articles"
          onChange={event => setFilterText(_ => (event->ReactEvent.Form.target)["value"])}
          className="w-full rounded-xl border border-stone-200 bg-white px-4 py-3 text-sm text-stone-700 shadow-sm focus:border-sky-300 focus:outline-none"
        />
        {switch showSlowFilterLabel {
        | true =>
          <span className="text-xs font-semibold uppercase tracking-wider text-stone-500">
            {React.string("Loading…")}
          </span>
        | false => React.null
        }}
        <label
          className="flex items-center gap-2 text-xs font-semibold uppercase tracking-wider text-stone-600"
        >
          <input
            type_="checkbox"
            checked={includeChildrenOnMatch}
            onChange={_ => setIncludeChildrenOnMatch(prev => !prev)}
            className="h-4 w-4 rounded border-stone-300 text-sky-600 focus:ring-sky-300"
          />
          {React.string("And children")}
        </label>
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
    <MemoListView
      error
      divisions
      debouncedFilterText
      includeChildrenOnMatch
      expanded
      expandedItems
      selectedLevels
      showHeaders
      focusedDivisionKey
      showAllDivisions
      setExpanded
      setExpandedItems
      setFocusedDivisionKey
      setShowAllDivisions
    />
  </div>
}
