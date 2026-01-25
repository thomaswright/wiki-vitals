open HistoryLevel5

@react.component
let make = () => {
  let (sections, setSections) = React.useState(() => None)
  let (error, setError) = React.useState(() => None)

  React.useEffect0(() => {
    HistoryLevel5.fetchSections()
    ->Promise.then(sections => {
      setSections(_ => Some(sections))
      Promise.resolve()
    })
    ->Promise.catch(_ => {
      setError(_ => Some("Failed to load the History page."))
      Promise.resolve()
    })
    ->ignore

    None
  })

  let renderLink = link => {
    let href = "https://en.wikipedia.org" ++ link.href
    <li key={link.href} className="my-1">
      <a
        href
        target="_blank"
        rel="noreferrer"
        className="text-sky-700 hover:text-sky-900 underline decoration-sky-300"
      >
        {React.string(link.title)}
      </a>
    </li>
  }

  let rec renderSection = section => {
    <div className="my-4">
      <div className="text-lg font-semibold text-stone-800"> {React.string(section.title)} </div>
      {switch section.items->Belt.Array.length > 0 {
      | true =>
        <ul className="ml-4 mt-2 list-disc text-sm text-stone-700">
          {section.items->Belt.Array.map(renderLink)->React.array}
        </ul>
      | false => React.null
      }}
      {switch section.children->Belt.Array.length > 0 {
      | true =>
        <div className="ml-4 mt-3 border-l border-stone-200 pl-4">
          {section.children->Belt.Array.map(renderSection)->React.array}
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
        {sections->Belt.Array.map(renderSection)->React.array}
      </div>
    }}
  </div>
}
