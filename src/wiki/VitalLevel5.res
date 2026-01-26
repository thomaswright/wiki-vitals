type rec link = {
  title: string,
  href: string,
  level: option<int>,
  children: array<link>,
}

type rec section = {
  title: string,
  level: int,
  items: array<link>,
  children: array<section>,
}

type rec division = {
  title: string,
  href: string,
  sections: array<section>,
  children: array<division>,
}

@module("./VitalLevel5.js")
external fetchDivisions: unit => promise<array<division>> = "fetchDivisions"
