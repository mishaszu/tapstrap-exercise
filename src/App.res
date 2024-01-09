@module("./logo.svg") external logo: string = "default"
%%raw(`import './App.css'`)

@module external img: string = "rodrigo-soares-1UF27kmDDg0-unsplash.jpg"

let s = React.string

type mapping = {
  key: string,
  code: int,
  pattern: array<int>,
  taps: int,
  switchMode: bool,
  name: Js.undefined<string>,
  numeric: Js.undefined<bool>,
  functional: Js.undefined<bool>,
  special: Js.undefined<bool>,
}

@module("../mappings.json") external mappings: array<mapping> = "default"

type window_

@val external window: window_ = "window"

@send
external addEventListener: (window_, string, Dom.keyboardEvent => unit) => unit = "addEventListener"
@send
external removeEventListener: (window_, string, Dom.keyboardEvent => unit) => unit =
  "removeEventListener"

@get external keyboardKey: Dom.keyboardEvent => string = "key"
@get external keyCode: Dom.keyboardEvent => string = "keyCode"

let space = mappings->Js.Array2.find(m => m.key == "space")->Belt.Option.getExn

let getRandom = (max: float) => {
  Js.Math.floor_float(Js.Math.random() *. max)->Belt.Int.fromFloat
}

let filterArray = (arr: array<mapping>, key) => {
  arr->Js.Array2.filter(value => value.key != key)
}

module Circle = {
  @react.component
  let make = (~extraMx: option<bool>=false, ~isOpen: bool) => {
    switch (extraMx, isOpen) {
    | (true, true) =>
      <div className="w-8 h-8 mx-8 rounded-full bg-white border-4 border-blue-500" />
    | (true, false) => <div className="w-8 h-8 mx-8 rounded-full bg-blue-500" />
    | (false, true) =>
      <div className="w-8 h-8 mx-2 rounded-full bg-white border-4 border-blue-500" />
    | (false, false) => <div className="w-8 h-8 mx-2 rounded-full bg-blue-500" />
    }
  }
}

module Fingers = {
  @react.component
  let make = (~value: array<int>) => {
    let c1 = value->Js.Array2.unsafe_get(0)
    let c2 = value->Js.Array2.unsafe_get(1)
    let c3 = value->Js.Array2.unsafe_get(2)
    let c4 = value->Js.Array2.unsafe_get(3)
    let c5 = value->Js.Array2.unsafe_get(4)
    <div className="flex">
      <Circle extraMx={true} isOpen={c1 == 0} />
      <Circle isOpen={c2 == 0} />
      <Circle isOpen={c3 == 0} />
      <Circle isOpen={c4 == 0} />
      <Circle isOpen={c5 == 0} />
    </div>
  }
}

type mode =
  | All
  | Alphabetical
  | Numeric
  | Functional
  | Special

@react.component
let make = () => {
  let (starterArr, setStarterArr) = React.useState(_ => mappings)
  let (playArr, setPlayArr) = React.useState(_ => mappings)
  let (current, setCurrent) = React.useState(_ => None)
  let (hidden, setHidden) = React.useState(_ => false)
  let (mode, setMode) = React.useState(_ => All)
  let (lastKey, setLastKey) = React.useState(_ => None)
  let (showSwitch, setShowSwitch) = React.useState(_ => false)
  let (showAll, setShowAll) = React.useState(_ => false)
  let (searchAll, setSearchAll) = React.useState(_ => "")

  React.useEffect1(() => {
    switch mode {
    | All => setStarterArr(_ => mappings)
    | Alphabetical =>
      setStarterArr(_ =>
        mappings->Js.Array2.filter(
          value =>
            value.numeric->Js.Undefined.toOption != Some(true) &&
            value.functional->Js.Undefined.toOption != Some(true) &&
            value.special->Js.Undefined.toOption != Some(true),
        )
      )
    | Numeric =>
      setStarterArr(_ =>
        mappings->Js.Array2.filter(value => value.numeric->Js.Undefined.toOption == Some(true))
      )
    | Functional =>
      setStarterArr(_ =>
        mappings->Js.Array2.filter(value => value.functional->Js.Undefined.toOption == Some(true))
      )
    | Special =>
      setStarterArr(_ =>
        mappings->Js.Array2.filter(value => value.special->Js.Undefined.toOption == Some(true))
      )
    }

    setCurrent(_ => None)
    None
  }, [mode])

  React.useEffect1(() => {
    let id = setTimeout(() => {
      setHidden(_ => false)
    }, 3000)

    Some(_ => clearTimeout(id))
  }, [current])

  let keypress = React.useCallback5((e: Dom.keyboardEvent) => {
    Js.log(e->keyCode)
    let key = keyboardKey(e)
    let key = switch key {
    | " " => "space"
    | "Enter" => "enter"
    | v => v
    }

    if current == None && key == "space" && playArr->Js.Array2.length == 0 {
      let currentIndex = getRandom(starterArr->Js.Array2.length->Js.Int.toFloat)
      setCurrent(_ => Some(starterArr->Js.Array2.unsafe_get(currentIndex)))
      setPlayArr(_ => starterArr)
      setHidden(_ => true)
    }

    if current == None && key == "space" {
      let currentIndex = getRandom(starterArr->Js.Array2.length->Js.Int.toFloat)
      setCurrent(_ => Some(starterArr->Js.Array2.unsafe_get(currentIndex)))
      setPlayArr(_ => starterArr)
      setHidden(_ => true)
    }

    switch current {
    | Some(value)
      if value.key == key ||
      value.key == "space" && key == " " ||
      (value.key == "enter" && key == "Enter") => {
        let filteredArr = filterArray(playArr, key)
        setPlayArr(_ => filteredArr)
        let currentIndex = getRandom(filteredArr->Js.Array2.length->Js.Int.toFloat)
        setCurrent(_ => Some(filteredArr->Js.Array2.unsafe_get(currentIndex)))
        setHidden(_ => true)
      }
    | _ => Js.log("Not match!")
    }
    setLastKey(_ => Some(key))
  }, (current, setCurrent, playArr, setPlayArr, starterArr))

  React.useEffect1(() => {
    window->addEventListener("keydown", keypress)
    Some(_ => window->removeEventListener("keydown", keypress))
  }, [keypress])

  <div className="App h-full">
    <div className="w-full h-1/2 bg-gray-100 flex items-center justify-center text-2xl">
      {switch playArr->Js.Array2.length {
      | 0 =>
        <div className="flex h-full flex-col items-center justify-center flex-start">
          <p className="flex mx-12 h-2/3 text-center items-center align-center shrink-0">
            {"You're completed! Press space to start again!"->React.string}
          </p>
          <div className="h-12 my-2" />
        </div>
      | _ =>
        switch current {
        | Some(value) =>
          <div className="flex h-full flex-col items-center justify-center flex-start">
            <p className="flex mx-12 h-2/3 text-center items-center align-center shrink-0">
              {`${value.key}`->Js.String.toUpperCase->React.string}
              {switch value.name->Js.Undefined.toOption {
              | Some(v) => ` ( ${v} )`->React.string
              | None => React.null
              }}
            </p>
            {switch hidden {
            | true => <div className="h-12 my-2" />
            | false =>
              <div className="h-12 m-0 my-2 flex shrink-1">
                <Fingers value={value.pattern} />
                {value.taps > 1 ? `x${value.taps->Belt.Int.toString}`->React.string : React.null}
                {value.switchMode ? "Switch mode"->React.string : React.null}
              </div>
            }}
          </div>
        | None =>
          <div className="flex h-full flex-col items-center justify-center flex-start">
            <p className="flex mx-12 h-2/3 text-center items-center align-center shrink-0">
              {"Press space to start"->React.string}
            </p>
            <div className="h-12 my-2">
              <Fingers value={space.pattern} />
            </div>
          </div>
        }
      }}
    </div>
    {switch lastKey {
    | Some(value) =>
      <div className="w-full h-8 bg-gray-100 flex items-center justify-center text-xs">
        {`Last key: ${value}`->React.string}
      </div>
    | None =>
      <div className="w-full h-8 bg-gray-100 flex items-center justify-center">
        {"Start"->React.string}
      </div>
    }}
    <label> {"Modes:"->s} </label>
    <select
      className="mx-4 p-1 border-2 border-gray-500"
      name="Mode"
      onChange={e => {
        let value = ReactEvent.Form.currentTarget(e)["value"]
        switch value {
        | "All" => setMode(_ => All)
        | "Alphabetical" => setMode(_ => Alphabetical)
        | "Functional" => setMode(_ => Functional)
        | "Numeric" => setMode(_ => Numeric)
        | "Special" => setMode(_ => Special)
        | _ => Js.log("Not a mode")
        }
      }}>
      <option value="All"> {"All"->s} </option>
      <option value="Alphabetical"> {"Alphabetical"->s} </option>
      <option value="Functional"> {"Functional"->s} </option>
      <option value="Numeric"> {"Numeric"->s} </option>
      <option value="Special"> {"Special"->s} </option>
    </select>
    <button className="mx-4" onClick={e => setShowSwitch(old => !old)}>
      {"Show Switch Mode"->s}
    </button>
    <button className="mx-4" onClick={e => setShowAll(old => !old)}> {"Show All"->s} </button>
    {showAll
      ? <input
          className=" mx-4 p-1 border-2 border-gray-500"
          type_="text"
          value={searchAll}
          placeholder="Search all"
          onChange={e => {
            let value = ReactEvent.Form.currentTarget(e)["value"]
            setSearchAll(_ => value)
          }}
        />
      : React.null}
    {showSwitch
      ? <div>
          <div className="flex justify-center items-center my-24">
            {"Switch Mode:"->s}
            <Fingers value={[0, 0, 1, 1, 1]} />
          </div>
          <div className="flex justify-center items-center my-24">
            {"Change mapping (Switch Mode):"->s}
            <Fingers value={[1, 0, 1, 1, 1]} />
          </div>
          <div className="flex justify-center items-center my-24">
            {"Shift Mode:"->s}
            <Fingers value={[1, 1, 1, 0, 0]} />
          </div>
          <div className="flex justify-center items-center my-24">
            {"Turn off:"->s}
            <Fingers value={[0, 0, 0, 1, 0]} />
            {`x3`->React.string}
          </div>
          <div className="flex justify-center items-center my-24">
            {"Vim visual"->s}
            <Fingers value={[1, 0, 1, 1, 0]} />
            {`x2`->React.string}
          </div>
          <div className="flex justify-center items-center my-24">
            {"CTRL + C"->s}
            <Fingers value={[0, 1, 0, 1, 1]} />
            {"Switch mode"->React.string}
          </div>
          <div className="flex justify-center items-center my-24">
            {"CTRL + A"->s}
            <Fingers value={[1, 1, 0, 1, 0]} />
            {"Switch mode"->React.string}
          </div>
          <div className="flex justify-center items-center my-24">
            {"CTRL + V"->s}
            <Fingers value={[0, 1, 1, 0, 1]} />
            {"Switch mode"->React.string}
          </div>
          <div className="flex justify-center items-center my-24">
            {"Tab"->s}
            <Fingers value={[1, 1, 1, 1, 0]} />
            {"Switch mode"->React.string}
          </div>
          <div className="flex justify-center items-center my-24">
            {"ESC"->s}
            <Fingers value={[1, 1, 0, 0, 1]} />
            {"Switch mode"->React.string}
          </div>
          <div className="flex justify-center items-center my-24">
            {"Arrow left"->s}
            <Fingers value={[1, 1, 0, 0, 0]} />
            {"Switch mode"->React.string}
          </div>
          <div className="flex justify-center items-center my-24">
            {"Arrow right"->s}
            <Fingers value={[0, 1, 1, 0, 0]} />
            {"Switch mode"->React.string}
          </div>
          <div className="flex justify-center items-center my-24">
            {"Arrow up"->s}
            <Fingers value={[0, 0, 1, 1, 0]} />
            {"Switch mode"->React.string}
          </div>
          <div className="flex justify-center items-center my-24">
            {"Arrow down"->s}
            <Fingers value={[0, 1, 0, 0, 1]} />
            {"Switch mode"->React.string}
          </div>
        </div>
      : React.null}
    {showAll
      ? <div className="flex flex-col gap gap-4 justify-center items-center mt-8">
          {starterArr
          ->Js.Array2.filter(value => {
            let search = searchAll->Js.String.toLowerCase
            let key = value.key->Js.String.toLowerCase
            if search == "" {
              true
            } else {
              Js.String.includes(key, search)
            }
          })
          ->Js.Array2.map(value => {
            <div className="flex">
              <p className="w-8 mr-8"> {`${value.key}`->Js.String.toUpperCase->React.string} </p>
              <Fingers value={value.pattern} />
              {value.taps > 1 ? `x${value.taps->Belt.Int.toString}`->React.string : React.null}
              {value.switchMode ? "Switch mode"->React.string : React.null}
            </div>
          })
          ->React.array}
        </div>
      : React.null}
  </div>
}
