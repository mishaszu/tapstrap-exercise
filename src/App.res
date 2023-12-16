@module("./logo.svg") external logo: string = "default"
%%raw(`import './App.css'`)

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
      <Circle isOpen={c1 == 0} />
      <Circle isOpen={c2 == 0} />
      <Circle isOpen={c3 == 0} />
      <Circle isOpen={c4 == 0} />
      <Circle extraMx={true} isOpen={c5 == 0} />
    </div>
  }
}

type mode = All | Numeric | Functional

@react.component
let make = () => {
  let (starterArr, setStarterArr) = React.useState(_ => mappings)
  let (playArr, setPlayArr) = React.useState(_ => mappings)
  let (current, setCurrent) = React.useState(_ => None)
  let (hidden, setHidden) = React.useState(_ => false)
  let (mode, setMode) = React.useState(_ => All)

  React.useEffect1(() => {
    switch mode {
    | All => setStarterArr(_ => mappings)
    | Numeric =>
      setStarterArr(_ =>
        mappings->Js.Array2.filter(value => value.numeric->Js.Undefined.toOption == Some(true))
      )
    | Functional =>
      setStarterArr(_ =>
        mappings->Js.Array2.filter(value => value.functional->Js.Undefined.toOption == Some(true))
      )
    }

    setCurrent(_ => None)
    None
  }, [mode])

  React.useEffect1(() => {
    let id = setTimeout(() => {
      setHidden(_ => false)
    }, 5000)

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
  }, (current, setCurrent, playArr, setPlayArr, starterArr))

  React.useEffect1(() => {
    window->addEventListener("keydown", keypress)
    Some(_ => window->removeEventListener("keydown", keypress))
  }, [keypress])

  <div className="App">
    <div className="w-full h-64 bg-gray-100 flex items-center justify-center text-2xl">
      {switch playArr->Js.Array2.length {
      | 0 => <> {"You're completed! Press space to start again!"->React.string} </>
      | _ =>
        switch current {
        | Some(value) =>
          <>
            <p className="mx-12">
              {`${value.key}`->Js.String.toUpperCase->React.string}
              {switch value.name->Js.Undefined.toOption {
              | Some(v) => `( ${v} )`->React.string
              | None => React.null
              }}
            </p>
            {switch hidden {
            | true => React.null
            | false =>
              <>
                <Fingers value={value.pattern} />
                {value.taps > 1 ? `x${value.taps->Belt.Int.toString}`->React.string : React.null}
                {value.switchMode ? "Switch mode"->React.string : React.null}
              </>
            }}
          </>
        | None =>
          <>
            <p className="mx-12"> {"Press space to start"->React.string} </p>
            <Fingers value={space.pattern} />
          </>
        }
      }}
    </div>
    <label> {"Modes:"->s} </label>
    <select
      name="Mode"
      onChange={e => {
        let value = ReactEvent.Form.currentTarget(e)["value"]
        switch value {
        | "All" => setMode(_ => All)
        | "Functional" => setMode(_ => Functional)
        | "Numeric" => setMode(_ => Numeric)
        | _ => Js.log("Not match!")
        }
      }}>
      <option value="All"> {"All"->s} </option>
      <option value="Functional"> {"Functional"->s} </option>
      <option value="Numeric"> {"Numeric"->s} </option>
    </select>
    <div className="flex justify-center items-center my-24">
      {"Switch Mode:"->s}
      <Fingers value={[0, 0, 1, 1, 1]} />
    </div>
  </div>
}
