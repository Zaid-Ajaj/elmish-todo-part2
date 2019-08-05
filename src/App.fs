module App

open System
open Elmish
open Elmish.React
open Feliz

type Todo = {
  Id : int
  Description : string
  Completed : bool
}

type State = {
  TodoList: Todo list
  NewTodo : string
}

type Msg =
  | SetNewTodo of string
  | AddNewTodo
  | DeleteTodo of int
  | ToggleCompleted of int

let init() = {
  TodoList = [
    { Id = 1; Description = "Learn F#"; Completed = true }
    { Id = 2; Description = "Learn Elmish"; Completed = false }
  ]
  NewTodo = ""
}

let update (msg: Msg) (state: State) =
  match msg with
  | SetNewTodo desc ->
      { state with NewTodo = desc }

  | AddNewTodo when state.NewTodo = "" ->
      state

  | AddNewTodo ->
      let nextTodoId =
        match state.TodoList with
        | [ ] -> 1
        | elems ->
            elems
            |> List.maxBy (fun todo -> todo.Id)
            |> fun todo -> todo.Id + 1

      let nextTodo =
        { Id = nextTodoId
          Description = state.NewTodo
          Completed = false }

      { state with
          NewTodo = ""
          TodoList = List.append state.TodoList [nextTodo] }

  | DeleteTodo todoId ->
      let nextTodoList =
        state.TodoList
        |> List.filter (fun todo -> todo.Id <> todoId)

      { state with TodoList = nextTodoList }

  | ToggleCompleted todoId ->
      let nextTodoList =
        state.TodoList
        |> List.map (fun todo ->
           if todo.Id = todoId
           then { todo with Completed = not todo.Completed }
           else todo)

      { state with TodoList = nextTodoList }

// Helper function to easily construct div with only classes and children
let div (classes: string list) (children: Fable.React.ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]

let appTitle =
    Html.p [
      prop.className "title"
      prop.text "Elmish To-Do List"
    ]

let inputField (state: State) (dispatch: Msg -> unit) =
  div [ "field"; "has-addons" ] [
    div [ "control"; "is-expanded" ] [
      Html.input [
        prop.classes [ "input"; "is-medium" ]
        prop.valueOrDefault state.NewTodo
        prop.onTextChange (SetNewTodo >> dispatch)
      ]
    ]

    div [ "control" ] [
      Html.button [
        prop.classes [ "button"; "is-primary"; "is-medium" ]
        prop.onClick (fun _ -> dispatch AddNewTodo)
        prop.children [
          Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
        ]
      ]
    ]
  ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
  div [ "box" ] [
    div [ "columns"; "is-mobile" ] [
      div [ "column"; "subtitle"] [
        Html.text todo.Description
      ]

      div [ "column"; "is-4" ] [
        div [ "buttons"; "is-right" ] [
          Html.button [
            prop.classList [ true, "button"; todo.Completed, "is-success"]
            prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-check" ] ]
            ]
          ]

          Html.button [
            prop.classes [ "button"; "is-danger" ]
            prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-times" ] ]
            ]
          ]
        ]
      ]
    ]
  ]

let todoList (state: State) (dispatch: Msg -> unit) =
  Html.ul [
    prop.children [
      for todo in state.TodoList ->
        renderTodo todo dispatch
    ]
  ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      appTitle
      inputField state dispatch
      todoList state dispatch
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run