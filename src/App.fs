module App

open System
open Elmish
open Elmish.React
open Fable.React 
open Fable.React.Props

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
                
let createTodoTextbox state dispatch = 
  div [ Class "field has-addons" ] [
    div [ Class "control is-expanded" ] [ 
      input [ 
        Class "input is-medium"
        valueOrDefault state.NewTodo
        OnChange (fun ev -> dispatch (SetNewTodo ev.Value)) ]
    ] 
    div [ Class "control" ] [ 
      button [ Class "button is-primary is-medium"; OnClick (fun _ -> dispatch AddNewTodo) ] [ 
        i [ Class "fa fa-plus" ] [ ]
      ]
    ] 
  ] 

let renderTodo (todo: Todo) (dispatch: Msg -> unit) = 
  let checkButtonStyle = 
    classList [ 
      "button", true
      "is-success", todo.Completed
      "is-outlined", not todo.Completed 
    ]
    
  div [ Class "box" ] [ 
    div [ Class "columns is-mobile" ] [
      div [ Class "column" ] [
        p [ Class "subtitle" ] [ str todo.Description ] 
      ] 
      div [ Class "column is-4" ] [
        div [ Class "buttons is-right" ] [
          button [ checkButtonStyle; OnClick (fun _ -> dispatch (ToggleCompleted todo.Id))  ] [
            i [ Class "fa fa-check" ] [ ] 
          ] 
          button [ Class "button is-danger"; OnClick (fun _ -> dispatch (DeleteTodo todo.Id)) ] [
            i [ Class "fa fa-times" ] [ ] 
          ] 
        ]
      ]
    ]
  ]  

let render (state: State) (dispatch: Msg -> unit) =
  div [ Style [ Padding 20 ] ] [
    h3 [ Class "title" ] [ str "Elmish To-Do list" ]
    createTodoTextbox state dispatch
    div [ Class "content"; Style [ MarginTop 20 ] ] [ 
      for todo in state.TodoList -> renderTodo todo dispatch
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run