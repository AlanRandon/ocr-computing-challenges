(* Events calendar
   Create a menu driven program that allows the user to add or delete events from a list of dates and timings, just like a calendar. The program should warn you if any of the events overlap when entering them. *)

open Core
open CalendarLib
module W = Nottui_widgets
module Ui = Nottui.Ui
module A = Notty.A
module Focus = Nottui.Focus

type calendar_event = { from : Calendar.t; till : Calendar.t; title : string }

module EventBin = struct
  type t = { from : float; till : float; title : string } [@@deriving bin_io]

  let of_cal (cal : calendar_event) =
    {
      title = cal.title;
      from = Calendar.to_unixfloat cal.from;
      till = Calendar.to_unixfloat cal.till;
    }

  let to_cal bin : calendar_event =
    {
      title = bin.title;
      from = Calendar.from_unixfloat bin.from;
      till = Calendar.from_unixfloat bin.till;
    }

  let read ic =
    let bytes = In_channel.input_all ic in
    let len = String.length bytes in
    let buf = Bin_prot.Common.create_buf len in
    Bin_prot.Common.blit_string_buf bytes buf ~len;
    let pos_ref = ref 0 in
    List.bin_read_t bin_read_t buf ~pos_ref |> List.map ~f:to_cal

  let write cal oc =
    let bytes =
      Bin_prot.Writer.to_bytes (List.bin_writer_t bin_writer_t)
      @@ List.map ~f:of_cal cal
    in
    Out_channel.output_bytes oc bytes
end

let date_format = "%d/%m/%y %T"

let parse_cal_opt ?(fmt = date_format) str =
  try Some (Printer.Calendar.from_fstring fmt str) with _ -> None

let cal_to_string ?(fmt = date_format) cal = Printer.Calendar.sprint fmt cal

let is_between_inclusive from till cal =
  match
    ( Ordering.of_int @@ Calendar.compare cal from,
      Ordering.of_int @@ Calendar.compare cal till )
  with
  | Equal, _ | _, Equal -> true
  | Less, Less | Greater, Greater -> false
  | _ -> true

let event_overlaps a b =
  is_between_inclusive a.from a.till b.from
  || is_between_inclusive a.from a.till b.till

let text_input ?(focus = Focus.make ()) text_var =
  let cursor_position = Lwd.var @@ String.length @@ Lwd.peek text_var in
  Lwd.map2 (Focus.status focus)
    (Lwd.pair (Lwd.get text_var) (Lwd.get cursor_position))
    ~f:(fun focus_status _ ->
      let text = Lwd.peek text_var in
      let pos = Lwd.peek cursor_position in
      let left = String.prefix text pos in
      let right = String.suffix text (String.length text - pos) in
      let clamp_pos ?(len = String.length text) pos = max 0 @@ min len pos in
      Ui.keyboard_area ~focus:focus_status
        (fun key ->
          if Focus.has_focus focus_status then
            match key with
            | `ASCII key, [] ->
                Lwd.set text_var @@ sprintf "%s%c%s" left key right;
                Lwd.set cursor_position
                @@ clamp_pos ~len:(String.length text + 1) (pos + 1);
                `Handled
            | `Arrow `Left, [] | `ASCII 'h', [ `Ctrl ] ->
                Lwd.set cursor_position @@ clamp_pos (pos - 1);
                `Handled
            | `Backspace, [] ->
                Lwd.set text_var
                @@ sprintf "%s%s"
                     (String.prefix left (max 0 (String.length left - 1)))
                     right;
                Lwd.set cursor_position
                @@ clamp_pos ~len:(String.length text - 1) (pos - 1);
                `Handled
            | `Arrow `Right, [] | `ASCII 'l', [ `Ctrl ] ->
                Lwd.set cursor_position @@ clamp_pos (pos + 1);
                `Handled
            | `Escape, [] ->
                Focus.release focus;
                `Handled
            | _ -> `Unhandled
          else `Unhandled)
        (if Focus.has_focus focus_status then
           Ui.hcat
             [
               W.string ~attr:A.(st reverse) left;
               W.string ~attr:A.(st underline ++ st reverse) " ";
               W.string ~attr:A.(st reverse) right;
             ]
         else W.string @@ sprintf "%s" text))

let center ui = Ui.resize ~pad:(Nottui.Gravity.make ~h:`Neutral ~v:`Neutral) ui

let button ?(focus = Focus.make ()) ~shortcut:(expected_key, shortcut) ~text
    handler =
  Lwd.map (Focus.status focus) ~f:(fun focus_status ->
      Ui.keyboard_area ~focus:focus_status
        (fun event_key ->
          if Poly.equal event_key expected_key then (
            handler ();
            `Handled)
          else if Focus.has_focus focus_status then
            match event_key with
            | `Enter, [] | `ASCII ' ', [] ->
                handler ();
                `Handled
            | `Escape, [] ->
                Focus.release focus;
                `Handled
            | _ -> `Unhandled
          else `Unhandled)
        (Ui.hcat
           [
             W.string
               ~attr:
                 (if Focus.has_focus focus_status then A.(st reverse)
                  else A.empty)
               text;
             Ui.space 1 1;
             W.string ~attr:A.(fg lightblack ++ st italic) shortcut;
           ]))

type state = {
  calendar : calendar_event list Lwd.var;
  calendar_path : string;
  root : Ui.t Lwd.t Lwd.var;
  quit : bool Lwd.var;
}

let rec add_event_form state =
  let now = Calendar.now () in
  let from = Lwd.var @@ cal_to_string now in
  let till =
    Calendar.add now @@ Calendar.Period.hour 1 |> cal_to_string |> Lwd.var
  in
  let title = Lwd.var "New Event" in
  let from_date = Lwd.map ~f:(fun str -> parse_cal_opt str) (Lwd.get from) in
  let till_date =
    Lwd.map2
      ~f:(fun from till ->
        Option.both from (till |> parse_cal_opt)
        |> Option.bind ~f:(fun (from, till) ->
               Option.some_if
                 (Calendar.compare from till = Ordering.to_int Ordering.Less)
                 till))
      from_date (Lwd.get till)
  in
  let event =
    Lwd.map2 (Lwd.pair from_date till_date) (Lwd.get title)
      ~f:(fun (from, till) title ->
        Option.map2 from till ~f:(fun from till -> { from; till; title }))
  in
  let title_editor = text_input title in
  let from_editor = text_input from in
  let till_editor = text_input till in
  let submit_focus = Focus.make () in
  let quit_button =
    Lwd.map ~f:center
    @@ button
         ~shortcut:((`ASCII 'q', []), "<q>")
         ~text:"Quit"
         (fun () -> Lwd.set state.root @@ menu state)
  in
  Lwd.bind
    (Lwd.pair event (Lwd.get state.calendar))
    ~f:(fun (event, calendar) ->
      W.vbox
        [
          Lwd.map ~f:center
          @@ W.grid
               [
                 [ Lwd.pure (W.printf "Title: "); title_editor ];
                 [
                   Lwd.map
                     ~f:(fun from ->
                       W.printf
                         (if Option.is_some from then "From: "
                          else "From (invalid): "))
                     from_date;
                   from_editor;
                 ];
                 [
                   Lwd.map
                     ~f:(fun till ->
                       W.printf
                         (if Option.is_some till then "Till: "
                          else "Till (invalid): "))
                     till_date;
                   till_editor;
                 ];
               ];
          (match event with
          | Some event -> (
              match List.find ~f:(event_overlaps event) calendar with
              | Some event ->
                  Lwd.pure @@ center
                  @@ W.printf
                       ~attr:
                         A.(fg (rgb_888 ~r:0xf9 ~g:0x92 ~b:0x3c) ++ st reverse)
                       " Warning: overlaps with %s " event.title
              | None -> W.empty_lwd)
          | None -> W.empty_lwd);
          Lwd.pure @@ Ui.space 1 1;
          Lwd.map ~f:center
          @@ button ~focus:submit_focus
               ~shortcut:((`ASCII 's', []), "<s>")
               ~text:"Submit"
               (fun () ->
                 match event with
                 | Some event ->
                     Lwd.set state.calendar
                       (List.sort ~compare:(fun a b ->
                            Calendar.compare a.from b.from)
                       @@ (event :: calendar));
                     Lwd.set state.root @@ menu state
                 | None -> ());
          quit_button;
        ])

and menu state =
  Lwd.bind (Lwd.get state.calendar) ~f:(fun calendar ->
      Out_channel.with_file state.calendar_path ~f:(EventBin.write calendar);
      W.vbox
        [
          W.vlist ~bullet:""
          @@ List.mapi calendar ~f:(fun i event ->
                 W.hbox
                   [
                     Lwd.pure
                     @@ W.printf "%s %s %s" event.title
                          (cal_to_string event.from) (cal_to_string event.till);
                     Lwd.pure @@ Ui.space 1 1;
                     button
                       ~shortcut:((`ASCII 'D', [ `Ctrl ]), "<C-S-d>")
                       ~text:"Delete"
                       (fun () ->
                         Lwd.set state.calendar
                           (List.filteri ~f:(fun j _ -> i <> j) calendar));
                   ]);
          Lwd.pure @@ Ui.space 1 1;
          Lwd.map ~f:center
          @@ button
               ~shortcut:((`ASCII 'a', []), "<a>")
               ~text:"Add event"
               (fun () -> Lwd.set state.root @@ add_event_form state);
          Lwd.map ~f:center
          @@ button
               ~shortcut:((`ASCII 'q', []), "<q>")
               ~text:"Quit"
               (fun () -> Lwd.set state.quit true);
        ])

let command =
  Command.basic ~summary:"Add/remove events from a calendar"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map calendar_path =
       anon
         (maybe_with_default "calendar.bin" ("path" %: Filename_unix.arg_type))
     in
     fun () ->
       let quit = Lwd.var false in
       let root = Lwd.var W.empty_lwd in
       let calendar =
         Lwd.var
         @@
         try In_channel.with_file calendar_path ~f:EventBin.read with _ -> []
       in
       Lwd.set root @@ menu { quit; root; calendar; calendar_path };
       let wm = W.window_manager @@ Lwd.join @@ Lwd.get root in
       Nottui.Ui_loop.run ~quit_on_ctrl_q:false ~quit_on_escape:false ~quit
       @@ Lwd.map (W.window_manager_view wm) ~f:center)

let () = Command_unix.run command
