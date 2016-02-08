open Types
open NuclideDiagnostic
open Atom

let reporterName = "Merlin";;

let makeFullPath fileInfo = (BatSys.getcwd ()) ^ "/" ^ fileInfo.name

let makeRange fileInfo =
  let (startColumn, endColumn) = fileInfo.cols in
  Some {
    Atom.Range.start = {
      row = fileInfo.line;
      column = startColumn;
    };
    Atom.Range.endd = {
      row = fileInfo.line;
      column = endColumn;
    }
  }

let makeFileDiagnosticMessage typee errorMessage ?tip fileInfo =
  let fullErrorMessage = errorMessage ^ (match tip with
    | None -> ""
    | Some h -> "\n" ^ h) in
  let open Message in
    FileDiagnosticMessage {
      scope = `file;
      providerName = reporterName;
      typee = typee;
      filePath = makeFullPath fileInfo;
      text = Some fullErrorMessage;
      html = None;
      range = makeRange fileInfo;
      trace = None;
    }

let nuclide_of_internal = function
  | NoErrorNorWarning err -> None
  | Warning_CatchAll {fileInfo; warningCode; message} ->
    let errorMessage = Printf.sprintf "Warning %d: %s" warningCode message in
    Some (makeFileDiagnosticMessage Warning errorMessage fileInfo)
  | UnparsableButWithFileInfo {fileInfo; error} ->
    Some (makeFileDiagnosticMessage Error error fileInfo)
  | Unparsable err ->
    (* Is this case even possible? *)
    None

  (* normal cases now! *)
  | Type_MismatchTypeArguments {fileInfo; typeConstructor; expectedCount; actualCount} ->
    let errorMessage = Printf.sprintf "This needs to be applied to %d argument(s), we found %d." expectedCount actualCount in
    Some (makeFileDiagnosticMessage Error errorMessage fileInfo)
  | Type_IncompatibleType {fileInfo; actual; expected} ->
    let errorMessage = Printf.sprintf "This is %s, wanted %s instead." actual expected in
    Some (makeFileDiagnosticMessage Error errorMessage fileInfo)
  | Type_NotAFunction {fileInfo; actual} ->
    let errorMessage = Printf.sprintf "This is %s. You seem to have called it as a function." actual in
    let tip = "Careful with the spaces and the parentheses, and whatever's in-between!" in
    Some (makeFileDiagnosticMessage Error errorMessage ~tip fileInfo)
  | Type_AppliedTooMany {fileInfo; functionType; expectedArgCount} ->
    let errorMessage = Printf.sprintf "This function has type %s.\nIt accepts only %d arguments. You gave more." functionType expectedArgCount in
    let tip = "Maybe you forgot a `;` somewhere?" in
    Some (makeFileDiagnosticMessage Error errorMessage ~tip fileInfo)
  | File_SyntaxError {fileInfo} ->
    let errorMessage = "The syntax is wrong." in
    let tip = "Note: the location indicated might not be accurate." in
    Some (makeFileDiagnosticMessage Error errorMessage ~tip fileInfo)
  | File_IllegalCharacter {fileInfo; character} ->
    let errorMessage = Printf.sprintf "The character `%s` is illegal. EVERY CHARACTER THAT'S NOT AMERICAN IS ILLEGAL!" character in
    Some (makeFileDiagnosticMessage Error errorMessage fileInfo)
  | Type_UnboundTypeConstructor {fileInfo; namespacedConstructor; suggestion} ->
    let errorMessage = Printf.sprintf "The type constructor %s can't be found." namespacedConstructor in
    begin match suggestion with
      | None -> Some (makeFileDiagnosticMessage Error errorMessage fileInfo)
      | Some h -> Some (makeFileDiagnosticMessage Error errorMessage ~tip:(Printf.sprintf "Hint: did you mean `%s`?" h) fileInfo)
    end
  | Type_UnboundValue {fileInfo; unboundValue; suggestion} ->
    let errorMessage = begin match suggestion with
      | None -> Printf.sprintf "`%s` can't be found. Could it be a typo?" unboundValue
      | Some h -> Printf.sprintf "`%s` can't be found. Did you mean `%s`?" unboundValue h
    end in
    Some (makeFileDiagnosticMessage Error errorMessage fileInfo)
  | Type_UnboundRecordField {fileInfo; recordField; suggestion} ->
    let errorMessage = begin match suggestion with
      | None -> Printf.sprintf "Field `%s` can't be found in any record type." recordField
      | Some h -> Printf.sprintf "Field `%s` can't be found in any record type. Did you mean `%s`?" recordField h
    end in
    Some (makeFileDiagnosticMessage Error errorMessage fileInfo)
  | Type_UnboundModule {fileInfo; unboundModule} ->
    let errorMessage = Printf.sprintf "Module `%s` not found in included libraries." unboundModule in
    let pckName = BatString.lowercase unboundModule in
    let tip =
      "Hint: your build rules might be missing a link. If you're using: \n" ^
      " - Oasis: make sure you have `"^ pckName ^"` under `BuildDepends` in your _oasis file.\n" ^
      " - ocamlbuild: make sure you have `-pkgs "^ pckName ^"` in your build command.\n" ^
      " - ocamlc | ocamlopt: make sure you have `-I +"^ pckName ^"` in your build command before the source files."in
    Some (makeFileDiagnosticMessage Error errorMessage ~tip fileInfo)
  | Warning_PatternNotExhaustive {fileInfo; unmatched; warningCode} ->
    let errorMessage = Printf.sprintf "Warning %d: this match doesn't cover all possible values of the variant.\n" warningCode in
    let tip = (match unmatched with
      | [oneVariant] -> Printf.sprintf "The case `%s` is not matched" oneVariant
      | many ->
        "These cases are not matched:\n" ^
        (List.fold_left (fun acc x -> acc ^ "- `"^ x ^"`\n") "" many)) in
    Some (makeFileDiagnosticMessage Warning errorMessage ~tip fileInfo)
  | Warning_OptionalArgumentNotErased {fileInfo; warningCode; argumentName} ->
    let errorMessage = Printf.sprintf "Warning %d: %s is an optional argument at last position; calling the function by omitting %s might be confused with currying.\n" warningCode argumentName argumentName in
    let tip = "The rule: an optional argument is erased as soon as the 1st positional (i.e. neither labeled nor optional) argument defined after it is passed in." in
    Some (makeFileDiagnosticMessage Warning errorMessage ~tip fileInfo)
  | _ -> None
;;


(* let pretty_error = try
  let err = BatPervasives.input_all stdin in
  let errLines = Main.split "\n" err in
  nuclide_of_internal (Main.prettyify err errLines)
with BatIO.No_more_input -> None
;; *)
