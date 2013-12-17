open Ast
open StringModules

(** Module to contain global class hierarchy type declarations *)

(** A full class record table as a type *)
type class_data = {
    known : StringSet.t; (** Set of known class names *)
    classes : class_def lookup_map; (** class name -> class def map *)
    parents : string lookup_map; (** class name -> parent name map *)
    children : (string list) lookup_map; (** class name -> children list map *)
    variables : (class_section * string) lookup_table; (** class name -> var name -> (section, type) map *)
    methods : (func_def list) lookup_table; (** class name -> method name -> func_def list map *)
    refines : (func_def list) lookup_table; (** class name -> host.refinement -> func_def list map *)
    mains : func_def lookup_map; (** class name -> main map *)
    ancestors : (string list) lookup_map; (** class name -> ancestor list (given to Object) *)
    distance : int lookup_table; (** subtype -> supertype -> # hops map *)
    dispatcher : string lookup_table; (** class -> host.refinement -> uid map *)
    refinable : (func_def list) lookup_table (** class -> host.refinement -> refinements (in subclasses) *)
}

(**
    All the different types of non-compiler errors that can occur (programmer errors)
  *)
type class_data_error
    = HierarchyIssue of string
    | DuplicateClasses of string list
    | DuplicateVariables of (string * string list) list
    | DuplicateFields of (string * (string * string) list) list
    | UnknownTypes of (string * (string * string) list) list
    | ConflictingMethods of (string * (string * string list) list) list
    | ConflictingInherited of (string * (string * string list) list) list
    | PoorlyTypedSigs of (string * (string * string option * (string * string) list) list) list
    | Uninstantiable of string list
    | ConflictingRefinements of (string * (string * string list) list) list
    | MultipleMains of string list
