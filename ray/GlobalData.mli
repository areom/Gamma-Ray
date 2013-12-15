open Ast
open StringModules

(** Module to contain global class hierarchy type declarations *)

(** A full class record table as a type *)
type class_data = {
    (** A set of known class names *)
    known : StringSet.t;

    (** A map that goes from class names to to class definition records *)
    classes : class_def lookup_map;

    (**
        A map that goes from class names to parents --
        everything should have a parent except Object
      *)
    parents : string lookup_map;

    (** A map that goes from class names to a list of children *)
    children : (string list) lookup_map;

    (**
        A table [map -> map -> value]; primary key is the class name;
        secondary key is variable name; result is a pair: the section
        variable was declared in and its type
      *)
    variables : (class_section * string) lookup_table;

    (**
        A table [map -> map -> value]; primary key is the class name;
        secondary key is the method name; result is a list of functions
        that have the given name in the given class
      *)
    methods : (func_def list) lookup_table;

    (**
        A table [map -> map -> value]; primary key is the class name;
        secondary key is the refinement `host.refinement'; the result is
        a list of functions that have the given name in the given class
      *)
    refines : (func_def list) lookup_table;

    (** A map from class names to main functions *)
    mains : func_def lookup_map;

    (**
        A map from class names to the list of ancestors -- the list runs
        from the given class back to Object in the expected fashion
      *)
    ancestors : (string list) lookup_map;

    (**
        A table [map -> map -> value]; primary key is subtype; secondary
        key is the ancestor; result is the positive integer distance that
        represents how many hops along the class tree are between the two
        classes. Distance from a type to itself is 0.
      *)
    distance : int lookup_table;

    (**
        A table [map -> map -> value]; primary key is a class name; the
        secondary key is the refinement's full name (host.name); the value
        is the uid of the dispatcher for this refinement.
      *)
    dispatcher : string lookup_table;
}

(**
    All the different types of non-compiler errors that can occur (programmer errors)
  *)
type class_data_error
    = HierarchyIssue of string
    | DuplicateClasses of string list
    | DuplicateVariables of (string * string list) list
    | DuplicateFields of (string * (string * string) list) list
    | ConflictingMethods of (string * (string * string list) list) list
    | ConflictingInherited of (string * (string * string list) list) list
    | Uninstantiable of string list
    | ConflictingRefinements of (string * (string * string list) list) list
    | MultipleMains of string list
