open! Core
open Common

type mode_t = UM | PM  [@@deriving eq,ord,sexp,hash,show { with_path = false }]

type payload_t = { k : int; gie : bool; umem_val : word_t; (* pmem_val : word_t; *) reg_val : word_t; timerA_counter : word_t; mode : mode_t } [@@deriving eq,ord,sexp,hash,show { with_path = false }]

type element_t =
  | OMaybeDiverge
  | OIllegal
  (* | OException *)
  | OReset
  | OJmpIn of payload_t
  | OJmpOut of payload_t
  | OReti of payload_t
  | OJmpOut_Handle of payload_t*payload_t
  | OTime_Handle of payload_t*payload_t
  (* | OHandle of int *)
  | OTime of payload_t
  | OSilent
  | OUnsupported [@@deriving eq,ord,sexp,hash,show { with_path = false }]

type t = (element_t list) * ((string*string) list) * int [@@deriving eq,ord,sexp,hash,show { with_path = false }]

let default : t = ([OIllegal], [], 0)

let merge_payload ~older ~newer = { newer with k = older.k + newer.k }

include (val Comparator.make ~compare ~sexp_of_t)
