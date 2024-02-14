open Core
open Learninglib.Lsharp
open Sancus
open Interop
(*
open Attacker
open Enclave *)

open QCheck

module IIBLSharpRW = LSharp (Sancus.Input) (Sancus.Output_internal) (Sancus.Verilog) (Learninglib.Randomwalkoracle.RandomWalkOracle)
module IIBLSharpPAC = LSharp (Sancus.Input) (Sancus.Output_internal) (Sancus.Verilog) (Learninglib.Pacoracle.PACOracle)
module IOInteropInternal = Interop (Sancus.Input) (Sancus.Output_internal)

module RWOracle = Learninglib.Randomwalkoracle.RandomWalkOracle (Sancus.Input) (Sancus.Output_internal) (Sancus.Verilog)
module PACOracle = Learninglib.Pacoracle.PACOracle (Sancus.Input) (Sancus.Output_internal) (Sancus.Verilog)

let spec_parse_or_fail spec =
  match Testdl.Parser.parse_spec spec with
  | Result.Ok r -> r
  | Result.Error e -> failwith e

let command =
  Command.basic
    ~summary:"Test NI on Sancus with the specified attacker and enclave spec"
    (let%map_open.Command
    dbg =
      flag
        "--debug"
        no_arg
        ~doc:"Enables debug-level logging"
    and info =
      flag
        "--info"
        no_arg
        ~doc:"Enables info-level logging"
    and enclave_spec_fn =
      flag
        "--encl-spec"
        (required string)
        ~doc:"filename Enclave specification file, using the TestDL language"
    and attacker_spec1_fn =
      flag
        "--att-spec1"
        (required string)
        ~doc:"filename Attacker specification file, using the TestDL language"
    and attacker_spec2_fn =
      flag
        "--att-spec2"
        (required string)
        ~doc:"filename Attacker specification file, using the TestDL language"
    and tmpdir =
      flag
        "--tmpdir"
        (required string)
        ~doc:"directory Temporary directory where intermediate results will be stored"
    and sancus_core_gap_dir =
      flag
        "--sancus"
        (required string)
        ~doc:"directory Directory where the sancus-core-gap repository was cloned"
    and sancus_master_key =
      flag
        "--sancus-master-key"
        (optional_with_default "cafe" string)
        ~doc:"key A hex number to be used as the master key when building the processor (default: cafe)"
    and commit =
      flag
        "--commit"
        (optional_with_default "ef753b6" string)
        ~doc:"checksum/label Checksum/label of the commit for which we want to learn the Mealy model (default: ef753b6, i.e., the version w/o Mind the Gap mitigations)"
    and step_limit =
      flag
        "--step-limit"
        (optional_with_default 500 int)
        ~doc:"limit (Only for randomwalk oracle) Maximum number of steps for the equivalence oracle before giving up looking for a counterexample (default: 500)"
    in
    fun () ->
        (* Random.self_init (); *)
        Random.init 0;
        Logs.set_reporter (Logs_fmt.reporter ());

        if dbg then Logs.set_level (Some Logs.Debug)
        else if info then Logs.set_level (Some Logs.Info)
        else Logs.set_level (Some Logs.App);

        let cwd = Sys_unix.getcwd () in
        (* Logs.debug (fun m -> m "Current directory: %s" cwd); *)
        (* Create tmpdir if not present *)
        (* If the last char of tmpdir is /, remove it. It causes problems to the Verilog compiler :( *)
        let tmpdir = if Char.equal tmpdir.[String.length tmpdir - 1] '/' then String.drop_suffix tmpdir 1 else tmpdir in
        (match Sys_unix.file_exists tmpdir with | `No -> Core_unix.mkdir_p tmpdir | _ -> ());
        (* Basic sanity checks on the repo *)
        assert (Sys_unix.file_exists_exn sancus_core_gap_dir);
        assert (Sys_unix.is_directory_exn sancus_core_gap_dir);
        (* (1) load the spec *)
        let enclave_spec_str = In_channel.read_all enclave_spec_fn in
        let attacker_spec1_str = In_channel.read_all attacker_spec1_fn in
        let attacker_spec2_str = In_channel.read_all attacker_spec2_fn in
        let complete_spec1 = spec_parse_or_fail (enclave_spec_str ^ " " ^ attacker_spec1_str) in
        let complete_spec2 = spec_parse_or_fail (enclave_spec_str ^ " " ^ attacker_spec2_str) in
        (* let (Enclave enclave1, ISR isr1, Prepare prepare1, Cleanup cleanup1) = complete_spec1 in
        let (Enclave enclave2, ISR isr2, Prepare prepare2, Cleanup cleanup2) = complete_spec2 in *)
        let spec_dfa1 = Inputgen.build_spec_dfa complete_spec1 in
        let spec_dfa2 = Inputgen.build_spec_dfa complete_spec2 in
        (* (2) initialize the interface with the processor's implementation *)
        let sul1 =
          Sancus.Verilog.make
            ~workingdir:cwd
            ~tmpdir:tmpdir
            ~basename:"generic"
            ~verilog_compile: (cwd ^ "/../scripts/verilog_compile")
            ~get_symbolpos: (cwd ^ "/../scripts/get_symbolpos.sh")
            ~pmem_elf:"pmem.elf"
            ~pmem_script:(cwd ^ "/../scripts/build_pmem")
            ~simulate_script:(cwd ^ "/../scripts/simulate")
            ~submitfile:(cwd ^ "/../src/submit.f")
            ~sancus_repo:sancus_core_gap_dir
            ~sancus_master_key:sancus_master_key
            ~commit:commit
            ~templatefile:(cwd ^ "/../src/generic_template.s43")
            ~filledfile:"generic.s43"
            ~dumpfile:"tb_openMSP430.vcd"
            ~initial_spec:spec_dfa1
            ~ignore_interrupts:false
            () in
        let sul2 =
          Sancus.Verilog.make
            ~workingdir:cwd
            ~tmpdir:tmpdir
            ~basename:"generic"
            ~verilog_compile: (cwd ^ "/../scripts/verilog_compile")
            ~get_symbolpos: (cwd ^ "/../scripts/get_symbolpos.sh")
            ~pmem_elf:"pmem.elf"
            ~pmem_script:(cwd ^ "/../scripts/build_pmem")
            ~simulate_script:(cwd ^ "/../scripts/simulate")
            ~submitfile:(cwd ^ "/../src/submit.f")
            ~sancus_repo:sancus_core_gap_dir
            ~sancus_master_key:sancus_master_key
            ~commit:commit
            ~templatefile:(cwd ^ "/../src/generic_template.s43")
            ~filledfile:"generic.s43"
            ~dumpfile:"tb_openMSP430.vcd"
            ~initial_spec:spec_dfa2
            ~ignore_interrupts:false
            () in
        (* (3) prepare the oracle *)
        (* let attacker_atoms1 =
          List.fold [isr1; prepare1; cleanup1] ~init:[] ~f:(fun acc b -> acc @ (Attacker.AtomSet.to_list (Attacker.get_atoms b))) in
        let attacker_atoms2 =
          List.fold [isr2; prepare2; cleanup2] ~init:[] ~f:(fun acc b -> acc @ (Attacker.AtomSet.to_list (Attacker.get_atoms b))) in *)
        (* This generates a valid sequence for spec_dfa *)
        let gen_fixed_encl eil_init sul spec_dfa n =
          QCheck.Gen.pure (Sancus.Verilog.pre sul;
          let rec _gen_fixed_encl eil sul spec_dfa n il ol =
            (match n with
            | 0 -> (il, ol)
            | _ ->
              match Inputgen.generate_next ?force_encl:(List.hd eil) spec_dfa il ol with
              | `Stop -> (il, ol)
              | `Next i ->
                  let eil' = match List.tl eil with | None -> eil_init | _ -> eil in
                  _gen_fixed_encl eil' sul spec_dfa (n-1) (il@[i]) (ol@[Sancus.Verilog.step ~silent:true sul i])
            ) in
            _gen_fixed_encl eil_init sul spec_dfa n [] []) in
        (* let cnt = ref 0 in *)
        let gen sul spec_dfa n =
          (* printf "\n=== %d/%d: n=%d ===\n" (incr cnt; !cnt) step_limit n; *)
          QCheck.Gen.pure (Sancus.Verilog.pre sul;
          let rec _gen sul spec_dfa n il ol =
            (match n with
            | 0 -> (il, ol)
            | _ -> match Inputgen.generate_next spec_dfa il ol with
              | `Stop -> (il, ol)
              | `Next i -> _gen sul spec_dfa (n-1) (il@[i]) (ol@[Sancus.Verilog.step ~silent:true sul i])) in
          _gen sul spec_dfa n [] []) in
        let includes (ol : Output_internal.element_t list) (o : Output_internal.element_t) =
          List.exists ol ~f:(fun oi -> Output_internal.equal_element_t oi o) in
        (* QCheck setup *)
        let p_gen = QCheck.Gen.(sized @@ (fun n ->
          (gen sul1 spec_dfa1 n) >>=
            (fun p ->
              let eil = List.filter (List.map ~f:(fun i -> `Next i) (fst p)) ~f:(fun i -> match i with | `Next (Input.IEnclave _) -> true | _ -> false) in
                pair (return p) (gen_fixed_encl eil sul2 spec_dfa2 n))
          )
        ) in
        let oi_print oi = Output_internal.show oi in
        let rec p_print ((il, ol), (il', ol')) = match (il, ol, il', ol') with
          | [], [], [], [] -> ""
          | [], [], i::is, o::os ->
            sprintf "_/_ ========== %s/%s;\n%s" (Input.show i) (oi_print o) (p_print (([],[]), (is, os)))
          | i::is, o::os, [], [] ->
            sprintf "%s/%s ========== _/_;\n%s" (Input.show i) (oi_print o) (p_print ((is, os), ([],[])))
          | i::is, o::os, i'::is', o'::os' ->
            sprintf "%s/%s ========== %s/%s\n %s" (Input.show i) (oi_print o) (Input.show i') (oi_print o') (p_print ((is, os), (is', os')))
          | _ -> failwith "Shouldn't happen?" in
        let same_encl p p' =
          List.equal Input.equal
            (List.filter p ~f:(fun i -> match i with | Input.IEnclave _ -> true | _ -> false))
            (List.filter p' ~f:(fun i -> match i with | Input.IEnclave _ -> true | _ -> false))
        in
        let open Output_internal in
        let flatten_oi = List.fold ~init:[] ~f:(fun acc (o, _, _) -> acc@o) in
        (* This should look more like a weak barber bisimilarity, for us this is simple... *)
        let low_equiv res_p res_p' =
          (* Either both have a Rst or none have it *)
          (includes res_p Output_internal.OReset && includes res_p' Output_internal.OReset) ||
          (not (includes res_p Output_internal.OReset) && not (includes res_p' Output_internal.OReset)) in
        let valid res_p =
          not (List.mem res_p OIllegal ~equal:Output_internal.equal_element_t &&
          List.mem res_p OUnsupported ~equal:Output_internal.equal_element_t) in
        let arbitrary_p = QCheck.make p_gen ~print:p_print in
        let test_ni =
          QCheck.Test.make
            ~if_assumptions_fail:(`Warning, 1.0)
            ~name:"non-interference"
            ~count:step_limit
            arbitrary_p
            (fun (p, p') ->
                assume (same_encl  (fst p) (fst p'));
                let res_p = flatten_oi (snd p) in
                let res_p' = flatten_oi (snd p') in
                  assert (not (List.mem res_p OIllegal ~equal:Output_internal.equal_element_t));
                  assert (not (List.mem res_p OUnsupported ~equal:Output_internal.equal_element_t));
                  assert (not (List.mem res_p' OIllegal ~equal:Output_internal.equal_element_t));
                  assert (not (List.mem res_p' OUnsupported ~equal:Output_internal.equal_element_t));
                  assume (valid res_p);
                  assume (valid res_p');
                  low_equiv res_p res_p'
              ) in
          ignore (QCheck_runner.run_tests ~verbose:true [test_ni]))

let () = Command_unix.run command
