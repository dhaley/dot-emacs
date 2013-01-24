;;; Compiled snippets and support files for `gud-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'gud-mode
                     '(("break" "break _ZL12diag_message13an_error_codeP17a_source_position17an_error_severity30a_diagnostic_category_kind_tag" "break" nil nil nil nil nil nil)
                       ("cd" "cd ~/Contracts/TI/bugslayer/cl_" "cd" nil nil nil nil nil nil)
                       ("cree" "run -d1 --c --llvm_file_name - ~/src/edg/test/eok_address_of.c" "cree" nil nil nil nil nil nil)
                       ("iar" "run --advice:power=all --diag_suppress=163 --ulp_standalone_mode=iar -D__IAR_SYSTEMS_ICC__ -D__TID__=11008 -D__intrinsic= -D__no_init= -D__persistent= --c iar.c" "iar" nil nil nil nil nil nil)
                       ("icd" "cd ~/Contracts/TI/tmp/iar" "icd" nil nil nil nil nil nil)
                       ("line" "(a_line_number)_Z15db_line_for_seqm (pos_curr_token.seq)" "line" nil nil nil nil nil nil)
                       ("mrun" "run -I../../exec/arm -Imc2_headers --check_misra=all,-5.3 --c mc2_$0.c" "mrun" nil nil nil nil nil nil)
                       ("opencl" "run --abi=eabi --opencl --c -I.. cl_$0.c" "opencl" nil nil nil nil nil nil)
                       ("operkind" "(an_expr_operator_kind_tag)$0->variant.operation.kind" "operkind" nil nil nil nil nil nil)
                       ("run" "run --abi=eabi --opencl --c -I.. cl_$0.c" "run" nil nil nil nil nil nil)
                       ("ulpcd" "cd ~/Contracts/TI/bugslayer/ulp_" "ulpcd" nil nil nil nil nil nil)
                       ("ulprun" "run --abi=eabi --advice:power=all --remarks --c -I.. ulp_$0.c" "ulprun" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Jan 24 12:21:34 2013
