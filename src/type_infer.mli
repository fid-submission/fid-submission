open Type

module TypeInfer :
sig 
  val type_infer : instr list -> instr -> bool
end 
