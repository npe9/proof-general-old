(*
      Example proof document for Isabelle/Isar Proof General,
      using symbols.  
      View and process this document with Unicode Tokens engaged.
   
      $Id$
*)

theory "Example-Xsym" imports Main begin

text {* Proper proof text -- \<^bitalic>naive version\<^eitalic>. *}

theorem and_comms: "\<phi> \<and> \<psi> \<longrightarrow> \<psi> \<and> \<phi>"
proof
  assume "\<phi> \<and> \<psi>"
  then show "\<psi> \<and> \<phi>"
  proof
    assume \<psi> and \<phi>
    then show ?thesis ..
 qed
qed

text {* \<^bbold>Unstructured\<^ebold> proof script. *}

theorem "\<phi>\<^isub>\<alpha> \<and> \<phi>\<^isub>\<beta> \<longrightarrow> \<phi>\<^isub>\<beta> ∧ \<phi>\<^isub>\<alpha>"
  apply (rule impI)
  apply (erule conjE)
  apply (rule conjI)
  apply assumption
  apply assumption
done

end
