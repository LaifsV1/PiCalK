require "substitution.k"

module PICAL-SYNTAX
  imports DOMAINS-SYNTAX
  imports SUBSTITUTION

  syntax Label ::= KVar "!" KVar 
                 | KVar "?" KVar 
                 | KVar "!" "(" KVar ")" 
                 | KVar "?" "(" KVar ")" 
                 | Tau
  
  syntax Process ::= Empty
                   | KVar "(" KVarVec ")"
                   | Tau "." Process
                   | KVar "<" KVar ">" "." Process
                   | KVar Receive
                   > "new" KVar "." Process        [binder]
                   > Process "+" Process             [left]
                   > Process "|" Process             [left]
                   | "(" Process ")"              [bracket]
  syntax Receive ::= "[" KVar "]" "." Process      [binder]
  
  syntax Program ::= Process 
                   | KVar Method
                   | "extern" KVar ";" Program     [binder]
  syntax Method ::= "(" KVarVec ")" "=" Definition [binder]
  syntax Definition ::= Process ";" Program

  syntax KVarVec ::= KVar | KVar "," KVarVec         
  syntax Empty ::= "end"
  syntax Tau   ::= "tau"
endmodule

module PICAL
  imports PICAL-SYNTAX
  imports SUBSTITUTION
  imports COLLECTIONS
  imports DOMAINS
  imports SET

  syntax KResult ::= Stop
  syntax Program ::= Mode
                   | Stop
  syntax Mode    ::= "__BETA__" 
                   | "__PROCESS__"
  syntax Stop    ::= "__STOP__" 
  syntax MBody   ::= KVarVec "->" Process [binder]

  configuration 
      <T>
          <k> $PGM:Program </k>
          <global> .Set </global>
          <local>  .Set </local>
          <processes>
              <process multiplicity="*"> .K </process>
          </processes>
          <methods> .Map </methods>
          <trace> .K </trace>
          <step> 5 </step>
      </T>


  // Termination
  rule <k> _ => __STOP__ </k>
       <step> 0 => -1 </step>
       [priority(1)]

  ////////////////////////////////////////////////////
  // Initialisation                                 //
  //------------------------------------------------//
  // can't rename unless I rename all processes     //
  ////////////////////////////////////////////////////
  // TODO: missing global update
  
  rule <k> extern X ; M => M </k>
       <global> ... (.Set => SetItem( X )) ... </global> 
  rule <k> X ( XS ) = P ; M => M </k>
       <methods> ... (.Map => (X |-> XS -> P)) ... </methods>
  rule <k> P:Process => __BETA__ </k>
       <processes> ... (.Bag => <process> P </process>) ... </processes>

  /////////////////////////////////////////////
  // Pi-Calculus                             //
  //-----------------------------------------//
  // we assume global and local are disjoint //
  // because we add fresh names to local     //
  /////////////////////////////////////////////
  // TODO: missing labels
  // TODO: missing global update
  
  ///////////////////////////////////
  // STRUCTURAL (BETA) TRANSITIONS //
  ///////////////////////////////////
  
  // Structural Transition: Parallel flattening
  rule <k> __BETA__ </k>
       <processes>
         ...
         (<process> P:Process | Q:Process => P </process>)
         (.Bag => <process> Q </process>)
         ...
       </processes>
       [transition]

  // Structural Transition: Name allocation
  rule <k> __BETA__ </k>
       <processes>
         ...
         (<process> new X:KVar . P:Process => P[!A:KVar/X] </process>)
         ...
       </processes>
       <local> ... (.Set => SetItem( !A )) ... </local>
       [transition]
       
  // Structural Transition: Return to process transitions  
  syntax Bool ::= NoMoreBeta(ProcessesCell) [function]
  rule NoMoreBeta( 
         <processes> 
           ... 
           (<process> new X:KVar . P:Process </process>) 
           ... 
          </processes> ) => false
         [priority(50)]
  rule NoMoreBeta( 
         <processes> 
           ... 
           (<process> P | Q </process>) 
           ... 
         </processes> ) => false
         [priority(60)]
  rule NoMoreBeta( _ ) => true 
       [priority(150)]
  
  rule <k> __BETA__ => __PROCESS__ </k>
       <processes> PROCESSES </processes>
       <step> I:Int => I -Int 1 </step>
       requires NoMoreBeta(<processes> PROCESSES </processes>)
       [transition]

  /////////////////////////
  // PROCESS TRANSITIONS //
  /////////////////////////

  // Labelled Transition: Unobservable action
  rule <k> __PROCESS__ => __BETA__ </k>
       <processes> 
         ... 
         <process> tau . P => P </process>
         ...
       </processes>
       <trace> _ => tau:Label </trace>
       <step> I </step>
       requires I =/=Int 0
       [transition]
       
  // Labelled Transition: Fresh output
  rule <k> __PROCESS__ => __BETA__ </k>
       <processes> 
         ... 
         <process> A:KVar < B:KVar > . P => P </process>
         ... 
       </processes>
       <global> 
         ... 
         SetItem( A:KVar ) 
         (.Set => SetItem( B ))
         ... 
       </global>
       <local> 
         ... 
         (SetItem( B:KVar ) => .Set)
         ... 
       </local>
       <trace> _ => (A ! (B)):Label </trace>
       <step> I </step>
       requires I =/=Int 0
       [transition]
       
  // Labelled Transition: Known output
  rule <k> __PROCESS__ => __BETA__ </k>
       <processes> 
         ... 
         <process> A:KVar < B:KVar > . P => P </process>
         ... 
       </processes>
       <global> 
         ... 
         SetItem( A:KVar ) 
         SetItem( B:KVar ) 
         ... 
       </global>
       <trace> _ => (A ! B):Label </trace>
       <step> I </step>
       requires I =/=Int 0
       [transition]
       
  // Labelled Transition: Fresh input
  rule <k> __PROCESS__ => __BETA__ </k>
       <processes> 
         ... 
         <process> A:KVar [ X:KVar ] . P => P[!B:KVar/X] </process> 
         ... 
       </processes>
       <global> 
         ...
         (SetItem( A ) => SetItem( A ) SetItem( !B ))
         ...
       </global>
       <trace> _ => (A ? (!B)):Label </trace>
       <step> I </step>
       requires I =/=Int 0
       [transition]
       
  // Labelled Transition: Known input
  rule <k> __PROCESS__ => __BETA__ </k>
       <processes> 
         ... 
         <process> A:KVar [ X:KVar ] . P => P[B/X] </process>
         ... 
       </processes>
       <global> ... SetItem( A ) SetItem( B ) ... </global>
       <trace> _ => (A ? B):Label </trace>
       <step> I </step>
       requires I =/=Int 0
       [transition]
       
  // Labelled Transition: Internal communication
  rule <k> __PROCESS__ => __BETA__ </k>
       <processes> 
         ... 
         (<process> A:KVar < B:KVar > . P:Process => P      </process>)
         (<process> A:KVar [ X:KVar ] . Q:Process => Q[B/X] </process>)
         ... 
       </processes>
       <trace> _ => (tau):Label </trace>
       <step> I </step>
       requires I =/=Int 0
       [transition]

  // Labelled Transition: Recursion
  syntax Process ::= Process "{" KVarVec "/" KVarVec "}"
  syntax Program ::= "__SUBS__"
  rule <k> __PROCESS__ => __SUBS__ </k>
       <processes> 
         ... 
         <process> X:KVar ( AS:KVarVec ) => P { AS / XS } </process> 
         ... 
       </processes>
       <methods> ... (X |-> XS -> P) ... </methods>
       <trace> _ => (tau):Label </trace>
       <step> I </step>
       requires I =/=Int 0
       [transition]
       
  // might need to make these deterministic
  rule <k> __SUBS__ => __SUBS__ </k>
       <processes> 
         ... 
         <process> 
           P { A:KVar , AS:KVarVec / X:KVar , XS:KVarVec } 
           => 
           P[A/X] { AS / XS }
         </process> 
         ... 
       </processes>
  rule <k> __SUBS__ => __BETA__ </k>
       <processes> 
         ... 
         <process> 
           P { A:KVar / X:KVar }
           => 
           P[A/X]
         </process> 
         ... 
       </processes>

  // Internal Transition: Left sum
  rule <k> __PROCESS__ => __BETA__ </k>
       <processes> 
         ... 
         <process> P + Q => P </process> 
         ... 
       </processes>
       <trace> _ => . </trace>
       <step> I </step>
       requires I =/=Int 0
       [transition]

  // Internal Transition: Right sum
  rule <k> __PROCESS__ => __BETA__ </k>
       <processes> 
         ... 
         <process> P + Q => Q </process> 
         ... 
       </processes>
       <trace> _ => . </trace>
       <step> I </step>
       requires I =/=Int 0
       [transition]
endmodule

