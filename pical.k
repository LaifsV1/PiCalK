require "substitution.k"

module PICAL-SYNTAX
  imports DOMAINS-SYNTAX
  imports SUBSTITUTION

  syntax Label ::= KVar "!" KVar 
                 | KVar "?" KVar 
                 | KVar "!" "(" KVar ")" 
                 | KVar "?" "(" KVar ")" 
                 | Tau
                 | "beta"
                 | "epsilon"
  
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

  syntax Program ::= "__BETA__" 
                   | "__PROCESS__" 
                   | "__STOP__" 
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
          <step> 0 </step>
      </T>


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
  // TODO: make structural transitions deterministic
  // TODO: missing labels
  // TODO: missing global update
  // TODO: __HALT__ might be useful to stop on explored states
  
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
       [priority(20)]

  // Structural Transition: Name allocation
  rule <k> __BETA__ </k>
       <processes>
         ...
         (<process> new X:KVar . P:Process => P[!A:KVar/X] </process>)
         ...
       </processes>
       <local> ... (.Set => SetItem( !A )) ... </local>
       [priority(22)]

  // Structural Transition: Return to process transitions  
  syntax Bool ::= NoMoreBeta(ProcessesCell) [function]
  rule NoMoreBeta( <processes> <process> new X:KVar . P:Process </process> ... </processes> ) => false
  rule NoMoreBeta( <processes> <process> P | Q     </process> ... </processes> ) => false
  rule NoMoreBeta( <processes> (<process> tau </process> => .Bag) ... </processes> )
  rule NoMoreBeta( <processes> (<process> A:KVar (AS:KVarVec) </process> => .Bag) ... </processes> )
  rule NoMoreBeta( <processes> (<process> tau . P </process> => .Bag) ... </processes> )
  rule NoMoreBeta( <processes> (<process> A:KVar < B > . P </process> => .Bag) ... </processes> )
  rule NoMoreBeta( <processes> (<process> A:KVar [ X ] . P </process> => .Bag) ... </processes> )
  rule NoMoreBeta( <processes> (<process> P + Q </process> => .Bag) ... </processes> )
  rule NoMoreBeta( <processes> .Bag </processes> ) => true
  
  rule <k> __BETA__ => __PROCESS__ </k>
       <step> I:Int => I +Int 1 </step>
       <processes> PROCESSES </processes>
       <trace> TRACE => TRACE ~> beta:Label </trace>
       requires NoMoreBeta(<processes> PROCESSES </processes>)
       [priority(24)]

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
       <trace> TRACE => TRACE ~> tau:Label </trace>
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
       <trace> TR => TR ~> (A ! (B)):Label </trace>
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
       <trace> TR => TR ~> (A ! B):Label </trace>
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
       <trace> TR => TR ~> (A ? (!B)):Label </trace>
       [transition]
       
  // Labelled Transition: Known input
  rule <k> __PROCESS__ => __BETA__ </k>
       <processes> 
         ... 
         <process> A:KVar [ X:KVar ] . P => P[B/X] </process>
         ... 
       </processes>
       <global> ... SetItem( A ) SetItem( B ) ... </global>
       <trace> TR => TR ~> (A ? B):Label </trace>
       [transition]
       
  // Labelled Transition: Internal communication
  rule <k> __PROCESS__ => __BETA__ </k>
       <processes> 
         ... 
         (<process> A:KVar < B:KVar > . P:Process => P      </process>)
         (<process> A:KVar [ X:KVar ] . Q:Process => Q[B/X] </process>)
         ... 
       </processes>
       <trace> TR => TR ~> (tau):Label </trace>
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
       <trace> TR => TR ~> (tau):Label </trace>
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
       <trace> TR => TR ~> (epsilon):Label </trace>
       [transition]

  // Internal Transition: Right sum
  rule <k> __PROCESS__ => __BETA__ </k>
       <processes> 
         ... 
         <process> P + Q => Q </process> 
         ... 
       </processes>
       <trace> TR => TR ~> (epsilon):Label </trace>
       [transition]
endmodule

