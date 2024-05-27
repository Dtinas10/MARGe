package marge.frontend

import caos.frontend.Configurator.*
import caos.frontend.{Configurator, Documentation}
import caos.view.*
import marge.backend.*
import marge.syntax.Program.RxGr
import marge.syntax.Program.System
import marge.syntax.{Program, Show}
import marge.syntax.Parser 
import marge.frontend.Examples.*

import scala.util.control.Breaks._


/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[System]:
  val name = "Animator of Labelled Reactive Graphs"
  override val languageName: String = "Input program"

  /** Parser, converting a string into a System in rgv3 */
  // val parser = str => Parser.pp(Parser.rg,str).getOrElse(???)
  val parser = marge.syntax.Parser.parseProgram

  /** Examples of programs that the user can choose from. The first is the default one. */
  val examples = List(
    "Penguin" -> Examples.gabbayExample -> "Figure 7.4 in Dov M Gabbay, Cognitive Technologies Reactive Kripke Semantics",
    // "Gabbay Example2" -> Example_GabbayExample2-> "Figure 7.9 of Dov M Gabbay, Cognitive Technologies Reactive Kripke Semantics",
    "Counter" ->  Examples.counter-> "Run 3 times only the action *act*",
    "Feature Model"->  Examples.featureModel -> "Fig 1 in Maxime Cordy et al. Model Checking Adaptive Software with Featured Transition Systems",
    // "VM"->  Examples.vendingMachine -> "We have 1$ only to spend in the vending machine and we need to decide the best option between cofee, chocolate and apple.",
    // "VM2"->  Examples.VMPaper -> "Example of Vending Machine presented in Reactive Graphs in action",
    //"Vending Machine 2"->  Examples.vendingMachine2 -> "We have 1$ only to spend in the vending machine and we need to decide the best option between cofee, chocolate and apple.",
    "Conflicts" -> Examples.conflict -> "Example of Reactive Graph with a conflict.",
    // "Example" -> Examples.exampleOfReport -> "Example of Report",
    // "Ex1" -> Examples.ex1,
    "Bissi" -> Examples.bissimulation,
    "VM_U" -> Examples.VM_U,	
    "Product" -> Examples.product,
    // "Product2" -> Examples.product2,
    "Intrusive Product" -> Examples.product3,
    )

  // /** Description of the widgets that appear in the dashboard. */
  // val widgets = List(
  //   "View pretty data" -> view[System](x => Show.toMermaid(x.main,""), Code("haskell")).moveTo(1),
  //   // "actionss" -> view[System](x => x.main.actions.toString(), Text).moveTo(1),
  //   "Dead Locks" -> view[System](Program.findDeadlockTracePP(_), Text).moveTo(1),
  //   "Conflicts / Contradictory effects" -> view[System](Program.findIncoPP(_), Text).moveTo(1),
  //   // "fffffffffffff" -> view[System](x=>Program.lts(x.main).se.toString, Text).moveTo(1),
  //   "Global structure view" -> view(x =>Show.toMermaid(x.main,"GSV"), Mermaid),
  //   "Local structure view" -> view(x => Show.toMermaid(x.main.getLevel0,"LSV"), Mermaid),
  //   // "Run semantics" -> steps(e=>e, Semantics, x => Show.toMermaid(x.main,"RS"), _.toString, Mermaid),
  //   "Run semantics" -> steps(e=>e, Warnings, x => Show.toMermaid(x.main,"RS"), _.toString, Mermaid),
  //   "Run semantics2Same" -> steps(e=>e, SemanticsTwo, x => Show.toMermaid_twoGraphs_Bissi(x,"AAAA"), _.toString, Mermaid),
  //   // "Run semanticstext" -> steps(e=>e, Semantics, x => Show.toMermaid(x.main,"RS"), _.toString, Text),
  //   "Run semantics with local structure" -> steps(e=>e, Semantics, x => Show.toMermaid_twoGraphs(x.main,x.main.getLevel0,"RSLS"), _.toString, Mermaid),
  //   "Build LTS" -> lts(x=>x, Semantics, x=>x.main.init, _.toString),
  //   "Generated LTS" -> view[System](x => Show.toMermaid(Program.lts(x.main),""), Mermaid),
  //   // "Build LTS (explore)" -> ltsExplore(e=>e, Semantics, x=>x.main.init, _.toString),
  //   "Two Reactive Graphs" -> view(x =>Show.toMermaid_twoGraphs_Bissi(x,"TG"), Mermaid),
  //   // "Build LTS" -> lts(x=>x, Semantics, x=>x.init, _.toString),
  //   // "Check" -> check(x=>Seq(x.toString)),
  //   // "Build LTS2" -> lts(x=>x, Semantics, x=>x.active.toString, _.toString),
  //   //  "Build LTS" -> lts((e:System)=>e, Semantics, Show.justTerm, _.toString).expand,
  //   //  "Build LTS (explore)" -> ltsExplore(e=>e, Semantics, x=>Show(x.main), _.toString),
  //   "Find strong bisimulation (given a program \"A ~ B\")" ->
  //     compareStrongBisim(Semantics, Semantics,
  //       (e: System) => System(e.main, None),
  //       (e: System) => System(e.toCompare.getOrElse(RxGr(Map.empty, Map.empty, " ", Set.empty)), None),
  //       Show.justTerm, Show.justTerm, _.toString),
  //  // "Find strong bisimulation with LTS function" ->
  //  //   compareStrongBisim(Semantics, Semantics,
  //  //     (e: System) => System(e.main, None),
  //  //     (e: System) => System(Program.lts(e.main), None),
  //  //     Show.justTerm, Show.justTerm, _.toString),
  //   // "Product" -> view[System](x =>Show.toMermaid(Program.pro(x),"TGG"), Mermaid),
  //   // "Product" -> view[System](x =>Show.toMermaid(Program.product(x,true),"TGG"), Mermaid),
  //   // "Run Product" -> steps(e=>System(Program.product(e),None), Warnings, x => Show.toMermaid(x.main,"RS"), _.toString, Mermaid),
  //   // "Union" -> view[System](x =>Show.toMermaid(Program.union(x),"TGG"), Mermaid),
  //   // "Run Union" -> steps(e=>System(Program.union(e),None), Warnings, x => Show.toMermaid(x.main,"RS"), _.toString, Mermaid),
  //   // "Merge" -> view[System](x =>Show.toMermaid(Program.merge(x),"TGG"), Mermaid),
  //   // "Run Merge" -> steps(e=>System(Program.merge(e),None), Warnings, x => Show.toMermaid(x.main,"RS"), _.toString, Mermaid),
  //   "Asynchronous Product tesste" -> lts(x=>x, AsynchronousProduct, indexedViewSt, _.toString),
  //   "Asynchronous Product" -> lts(x=>x, AsynchronousProduct, x=>("<"+ x.main.init+", "+x.toCompare.getOrElse(x.main.empty).init+">"), _.toString),
  //   "Asynchronous Product Explore" -> ltsExplore(x=>x, AsynchronousProduct, x=>("<"+ x.main.init+", "+x.toCompare.getOrElse(x.main.empty).init+">"), _.toString),
  //   "Synchronous Product" -> lts(x=>x, SynchronousProduct, x=>("<"+ x.main.init+", "+x.toCompare.getOrElse(x.main.empty).init+">"), _.toString),
  //   "Synchronous Product Explore" -> ltsExplore(x=>x, SynchronousProduct, x=>("<"+ x.main.init+", "+x.toCompare.getOrElse(x.main.empty).init+">"), _.toString),
  // )

  val widgets = List(
    "View Pretty Data" -> view[System](x => Show.toMermaid(x.main,""), Code("haskell")).moveTo(1),
    "Dead Locks" -> view[System](Program.findDeadlockTracePP(_), Text).moveTo(1),
    "Conflicts / Contradictory Effects" -> view[System](Program.findIncoPP(_), Text).moveTo(1),    
    // "Global Structure View" -> view(x =>Show.toMermaid_twoGraphs_Bissi(x,"TG"), Mermaid),
    "Global Structure View" -> view(x =>Show.toMermaid_Intrusive(x), Mermaid),
    "Local Structure View" -> view(x =>Show.toMermaid_twoGraphs_Bissi(System(x.main.getLevel0,Option(x.toCompare.getOrElse(x.main.empty).getLevel0)),"TG"), Mermaid),
    "Run Semantics (First Graph)" -> steps(e=>e, Warnings, x =>  Show.toMermaid(x.main,""), _.toString, Mermaid),
    "Run Semantics (Second Graph)" -> steps(e=> System(e.toCompare.getOrElse(e.main.empty),None), Warnings, x => Show.toMermaid(x.main,""), _.toString, Mermaid),
    // "Run Semantics" -> steps(e=>e, SemanticsTwo, x => Show.toMermaid_twoGraphs_Bissi(x,"AAAA"), _.toString, Mermaid),
    "Run Semantics With Intrusive Edges" -> steps(e=>e, IntrusiveProductA, x => Show.toMermaid_Intrusive(x), _.toString, Mermaid),
    "Run Semantics With Local Structure" -> steps(e=>e, Semantics, x => Show.toMermaid_twoGraphs(x.main,x.main.getLevel0,"RSLS"), _.toString, Mermaid),
    // "Build LTS" -> lts(x=>x, Semantics, x=>x.main.init, _.toString),
    "Generated LTS" -> view[System](x => Show.toMermaid(Program.lts(x.main),""), Mermaid),
    // "Build LTS (explore)" -> ltsExplore(e=>e, Semantics, x=>x.main.init, _.toString),
    // "Build LTS" -> lts(x=>x, Semantics, x=>x.init, _.toString),
    "Find Strong Bisimulation (given a program \"A ~ B\")" ->
      compareStrongBisim(Semantics, Semantics,
        (e: System) => System(e.main, None),
        (e: System) => System(e.toCompare.getOrElse(RxGr(Map.empty, Map.empty, " ", Set.empty)), None),
        Show.justTerm, Show.justTerm, _.toString),
    // "Asynchronous Product tesste" -> lts(x=>x, AsynchronousProduct, indexedViewSt, _.toString),
    "Run Semantics (Asynchornous Product)" -> steps(e=>e, AsynchronousProduct, x => Show.toMermaid_twoGraphs_Bissi(x,"AAAA"), _.toString, Mermaid),
    "Asynchronous Product" -> lts(x=>x, AsynchronousProduct, indexedViewSt, _.toString),
    // "Asynchronous Product" -> lts(x=>x, AsynchronousProduct, x=>("<"+ x.main.init+", "+x.toCompare.getOrElse(x.main.empty).init+">"), _.toString),
    "Run Semantics (Synchornous Product)" -> steps(e=>e, SynchronousProduct, x => Show.toMermaid_twoGraphs_Bissi(x,"AAAA"), _.toString, Mermaid),
    "Synchronous Product" -> lts(x=>x, SynchronousProduct, indexedViewSt, _.toString),
    // "Synchronous Product" -> lts(x=>x, SynchronousProduct, x=>("<"+ x.main.init+", "+x.toCompare.getOrElse(x.main.empty).init+">"), _.toString),
    "Run Semantics (Asynchornous Intrusive Product)" -> steps(e=>e, IntrusiveProductA, x => Show.toMermaid_Intrusive(x), _.toString, Mermaid),
    "Asynchornous Intrusive Product" -> lts(x=>x, IntrusiveProductA, indexedViewSt, _.toString),
    "Run Semantics (Synchornous Intrusive Product)" -> steps(e=>e, IntrusiveProductS, x => Show.toMermaid_Intrusive(x), _.toString, Mermaid),
    "Synchornous Intrusive Product" -> lts(x=>x, IntrusiveProductS, indexedViewSt, _.toString),
   )


    private var index = 0  
    def indexedViewSt(x: System): String = {
      index += 1
      s"<${x.main.init}, ${x.toCompare.getOrElse(x.main.empty).init}>_${index}"
    }

    // def indexedViewSt(x: System): String = {
    //   var index = 0  
    //   def f(x:System):String = 
    //     index += 1
    //     s"<${x.main.init}, ${x.toCompare.getOrElse(x.main.empty).init}>_${index}"
    //   f(x)
    //   }


  //// Documentation below

  override val footer: String =
    """Simple animator of Labelled Reactive Graphs, meant to exemplify the
      | CAOS libraries, used to generate this website.""".stripMargin 
      // Source code available online:
      // | <a target="_blank" href="https://github.com/arcalab/CAOS">
      // | https://github.com/arcalab/CAOS</a> (CAOS).""".stripMargin

  private val sosRules: String =    
    """ """.stripMargin

  override val documentation: Documentation = List(
    languageName -> "More information on the syntax of Reactive Graph" ->
    """|A program <code>RG</code> is a Reactive Graph with a syntax that follows the following template:
       |<pre>
       |init = Initial State;
       |l0 = {
       |    State from  --> State to by action, 
       |    };
       |ln = {
       |    (HE from, HE to, active, function),
       |    }
       |</pre>
       |
       |where:
       |</p><code>init</code> is the initial state; </p>
       |</p><code>l0</code> is a set of level 0 edges (E); use <code>--></code> to represent an enabled edge and <code>-.-></code> a disable edge; </p>
       |</p><code>ln</code> is a set of hyper edges (GE); these can start and end in either E or another HE. 
       | An HE is defined recursively, i.e., both the "from" and the "to" fields can be another HE, or a simpler E in the base case;</p>
       |</p><code>action</code> is a string that labels an E; it can have only letters in lower or upper case,  digits, and the symbols <code>_</code>, <code><</code>, <code>></code>, <code>.</code>, <code>-</code>, <code>€</code>, and <code>$</code>; </p>
       |</p><code>funtion</code> is either <code>ON</code> or <code>OFF</code>; representing whether the HE enables or disables the target edge, respectively.</p>
       """.stripMargin,
    //"Build LTS" -> "More information on the operational rules used here" -> sosRules,
    //"Build LTS (explore)" -> "More information on the operational rules used here" -> sosRules,
    //"Run semantics" -> "More information on the operational rules used here" -> sosRules,
    "Find strong bisimulation (given a program \"A ~ B\")" -> "More information on this widget" ->
     ("<p>When the input consists of the comparison of 2 programs separated by <code>~</code>, this widget " +
       "searches for a (strong) bisimulation between these 2 programs, providing either a " +
       "concrete bisimulation or an explanation of where it failed.</p>" +
       "<p>When only a program is provided, it compares it against the empty process <code>0</code>.</p>"),
  )


