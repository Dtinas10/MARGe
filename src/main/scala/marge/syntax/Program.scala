package marge.syntax

import caos.sos.SOS
import marge.syntax.Program.Edge.{HyperEdge, SimpleEdge}

import scala.annotation.tailrec
import scala.scalajs.js.`new`
import cats.instances.boolean
import javax.swing.event.HyperlinkEvent
import javax.swing.text.StyledEditorKit.BoldAction

import marge.backend.Semantics

object Program:
  type State = String
  type Action = String
  type Weight = Double

  /** An edge can be simple (from state to state) or hyper (from a simlpe edge to any edge. */
  enum Edge:
    case SimpleEdge(from: State,
                    to: State,
                    action: Action,
                    weight: Weight = 0.0)
    case HyperEdge(from: Edge,
                   to: Edge,
                  //  action: Action,
                   weight: Weight = 0.0,
                   activate: Boolean)
    def pretty:String = this match
      case e:SimpleEdge =>
        s"${e.from}-${e.action}->${e.to}"
      case e:HyperEdge =>
        s"[${e.from.pretty}]->[${e.to.pretty}]"

    /** Returns the action label of any edge */
    def act: Action = this match
      case s:SimpleEdge => s.action
      case h:HyperEdge  => ""//h.action

  /** A reactive graph Rx has Edge, an initial state, and a set of active Edge. */
  case class RxGr(se:Map[State,Set[SimpleEdge]],// simple Edge from st to outgoing Edge
                  he:Map[Edge,Set[Edge]],       // hyperEdge, from any edge to another edge (initially was from simple edge to any adge)
                  init: State,                  // initial state
                  active: Set[Edge]):           // set of active Edge

    def toString2: String = s"$init${active.map(e=>s"\n${e.pretty}").mkString}"
    override def toString: String = s"$init${""}" 
    /** Auxiliary function to collect all states */
    // def states:Set[State] = se.keySet + init
    def states:Set[State] = 
      var states:Set[State] = Set.empty
      for ((state, set) <- se ){
        states += state
        for (i <- set){
          states += i.to
        }
      } 
      states
    /** Auxiliary function to collect all Edge from a given simple edge */
    def getHe(e: Edge): Set[Edge] = he.getOrElse(e, Set())
    /** Auxiliary function to collect all Edge from a given state */
    def getSe(st:State): Set[SimpleEdge] = se.getOrElse(st,Set())
    /** Returns the set of Edge that exist from the initial state */
    def nextEdg: Set[SimpleEdge] = getSe(init)
    // def nextEdgAction(actions: Set[Action]): Set[SimpleEdge] = for (a <- actions) {for edge  <- nextEdg  if edge.action == a yield edge} 
    def nextEdgAction(actions: Set[Action]): Set[SimpleEdge] = nextEdg.filter(edge => actions.contains(edge.action)) 
    /** Returns a new RxGr with SimpleEdge only*/
    def getLevel0: RxGr =  
      val newactive = for case SimpleEdge(f,t,a,w) <- active yield SimpleEdge(f, t, a, w) 
      new RxGr(se,Map.empty,init,newactive)
      
    def empty: RxGr = RxGr(Map.empty, Map.empty,"", Set.empty)
    def isEmpty: Boolean = this == this.empty 
    def actions: Set[Action] = 
      var action: Set[Action] = Set.empty
      for ((s,e) <- se){ 
        for (edge <- e){
          action += edge.act
        }
      }
      action

  // def lts(g: RxGr): RxGr = 
    // var newse: Map[State, Set[SimpleEdge]] = Map.empty
    // var allConf: Set[RxGr] = Set.empty
    // var lessConf: Set[RxGr] = Set(g)
    // var current:RxGr  = g
    // while(!lessConf.isEmpty) {
    //   current = lessConf.head
    //   for {i <- current.nextEdg}{
    //     step(current,i) match{
    //       case None => None
    //       case next2 =>
    //         var next = next2.map(_._1).get
    //         if allConf.contains(next) then
    //           newse += (current.init -> Set())
    //         else lessConf += next 
    //     }
    //   }
    //   lessConf -= current
    // }
    // RxGr(se = newse, he = Map.empty, init = g.init, active = Set())


  def lts(g: RxGr): RxGr = 
    var newse: Map[State, Set[SimpleEdge]] = Map.empty
    var newactive: Set[Edge] = Set.empty
    var allConf: Set[(RxGr,Int)] = Set.empty
    var lessConf: Set[(RxGr,Int)] = Set((g,0))
    // var current:RxGr  = g
    var counter: Int = 1
    while(!lessConf.isEmpty) {
      var (current,cid) = lessConf.head
      var next = Semantics.next(System(current,None))
      for{(action,i) <- next}{
        val id: Option[Int] = allConf.find(_._1 == i.main).map(_._2)
        id match{
          case None => 
            var edge:SimpleEdge = SimpleEdge(current.init + ".."+cid,i.main.init + ".."+counter,action)
            newse = addEdge(newse,edge)
            newactive += edge
            lessConf += (i.main,counter)
            counter += 1
          case Some(n:Int) =>
            var edge:SimpleEdge = SimpleEdge(current.init+".."+cid,i.main.init + ".."+n,action)
            newse = addEdge(newse,edge)
            newactive += edge
        }
      }
      allConf += (current,cid)
      lessConf -= (current,cid)
    }
    RxGr(se = newse, he = Map.empty, init = g.init+"..0", active = newactive)

  // def addEdge(m: Map[State, Set[SimpleEdge]], e: SimpleEdge): Map[State, Set[SimpleEdge]] = m.get(e.from) match{
  //     case None => m + (e.from -> Set(e))
  //     case set: Set[SimpleEdge]  => m + (e.from -> set + e)
  //   }

  def addEdge(m: Map[State, Set[SimpleEdge]], e: SimpleEdge): Map[State, Set[SimpleEdge]] =
    val currentEdges = m.getOrElse(e.from, Set.empty[SimpleEdge])
    m.updated(e.from, currentEdges + e)



  /**
   * Evolves a reactive graph rxGr by performing a simple edge
   * @param rxGr the starting reactive graph
   * @param se the simple edge to be performed
   * @return None if `se` is not active or an inconsistency is found,
   *         or a new graph by updating the init state and the active Edge,
   *         together with all action labels involved.
   */
  def step(rxGr: RxGr, se:SimpleEdge): Option[(RxGr,Set[Action])] = // | String =
    // stop if the edge is not active
    if !rxGr.active(se) then return None //"The edge are disable"
    // collect Edge involved
    val Edge =
      collectEdge(rxGr,rxGr.getHe(se),Set(se))
    // collect actions
    val actions = Edge.map(_.act)
    // update active Edge
    val active = updateActive(rxGr,Edge)
    active.map(a => (RxGr(rxGr.se,rxGr.he,se.to,a),actions))

  /** Calculates all Edge that are triggered, avoiding loops. */
  @tailrec
  private def collectEdge(gr: RxGr,
                           missing: Set[Edge],
                           done: Set[Edge]): Set[Edge] =
    missing.headOption match
      case Some(e) =>
        if gr.active(e) && !done(e)
        then collectEdge(gr,missing ++ gr.getHe(e) - e,done+e)
        else collectEdge(gr,missing-e,done)
      case None => done

  /** Given a set of Edge that are triggered, calculates a new set of
   * active Edge, returning None if some edge is both activated and deactivated. */
  private def updateActive(gr: RxGr, es: Set[Edge]): Option[Set[Edge]] =
    val toActivate   = for case HyperEdge(_,to,_,true)  <-es yield to
    val toDeactivate = for case HyperEdge(_,to,_,false) <-es yield to
    if toActivate.intersect(toDeactivate).nonEmpty
    then None
    else Some(gr.active--toDeactivate++toActivate)

  /**
   * Searches for a deadlock state of a system
   * @param g initial graph
   * @param maxit maximal number of states to traverse before timing out
   * @return String explaining the search result.
   */
  def findDeadlockPP(g:System,maxit: Int = 500): String =
    findDeadlock(Set(g),Set(),maxit)(using marge.backend.Semantics) match
      case (None,0) => s"No deadlocks found, but stopped after $maxit states."
      case (None,n) => s"No deadlocks found after ${maxit-n} states"
      case (Some(g),n) => s"Found deallock @ ${g}"

  // I implemented findDeadlock for any SOS, but I could have done it just for "System".
  private def findDeadlock[Act,St](miss: Set[St], know: Set[St], maxit: Int)(using sos: SOS[Act,St]): (Option[St],Int) =
    if maxit <= 0 then (None,0)          // reached maximum iterations
    else miss.headOption match
      case None => (None,maxit)            // no more states to traverse
      case Some(nextSt) if know(nextSt) => // next state exists but is known
        findDeadlock(miss-nextSt,know,maxit)
      case Some(nextSt) =>                 // next state exists and is new
        val more = sos.next(nextSt)
        if more.isEmpty
        then (Some(nextSt),maxit)
        else
          val newMiss = (miss-nextSt) ++ more.map(_._2)
          findDeadlock(newMiss, know+nextSt, maxit-1)

  def findDeadlockTracePP(g:System,maxit: Int = 500): String =
    findDeadlockTrace(Map(g->None),Map(),maxit)(using marge.backend.Semantics) match
      case (_,None,0) => s"No deadlocks found, but stopped after $maxit states. "
      case (_,None,n) => s"No deadlocks found after ${maxit-n} states"
      case (trace,Some(g),n) => s"Found deallock @ ${g} ${trace.map(ap=>s"\n   <-[${ap._1}]- ${ap._2}").mkString}"

  private def findDeadlockTrace[Act, St](miss: Map[St,Option[(Act,St)]],
                                 know: Map[St,Option[(Act,St)]],
                                 maxit: Int)(using sos: SOS[Act, St]): (Seq[(Act,St)],Option[St], Int) =
    def buildTrace(parent:Option[(Act,St)]): List[(Act,St)] =
      parent match
        case None => List[(Act,St)]()
        case Some((act,prev)) => (act,prev)::buildTrace(know.getOrElse(prev,None))

    if maxit <= 0 then (Nil,None, 0) // reached maximum iterations
    else miss.headOption match
      case None => (Nil,None, maxit) // no more states to traverse
      case Some((nextSt,_)) if know contains nextSt => // next state exists but is known
        findDeadlockTrace(miss - nextSt, know, maxit)
      case Some((nextSt,parent)) => // next state exists and is new
        val more = sos.next(nextSt)
        if more.isEmpty
        then (buildTrace(parent),Some(nextSt), maxit)
        else
          val newMiss = (miss - nextSt) ++ more.map(kv=>(kv._2,Some(kv._1->nextSt)))
          findDeadlockTrace(newMiss, know + (nextSt->parent), maxit - 1)


  def findIncoPP(g:System,maxit: Int = 500): String =
    findIncoTrace(Map(g-> None),Map(),maxit)(using marge.backend.Semantics) match
      case (_,None,0) => s"No Conflits found, but stopped after $maxit states."
      case (_,None,n) => s"No Conflits found after ${maxit-n} states"
      case (trace,Some(g),n) => s"Found Conflits @ ${g} ${trace.map(ap=>s"\n   <-[${ap._1}]- ${ap._2}").mkString}"


  private def findIncoTrace[Act, St](miss: Map[St,Option[(Act,St)]],
                              know: Map[St,Option[(Act,St)]],
                              maxit: Int)(using sos: SOS[Act, St]): (Seq[(Act,St)],Option[St], Int) =
    def buildTrace(parent:Option[(Act,St)]): List[(Act,St)] =
      parent match
        case None => List[(Act,St)]()
        case Some((act,prev)) => (act,prev)::buildTrace(know.getOrElse(prev,None))

    if maxit <= 0 then (Nil,None, 0) // reached maximum iterations
    else miss.headOption match
      case None => (Nil,None, maxit) // no more states to traverse
      case Some((nextSt,_)) if know contains nextSt => // next state exists but is known
        findIncoTrace(miss - nextSt, know, maxit)
      case Some((nextSt,parent)) => // next state exists and is new
        val more = sos.next(nextSt)
        if more.isEmpty then
          val gg: RxGr = nextSt match {case x:System => x.main}
          if !gg.nextEdg.isEmpty then
            for (i <- gg.nextEdg){
              if gg.active.contains(i) then
                return (buildTrace(parent),Some(nextSt), maxit)
            }
        val newMiss = (miss - nextSt) ++ more.map(kv=>(kv._2,Some(kv._1->nextSt)))
        findIncoTrace(newMiss, know + (nextSt->parent), maxit - 1)

  
  case class System(main:RxGr, toCompare:Option[RxGr]):
    def apply(newMain:RxGr) = System(newMain,toCompare)
    override def toString: String = s"${main.init}${""}" 

// ------------------------------ EXPERIMENTS


  // def pro(st:System):RxGr =
  //   var g: RxGr = st.main 
  //   var g2: RxGr = st.toCompare.getOrElse(g.empty)
  //   union(
  //     System(
  //       product(System(g,Option(g2)),true),
  //       Option(product(System(g2,Option(g)),false))
  //       )
  //     )


  // def product(g:System, bool: Boolean): RxGr =
  //   val g1: RxGr = g.main 
  //   val g2: RxGr = g.toCompare.getOrElse(g1.empty)// match {case Some(x) => x}
  //   // var newse: Map[State,Set[SimpleEdge]] =  combinedse(expandse(g1.se,g2.states,true), expandse(g2.se, g1.states,false)) 
  //   var newse: Map[State,Set[SimpleEdge]] =  expandse(g1.se,g2.states,bool) 
  //   // var newhe: Map[Edge,Set[Edge]] = combinedhe(expandhe(g1.he,g2.states,true), expandhe(g2.he, g1.states,false)) 
  //   var newhe: Map[Edge,Set[Edge]] = expandhe(g1.he,g2.states,bool) 
  //   // var newactive: Set[Edge] = expandActive(g1.active,g2.states,true) ++ expandActive(g2.active,g1.states,false)
  //   var newactive: Set[Edge] = expandActive(g1.active,g2.states,bool)
  //   var newinit = g1.init+g2.init

  //   RxGr(newse, newhe, newinit, newactive)



  // def expandse(origise: Map[State, Set[SimpleEdge]],s: Set[State],bool: Boolean): Map[State, Set[SimpleEdge]] = {
  //   var map : Map[State, Set[SimpleEdge]] = Map.empty
  //   for (e <- s){
  //     for ((origiSt, origiSet) <- origise){
  //       if bool then
  //         val newSt = origiSt + e 
  //         val newSet:Set[SimpleEdge] = origiSet.map{case edge:SimpleEdge => SimpleEdge(edge.from + e, edge.to + e, edge.act,edge.weight)}
  //         map = map + (newSt -> newSet) 
  //       else
  //         val newSt = e + origiSt  
  //         val newSet:Set[SimpleEdge] = origiSet.map{case edge:SimpleEdge => SimpleEdge(e + edge.from, e + edge.to, edge.act,edge.weight)}
  //         map = map + (newSt -> newSet) 
  //     }  
  //   }
  //   map
  // }  

  // def expandhe(origihe: Map[Edge, Set[Edge]],s: Set[State],bool: Boolean): Map[Edge, Set[Edge]] = {
  //   var map : Map[Edge, Set[Edge]] = Map.empty
  //   for (e <- s){
  //     for ((origiEd, origiSet) <- origihe){
  //       if bool then
  //         val newEd = addAct(origiEd,e) 
  //         val newSet:Set[Edge] = origiSet.map{case edge:HyperEdge => addAct(edge,e)}
  //         map = map + (newEd -> newSet) 
  //       else
  //         val newEd = addAct2(origiEd,e)  
  //         val newSet:Set[Edge] = origiSet.map{case edge:HyperEdge => addAct2(edge,e)}
  //         map = map + (newEd -> newSet) 
  //     }  
  //   }
  //   map
  // }

  // def expandActive(set: Set[Edge], s: Set[State],bool:Boolean): Set[Edge]= {
  //   var newset : Set[Edge] = Set.empty
  //   for (e <- s){
  //     for (edge <- set){
  //       if bool then
  //         newset = newset  + addAct(edge,e) 
  //       else
  //         newset = newset  + addAct2(edge,e) 
  //     }  
  //   }
  //   newset
  // }
      
  // def addAct(e: Edge, s: String): Edge = e match {
  //   case SimpleEdge(from, to, action, weight) =>
  //     SimpleEdge(from + s, to + s, action, weight)
  //   case HyperEdge(from, to, weight, activate) =>
  //     HyperEdge(addAct(from, s), addAct(to, s), weight, activate) 
  // } 

  // def addAct2(e: Edge, s: String): Edge = e match {
  //   case SimpleEdge(from, to, action, weight) =>
  //     SimpleEdge(s + from, s + to, action, weight)
  //   case HyperEdge(from, to, weight, activate) =>
  //     HyperEdge(addAct2(from, s), addAct2(to, s), weight, activate) 
  // }


  // def combinedse(map1:Map[State, Set[SimpleEdge]],map2:Map[State, Set[SimpleEdge]]): Map[State, Set[SimpleEdge]] = 
  //   (map1.keySet ++ map2.keySet).map { key =>
  //   key -> (map1.getOrElse(key, Set.empty) ++ map2.getOrElse(key, Set.empty))}.toMap
  
  // def combinedhe(map1:Map[Edge, Set[Edge]],map2:Map[Edge, Set[Edge]]): Map[Edge, Set[Edge]] = 
  //   (map1.keySet ++ map2.keySet).map { key =>
  //   key -> (map1.getOrElse(key, Set.empty) ++ map2.getOrElse(key, Set.empty))}.toMap

  // def union(g:System): RxGr =
  //   val g1: RxGr = g.main 
  //   val g2: RxGr = g.toCompare.getOrElse(g1.empty)// match {case Some(x) => x}
  //   // val e1:SimpleEdge =  SimpleEdge("NIS",g1.init,"-",0)
  //   // val e2:SimpleEdge =  SimpleEdge("NIS",g2.init,"-",0)
  //   // var newse: Map[State,Set[SimpleEdge]] =  combinedse(combinedse(g1.se,g2.se), Map("NIS" -> Set(e1,e2)))
  //   var newse: Map[State,Set[SimpleEdge]] =  combinedse(g1.se,g2.se)
  //   var newhe: Map[Edge,Set[Edge]] = combinedhe(g1.he, g2.he) 
  //   var newactive: Set[Edge] = g1.active ++ g2.active //++ Set(e1,e2)
  //   // RxGr(newse, newhe,"NIS", newactive)
  //   RxGr(newse, newhe,g1.init+g2.init, newactive)

  // def merge(g:System): RxGr =
  //   val g1: RxGr = g.main 
  //   val g2: RxGr = g.toCompare.getOrElse(g1.empty)// match {case Some(x) => x}
  //   var newmap: Map[State, Set[SimpleEdge]] = Map.empty
  //   var newactive: Set[Edge] = g1.active ++ g2.active
  //   for (st <- g1.states){
  //     newmap = newmap + ( st -> Set(SimpleEdge(st,g2.init,"-",0)))
  //     newactive += SimpleEdge(st,g2.init,"-",0)
  //   }
  //   var newse: Map[State,Set[SimpleEdge]] =  combinedse(combinedse(g1.se,g2.se), newmap)
  //   var newhe: Map[Edge,Set[Edge]] = combinedhe(g1.he, g2.he) 
  //   RxGr(newse, newhe,g1.init, newactive)




  // case class SystemT(main:RxGr, prod: RxGr, result: Oprion[RxGr]):
  //   def apply(newMain:RxGr) = System(newMain,toCompare)
  //   override def toString: String = s"${main.init}${""}"

