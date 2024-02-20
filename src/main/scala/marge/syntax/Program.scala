package marge.syntax

import caos.sos.SOS
import marge.syntax.Program.Edge.{HyperEdge, SimpleEdge}

import scala.annotation.tailrec
import scala.scalajs.js.`new`
import cats.instances.boolean


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

  /** A reactive graph Rx has edges, an initial state, and a set of active edges. */
  case class RxGr(se:Map[State,Set[SimpleEdge]],// simple edges from st to outgoing edges
                  he:Map[Edge,Set[Edge]],       // hyperedges, from any edge to another edge (initially was from simple edge to any adge)
                  init: State,                  // initial state
                  active: Set[Edge]):           // set of active edges

    def toString2: String = s"$init${active.map(e=>s"\n${e.pretty}").mkString}"
    override def toString: String = s"$init${""}" 
    /** Auxiliary function to collect all states */
    def states:Set[State] = se.keySet + init
    /** Auxiliary function to collect all edges from a given simple edge */
    def getHe(e: Edge): Set[Edge] = he.getOrElse(e, Set())
    /** Auxiliary function to collect all edges from a given state */
    def getSe(st:State): Set[SimpleEdge] = se.getOrElse(st,Set())
    /** Returns the set of edges that exist from the initial state */
    def nextEdg: Set[SimpleEdge] = getSe(init)
    /** Returns a new RxGr with SimpleEdges only*/
    def getLevel0: RxGr =  
      val newactive = for case SimpleEdge(f,t,a,w) <- active yield SimpleEdge(f, t, a, w) 
      new RxGr(se,Map.empty,init,newactive)
      
    def empty: RxGr = RxGr(Map.empty, Map.empty,"", Set.empty)
    def isEmpty: Boolean = this == this.empty 

  /**
   * Evolves a reactive graph rxGr by performing a simple edge
   * @param rxGr the starting reactive graph
   * @param se the simple edge to be performed
   * @return None if `se` is not active or an inconsistency is found,
   *         or a new graph by updating the init state and the active edges,
   *         together with all action labels involved.
   */
  def step(rxGr: RxGr, se:SimpleEdge): Option[(RxGr,Set[Action])] = // | String =
    // stop if the edge is not active
    if !rxGr.active(se) then return None //"The edge are disable"
    // collect edges involved
    val edges =
      collectEdges(rxGr,rxGr.getHe(se),Set(se))
    // collect actions
    val actions = edges.map(_.act)
    // update active edges
    val active = updateActive(rxGr,edges)
    active.map(a => (RxGr(rxGr.se,rxGr.he,se.to,a),actions))

  /** Calculates all edges that are triggered, avoiding loops. */
  @tailrec
  private def collectEdges(gr: RxGr,
                           missing: Set[Edge],
                           done: Set[Edge]): Set[Edge] =
    missing.headOption match
      case Some(e) =>
        if gr.active(e) && !done(e)
        then collectEdges(gr,missing ++ gr.getHe(e) - e,done+e)
        else collectEdges(gr,missing-e,done)
      case None => done

  /** Given a set of edges that are triggered, calculates a new set of
   * active edges, returning None if some edge is both activated and deactivated. */
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
      case (_,None,0) => s"No deadlocks found, but stopped after $maxit states."
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

