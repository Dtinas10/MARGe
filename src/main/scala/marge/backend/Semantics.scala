package marge.backend

import caos.sos.SOS
import marge.syntax.Program
import marge.syntax.Program.*
import marge.syntax.Program.RxGr.*
import marge.syntax.Program.Edge.*
import marge.syntax.Program.System


val cross = '\u274C' 
val proibitionMark = '\u26D4' 
val exclamationMark  = '\u2757' 

object Semantics extends SOS[String,System]: 
  def next[A>:String](st : System): Set[(A,System)] =
    val g: RxGr = st.main 
    var s: Set[(A,System)] = Set.empty
    for (i<- g.nextEdg) {
      var k = step(g,i)
      if k != None then
        s = s ++ Set((i.action,System(k.map(_._1).get,st.toCompare)))
    }
    s

object SemanticsTwo extends SOS[String,System]: 
  def next2[A>:String](st : System): Set[(A,System)] = 
    Semantics.next(st) ++ 
    Semantics.next(System(st.toCompare.getOrElse(st.main.empty),Option(st.main)))

  def next[A>:String](st : System): Set[(A,System)] =
    val g: RxGr = st.main 
    val g2: RxGr = st.toCompare.getOrElse(g.empty) 
    var s: Set[(A,System)] = Set.empty
    for (i<- g.nextEdg) {
      var k = step(g,i)
      if k != None then
        s = s ++ Set((i.action,System(k.map(_._1).get,st.toCompare)))
    }
    for (i<- g2.nextEdg) {
      var k = step(g2,i)
      if k != None then
        s = s ++ Set((i.action,System(st.main,Option(k.map(_._1).get))))
    }
    s
  def next1[A >: String](st: System): Set[(A, System)] = st.toCompare match{
    case None => Warnings.next(st)
    case _ =>
      var g: RxGr = st.main 
      var g2: RxGr = st.toCompare.getOrElse(g.empty)
      var s: Set[(A,System)] = Set.empty
      val actions1 = g.actions
      val actions2 = g2.actions
      for ( i <- g.nextEdg ) {
        var k = step(g,i)
        if k != None && !actions2.contains(i.action) then
          s = s ++ Set((i.action,System(k.map(_._1).get,Option(g2))))
      }
      
      for (i<- g2.nextEdg) {
        var k = step(g2,i)  
        if k != None && !actions1.contains(i.action) then
          s = s ++ Set((i.action,System(g,Option(k.map(_._1).get))))
      }

      val inter = actions1.intersect(actions2)
      if !inter.isEmpty then
        for (i <- g.nextEdgAction(inter)) {
          for (j <- g2.nextEdgAction(inter)) {
            if i.action == j.action then
              var k1 = step(g,i)  
              var k2 = step(g2,j)  
              if k1 != None && k2 != None then
                s = s ++ Set((i.action,System(k1.map(_._1).get,Option(k2.map(_._1).get))))
          }
        }
      s
  }







object AsynchronousProduct2 extends SOS[String,System]:
  def next[A >: String](st: System): Set[(A, System)] = 
    var g: RxGr = st.main 
    var g2: RxGr = st.toCompare.getOrElse(g.empty)
    var s: Set[(A,System)] = Set.empty
    var counter: Int = 0
    for ( i <- g.nextEdg ) {
      var k = step(g,i)
      if k != None then
        var r: RxGr = k.map(_._1).get
        r = RxGr(r.se,r.he,r.init + counter, r.active)
        s = s ++ Set((i.action,System(r,Option(g2))))
        counter += 1
    }
    for (i<- g2.nextEdg) {
      var k = step(g2,i)  
      if k != None then
        s = s ++ Set((i.action,System(g,Option(k.map(_._1).get))))
    }
    s

object AsynchronousProduct extends SOS[String,System]:
  def next[A >: String](st: System): Set[(A, System)] = 
    var g: RxGr = st.main 
    var g2: RxGr = st.toCompare.getOrElse(g.empty)
    var s: Set[(A,System)] = Set.empty
    for ( i <- g.nextEdg ) {
      var k = step(g,i)
      if k != None then
        s = s ++ Set((i.action,System(k.map(_._1).get,Option(g2))))
    }
    for (i<- g2.nextEdg) {
      var k = step(g2,i)  
      if k != None then
        s = s ++ Set((i.action,System(g,Option(k.map(_._1).get))))
    }
    s


object SynchronousProduct extends SOS[String,System]:
  def next[A >: String](st: System): Set[(A, System)] = 
    var g: RxGr = st.main 
    var g2: RxGr = st.toCompare.getOrElse(g.empty)
    var s: Set[(A,System)] = Set.empty
    val actions1 = g.actions
    val actions2 = g2.actions
    for ( i <- g.nextEdg ) {
      var k = step(g,i)
      if k != None && !actions2.contains(i.action) then
        s = s ++ Set((i.action,System(k.map(_._1).get,Option(g2))))
    }
    
    for (i<- g2.nextEdg) {
      var k = step(g2,i)  
      if k != None && !actions1.contains(i.action) then
        s = s ++ Set((i.action,System(g,Option(k.map(_._1).get))))
    }

    val inter = actions1.intersect(actions2)
    if !inter.isEmpty then
      for (i <- g.nextEdgAction(inter)) {
        for (j <- g2.nextEdgAction(inter)) {
          if i.action == j.action then
            var k1 = step(g,i)  
            var k2 = step(g2,j)  
            if k1 != None && k2 != None then
              s = s ++ Set((i.action,System(k1.map(_._1).get,Option(k2.map(_._1).get))))
        }
      }
    s


  
object Warnings extends SOS[String,System]:
  def next[A>:String](st : System): Set[(A,System)] =
    val g: RxGr = st.main 
    var s: Set[(A,System)] = Set.empty
    for (i<- g.nextEdg) {
      var k = step(g,i)
      if k != None then
        s = s ++ Set((i.action,System(k.map(_._1).get,st.toCompare)))
      else 
        g.se.get(g.init) match{
          case None => 
              s = s ++ Set((s"${exclamationMark}Warning$exclamationMark: $cross ${i.action}",System(g.empty,st.toCompare)))        
          case Some(t) => s = s 
          }  
    }
    s

object IntrusiveProductA extends SOS[String,System]:
  def next[A >: String](st: System): Set[(A, System)] = 
    var g: RxGr = st.main 
    var g2: RxGr = st.toCompare.getOrElse(g.empty)
    var intro: Map[Edge,Set[Edge]] = st.intro._1
    var s: Set[(A,System)] = Set.empty
    var nextedg1: Set[SimpleEdge] = (for {edge <- g.nextEdg if g.active.contains(edge)} yield edge).toSet
    var nextedg2: Set[SimpleEdge] = (for {edge <- g2.nextEdg if g2.active.contains(edge)} yield edge).toSet

    var gg2:RxGr = g2
    for ( i <- nextedg1 ) {
      if intro.keySet.contains(i) then gg2 = RxGr(g2.se,g2.he,g2.init,updateActive(g2,intro(i)))
      else gg2 = g2
      var k = step(g,i)
      if k != None  then
        s = s ++ Set((i.action,System(k.map(_._1).get,Option(gg2),st.intro)))
    }
    var gg1 = g
    for ( j <- nextedg2) {
      if intro.keySet.contains(j) then  gg1 = RxGr(g.se,g.he,g.init,updateActive(g,intro(j)))
      else gg1 = g
      var k = step(g2,j)  
      if k != None then
        s = s ++ Set((j.action,System(gg1,Option(k.map(_._1).get),st.intro)))
    }
    s






object IntrusiveProductS extends SOS[String,System]:
  def next[A >: String](st: System): Set[(A, System)] = 
    var g: RxGr = st.main 
    var g2: RxGr = st.toCompare.getOrElse(g.empty)
    var intro: Map[Edge,Set[Edge]] = st.intro._1
    var s: Set[(A,System)] = Set.empty
    var nextedg1: Set[SimpleEdge] = (for {edge <- g.nextEdg if g.active.contains(edge)} yield edge).toSet
    var nextedg2: Set[SimpleEdge] = (for {edge <- g2.nextEdg if g2.active.contains(edge)} yield edge).toSet
    val actions1 = g.actions
    val actions2 = g2.actions

    var gg2:RxGr = g2
    for ( i <- nextedg1 ) {
      if intro.keySet.contains(i) then gg2 = RxGr(g2.se,g2.he,g2.init,updateActive(g2,intro(i)))
      else gg2 = g2
      var k = step(g,i)
      if k != None && !actions2.contains(i.action) then
        s = s ++ Set((i.action,System(k.map(_._1).get,Option(gg2),st.intro)))
    }
    var gg1 = g
    for ( j <- nextedg2) {
      if intro.keySet.contains(j) then  gg1 = RxGr(g.se,g.he,g.init,updateActive(g,intro(j)))
      else gg1 = g
      var k = step(g2,j)  
      if k != None && !actions1.contains(j.action) then
        s = s ++ Set((j.action,System(gg1,Option(k.map(_._1).get),st.intro)))
    }
    val inter = actions1.intersect(actions2)
    if !inter.isEmpty then
      for (i <- g.nextEdgAction(inter)) {
        for (j <- g2.nextEdgAction(inter)) {
          if i.action == j.action then
            var kk1 = step(g,i)  
            var kk2 = step(g2,j)  
            if kk1 != None && kk2 != None then
              var k1 = kk1.map(_._1).get
              var k2 = kk2.map(_._1).get
              if intro.keySet.contains(i) then k2 = RxGr(k2.se,k2.he,k2.init,updateActive(k2,intro(i)))
              if intro.keySet.contains(j) then k1 = RxGr(k1.se,k1.he,k1.init,updateActive(k1,intro(j)))
              s = s ++ Set((i.action,System(k1,Option(k2),st.intro)))
              // s = s ++ Set((i.action,System(k1.map(_._1).get,Option(k2.map(_._1).get),intro)))
        }
      }
    s

    
/** Given a set of Edge that are triggered, calculates a new set of
 * active Edge, returning None if some edge is both activated and deactivated. */
def updateActive(gr: RxGr, es: Set[Edge]): Set[Edge] =
  val toActivate   = for case HyperEdge(_,to,_,true)  <-es yield to
  val toDeactivate = for case HyperEdge(_,to,_,false) <-es yield to
  gr.active++toActivate--toDeactivate

  
