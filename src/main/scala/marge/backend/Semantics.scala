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


  
