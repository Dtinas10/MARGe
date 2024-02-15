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
          case Some(t) => s
          }  
    }
    s


  
