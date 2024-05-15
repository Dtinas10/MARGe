package marge.syntax

import cats.parse.Parser.*
import cats.parse.{LocationMap, Parser as P, Parser0 as P0}
import cats.parse.Rfc5234.{sp, alpha, digit}
import marge.syntax.Program.*
import marge.syntax.Program.RxGr.*
import marge.syntax.Program.Edge.*

import scala.sys.error

object Parser :

  // /** Parse a command  */
  def parseProgram(str:String):System = 
    
    pp(program,str) match {
      case Left(e) => error(e)
      case Right(c) => c
    }

  /** Applies a parser to a string, and prettifies the error message */ 
  def pp[A](parser:P[A], str:String): Either[String,A] =
    parser.parseAll(str) match
      case Left(e) => Left(prettyError(str,e))
      case Right(x) => Right(x)
  
  private def prettyError(str:String, err:Error): String =
    val loc = LocationMap(str)
    val pos = loc.toLineCol(err.failedAtOffset) match
      case Some((x,y)) =>
        s"""at ($x,$y):
           |"${loc.getLine(x).getOrElse("-")}"
           |${("-" * (y+1))+"^\n"}""".stripMargin
      case _ => ""
    s"${pos}expected: ${err.expected.toList.mkString(", ")}\noffsets: ${
      err.failedAtOffset};${err.offsets.toList.mkString(",")}"



  // Simple parsers for spaces and comments
  /** Parser for a sequence of spaces or comments */
  private val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  private val comment: P[Unit] = string("//") *> P.charWhere(_!='\n').rep0.void
  private val sps: P0[Unit] = (whitespace | comment).rep0.void

  // Parsing smaller tokens
  private def alphaDigit: P[Char] =
    P.charIn('A' to 'Z') | P.charIn('a' to 'z') | P.charIn('0' to '9') | P.charIn('_', '<','>','.','-','€','$')
  private def Digit: P[Char] =
    P.charIn('0' to '9') | P.charIn('.')
  def varName: P[String] =
    (charIn('a' to 'z') ~ alphaDigit.rep0).string
  def procName: P[String] =
    (charIn('A' to 'Z') ~ alphaDigit.rep0).string
  private def symbols: P[String] =
    // symbols starting with "--" are meant for syntactic sugar of arrows, and ignored as symbols of terms
    P.not(string("--")).with1 *>
    oneOf("+-><!%/*=|&".toList.map(char)).rep.string

    
  import scala.language.postfixOps

  
  private def on:  P[Boolean] = P.string("ON").map( x => true)
  private def off: P[Boolean] = P.string("OFF").map( x => false)
  private def bullet: P[Boolean] = P.string("Bullet").map(x => true)
  private def circ:   P[Boolean] = P.string("Circ").map(x => false)
  private def state: P[State] = alphaDigit.rep.string
  private def edgeaction: P[Action] = alphaDigit.rep.string 
  private def edgeweight: P[Weight] = ((char('-').?.with1 ~ digit.rep(1) ~ (char('.').with1 ~ digit.rep(1)).?).string).map(_.toDouble)
  private def edgeactive: P[Boolean] = bullet | circ
  private def edgefunction: P[Boolean] = on | off

  private def bullet2: P[Boolean] = P.string("-->").map(x => true)
  private def circ2:   P[Boolean] = P.string("-.->").map(x => false)
  private def edgeactive2: P[Boolean] = bullet2 | circ2
  
  // Verison with parantesis
  private def simpleEdge: P[(SimpleEdge,Boolean)] = 
    (((((P.char('(') *> 
       state.surroundedBy(sps) <* P.string(",")) ~
       state.surroundedBy(sps) <* P.string(",")) ~
       edgeaction.surroundedBy(sps) <* P.char(',')) ~
       edgeweight.surroundedBy(sps) <* P.char(',')) ~
       edgeactive.surroundedBy(sps) <* P.char(')'))
       .map { case (((((from,to),action),w),active)) => 
        (SimpleEdge(from, to, action, w),active)}

  // verison a --> b by a,0,      
  private def simpleEdge2: P[(SimpleEdge,Boolean)] = 
    (((( state.surroundedBy(sps) ~
       edgeactive2.surroundedBy(sps)) ~
       state.surroundedBy(sps) <* P.string("by")) ~
       edgeaction.surroundedBy(sps) <* P.char(',')) ~
       edgeweight.surroundedBy(sps))
       .map { case (((((from,active),to),action),w)) => 
        (SimpleEdge(from, to, action, w),active)}
  
  //verison without weight      
  private def simpleEdge_withoutWeight: P[(SimpleEdge,Boolean)] = 
    ((( state.surroundedBy(sps) ~
       edgeactive2.surroundedBy(sps)) ~
       state.surroundedBy(sps) <* P.string("by")) ~
       edgeaction.surroundedBy(sps))
       .map { case ((((from,active),to),action)) => 
        (SimpleEdge(from, to, action, 0),active)}
  
  private def simpleEdgeN: P[(SimpleEdge,Boolean)] = 
    (((((P.char('(') *> 
       state.surroundedBy(sps) <* P.string(",")) ~
       state.surroundedBy(sps) <* P.string(",")) ~
       edgeaction.surroundedBy(sps) <* P.char(',')) ~
       edgeweight.surroundedBy(sps) <* P.char(')')))
       .map { case (((from,to),action),w) => (SimpleEdge(from, to, action, w), true)}

  //version without weight     
  private def simpleEdgeN_withoutWeight: P[(SimpleEdge,Boolean)] = 
    (((P.char('(') *> 
       state.surroundedBy(sps) <* P.string(",")) ~
       state.surroundedBy(sps) <* P.string(",")) ~
       edgeaction.surroundedBy(sps) <* P.char(')'))
       .map { case ((from,to),action) => (SimpleEdge(from, to, action, 0), true)}

  private def edge: P[(Edge,Boolean)] =  P.defer(simpleEdgeN.backtrack | hyperEdge)
  //version without weight 
  private def edge_withoutWeight: P[(Edge,Boolean)] =  P.defer(simpleEdgeN_withoutWeight.backtrack | hyperEdge_withoutWeight) 

  private def hyperEdge: P[(HyperEdge,Boolean)] = 
    (((((P.char('(') *> edge.surroundedBy(sps) <* P.string(",")) ~
      (edge.surroundedBy(sps)) <* P.char(',')) ~
      edgeweight.surroundedBy(sps) <* P.char(',')) ~
      edgeactive.surroundedBy(sps) <* P.char(',')) ~
      edgefunction.surroundedBy(sps) <* P.char(')'))
      .map{ case (((((from,to),w),active),function)) => 
        (HyperEdge(from._1, to._1, w, function),active)
  }

  //version without weight
  private def hyperEdge_withoutWeight: P[(HyperEdge,Boolean)] = 
    ((((P.char('(') *> edge_withoutWeight.surroundedBy(sps) <* P.string(",")) ~
      (edge_withoutWeight.surroundedBy(sps)) <* P.char(',')) ~
      edgeactive.surroundedBy(sps) <* P.char(',')) ~
      edgefunction.surroundedBy(sps) <* P.char(')'))
      .map{ case ((((from,to),active),function)) => 
        (HyperEdge(from._1, to._1, 0, function),active)
  }

  private def level0: P[(Map[State, Set[SimpleEdge]], Set[Edge])] =
    (((P.string("l0") *> P.char('=').surroundedBy(sps)) *> P.char('{').surroundedBy(sps)) *>
      (simpleEdge_withoutWeight.repSep(char(',').surroundedBy(sps))).surroundedBy(sps) <* P.char('}').surroundedBy(sps))
      .map(edges => {
        val edgesMap = edges.collect { case (edge, _) => edge.from -> edge }.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
        val activeEdgesSet:Set[Edge] = edges.collect { case (edge, true) => edge }.toSet
        (edgesMap, activeEdgesSet)
      })

  private def levelN: P[(Map[Edge,Set[Edge]],Set[Edge])] = 
      (((P.string("ln") *> P.char('=').surroundedBy(sps)) *> P.char('{').surroundedBy(sps)) *> 
      (hyperEdge_withoutWeight.repSep0(char(',').surroundedBy(sps))).surroundedBy(sps) <* P.char('}').surroundedBy(sps))
      .map(edges => {
        val edgesMap:Map[Edge,Set[Edge]] = edges.collect { case (edge, _) => edge.from -> edge }.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
        val activeEdgesSet:Set[Edge] = edges.collect { case (edge, true) => edge }.toSet
        (edgesMap, activeEdgesSet)
      })
  private def levelNI: P[(Map[Edge,Set[Edge]],Set[Edge])] = 
      (((P.string("lnI") *> P.char('=').surroundedBy(sps)) *> P.char('{').surroundedBy(sps)) *> 
      (hyperEdge_withoutWeight.repSep0(char(',').surroundedBy(sps))).surroundedBy(sps) <* P.char('}').surroundedBy(sps))
      .map(edges => {
        val edgesMap:Map[Edge,Set[Edge]] = edges.collect { case (edge, _) => edge.from -> edge }.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
        val activeEdgesSet:Set[Edge] = edges.collect { case (edge, true) => edge }.toSet
        (edgesMap, activeEdgesSet)
      })

  private def init: P[State] = ((P.string("init") *> P.char('=').surroundedBy(sps)) *> state).map(x => String(x))

  private def oneProgram: P[RxGr] = 
      (( init.surroundedBy(sps) <* P.char(';').surroundedBy(sps)) ~
      (level0.surroundedBy(sps) <* P.char(';').surroundedBy(sps)) ~
      levelN.surroundedBy(sps)) 
      .map{ case (((init,se),he)) => 
        RxGr(se._1, he._1, init, se._2 ++ he._2)
  }

  // private def program: P[System] =
  //   ((oneProgram.surroundedBy(sps)) ~
  //   ((char('~').surroundedBy(sps) *> oneProgram.surroundedBy(sps)).?))
  //     .map((x,y) => System(x,y))

  private def program: P[System] =
    ((oneProgram.surroundedBy(sps)) ~
    ((char('~').surroundedBy(sps) *> oneProgram.surroundedBy(sps)).?) ~
    ((char('~').surroundedBy(sps) *> levelNI.surroundedBy(sps)).?))
      .map{case (((x,y),z)) => System(x,y,(z.map(_._1).getOrElse(Map.empty),z.map(_._2).getOrElse(Set.empty)))}

      
  //////////////////////////////
  // Examples and experiments //
  //////////////////////////////

  object Examples:
    val ex1 =
      """
        init = s0; 
        l0={(s0,s1,a,0,Bullet),(s1,s1,b,0,Circ)};
        
        ln = {((s1,s1,b,0,Circ), (s1,s1,b,0,Circ),0,Bullet,ON),((s1,s1,b,0,Circ), (s2,s1,b,0,Circ),0,Bullet,ON),((s1,s1,b,0,Circ),((s1,s1,b,0,Circ),(s1,s1,b,0,Circ),0,Bullet,ON),0,Circ,OFF)}
        
      """