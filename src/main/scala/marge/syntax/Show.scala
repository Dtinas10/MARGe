package marge.syntax

import marge.syntax.Program.*
import marge.syntax.Program.RxGr.*
import marge.syntax.Program.System
import marge.syntax.Program.Edge.*


/**
 * List of functions to produce textual representations of commands
 */
object Show:

  def justTerm(s: System): String = s.main.init

  // def apply(g: RxGr): String = if g.nextEdg.isEmpty then "NONE" else g.nextEdg.head.pretty

  def toMermaid_twoGraphs_Bissi(st: System, id:String): String = 
    var s: String = st.toCompare match{
      case None => ""
      case Some(t) => _toMermaid(t,"\n subgraph Second Graph\n direction LR \n",".", "pluslines", countNumLines(st.main)) + "\n end"
    }
    // _toMermaid(st.main, "flowchart LR \n subgraph First Graph  \n direction LR \n", ".",id,intro = st.intro)  + "\n end" + s
    _toMermaid(st.main, "flowchart LR \n subgraph First Graph  \n direction LR \n", "",id)  + "\n end" + s

  def toMermaid_Intrusive(newst:System): String = 
    var st: System = System(newst.main,None,newst.intro)
    var second: RxGr = newst.toCompare.getOrElse(newst.main.empty)
    if !second.isEmpty then{
        second = RxGr(second.se,
        second.he ++ newst.intro._1.map {case (key, newValueSet) => key -> (second.he.getOrElse(key, Set.empty[Edge]) ++ newValueSet)},
        second.init,
        second.active ++ newst.intro._2)
        st = System(newst.main,Option(second),newst.intro)
      }
    var s: String = st.toCompare match{
      case None => ""
      case Some(t) => _toMermaid(t,"\n subgraph Second Graph\n direction LR \n",".", "pluslines", countNumLines(st.main)) + "\n end"
    }
    // _toMermaid(st.main, "flowchart LR \n subgraph First Graph  \n direction LR \n", ".",id,intro = st.intro)  + "\n end" + s
    _toMermaid(st.main, "flowchart LR \n subgraph First Graph  \n direction LR \n", "","pluslines",intro=newst.intro._1)  + "\n end" + s
  // +_toMermaid(RxGr(Map.empty,st.intro,"",Set.empty), sInitial=  "", s2= ".", id = "intros")


  /** Put the reactive graph RxGr in Mermaid Code*/
  def toMermaid(g: RxGr,id:String): String = _toMermaid(g,"flowchart LR \n","",id)

  /** Put the RG in Mermaid Code + RG with level0 only in Mermaid Code*/ 
  def toMermaid_twoGraphs(g1:RxGr, g2: RxGr, id:String): String = 
    _toMermaid(g1, "flowchart LR \n subgraph Global View  \n direction LR \n", "",id)   
    + _toMermaid(g2,"\n end \n subgraph Local View \n direction LR \n",".",id) + "\n end"
  
  /* put the RA in mermaid Code wich  received 4 arguments:
    g:RxGr -> is the RA
    sInital:String ->  is the string two begin a mermaid
    s2:String -> is the string to change name' nodes  for level0 only
    id:String -> is the id to identify nodes in widget*/
  private def _toMermaid(g: RxGr, sInitial: String, s2:String, id:String, nL: Int = 0, intro: Map[Edge,Set[Edge]] = Map.empty): String = {
  // private def _toMermaid(g: RxGr, sInitial: String, s2:String, id:String, nL: Int = 0): String = {
    val colors: List[String] = List("gold", "red","blue","gray","orange","pink","green","purple") //miss and black
    // var mermaid = "```mermaid \nflowchart LR \n"
    var mermaid = sInitial //"flowchart LR \n"

    if g.isEmpty then return mermaid + s"A$s2[\\WARNING/] \n style A$s2 fill:#ff0000,stroke:#333,stroke-width:4px,color:#fff"

    //Counter to put style in edges
    var numLinhas: Int = nL

    // Loops to draw edges level0
    for ((from, edge) <- g.se){
      for (e <- edge){
        if haveMiddle(e,g) || haveIntru(e,intro) then{
          if g.active(e) then
            mermaid = mermaid + s2 + e.from + id + "(" + e.from + ") ---"+  n(e) + id + "( ) --> |" + e.act + "|"+ s2 + e.to + id + "(" + e.to + ") \n"
          else
            mermaid = mermaid + s2 + e.from + id + "(" + e.from + ") -.-"+  n(e) + id + "( ) -.-> |" + e.act + "|"+ s2 + e.to + id + "(" + e.to + ") \n"
          mermaid = mermaid + "style " + n(e) + id + " width:0px \n"
          mermaid = mermaid + "linkStyle " + numLinhas +" stroke:black"+", stroke-width:2px \n"
          mermaid = mermaid + "linkStyle " + (numLinhas + 1) +" stroke:black"+", stroke-width:2px \n"
          numLinhas = numLinhas + 2
        }
        else{
          if g.active(e) then
            mermaid = mermaid + s2 + e.from + id +"(" + e.from + ") --> |" + e.act + "|" + s2 + e.to + id +"(" + e.to + ") \n"
          else
            mermaid = mermaid + s2 + e.from + id + "(" + e.from + ") -.-> |" + e.act + "|"+ s2 + e.to + id +"(" + e.to + ") \n"
          mermaid = mermaid + "linkStyle " + numLinhas + " stroke:black"+", stroke-width:2px \n"
          numLinhas = numLinhas + 1
        }
      }
    }

    // Loops to draw edges levelN
    for ((from, edge) <- g.he){
      for (ee <- edge){
        ee match {
          case e: SimpleEdge => mermaid = mermaid + " "
          case e: HyperEdge =>
            if haveMiddle(e,g) || haveIntru(e,intro) then{
              if g.active(e) then
                mermaid = mermaid + n(e.from) + id +"( ) ---" + n(e) + id +"( ) --"+ head(e.activate) + n(e.to) + id +"( ) \n"
              else
                mermaid = mermaid + n(e.from) + id +"( ) -.-" + n(e) + id +"( ) -.-"+ head(e.activate)+ n(e.to) + id +"( ) \n"

              mermaid = mermaid + "style " + n(e) + id +" width:0px \n"
              if (order(e.from) < order(e.to)) then{
                mermaid = mermaid + "linkStyle " + numLinhas +" stroke:dark"+ colors(order(e))+", stroke-width:2px \n"
                mermaid = mermaid + "linkStyle " + (numLinhas + 1) +" stroke:dark"+ colors(order(e))+", stroke-width:2px \n"
              }else{
                mermaid = mermaid + "linkStyle " + numLinhas +" stroke:"+ colors(order(e))+", stroke-width:2px \n"
                mermaid = mermaid + "linkStyle " + (numLinhas + 1) +" stroke:"+ colors(order(e))+", stroke-width:2px \n"
              } 
              numLinhas = numLinhas + 2

            }else{

              if g.active(e) then
                mermaid = mermaid + n(e.from) + id +"( ) --"+ head(e.activate) + n(e.to) + id +"( ) \n"
              else
                mermaid = mermaid + n(e.from) + id +"( ) -.-"+ head(e.activate)+ n(e.to) + id +"( ) \n"

              if (order(e.from) < order(e.to)) then{
                mermaid = mermaid + "linkStyle " + numLinhas +" stroke:"+ colors(order(e))+", stroke-width:3px \n"
              }else{
                mermaid = mermaid + "linkStyle " + numLinhas +" stroke:dark"+ colors(order(e))+", stroke-width:2px \n"
              } 
              numLinhas = numLinhas + 1
            }
        }
      }
    }

    if id != "pluslines" then
      mermaid +=  "style " + g.init + id + " fill:#8f7,stroke:#363,stroke-width:4px \n" //+"```"
    else
      mermaid +=  "style " + s2 + g.init + id + " fill:#8f7,stroke:#363,stroke-width:4px \n" //+"```"
    mermaid
  }

  /* function to decide the head arrow (ON, OFF)*/
  private def head(b: Boolean):String ={
    if b then ">"
    else "x" 
  }

  /* function to get the name in the middle of the edge
  Because there is some edges wich start in the middle*/
  private def n(edge: Edge): String = {
    var m: String = ""
    edge match
      case e: SimpleEdge => m = m + e.from + e.to + e.act
      case he: HyperEdge => m = n(he.from) + n(he.to)
    m
  }

  /*function to find the order of edge, it's important to choose the color in Mermaid code */
  private def order(edge: Edge): Int = {
    var n: Int = 1
    edge match
      case e: SimpleEdge => n = 0
      case he: HyperEdge => n = n + order(he.to) + order(he.from)
    n
  }

  /*function wich give true if the edege needs midle*/ 
  private def haveIntru(e: Edge, m:Map[Edge,Set[Edge]]):Boolean = 
    m.contains(e) || m.exists{ 
      case (_, s) => 
        var f: Boolean   = false
        for (edge <- s){ 
          edge match{
            case ee: SimpleEdge => if (ee == e) then f = true  
            case ee: HyperEdge => if (ee.to == e) then f = true
          }
        }
        f
      }
  private def haveMiddle(e: Edge, g: RxGr):Boolean = 
    g.he.contains(e) || g.he.exists{ 
      case (_, s) => 
        var f: Boolean   = false
        for (edge <- s){ 
          edge match{
            case ee: SimpleEdge => if (ee == e) then f = true  
            case ee: HyperEdge => if (ee.to == e) then f = true
          }
        }
        f
      }
  
  private def countNumLines(g: RxGr): Int =
    var n: Int = 0
    for ((st,edgeset) <- g.se){
      for (edge <- edgeset){
        if haveMiddle(edge,g) then n = n + 2
        else n = n + 1  
      }
    }    
    for ((st,edgeset)<- g.he){
      for (edge <- edgeset){
        if haveMiddle(edge,g) then n = n + 2
        else n = n + 1  
      }
    }
    n

    
  


  




