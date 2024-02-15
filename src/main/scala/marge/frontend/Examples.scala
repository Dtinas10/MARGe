package marge.frontend

import caos.frontend.Configurator.*
import caos.frontend.{Configurator, Documentation}
import caos.view.*
import marge.backend.*
import marge.syntax.Program.RxGr
import marge.syntax.{Program, Show}
import marge.syntax.Parser 


object Examples:

  val exampleOfReport: String = 
    """init = s0;
      |l0 = {
      |  s0 --> s1 by a,
      |  s1 -.-> s1 by b};
      |ln ={
      |  ((s0,s1,a),(s1,s1,b),Bullet,ON),
      |  ((s0,s1,a),((s0,s1,a),(s1,s1,b),Bullet,ON),Bullet,OFF),
      |  ((s1,s1,b),((s0,s1,a),((s0,s1,a),(s1,s1,b),Bullet,ON),Bullet,OFF),Circ,ON),
      |  ((s0,s1,a),((s1,s1,b),((s0,s1,a),((s0,s1,a),(s1,s1,b),Bullet,ON),Bullet,OFF),Circ,ON),Bullet,ON)}
    """.stripMargin

  val ex1:String = 
    """init = 0; 
      |l0={ 
      |	0 --> 1 by a,
      |  1 --> 2 by b,
      |  2 -.-> 0 by c};
      |ln = {
      |	((0,1,a), (1,2,b),Bullet,ON),
      |  (((0,1,a), (1,2,b),Bullet,ON), (2,0,c),Bullet,ON)}
    """.stripMargin

  val gabbayExample:String = 
  """init= Son_of_Tweetie;
    |l0={
    |  Son_of_Tweetie --> Special_Penguin by -,
    |  Special_Penguin --> Penguin by -,
    |  Penguin --> Bird by -,
    |  Bird --> Does_Fly by Fly};
    |ln={
    |  ((Penguin,Bird,-),(Bird,Does_Fly,Fly),Bullet,OFF),
    |  ((Special_Penguin,Penguin,-),((Penguin,Bird,-),(Bird,Does_Fly,Fly),Bullet,OFF),Bullet,OFF)}
  """.stripMargin

  val gabbayExample2:String = 
    """init = n1;
      |l0={
      |  n1 --> t1 by -,
      |  t1 --> c1 by -,
      |  c1 --> n1 by -,
      |  n2 --> t2 by -,
      |  t2 --> c2 by -,
      |  c2 --> n2 by -};
      |ln={
      |  ((c1,n1,-),(t2,c2,-),Bullet,ON),
      |  ((c2,n2,-),(t1,c1,-),Bullet,ON),
      |  ((t1,c1,-),(t2,c2,-),Bullet,ON),
      |  ((t2,c2,-),(t1,c1,-),Bullet,ON)}
    """ .stripMargin

  val counter:String = 
    """init = 0;
      |l0 = { 0 --> 0 by act };
      |ln = {
      |  ((0,0,act),(0,0,act),Circ,OFF),
      |  ((0,0,act),((0,0,act),(0,0,act),Circ,OFF), Circ,ON),
      |  ((0,0,act),((0,0,act),((0,0,act),(0,0,act),Circ,OFF), Circ,ON),Bullet,ON)}
    """.stripMargin
  
  val featureModel:String =
    """init = setup;
      |l0 = {
      |  setup --> setup by safe,   
      |  setup --> setup by unsafe,
      |  setup --> setup by encrypt,     
      |  setup --> setup by dencrypt,
      |  setup --> ready by -,
      |  ready --> setup by -,
      |  ready --> received by receive,
      |  received --> routed-safe by route,
      |  received --> routed-unsafe by route,
      |  routed-safe --> sent by send,
      |  routed-unsafe --> sent by send,
      |  routed-unsafe --> sent-encrypt by send,
      |  sent-encrypt --> ready by ready,
      |  sent --> ready by ready };
      |
      |ln = {
      |  ((setup,setup,safe),(received,routed-safe,route), Bullet, ON),
      |  ((setup,setup,safe),(received,routed-unsafe,route), Bullet, OFF),
      |  ((setup,setup,unsafe),(received,routed-safe,route), Bullet, OFF),
      |  ((setup,setup,unsafe),(received,routed-unsafe,route), Bullet, ON),
      |  ((setup,setup,encrypt),(routed-unsafe,sent,send), Bullet, OFF),
      |  ((setup,setup,encrypt),(routed-unsafe,sent-encrypt,send), Bullet, ON),
      |  ((setup,setup,dencrypt),(routed-unsafe,sent,send), Bullet, ON),
      |  ((setup,setup,dencrypt),(routed-unsafe,sent-encrypt,send), Bullet, OFF)}
    """.stripMargin

  val vendingMachine: String = 
    """init = Insert;
      |l0 = {
      |  Insert --> Cofee by 0.5$,
      |  Insert --> Apple by 0.5$,
      |  Insert --> Chocolate by 1$,
      |  Cofee --> Insert by Get_cofee,
      |  Apple --> Insert by Get_apple,
      |  Chocolate --> Insert by Get_choc};
      |ln = {
      |  ((Insert,Chocolate,1$),(Insert,Cofee,0.5$),Bullet,OFF),
      |  ((Insert,Chocolate,1$),(Insert,Apple,0.5$),Bullet,OFF),
      |  ((Insert,Chocolate,1$),(Insert,Chocolate,1$),Bullet,OFF),
      |  ((Insert,Cofee,0.5$),(Insert,Cofee,0.5$),Circ,OFF),
      |  ((Insert,Cofee,0.5$),(Insert,Chocolate,1$),Bullet,OFF),
      |  ((Insert,Cofee,0.5$),((Insert,Cofee,0.5$),(Insert,Cofee,0.5$),Circ,OFF),Bullet,ON),
      |  ((Insert,Cofee,0.5$),((Insert,Apple,0.5$),(Insert,Apple,0.5$),Circ,OFF),Bullet,ON),
      |  ((Insert,Apple,0.5$),(Insert,Apple,0.5$),Circ,OFF),
      |  ((Insert,Apple,0.5$),(Insert,Chocolate,1$),Bullet,OFF),
      |  ((Insert,Apple,0.5$),((Insert,Apple,0.5$),(Insert,Apple,0.5$),Circ,OFF),Bullet,ON),
      |  ((Insert,Apple,0.5$),((Insert,Cofee,0.5$),(Insert,Cofee,0.5$),Circ,OFF),Bullet,ON),
      |  ((Insert,Apple,0.5$),(Insert,Cofee,0.5$),Circ,OFF),
      |  ((Insert,Cofee,0.5$),(Insert,Apple,0.5$),Circ,OFF),
      |  ((Insert,Apple,0.5$),((Insert,Apple,0.5$),(Insert,Cofee,0.5$),Circ,OFF),Bullet,ON),
      |  ((Insert,Cofee,0.5$),((Insert,Apple,0.5$),(Insert,Cofee,0.5$),Circ,OFF),Bullet,ON),
      |  ((Insert,Apple,0.5$),((Insert,Cofee,0.5$),(Insert,Apple,0.5$),Circ,OFF),Bullet,ON),
      |  ((Insert,Cofee,0.5$),((Insert,Cofee,0.5$),(Insert,Apple,0.5$),Circ,OFF),Bullet,ON)}
    """.stripMargin

  val vendingMachine2: String = 
    """init = Insert;
      |l0 = {
      |  Insert --> Cofee by 0.5$,
      |  Insert --> Chips by 1$,
      |  Insert --> Chocolate by 1$,
      |  Cofee --> Insert by Get_cofee,
      |  Chips --> Insert by Get_apple,
      |  Chocolate --> Insert by Get_choc};
      |ln = {
      |  ((Insert,Chocolate,1$),(Insert,Cofee,0.5$),Bullet,OFF),
      |  ((Insert,Chocolate,1$),(Insert,Chips,1$),Bullet,OFF),
      |  ((Insert,Chocolate,1$),(Insert,Chocolate,1$),Bullet,OFF),
      |  ((Insert,Cofee,0.5$),(Insert,Cofee,0.5$),Circ,OFF),
      |  ((Insert,Cofee,0.5$),(Insert,Chocolate,1$),Bullet,OFF),
      |  ((Insert,Cofee,0.5$),(Insert,Chips,1$),Bullet,OFF),
      |  ((Insert,Cofee,0.5$),((Insert,Cofee,0.5$),(Insert,Cofee,0.5$),Circ,OFF),Bullet,ON),
      |  ((Insert,Chips,1$),(Insert,Cofee,0.5$),Bullet,OFF),
      |  ((Insert,Chips,1$),(Insert,Chips,1$),Bullet,OFF),
      |  ((Insert,Chips,1$),(Insert,Chocolate,1$),Bullet,OFF)}
    """.stripMargin

  val inconsistency: String =
    """init = s;
    |l0 = {
    |  s --> s0 by a,
    |  s0 --> s1 by a,
    |  s1 -.-> s2 by b};
    |ln ={
    |  ((s0,s1,a),(s1,s2,b),Bullet,ON),
    |  ((s0,s1,a),(s1,s2,b),Bullet,OFF)}
    """.stripMargin

  val bissimulation: String =
    """init = 0;
    |l0 = { 0 --> 0 by act };
    |ln = {
    |  ((0,0,act),(0,0,act),Circ,OFF),
    |  ((0,0,act),((0,0,act),(0,0,act),Circ,OFF), Bullet,ON)}
    |~
    |init = 1;
    |l0 = { 1 --> 2 by act,
    |2 --> 3 by act};
    |ln = {}
    """.stripMargin
    
// init = 0; 
// l0={ 
// 	0 --> 1 by a,
//   1 --> 2 by b,
//   2 -.-> 0 by c};
// ln = {
// 	((0,1,a), (1,2,b),Bullet,ON),
//   (((0,1,a), (1,2,b),Bullet,ON), (2,c),Bullet,ON),
//   ((((0,1,a), (1,2,b),Bullet,ON), (2,c),Bullet,ON),(1,2,b),Bullet,OFF)}
    