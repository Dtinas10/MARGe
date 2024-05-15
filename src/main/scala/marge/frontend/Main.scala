package marge.frontend

import caos.frontend.Site.initSite
import marge.syntax.Program
import marge.syntax.Program.RxGr
import marge.syntax.Program.System

/** Main function called by ScalaJS' compiled javascript when loading. */
object Main {
  def main(args: Array[String]):Unit =
    initSite[System](CaosConfig)
}