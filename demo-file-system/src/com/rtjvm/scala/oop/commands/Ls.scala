package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.Files.DirEntry
import com.rtjvm.scala.oop.filesystem.State

class Ls extends Command {

  override def apply(state: State): State = {
    val contents = state.wd.contents
    val niceOutput = createNiceOutput(contents)
    state.setMessage(niceOutput)
  }

  def createNiceOutput(value: List[DirEntry]): String = {
    if (value.isEmpty) ""
    else {
      val entry = value.head
      s"${entry.name} [${entry.getType}] \n ${createNiceOutput(value.tail)}"
    }
  }
}
