package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.Files.Directory
import com.rtjvm.scala.oop.filesystem.State

class Rm(dirName: String) extends Command {

  override def apply(state: State): State = {
    //Get working dir
    val wd = state.wd

    //Get the absolute path
    val absolutePath =
      if (dirName.startsWith(Directory.SEPARATOR)) dirName
      else if (wd.isRoot) wd.path.+(dirName)
      else wd.path.+(Directory.SEPARATOR).+(dirName)

    //Do some checks
    if (Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("Nuclear war not supported")
    else doRM(state, absolutePath)
  }


  def doRM(state: State, absolutePath: String): State = {

    //TODO: Remember to implement find descendant in directory class
    def rmHelper(currDir: Directory, path: List[String]): Directory = {
      if (path.isEmpty) currDir
      else if (path.tail.isEmpty) currDir.removeEntry(path.head)
      else {
        val nextDir = currDir.findEntry(path.head)
        if (nextDir.isFile) currDir
      }
    }

    //Find the entry to remove
    //Update structure like we do for mkdir

    val tokens = absolutePath.substring(1).split(Directory.SEPARATOR).toList
    val newRoot: Directory = rmHelper(state.root, tokens)

    if (newRoot == state.root) state.setMessage(s"$absolutePath: No such file or directory")
    else State(newRoot, newRoot.findDescendant(state.wd.path.substring(1)))
  }
}
