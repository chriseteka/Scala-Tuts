package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.Files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

abstract class CreateEntry(name: String) extends Command {

  override def apply(state: State): State = {

    val wd = state.wd
    if (wd.hasEntry(name))
      state.setMessage(s"Entry: $name already exist!")
    else if (name.contains(Directory.SEPARATOR))
      state.setMessage(s"$name must not contain separators")
    else if (checkIllegal(name))
      state.setMessage(s"$name: illegal entry")
    else doCreateEntry(state, name)
  }

  def checkIllegal(name: String): Boolean = name.contains(".")

  def doCreateEntry(state: State, name: String): State = {

    def updateStructure(currDir: Directory, path: List[String], newEntry: DirEntry): Directory = {

      if (path.isEmpty) currDir.addEntry(newEntry)
      else {
        val oldEntry = currDir.findEntry(path.head).asDirectory
        currDir.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }

    val wd = state.wd

    // Get all pre-existing directories
    val allDirsInPath = wd.getAllFoldersInPath

    // Create a new directory with current path and the directory name
    val newEntry: DirEntry = createSpecificEntry(state)

    // Updating the pre-existing structure with the new incoming data
    val newRoot = updateStructure(state.root, allDirsInPath, newEntry)

    // Retrieve the new working directory, given an updated list of all dirs in that path
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }

  def createSpecificEntry(state: State): DirEntry
}
