package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.Files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Cd(dirName: String) extends Command {

  override def apply(state: State): State = {

    // Find the root and the working directory
    val root = state.root
    val wd = state.wd

    // Find the absolute path of the directory to cd to
    val absolutePath =
      if (dirName.startsWith(Directory.SEPARATOR)) dirName
      else if (wd.isRoot) s"${wd.path}$dirName"
      else s"${wd.path}${Directory.SEPARATOR}$dirName"

    // Given the current path, find the directory to cd to
    val destinationDir = doFindEntry(root, absolutePath)

    // Change the state to the new directory
    if (destinationDir == null || !destinationDir.isDirectory)
      state.setMessage(s"$dirName: no such directory")
    else State(root, destinationDir.asDirectory)
  }

  def doFindEntry(root: Directory, absolutePath: String): DirEntry = {

    @tailrec
    def findEntryHelper(currDirectory: Directory, path: List[String]): DirEntry = {
      if (path.isEmpty || path.head.isEmpty) currDirectory
      else if (path.tail.isEmpty) currDirectory.findEntry(path.head)
      else {
        val nextDir = currDirectory.findEntry(path.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }
    }

    @tailrec
    def collapseRelativeTokens(path: List[String], result: List[String]): List[String] = {
      if (path.isEmpty) result
      else if (".".equals(path.head)) collapseRelativeTokens(path.tail, result)
      else if ("..".equals(path.head)) {
        if (result.isEmpty) null
        else collapseRelativeTokens(path.tail, result.init)
      }
      else collapseRelativeTokens(path.tail, result :+ path.head)
    }

    // Get tokens
    val tokens: List[String] =
      absolutePath.substring(1).split(Directory.SEPARATOR).toList

    val newTokens: List[String] = collapseRelativeTokens(tokens, List())

    // Navigate to the correct entry
    if (newTokens == null) null
    else findEntryHelper(root, newTokens)
  }
}
