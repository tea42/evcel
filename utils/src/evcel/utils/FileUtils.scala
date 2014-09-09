package evcel.utils

import java.io.File

object FileUtils{
  def recursiveDelete(file : File){
    if (file.exists && file.isDirectory){
      Option(file.listFiles).foreach(_.foreach(recursiveDelete))
      file.delete
    } else
      file.delete
  }
}
