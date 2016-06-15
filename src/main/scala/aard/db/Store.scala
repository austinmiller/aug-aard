package aard.db

import java.io.File
import java.nio.charset.StandardCharsets

import aug.util.JsonUtil
import org.apache.commons.io.FileUtils

object Store {

  val dataDir = new File("aug-aard/data")
  val charset = StandardCharsets.UTF_8

  def loadAll[A](path: String, ext: String)(implicit m: Manifest[A]) : List[A] = {
    val dir = new File(dataDir,path)
    val objs = for(file <- dir.listFiles if file.getName.endsWith(ext)) yield load[A](file)
    objs.toList
  }

  def load[A](file: File)(implicit m: Manifest[A]) : A = {
    val s = FileUtils.readFileToString(file,charset)
    JsonUtil.fromJson[A](s)
  }

  def load[A](path: String)(implicit m: Manifest[A]) : A = {
    load[A](new File(dataDir,path))
  }

  def save(path: String, obj: Any) = {
    val file = new File(dataDir,path)
    FileUtils.writeStringToFile(file,JsonUtil.toJson(obj),charset)
  }
}
