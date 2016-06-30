package aard.db

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.charset.StandardCharsets

import aug.util.{JsonUtil, TryWith}
import org.apache.commons.io.FileUtils

import scala.util.{Failure, Success, Try}

object Store {

  val dataDir = new File("aug-aard/data")
  val charset = StandardCharsets.UTF_8

  def loadAll[A](path: String, ext: String)(implicit m: Manifest[A]) : List[A] = {
    val dir = new File(dataDir,path)
    val objs: Array[Try[A]] = for(file <- dir.listFiles if file.getName.endsWith(ext)) yield load[A](file)
    objs.toList.filter(_.isSuccess).map(_.get)
  }

  def load[A](file: File)(implicit m: Manifest[A]): Try[A] = {
    val s = FileUtils.readFileToString(file,charset)
    TryWith(new FileInputStream(file)) { in=>
      JsonUtil.fromJson[A](in)
    }
  }

  def load[A](path: String)(implicit m: Manifest[A]) : Try[A] = {
    load[A](new File(dataDir,path))
  }

  def loadOrElse[A](path: String,default: => A)(implicit m: Manifest[A]) : A = {
    val file = new File(dataDir,path)
    if(file.exists()) {
      load[A](file) match {
        case Success(a) => a
        case Failure(e) => default
      }
    } else default
  }

  def save(path: String, obj: Any): Try[Unit] = {
    val file = new File(dataDir,path)
    TryWith(new FileOutputStream(file)) { out=>
      JsonUtil.toJson(out,obj)
    }
  }

  def delete(path: String) = {
    FileUtils.deleteQuietly(new File(dataDir,path))
  }
}
