package derivation

trait FileSystem {
  def add(file: File, dir: Directory)
  type File
  type Directory
}

class NtfsFileSystem extends FileSystem {
  class File
  class Directory

  override def add(file: File, dir: Directory): Unit = {
    println("Added NTFS.")
  }
}

class Ext4FileSystem extends FileSystem {
  class File
  class Directory

  override def add(file: File, dir: Directory): Unit = {
    println("Added EXT4.")
  }
}

object syntax2 {
  def add(f: FileSystem)(file: f.File, dir: f.Directory): Unit = {
    println(s"Added $f")
  }
}
object Test2 extends App {

  import syntax2._
  val e = new Ext4FileSystem
  e.add(new e.File, new e.Directory)
  val n = new NtfsFileSystem
  n.add(new n.File, new n.Directory)
  add(e)(new e.File, new e.Directory)
}
