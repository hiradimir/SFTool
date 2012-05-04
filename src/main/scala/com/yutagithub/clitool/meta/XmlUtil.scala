package com.yutagithub.clitool.meta

import scala.xml._

object XmlUtil {

  def parseXml(fileName: String): Map[String, Traversable[String]] = {
    val file = new java.io.File(fileName)
    if (file.exists()) {
      parseXml(file)
    } else {
      println("fileNotFound", file, file.getAbsolutePath())
      Map[String, List[String]]()
    }
  }

  def parseXml(file: java.io.File): Map[String, Traversable[String]] = {

    try {
      val packagexml = scala.xml.XML.loadFile(file)

      (packagexml \ "types").map(metatype => {
        val name = (metatype \ "name").text
        val values = (metatype \ "members").map(x => x.text)
        (name, values)
      }).toMap

    } catch {
      case e => Map[String, Traversable[String]]()
    }
  }

  val root = new scala.xml.transform.RewriteRule {
    override def transform(n: Node): NodeSeq =
      n match {
        case Elem(_, "Package", _, _, _) => n;
        case Elem(_, "types", _, _, _) => NodeSeq.Empty;
        case Elem(_, "name", _, _, _) => NodeSeq.Empty;
        case Elem(_, "members", _, _, _) => NodeSeq.Empty;
        case n => n;
      }
  }

  def test() = {
    val p1 = scala.xml.XML.loadFile("package.xml")
    val p2 = scala.xml.XML.loadFile("package.xml")
    val b = p1 ++: p2

    val r = new scala.xml.transform.RuleTransformer(root).transform(p1)
    println(r)

  }

  def main(args: Array[String]): Unit = {
    
    val metaTypes = MetaUtil.MetaLabel.metaNames("/Users/ysato/Documents/workspace/eclipse/workspace/JP_2011_JPNTS2/ant/forMergeDeploy/src")
    
    for((typeName, metaList) <- metaTypes){
      println(typeName)
      for(meta <- metaList.toList.sortBy(_._1)){
        println(meta._1 + "\t" + meta._2)
      }
    }
    
    return;

    val rootDir = "/Users/ysato/Documents/workspace/eclipse/workspace/JP_2011_JPNTS2/ant/forMergeDeploy/";
    val targets = List("package.xml", "destructiveChanges.xml", "backup.xml")
    val dirs = List("nts2", "yms", "jhs", "psw_fukumaru", "psw_kawamura")

    targets.foreach(target => {

      val packagexml = PackageUtil.XMLMerge.merge(dirs.map(rootDir + _ + "/" + target): _*)
//      println((new scala.xml.PrettyPrinter(120, 4)).format(packagexml))
      scala.xml.XML.save(rootDir + target, packagexml, "UTF-8", true);

    })

    //
    //    println(parseXml("package.xml"))
    //
    //    val a = TypesElement("ApexClass")

    //    println(a.xml)
  }

}