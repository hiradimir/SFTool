package com.yutagithub.clitool.meta

import scala.xml._
import java.io.File

object PackageUtil {
  object XMLMerge {

    def merge(args: String*): Elem = {
      val packageElem = try{
       scala.xml.XML.loadFile(args.first)
      }catch{
        case e => println(args.first , " loads failer"); throw e;
      }
      
      args.tail.foldLeft(packageElem)((p1, p2) => {
        try {
          merge(p1, scala.xml.XML.loadFile(p2))
        } catch {
          case e: java.io.FileNotFoundException => { println(p2 + " is not found..."); p1 }
          case e@_ => {println(p1, p2); throw e;}
        }
      })

    }

    def merge(package1: Elem, package2: Elem): Elem = {
      val combinedPackage = package1 ++: package2
      val typeMapNotDict = (combinedPackage \ "types").map(metatype => {
        val name = (metatype \ "name").text
        val values = (metatype \ "members").map(x => x.text)
        (name, values)
      }).groupBy(_._1).map(x => {
        (x._1, x._2.flatMap(_._2))
      })

      val typeMap = typeMapNotDict.map(x => (x._1, x._2.distinct));

      if (typeMap.filter(x => typeMapNotDict(x._1).length != x._2.length).size > 0) {
        throw new java.lang.IllegalStateException("" + typeMap + typeMapNotDict)
      }

      <package com.yutagithub.clitool.meta="http://soap.sforce.com/2006/04/metadata">
        {
          typeMap.map(x => {
            val (typeName, memberList) = x
            <types>
              { memberList.map(x => { <members>{ x }</members> }) }
              <name>{ typeName }</name>
            </types>
          })
        }
        <version>20.0</version>
      </package>
    }
  }

  import java.io.{ File, PrintWriter }
  def withPrintWriter(file: File, op: PrintWriter => Unit) {
    val writer = new PrintWriter(file)
    try {
      op(writer)
    } finally {
      writer.close()
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.size < 2) {
      println("Usage <outputDir> <targetDir1:relative> <targetDir2:relative> ...");
      System.exit(-1);
    }
    
    val options = args.filter(_.startsWith("-"))
    val params = args.filter(!_.startsWith("-"))
    
    val rootDir = params(0)
    val targets = List("package.xml", "destructiveChanges.xml", "backup.xml")
    val dirs = params.tail.map(d=>new File(rootDir,d).getCanonicalPath()) 
    
    targets.foreach(target => {

      val packagexml = PackageUtil.XMLMerge.merge(dirs.map(d => { new File(d, target).getCanonicalPath() }): _*);
      scala.xml.XML.save(new File(rootDir, target).getCanonicalPath(), packagexml, "UTF-8", true);

      withPrintWriter(
        new File(rootDir, target),
        pw => {
          pw.println("""<?xml version="1.0" encoding="UTF-8"?>""")
          pw.println((new scala.xml.PrettyPrinter(120, 4)).format(packagexml))
        })

    })

  }

}