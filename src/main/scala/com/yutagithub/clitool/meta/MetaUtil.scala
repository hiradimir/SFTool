package com.yutagithub.clitool.meta
import scala.xml._

object MetaUtil {

  object MetaLabel {

    trait LabelResolver {
      def resolve(metaName: String, typeName: String, folderName: String, extention: String): String
    }

    trait XmlLabelLoader {
      def xmlLabelLoad(fileName: String, label: String = "label") = {
        val labelContainsXml = new java.io.File(fileName)
        if (!labelContainsXml.exists) throw new java.io.FileNotFoundException((fileName, label).toString)
        val t = scala.xml.XML.loadFile(labelContainsXml)
        (t \ label).text
      }
    }

    case class MetaNameIsMetaName extends LabelResolver {
      def resolve(metaName: String, typeName: String, folderName: String, extention: String) = metaName
    }

    case class MetaLabelIsMetaName(label: String = "label") extends LabelResolver with XmlLabelLoader {
      def resolve(metaName: String, typeName: String, folderName: String, extention: String) = {
        xmlLabelLoad(List(folderName, metaName).mkString("/") + "." + extention, label)
      }
    }

    case class MetaXmlLabelIsMetaName(label: String = "label") extends LabelResolver with XmlLabelLoader {
      def resolve(metaName: String, typeName: String, folderName: String, extention: String) = {
        xmlLabelLoad(List(folderName, metaName).mkString("/") + "." + extention + "-meta.xml", label)
      }
    }

    case class ExistFileNameIsMetaName(label: String = "name") extends LabelResolver with XmlLabelLoader {
      def resolve(metaName: String, typeName: String, folderName: String, extention: String) = {
        val filePrefix = List(folderName, metaName).mkString("/");
        val l = List(filePrefix + "." + extention, filePrefix + "-meta.xml").filter(fileName => (new java.io.File(fileName)).exists)
          .map(fileName => xmlLabelLoad(fileName, label))
        l.first
      }
    }

    val typeToDir = Map(
      "ApexClass" -> ("classes", "cls", MetaNameIsMetaName()),
      "ApexComponent" -> ("components", "component", MetaXmlLabelIsMetaName()),
      "ApexPage" -> ("pages", "page", MetaXmlLabelIsMetaName()),
      "CustomObject" -> ("objects", "object", MetaLabelIsMetaName()),
      "Layout" -> ("layouts", "layout", MetaNameIsMetaName()),
      "StaticResource" -> ("staticresources", "resource", MetaNameIsMetaName()),
      "ApexTrigger" -> ("triggers", "trigger", MetaNameIsMetaName()),
      "CustomApplication" -> ("applications", "app", MetaLabelIsMetaName()),
      "CustomTab" -> ("tabs", "tab", MetaLabelIsMetaName()),
      "Report" -> ("reports", "report", MetaLabelIsMetaName()),
      "ReportType" -> ("reportTypes", "reportType", ExistFileNameIsMetaName()),
      "Workflow" -> ("workflows", "workflow", MetaNameIsMetaName()))

    def metaNames(targetFolder: String) = {
      val p1 = scala.xml.XML.loadFile(new java.io.File(targetFolder, "package.xml"))
      val c = (p1 \ "types").map(metatype => {
        val name = (metatype \ "name").text
        val values = (metatype \ "members").map(x => x.text)
        (name, values)
      })

      c.map(types => {
        val (typeName, metaList) = types
        val matched = typeToDir.get(typeName) match {
          case Some((dir, extention, resolver)) => metaList.map(metaName => {
            (metaName + "." + extention, resolver.resolve(metaName, typeName, targetFolder + "/" + dir, extention))
          })
          case None => println(typeName + "is key Nothing"); metaList.map(metaName => (metaName, metaName))
        }
        (typeName, matched.toMap)
      }).toMap
    }

    def getExistMeta(targetFolder: String) = {
      val dir = new java.io.File(targetFolder);

      val a = typeToDir.toList.sortBy(_._1).map(t => {
        val metaDir = new java.io.File(dir.getCanonicalPath(), t._2._1);

        val files = metaDir.listFiles match {
          case null => {
            Array[java.io.File]()
          }
          case x => x
        }

        val b = files.filter(_.getName().startsWith(".") == false).map(x => {
          x match {
            case x if x.isFile => List(x)
            case x if x.isDirectory && x.listFiles != null =>
              x.listFiles.toList.filter(_.getName().startsWith(".") == false)
            case _ => List()
          }
        }).flatMap(x => x)

        val filesa = b.filter(_.getName().endsWith("-meta.xml") == false).map(_.getCanonicalPath().replace(metaDir.getCanonicalPath() + "/", "").takeWhile(_ != '.'))
        (t, filesa.toList)
      }).filter(_._2.length > 0)
      a
    }

    def createPackageXml(targetFolder: String, metaDir: String = "src") = {
      val dir = new java.io.File(targetFolder);
      val exixstMeta = getExistMeta(new java.io.File(dir.getCanonicalPath(), metaDir).getCanonicalPath())

      <package xmlns="http://soap.sforce.com/2006/04/metadata">
        {
          exixstMeta.map(x => {
            val (typeName, memberList) = x
            <types>
              {
                memberList.sortBy(x => x).map(x => {
                  <members>{
                    (x match {
                      case x if x.endsWith("/") => {
                        x.takeWhile(_ != '/')
                      }
                      case _ => x
                    })
                  }</members>
                })
              }
              <name>{ typeName._1 }</name>
            </types>
          })
        }
        <version>23.0</version>
      </package>
    }

  }
  // 必要なjavaパッケージを読み込みますよー
  import java.io.{ File, PrintWriter }

  // 引数としてファイルとファイルに対する操作をとる制御構造を定義します
  def withPrintWriter(file: File, op: PrintWriter => Unit) {
    // ファイル書き込みのインスタンスを取得
    val writer = new PrintWriter(file)
    try {
      // ファイル操作をしますよー
      op(writer)
      // 成功失敗にかかわらずファイルクローズしますね
    } finally {
      writer.close()
    }
  }

  def createMetaXML(args: String*) = {

    var workingDir = args(0);
    var targetDir = args(1);
    var outputFileName = args(2);

    val elem = MetaLabel.createPackageXml(workingDir, targetDir)

    withPrintWriter(
      new File(new File(workingDir).getCanonicalPath(), outputFileName),
      pw => {
        pw.println("""<?xml version="1.0" encoding="UTF-8"?>""")
        pw.println((new scala.xml.PrettyPrinter(120, 4)).format(elem))
      })
  }

  def outMetaLabels(args: String*) = {
    val metaTypes = MetaUtil.MetaLabel.metaNames(args(0))
    for ((typeName, metaList) <- metaTypes) {
      println(typeName)
      for (meta <- metaList.toList.sortBy(_._1)) {
        println(meta._2 + "\t" + meta._1)
      }
    }
  }

  def expandObjectDefinition(target: String) = {
    val prefix = {
      target.split("/").last.takeWhile(_ != '.')
    }
    def eletyushutu(e: String, fields: String*) = {
      val elem = scala.xml.XML.loadFile(e)
      fields.map(field => {
        (field, (elem \\ field \ "fullName").map(prefix + "." + _.text))
      })
    }
    val diffSearchTag = Map("fields" -> "CustomField", "validationRules" -> "ValidationRule", "webLinks" -> "Weblink")

    eletyushutu(target, diffSearchTag.keys.toList: _*)

  }

  def diffMeta(args: String*) = {
    val target1: String = args(0)
    val target2: String = args(1)
    val workingDir: String = args(2)

    val t1 = MetaLabel.getExistMeta(new java.io.File(target1).getCanonicalPath()).map(x => { (x._1._1, x._2) }).toMap
    val t2 = MetaLabel.getExistMeta(new java.io.File(target2).getCanonicalPath()).map(x => { (x._1._1, x._2) }).toMap
    def objectExpand(targetDir: String) = {
      val target = MetaLabel.getExistMeta(new java.io.File(targetDir).getCanonicalPath()).map(x => { (x._1._1, x._2) }).toMap
      target.get("CustomObject") match {
        case Some(metainf) => {
          val (dir, suffix, _) = MetaLabel.typeToDir("CustomObject")
          val path = new File(targetDir, dir).getCanonicalPath();
          val expanded = metainf.map(metaname => {
            expandObjectDefinition(new File(path, metaname + "." + suffix).getCanonicalPath())
          })
          val a = expanded.flatten.groupBy(_._1).map(x => (x._1, x._2.map(_._2).flatten))
          val b = target ++ a
          b
        }
        case None => {
          target
        }
      }
    }

    //val t1 = objectExpand(target1)
    //val t2 = objectExpand(target2)

    val diff = t1.map {
      case (types, metainf) =>
        t2.get(types) match {
          case Some(metainf2) => {
            types match {
              case "CustomObject" => {
                def pathCreate(path: String, fileName: String) = {
                  val (dir, suffix, _) = MetaLabel.typeToDir("CustomObject")
                  new File(new File(path, dir).getCanonicalPath(), fileName + "." + suffix).getCanonicalPath()
                }
                val a = metainf.filter(s => {
                  metainf2.contains(s)
                }).map(f => {
                  diffObject(pathCreate(target1, f), pathCreate(target2, f))
                }).flatten.groupBy(_._1).map{case (k,v) => (k, v.map(_._2).flatten)}

                List((types,
                  metainf.filter(s => {
                    !metainf2.contains(s)
                  }))) ++ a.toList
              }
              case _ => {
                List((types,
                  metainf.filter(s => {
                    !metainf2.contains(s)
                  })))
              }
            }
          }
          case None => {
            List((types, metainf))
          }
        }
    }.flatten.filter(_._2.size > 0)

    val elem = <package com.yutagithub.clitool.meta="http://soap.sforce.com/2006/04/metadata">
                 {
                   diff.toList.sortBy(_._1).map(x => {
                     val (typeName, memberList) = x
                     <types>
                       {
                         memberList.sortBy(x => x).map(x => {
                           <members>{
                             (x match {
                               case x if x.endsWith("/") => {
                                 x.takeWhile(_ != '/')
                               }
                               case _ => x
                             })
                           }</members>
                         })
                       }
                       <name>{ typeName }</name>
                     </types>
                   })
                 }
                 <version>23.0</version>
               </package>

    withPrintWriter(
      new File(workingDir),
      pw => {
        pw.println("""<?xml version="1.0" encoding="UTF-8"?>""")
        pw.println((new scala.xml.PrettyPrinter(120, 4)).format(elem))
      })
  }

  def diffObject(args: String*) = {

    val target1: String = args(0)
    val target2: String = args(1)
    val prefix = {
      target1.split("/").last.takeWhile(_ != '.')
    }
    def eletyushutu(e: String, fields: String*) = {
      val elem = scala.xml.XML.loadFile(e)
      fields.map(field => {
        (field, (elem \\ field \ "fullName").map(_.text))
      }).toMap
    }
    val diffSearchTag = Map("fields"->"CustomField", "validationRules"->"ValidationRule", "webLinks"->"Weblink")
    val ele1 = eletyushutu(target1, diffSearchTag.keys.toList: _*)
    val ele2 = eletyushutu(target2, diffSearchTag.keys.toList: _*)

    val diff = ele1.map(fe => {
      (fe._1, fe._2.filter(f => {
        !ele2(fe._1).contains(f)
      }))
    }).toMap.filter(_._2.size > 0)

    diff.map(fe => {
      (diffSearchTag(fe._1), fe._2.map(f => {
        prefix + "." + f
      }))
    })

  }

  def main(args: Array[String]): Unit = {

    val options = args.filter(_.startsWith("-")).map(_.tail)
    val _args = args.filter(!_.startsWith("-"));

    options.firstOption match {
      case Some("l") => {
        outMetaLabels(_args: _*);
      }
      case Some("c") => {
        if (_args.size != 3) {
          println("Usage <workDir> <metaDir:relative> <outFilePath:relative>");
          System.exit(-1);
        }
        createMetaXML(_args: _*);
      }
      case Some("d") => {
        if (_args.size != 3) {
          println("Usage <target1:etc[backup]> <target2:etc[deploy]> <outputPath>");
          println("usually create destructiveChanges");
          System.exit(-1);
        }
        diffMeta(_args: _*);
      }
    }

  }

}