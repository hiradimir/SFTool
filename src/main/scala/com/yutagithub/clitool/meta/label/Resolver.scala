package com.yutagithub.clitool.meta.label

object Resolver {

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

}