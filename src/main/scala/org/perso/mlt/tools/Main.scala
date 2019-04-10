package org.perso.mlt.tools

import java.io.{BufferedWriter, FileWriter, _}
import java.nio.file.{Files, Paths}

import io.lemonlabs.uri._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model._

object Main extends App {
  val cacheDir = "/data/perso/dev/mlt-tools/.cache"

  def fetchWebPage(sourceDir: String, host: String, page: String) = {
    val url = s"$host/$page"
    val parsedURL = Url.parse(url)
    println(s"url: $url")

    val cacheSourceDir = s"$cacheDir/$sourceDir"
    Files.createDirectories(Paths.get(cacheSourceDir))
    val cachePath = new File(cacheSourceDir, parsedURL.path.toString().replace('/', '_') + parsedURL.query.toString().replaceAll("[?=&]", "_"))
    println(s"cache path: $cachePath")

    if (!cachePath.exists()) {
      println(s"Download $url ...")

      val cacheFileWriter = new FileWriter(cachePath, false)
      val cacheBufferedWriter = new BufferedWriter(cacheFileWriter)
      cacheBufferedWriter.write(JsoupBrowser().get(url).toHtml)
      cacheBufferedWriter.flush()
      cacheBufferedWriter.close()
      cacheFileWriter.close()
    }

    JsoupBrowser().parseFile(cachePath)
  }

  def extractOxfordCategories: Seq[Category] = {
    val host = "https://www.oxfordlearnersdictionaries.com"
    val sourceDir = "oxford_topic"

    val cat1Document = fetchWebPage(sourceDir, host, "topic")
    val cat1Elements: Seq[Element] = cat1Document >> elementList("dl#topic-list > dt, dl#topic-list > dd")
    var cat1Items = List[Seq[Element]]()
    for (i <- cat1Elements.grouped(2)) {
      cat1Items +:= i
    }

    val cat1 = cat1Items.reverse.map { case Seq(cat1DT, cat1DD) =>
      val cat1Name = cat1DT.text
      val cat2Elements = cat1DD >> elementList("dl > dt, dl > dd")
      var cat2Items = List[Seq[Element]]()
      for (i <- cat2Elements.grouped(2)) {
        cat2Items +:= i
      }

      val cat2 = cat2Items.reverse.map { case Seq(cat2DT, cat2DD) =>
        val cat3Elements = cat2DD >> elementList("li")
        val cat3 = cat3Elements.map(c => {
          val cat3Link = c >> attr("href")("a")
          val cat3Document = fetchWebPage(sourceDir, host, Url.parse(cat3Link).path.toString().dropWhile(_ == '/'))
          val cat3Words = cat3Document >> elementList(".topic-explore > .tint_panel > .wordpool > li > a")
          Category(c.text, cat3Link, words = cat3Words.map(w => w.text))
        })
        Category(cat2DT.text, subs = cat3)
      }
      Category(cat1Name, subs = cat2)
    }
    cat1.foreach(c => {
      println(c.name)
      c.subs.foreach(s => {
        println(s"\t${s.name}")
        s.subs.foreach(ss => {
          println(s"\t\t${ss.name}: ${ss.link}")
          println(s"\t\t\t${ss.words.mkString(", ")}")
        })
      })
    })

    println(cat1.map(c => c.subs.map(s => s.subs.map(ss => ss.words.length).sum).sum).sum)

    cat1
  }

  def extractOxfordList(cacheDir: String, sourceDir: String, sourcePath: String) = {
    val host = "https://www.oxfordlearnersdictionaries.com"

    val cat1Document = fetchWebPage(cacheDir, host, s"wordlist/english/$sourceDir")
    val cat1Elements: Seq[Element] = cat1Document >> elementList("#entries-selector li")
    cat1Elements.flatMap(c => {
      println(c.text)
      Seq.range(1, 10).flatMap(p => {
        val cat2Document = fetchWebPage(cacheDir, host, s"wordlist/english/$sourceDir/${sourcePath}_${c.text.replaceAll(" ", "_")}/?page=$p")
        val cat2Elements: Seq[Element] = cat2Document >> elementList("#entrylist1 .result-list1 a")
        cat2Elements.map(_.text.replaceAll("\\s+\\d$", "")).distinct
      })
    })
  }

  val words3000 = extractOxfordList("oxford_3000", "oxford3000", "Oxford3000")
  val wordsAcademic = extractOxfordList("oxford_academic", "academic", "AcademicWordList")
  val tree = extractOxfordCategories

  val words = tree.flatMap(cat1 => cat1.subs.flatMap(cat2 => cat2.subs.flatMap(cat3 => cat3.words.map(w => Word(
    Seq(cat1.name, cat2.name, cat3.name),
    w,
    words3000.exists(w3 => w3.equalsIgnoreCase(w)),
    wordsAcademic.exists(wa => wa.equalsIgnoreCase(w))
  )))))

  words.sortBy(w => w.english).foreach(w => {
    println(s"${if (w.word3000) "*" else " "} ${if (w.wordAcademic) "*" else " "} ${w.english}\t\t${w.tree.mkString(" / ")}")
  })

  println(s"${words.map(w => w.english).distinct.length} words, ${words3000.length} of '3000', ${wordsAcademic.length} academic")
}

case class Category(name: String, link: String = "", subs: Seq[Category] = Seq.empty, var words: Seq[String] = Seq.empty)

case class Word(tree: Seq[String], english: String, word3000: Boolean, wordAcademic: Boolean)
