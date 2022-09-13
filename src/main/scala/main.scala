import org.jsoup.Jsoup
import org.jsoup.nodes.Document

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._
//import scalax.file.Path

import java.io.{FileOutputStream, OutputStreamWriter}

object Global {
  val USE = false
  val END = "横浜"
}

/** arg0: URL
  * arg1: 平日は0，土曜は1，日曜は2
  * arg2: 順方向は0，逆方面は1
  * arg3: 中間ファイルを読み込む場合は1, 最新データを取る場合は0，省略可
  * arg4: ソートに利用したい中間駅名，コンマ区切り，省略可，省略しない場合はargs3必須
  * sbt run https://www.navitime.co.jp/diagram/timetable?node=00004848&lineId=00000123&trainType=&updown=0&time=2020-03-16 0 0
  * console, sbt shellともうまく動かないので設定で引数を指定してmainを実行すること
  * terminalでsbtを打ってsbtに入ってから，run https://www.navitime.co.jp/diagram/timetable?node=00004848&lineId=00000123&trainType=&updown=0&time=2020-03-16 0 0 でもいける
  */
object main {
  def main(args: Array[String]): Unit = {
    //val uri = "https://www.navitime.co.jp/diagram/timetable?node=00007965&lineId=00000123"
    //val uri = "https://www.navitime.co.jp/diagram/timetable?node=00006668&lineId=00000185&trainType=&updown=1&time=2019-12-30"
    //val uri = "https://www.navitime.co.jp/diagram/timetable?node=00000296&lineId=00000190&updown=0"
    //val uri = "https://www.navitime.co.jp/diagram/timetable?node=00004848&lineId=00000123&trainType=&updown=0&time=2020-03-16"
    //val uri = "https://www.navitime.co.jp/diagram/timetable?node=00001957&lineId=00000213"
    val uriArray = args(0).split(',')

    // 平日は0，土曜は1，日曜は2
    //val date = weekday
    val dateIntArray = args(1).split(',')
    // 順方向は0，逆方面は1
    //val dir = 1
    val dirArray = args(2).split(',')
    val READ_FILE = if (args.length > 3 && args(3) == "1") true else false
    val sortTargetArray =
      if (args.length > 4) args(4).split(",") else new Array[String](0)

    val tmpFileName = "tmp.csv"
    val encode = "UTF-8"
    val inputs = uriArray.zip(dateIntArray).zip(dirArray)
    val nameTimeTupleListListBufArray =
      if (!READ_FILE) {
        val tmpNameTimeTupleListListBufArray = for (input <- inputs) yield {
          val uri = input._1._1
          val dateInt = input._1._2.toInt
          val dir = input._2.toInt
          println(s"uri: $uri, dateInt: $dateInt, dir: $dir")
          mainProcess(uri, dateInt, dir)
        }

        val tmpNameTimeTupleListListBuf =
          tmpNameTimeTupleListListBufArray.flatten

        // 中間データを出力
        // ファイル出力用，最初に前のファイルを削除
        val tmpFileOutPutStreamPre = new FileOutputStream(tmpFileName, false)
        val tmpWriterPre =
          new OutputStreamWriter(tmpFileOutPutStreamPre, encode)
        tmpWriterPre.close()

        val tmpFileOutPutStream = new FileOutputStream(tmpFileName, true)
        val tmpWriter = new OutputStreamWriter(tmpFileOutPutStream, encode)
        for (nameTimeTupleListList <- tmpNameTimeTupleListListBuf) {
          for (nameTimeTuple <- nameTimeTupleListList) {
            tmpWriter.write(
              s"${nameTimeTuple._1},${nameTimeTuple._2},${nameTimeTuple._3.mkString(",")}\n"
            )
          }
        }
        tmpWriter.close()

        tmpNameTimeTupleListListBufArray
      } else {
        val source = scala.io.Source.fromFile(tmpFileName, encode)
        val lines = source.getLines.toList
        val x = for (line <- lines) yield {
          val lineArray = line.split(",")
          val syubetsu = lineArray(0)
          val dest = lineArray(1)
          val nameTimeString =
            lineArray.slice(2, lineArray.length).mkString(",")
          val tmp = nameTimeString.split(')')
          val nameTimeTupleSeq = for (tmp2 <- tmp.toSeq) yield {
            val tmpArray = tmp2.split('(')
            val nameTimeArray = tmpArray(1).split(",")
            (nameTimeArray(0), nameTimeArray(1))
          }
          (syubetsu, dest, nameTimeTupleSeq)
        }
        Array(mutable.Buffer(x))
      }

    val nameTimeTupleListListBuf = nameTimeTupleListListBufArray.flatten

//    val nameTimeTupleListListBufArray = for (input <- inputs) yield {
//      val uri = input._1._1
//      val dateInt = input._1._2.toInt
//      val dir = input._2.toInt
//      println(s"uri: $uri, dateInt: $dateInt, dir: $dir")
//      mainProcess(uri, dateInt, dir)
//    }
//
//    val nameTimeTupleListListBuf = nameTimeTupleListListBufArray.flatten

    // このテーブルに含まれるすべての列車の停車駅と時刻の組を取得
    val nameTimeTable = nameTimeTupleListListBuf.flatten.toSeq
    //println("All: " + nameTimeTable.size)

    for (nameTimeSeq <- nameTimeTable) {
      println(s"${nameTimeSeq._1} ${nameTimeSeq._2}: ${nameTimeSeq._3}")
    }

    val firstNameSeq = createFirstNameSeq(nameTimeTable)
    //println(firstNameSeq)

    // このテーブルに含まれる列車のすべての停車駅のリストを作成
    val allNameSeq = createNameSeq(firstNameSeq, 0, nameTimeTable)

    //println(allNameSeq)

    // 種別と行き先のリスト
    val syubetsuDestSeqPre = for (nameTimeSeqT <- nameTimeTable) yield {
      (nameTimeSeqT._1, nameTimeSeqT._2)
    }

    // このテーブルに含まれる列車のすべての停車時刻のリストを作成
    val timeSeqSeqPre = for (nameTimeSeqT <- nameTimeTable) yield {
      val nameTimeSeq = nameTimeSeqT._3
      val checkNameSeq = for (nameTimeTuple <- nameTimeSeq) yield {
        nameTimeTuple._1
      }
      // allNameSeqに含まれる停車駅の時刻データがあれば時刻を，なければ空白を入れたリストを作成
      val timeSeq = for (name <- allNameSeq) yield {
        if (checkNameSeq.contains(name)) {
          val point = checkNameSeq.indexOf(name)
          val time = nameTimeSeq(point)._2
          // 着発の両方が設定されている駅があれば，その駅の時刻を2つにわける
          // 片方のみ設定されている場合は，すべて先頭に時刻を入れる
          if (time.contains(" ")) {
            val tmp = time.split(" ")
            (tmp(0), tmp(1))
          } else { (time, "") }
        } else { ("", "") }
      }
      timeSeq
    }
//    println(timeSeqSeq)

    // 一番，時刻が入っている数が多い駅を探したい
    val countSeq = for (_ <- allNameSeq) yield { 0 }
    val mCountSeq = scala.collection.mutable.ArraySeq(countSeq: _*)
    for (timeSeq <- timeSeqSeqPre) {
      var i = 0
      for (time <- timeSeq) {
        if (time._1 != "" || time._2 != "") {
          mCountSeq.update(i, mCountSeq(i) + 1)
        }
        i += 1
      }
    }
//    println(s"mCountSeq: $mCountSeq")
    // 一番，時刻が入っている数が多い駅のインデックス
    val maxIndex = mCountSeq.indexOf(mCountSeq.max)
    println(s"maxIndex: $maxIndex, ${allNameSeq(maxIndex)}")

    // 種別，行き先も一緒に並び替え，削除するために一度結合する
    val nameTimeTablePre = syubetsuDestSeqPre.zip(timeSeqSeqPre)

    // sortTargetArrayが入力されている場合は，そちらを採用し，入力されていない場合はmaxIndexを採用する
    val sortIndexArray = if (sortTargetArray.length > 0) {
      for (sortTarget <- sortTargetArray) yield {
        allNameSeq.indexOf(sortTarget)
      }
    } else {
      Array(maxIndex)
    }

    // Indexの発車時刻でソート，発車時刻がない場合は到着時刻
    def sortByIndexStation(
        index: Int,
        _nameTimeTable: Seq[((String, String), Seq[(String, String)])]
    ): Seq[((String, String), Seq[(String, String)])] = {
      if (index >= 0) {
        _nameTimeTable
          .sortWith((a, b) => {
            val aVal =
              if (a._2(index)._2 != "") a._2(index)._2 else a._2(index)._1
            val bVal =
              if (b._2(index)._2 != "") b._2(index)._2 else b._2(index)._1
            aVal < bVal
          })
      } else _nameTimeTable
    }

    var nameTimeTablePre2 = nameTimeTablePre
    for (sortIndex <- sortIndexArray) {
      println(s"sortIndex: $sortIndex, ${allNameSeq(sortIndex)}")
      nameTimeTablePre2 = sortByIndexStation(sortIndex, nameTimeTablePre2)
    }
    // 同値を削除する（すべての発着時刻を文字列に結合して比較）
    val nameTimeTablePre3 =
      nameTimeTablePre2.distinctBy(a => a._2.mkString(","))
    val timeSeqSeq = for (nameTime <- nameTimeTablePre3) yield {
      nameTime._2
    }
    val syubetsuDestSeq = for (nameTime <- nameTimeTablePre3) yield {
      nameTime._1
    }

    // 通過駅の場合はレを入れる
    val timeSeqSeq2 = for (timeSeq <- timeSeqSeq) yield {
      // 一番最後の駅は通過できないので除く
      val tmpSeq = for (i <- 0 to timeSeq.size - 2) yield {
        if (timeSeq(i) == ("", "")) {
          // 着時刻の部分をすべてつなげて，前方に空白しかない場合は始発駅
          val checkStrSeq = timeSeq.slice(0, i)
          val checkStrString = checkStrSeq.map(_._1).mkString("")
          // 着時刻の部分をすべてつなげて，以降空白しかない場合はもう終点についている
          val checkEndSeq = timeSeq.slice(i + 1, timeSeq.size)
          //println(checkEndSeq)
          val checkEndString = checkEndSeq.map(_._1).mkString("")
          //println(checkEndString)
          if (checkEndString != "" && checkStrString != "") {
            ("レ", "レ")
          } else {
            timeSeq(i)
          }
        } else {
          timeSeq(i)
        }
      }
      tmpSeq :+ timeSeq.last
    }

    // 着発表示を作る
    val allStrEndSeq = for (i <- timeSeqSeq.head.indices) yield {
      checkStrEnd(i, timeSeqSeq)
    }

    // 着発表示に合わせて駅名を調整する
    val allNameTupleSeq2 = for (i <- allStrEndSeq.indices) yield {
      if (allStrEndSeq(i)._2 != "") {
        (allNameSeq(i), allNameSeq(i))
      } else {
        (allNameSeq(i), "")
      }
    }

    // timeSeqSeqに種別と行き先を足す
    val syubetsuDestTimeSeqSeq = syubetsuDestSeq.zip(timeSeqSeq2)

    // ファイル出力用，最初に前のファイルを削除
    val fileName = "result.csv"
    val fileOutPutStreamPre = new FileOutputStream(fileName, false)
    val writerPre = new OutputStreamWriter(fileOutPutStreamPre, encode)
    writerPre.close()

    val fileOutPutStream = new FileOutputStream(fileName, true)
    val writer = new OutputStreamWriter(fileOutPutStream, encode)

    def printAndWrite(writer: OutputStreamWriter, str: String): Unit = {
      writer.write(str)
      print(str)
    }

    // 表示用
    // 駅名
    printAndWrite(writer, ",,")
    for (nameTuple <- allNameTupleSeq2) {
      // 駅名の（福井県）や〔東福バス〕などを削除する
      // （も）も含まない0文字以上の文字列を（）で囲んだ文字列にマッチする正規表現
      // 〔〕も同様の処理
      val rename =
        nameTuple._1.replaceFirst("（[^（）]*）$", "").replaceFirst("〔[^〔〕]*〕$", "")
      if (nameTuple._2 != "") {
        printAndWrite(writer, rename + "," + rename + ",")
      } else {
        printAndWrite(writer, rename + ",")
      }
    }
    printAndWrite(writer, "\n,,")
    // 着発
    for (strEnd <- allStrEndSeq) {
      if (strEnd._2 != "") {
        printAndWrite(writer, strEnd._1 + "," + strEnd._2 + ",")
      } else {
        printAndWrite(writer, strEnd._1 + ",")
      }
    }
    printAndWrite(writer, "\n")
    // 時刻
    for (syubetsuDestTimeSeq <- syubetsuDestTimeSeqSeq) {
      val syubetsu = syubetsuDestTimeSeq._1._1
      val dest = syubetsuDestTimeSeq._1._2
      printAndWrite(writer, syubetsu + "," + dest + ",")
      val timeTupleSeq = syubetsuDestTimeSeq._2
      // tupleの先頭にしか時刻が入っていない場合もあるが，
      // その駅が着発になっている駅の場合もあるので，確認しなければならない
      for (i <- allStrEndSeq.indices) {
        // allStrEndSeqが発のみの駅の場合，tupleの先頭だけを表示
        if (allStrEndSeq(i)._2 == "") {
          printAndWrite(writer, timeTupleSeq(i)._1 + ",")
        } else {
          // allStrEndSeqが着発の駅
          // tupleの発が空の場合，着時刻を発時刻にも入れる
          if (timeTupleSeq(i)._2 == "") {
            // 着時刻の部分をすべてつなげて，前方に空白しかない場合は始発駅
            val checkStrSeq = timeTupleSeq.slice(0, i)
            val checkStrString = checkStrSeq.map(_._1).mkString("")
            // 着時刻の部分をすべてつなげて，以降空白しかない場合はもう終点についている
            val checkEndSeq = timeTupleSeq.slice(i + 1, timeTupleSeq.size)
            //println(checkSeq)
            val checkEndString = checkEndSeq.map(_._1).mkString("")
            if (checkStrString == "") {
              printAndWrite(writer, "," + timeTupleSeq(i)._1 + ",")
            } else if (checkEndString != "") {
              printAndWrite(
                writer,
                timeTupleSeq(i)._1 + "," + timeTupleSeq(i)._1 + ","
              )
            } else {
              printAndWrite(
                writer,
                timeTupleSeq(i)._1 + "," + timeTupleSeq(i)._2 + ","
              )
            }
          } else {
            printAndWrite(
              writer,
              timeTupleSeq(i)._1 + "," + timeTupleSeq(i)._2 + ","
            )
          }
        }
      }
      printAndWrite(writer, "\n")
    }
    writer.close()
  }

  // 順方向は0，逆方面は1
  //val dir = 1
  def mainProcess(
      uri: String,
      dateInt: Int,
      dir: Int
  ): mutable.Buffer[List[(String, String, Seq[(String, String)])]] = {
    val doc = Jsoup.connect(uri).get

    // 平日はweekday，土曜はsaturday，日曜はholiday
    //val date = weekday
    val date =
      if (dateInt == 0) { "weekday" }
      else if (dateInt == 1) { "saturday" }
      else { "sunday" }

    val id = date + "-" + dir
    val id2 = "segment-" + dir

    //val divEleStr = "time-table-frame"
    //val divEle = doc.getElementsByAttributeValueContaining("id", id)(0)
    val divEles = doc.getElementsByAttributeValueContaining("id", id)
    val divEle = if (divEles.size > 0) {
      divEles.asScala.head
    } else {
      doc.getElementsByAttributeValueContaining("id", id2).asScala.head
    }

    val dlEles = divEle.child(0).children
    println(dlEles.size())
    // ～時台，ごとに切り出す
    val nameTimeTupleListListBuf = for (dlEle <- dlEles.asScala) yield {
      println(dlEle.text)
      val liEles = dlEle.child(1).child(0).children
      // 時刻1つ，ごとに切り出す
      val nameTimeTupleListBuf =
        for (
          liEle <- liEles.asScala
          // 特定の種別のみ抽出したい場合
          //          if liEle.attr("data-long-name").contains("ひたち")
        ) yield {
          val uri = "https://www.navitime.co.jp" + liEle.child(0).attr("href")
          // 新幹線だと号数，それ以外だと路線名が入る想定
          val shubetsu2 = liEle.attr("data-long-name")
          // 号数っぽい表記の場合はそちらを種別として選択
          val numberPattern = ".*[0-9]+号.*".r
          val shubetsu = numberPattern.findFirstMatchIn(shubetsu2) match {
            case Some(_) => shubetsu2
            case None    => liEle.attr("data-name")
          }

          //println(shubetsu)
          // 駅名の（福井県）や〔東福バス〕などを削除する
          // （も）も含まない0文字以上の文字列を（）で囲んだ文字列にマッチする正規表現
          // 〔〕も同様の処理
          val dest = liEle
            .attr("data-dest")
            .replaceFirst("（[^（）]*）$", "")
            .replaceFirst("〔[^〔〕]*〕$", "")
          //println(dest)
          println(uri)
          val nameTimeTupleList = getOnePage(uri)
          (shubetsu, dest, nameTimeTupleList)
        }
      nameTimeTupleListBuf.toList
    }
    nameTimeTupleListListBuf
  }

  // 最も停車駅が多いものを初期のリストにする
  def createFirstNameSeq(
      nameTimeTable: Seq[(String, String, Seq[(String, String)])]
  ): Seq[String] = {
    val sizeSeq = for (nameTimeSeqT <- nameTimeTable) yield {
      val stopStationSeq = nameTimeSeqT._3.map(_._1)
      if (Global.USE) {
        if (stopStationSeq.contains(Global.END)) {
          nameTimeSeqT._3.size
        } else {
          0
        }
      } else {
        nameTimeSeqT._3.size
      }
    }
    val maxSize = sizeSeq.max

    val maxSizeNameSeq =
      for (nameTimeSeqT <- nameTimeTable if nameTimeSeqT._3.size == maxSize)
        yield {
          for (nameTime <- nameTimeSeqT._3) yield {
            nameTime._1
          }
        }
    //println(maxSizeNameSeq)
    maxSizeNameSeq.head
  }

  // 全リストから，次に比較するリストを取り出す
  @tailrec
  def createNameSeq(
      oldNameSeq: Seq[String],
      checkPoint: Int,
      nameTimeTable: Seq[(String, String, Seq[(String, String)])]
  ): Seq[String] = {
    //println(checkPoint)

    val checkNameTimeSeq = nameTimeTable(checkPoint)._3
    val checkNameSeq = for (checkNameTimeTuple <- checkNameTimeSeq) yield {
      checkNameTimeTuple._1
    }
    val newNameSeq = createNameSeqOne(oldNameSeq, 0, checkNameSeq)
    if (checkPoint + 1 < nameTimeTable.size) {
      createNameSeq(newNameSeq, checkPoint + 1, nameTimeTable)
    } else {
      newNameSeq
    }
  }

  // 古いリストと新しいリストを比較する
  // 古いリストにない駅があった場合は，その駅を間に挿入し，新しいリストとして返す
  @tailrec
  def createNameSeqOne(
      oldNameSeq: Seq[String],
      checkPoint: Int,
      checkNameSeq: Seq[String]
  ): Seq[String] = {
    val checkName = checkNameSeq(checkPoint)
    val newNameSeq = if (!oldNameSeq.contains(checkName)) {
      //println(checkName)
      val checkPoint = checkNameSeq.indexOf(checkName)
      val newSeq = if (checkPoint > 0) {
        val preCheckName = checkNameSeq(checkPoint - 1)
        if (oldNameSeq.contains(preCheckName)) {
          //println(oldNameSeq)
          val splitPoint = oldNameSeq.indexOf(preCheckName)
          //println(splitPoint)
          val oldSeqTuple = oldNameSeq.splitAt(splitPoint + 1)
          //println(oldSeqTuple._1)
          (oldSeqTuple._1 :+ checkName) ++: oldSeqTuple._2
        } else {
          checkName +: oldNameSeq
        }
      } else {
        checkName +: oldNameSeq
      }
      newSeq
    } else {
      oldNameSeq
    }

    if (checkPoint + 1 < checkNameSeq.size) {
      createNameSeqOne(newNameSeq, checkPoint + 1, checkNameSeq)
    } else {
      newNameSeq
    }
  }

  // i番目の駅が着発なのか発だけなのか調べる
  // 1本でも着発の両方が設定されている列車のある駅は("着", "発")を返す
  // 最後の駅は("着", "")を返す
  // それ以外の駅は("発", "")を返す
  def checkStrEnd(
      i: Int,
      timeSeqSeq: Seq[Seq[(String, String)]]
  ): (String, String) = {
    // 検索用に2番目に時刻が入っていればtrue，それ以外はfalseが入ったSeqを作っておく
    val checkStrSeq = for (timeSeq <- timeSeqSeq) yield {
      if (timeSeq(i)._2 != "") { true }
      else { false }
    }
    if (checkStrSeq.contains(true)) { ("着", "発") }
    else {
      if (i == timeSeqSeq.head.size - 1) { ("着", "") }
      else { ("発", "") }
    }
  }

  /*
   * 1列車の停車駅と時刻のリスト
   * name, time
   */
  def getOnePage(uri: String): Seq[(String, String)] = {
    Thread.sleep(500)
    //val urlHead = uri.split("/").init.mkString("/")
    val doc = getData(uri)

    //val divEle = doc.getElementById("stoplist-matrix")
    val divEle = doc.getElementsByClass("stops-area")
    val tableEles = divEle.asScala.head.children

    val nameTimeTupleBuf = for (tableEle <- tableEles.asScala) yield {
      val name = tableEle.getElementsByClass("station-name").text
      //println(name)
      //val time = tableEle.getElementsByClass("time").text.replace("発", "").replace("着", "")
      val time = if (tableEle.getElementsByClass("time").size > 0) {
        tableEle
          .getElementsByClass("time")
          .text
          .replace("発", "")
          .replace("着", "")
      } else {
        tableEle
          .getElementsByClass("from-to-time")
          .text
          .replace("発", "")
          .replace("着", "")
      }
      //println(name + "," + time)
      (name, time)
    }
    nameTimeTupleBuf.toSeq
  }

  @tailrec
  def getData(uri: String): Document = {
    try {
      Jsoup.connect(uri).get
    } catch {
      case e: Throwable =>
        println(e)
        getData(uri)
    }
  }
}
