package getdata

import org.cai.store.StoreSequence.{findConsecutiveLowerCasePositions, removeAndRecord, removeAndRecordN}
import org.cai.utils.file.{FileNormalize, FileProcess}
import org.junit.jupiter.api.Test

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.util.Random

/**
 * @author cai584770
 * @date 2024/8/6 15:54
 * @Version
 */
class CreateTranslateData {
  val hg38_chr1 = "D:\\GithubRepository\\BioSequence\\src\\test\\getdata\\data\\chr1.fa"

  @Test
  def create():Unit={
    val (information, data) = FileProcess.getInformationAndSequence(hg38_chr1)

    val sequence = FileNormalize.remove(data)

    val (noLowerCaseSequence, lowerCaseList) = findConsecutiveLowerCasePositions(sequence)
    val (noNSequence, nCaseList) = removeAndRecordN(noLowerCaseSequence)
    val (agctSequence, otherCaseList) = removeAndRecord(noNSequence)

    println(agctSequence.length)

    val lengths = List(10, 100, 1000, 10000, 100000, 1000000, 10000000)

    extractAndSave(agctSequence, lengths)

  }

  def extractAndSave(sequence: String, lengths: List[Int]): Unit = {
    val rand = new Random

    lengths.foreach { length =>
      if (length <= sequence.length) {
        val startPos = rand.nextInt(sequence.length - length + 1)
        val subSeq = sequence.substring(startPos, startPos + length)

        val fileName = s"./data/${length}.fa"
        createDirectoriesIfNotExist(fileName)
        val file = new File(fileName)
        val writer = new PrintWriter(file)

        writer.println(s">$length")
        writer.println(subSeq)

        writer.close()
        println(s"Saved sequence of length $length to $fileName")
      } else {
        println(s"Requested length $length is greater than the available sequence length.")
      }
    }
  }

  def createDirectoriesIfNotExist(filePath: String): Unit = {
    val directory = Paths.get(filePath).getParent
    if (Files.notExists(directory)) {
      Files.createDirectories(directory)
    }
  }
}
