package translate

import getdata.DataPath
import org.cai.store.StoreSequence
import org.cai.store.StoreSequence.{findConsecutiveLowerCasePositions, removeAndRecord, removeAndRecordN}
import org.cai.translate.{Encoding, TranslateTools}
import org.cai.utils.file.{FileNormalize, FileProcess}
import org.junit.jupiter.api.Test


/**
 * @author cai584770
 * @date 2024/8/6 12:38
 * @Version
 */
class TranslateTest {
  private val dnaSequence = "ggaagatgacgtggtagttgtcgcggcagctgccaggagaagtagcaagaaaaataacatgataattatcacgacaactacctggtgatgttgctagtaatattacttgttatttttctcgtcatcttcccggcgacgtcgccagcaacatcacctgctacttctcccgccacctccc"

  @Test
  def translate(): Unit = {
//    DataPath.hg38_10, DataPath.hg38_100, DataPath.hg38_1000, DataPath.hg38_10000, DataPath.hg38_100000, DataPath.hg38_1000000
    val filePath = DataPath.hg38_100000

    val (information, data) = FileProcess.getInformationAndSequence(filePath)

    val sequence = FileNormalize.remove(data)

    val (noLowerCaseSequence, lowerCaseList) = findConsecutiveLowerCasePositions(sequence)
    val (noNSequence, nCaseList) = removeAndRecordN(noLowerCaseSequence)
    val (agctSequence, otherCaseList) = removeAndRecord(noNSequence)

    val (sup, ba) = StoreSequence.to2bit(agctSequence)

    val t1 = System.currentTimeMillis()
    val protein = TranslateTools.translate(ba)
    println(s",file:${filePath},runtime:${System.currentTimeMillis() - t1}")
  }

}
