import scala.io.Source
/**
 * Created by yosuke on 11/1/15.
 * read HiC data from File
 */
class ReadHiC(datasetPath : String, chr : String, res : Int) {
  private val path = Array(datasetPath, res2resstr(res) + "_resolution_intrachromosomal", chr, "MAPQGE30").mkString("/")

  /* read various normalize vectors*/
  private val KRnorm = readNorm("KR")
  private val VCnorm = readNorm("VC")
  private val SQRTVCnorm = readNorm("SQRTVC")

  /* read various expectation values*/
  private val RAWexpected = readExpected("RAW")
  private val KRexpected = readExpected("KR")
  private val VCexpected = readExpected("VC")
  private val SQRTVCexpected = readExpected("SQRTVC")

  /* read raw contact frequency matrix */
  private val RAWobserved = readRawObserved

  println(s"Hi-C Data is loaded from $path")


  def this(fpath : String, chr : Int, res : Int) = {
    this(fpath, s"chr$chr", res)
  }

  def data(normMethod : Option[String] = None,
           expectedMethod : Option[String] = None,
           minInterval : Int = 0,
           //maxInterval : Int = Integer.MAX_VALUE) = {
           maxInterval : Int = Integer.MAX_VALUE) : scala.collection.parallel.mutable.ParArray[Option[(Int, Int, Double)]] = {

    val normv = getNormVector(normMethod)
    val expectedv = getExpectedVector(expectedMethod)

    RAWobserved.map {
      case (i, j, m) if m.isNaN => None
      case (i, j, m) if Math.abs(i - j) > maxInterval => None
      case (i, j, m) if Math.abs(i - j) < minInterval => None
      case (i : Int, j: Int, m: Double) =>
        normv match {
          case Some(norm) =>
            expectedv match {
              case Some(expected) => Some((i, j, m / (norm(i / res) * norm(j / res) * expected(Math.abs(i -j) / res))))
              case None           => Some((i, j, m / (norm(i / res) * norm(j / res))))
            }
          case None =>
            expectedv match {
              case Some(expected) => Some((i, j, m / expected(Math.abs(i -j) / res)))
              case None           => Some((i, j, m))
            }
        }
      case _ => throw new Exception("Wrong input in RAW observed file")
    }

  }

  private def getNormVector(method :Option[String]) : Option[Array[Double]] = {
    method match {
      case Some(m) =>
        m match {
          case "KR"     => Some(KRnorm)
          case "VC"     => Some(VCnorm)
          case "SQRTVC" => Some(SQRTVCnorm)
          case _ => throw new NormMethodException(m)
        }
      case None    => None
    }
  }

  private def getExpectedVector(method :Option[String]) : Option[Array[Double]] = {
    method match {
      case Some(e) =>
        e match {
          case "RAW"    => Some(RAWexpected)
          case "KR"     => Some(KRexpected)
          case "VC"     => Some(VCexpected)
          case "SQRTVC" => Some(SQRTVCexpected)
          case _ => throw new ExpectedMethodException(e)
        }
      case None    => None
    }
  }


  private def readRawObserved = {
    val f = Source.fromFile(Array(path, "/", chr, "_", res2resstr(res), ".RAWobserved").mkString(""))
    val buf = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, Double)]
    try{
      for (line <- f.getLines()) {
        val lineElements = line split "\t"
        buf.append((lineElements(0).toInt, lineElements(1).toInt, lineElements(2).toDouble))
      }
    } finally {
      f.close()
    }
    buf.toArray.par
  }

  private def read(method : String, dataType : String) = {
    /* read normalization vector / expected value */
    val f = Source.fromFile(Array(path, "/", chr, "_", res2resstr(res), ".", method, dataType).mkString(""))
    val buf = scala.collection.mutable.ArrayBuffer.empty[Double]
    try{
      for (line <- f.getLines()) {
        buf.append(line.toDouble)
      }
    } finally {
      f.close()
    }
    buf.toArray
  }

  private def readNorm(method : String) = read(method, "norm")

  private def readExpected(method : String) = read(method, "expected")

  private def res2resstr(res : Int) : String = {
    res match {
      case 1000    => "1kb"
      case 5000    => "5kb"
      case 10000   => "10kb"
      case 25000   => "25kb"
      case 50000   => "50kb"
      case 100000  => "100kb"
      case 250000  => "250kb"
      case 500000  => "500kb"
      case 1000000 => "1Mb"
      case _ => throw new ResSizeException(res)
    }
  }

}
